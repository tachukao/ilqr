open Owl
module AD = Algodiff.D

type final_loss = x:AD.t -> AD.t
type running_loss = x:AD.t -> u:AD.t -> AD.t

module type P = sig
  val n : int
  val m : int
  val dyn : u:AD.t -> x:AD.t -> AD.t
  val final_loss : final_loss
  val running_loss : running_loss
end

module Make (P : P) = struct
  include P

  let dyn_u ~x ~u = AD.jacobian (fun u -> dyn ~x ~u) u |> AD.Maths.transpose
  let dyn_x ~x ~u = AD.jacobian (fun x -> dyn ~x ~u) x |> AD.Maths.transpose
  let l_u ~x ~u = AD.grad (fun u -> running_loss ~x ~u) u
  let l_x ~x ~u = AD.grad (fun x -> running_loss ~x ~u) x
  let l_uu ~x ~u = AD.jacobian (fun u -> l_u ~x ~u) u |> AD.Maths.transpose
  let l_xx ~x ~u = AD.jacobian (fun x -> l_x ~x ~u) x |> AD.Maths.transpose
  let l_ux ~x ~u = AD.jacobian (fun x -> l_u ~x ~u) x

  let forward x0 us =
    List.fold_left
      (fun (x, xs, us) u ->
        let xs = x :: xs in
        let us = u :: us in
        let x = dyn ~x ~u in
        x, xs, us)
      (x0, [], [])
      us


  let update x0 us =
    (* xf, xs, us are in reverse *)
    let xf, xs, us = forward x0 us in
    let _, _, acc =
      let vxx, vx =
        let g x = AD.grad (fun x -> final_loss ~x) x in
        AD.jacobian g xf |> AD.Maths.transpose, g xf
      in
      List.fold_left2
        (fun (vxx, vx, acc) x u ->
          let a = dyn_x ~x ~u in
          let at = AD.Maths.transpose a in
          let b = dyn_u ~x ~u in
          let bt = AD.Maths.transpose b in
          let lx = l_x ~x ~u
          and lu = l_u ~x ~u
          and lxx = l_xx ~x ~u
          and luu = l_uu ~x ~u
          and lux = l_ux ~x ~u in
          let qx = AD.Maths.(lx + (vx *@ at)) in
          let qu = AD.Maths.(lu + (vx *@ bt)) in
          let qxx = AD.Maths.(lxx + (a *@ vxx *@ at)) in
          let quu = AD.Maths.(luu + (b *@ vxx *@ bt)) in
          let qux = AD.Maths.(lux + (b *@ vxx *@ at)) in
          let _K = AD.Linalg.(linsolve quu qux) |> AD.Maths.transpose |> AD.Maths.neg in
          let _k =
            AD.Linalg.(linsolve quu (AD.Maths.transpose qu))
            |> AD.Maths.transpose
            |> AD.Maths.neg
          in
          let vxx = AD.Maths.(qxx + transpose (_K *@ qux)) in
          let vx = AD.Maths.(qx + (qu *@ transpose _K)) in
          let acc = (x, u, (_K, _k)) :: acc in
          vxx, vx, acc)
        (vxx, vx, [])
        xs
        us
    in
    fun alpha ->
      let _, _, uhats =
        List.fold_left
          (fun (i, xhat, uhats) (x, u, (_K, _k)) ->
            let dx = AD.Maths.(xhat - x) in
            let du = AD.Maths.((dx *@ _K) + (AD.F alpha * _k)) in
            let uhat = AD.Maths.(u + du) in
            let uhats = uhat :: uhats in
            let xhat = dyn ~x:xhat ~u:uhat in
            succ i, xhat, uhats)
          (0, x0, [])
          acc
      in
      List.rev uhats


  let trajectory x0 us =
    let xf, xs, _ = forward x0 us in
    let xs = List.rev xs |> Array.of_list |> AD.Maths.concatenate ~axis:0 in
    AD.Maths.concatenate ~axis:0 [| xs; xf |]


  let loss x0 us =
    let xf, xs, us = forward x0 us in
    let fl = final_loss ~x:xf in
    let rl =
      List.fold_left2 (fun rl x u -> AD.Maths.(rl + running_loss ~x ~u)) (AD.F 0.) xs us
    in
    AD.Maths.(fl + rl) |> AD.unpack_flt


  let learn ~stop x0 us =
    let rec loop iter us =
      if stop iter us
      then us
      else (
        let f0 = loss x0 us in
        let update = update x0 us in
        let f alpha =
          let us = update alpha in
          let fv = loss x0 us in
          fv, us
        in
        match Linesearch.backtrack f0 f with
        | Some us -> loop (succ iter) us
        | None    -> failwith "linesearch did not converge ")
    in
    loop 0 us
end
