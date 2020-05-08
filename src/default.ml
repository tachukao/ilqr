open Owl
module AD = Algodiff.D

type final_loss = k:int -> x:AD.t -> AD.t
type running_loss = k:int -> x:AD.t -> u:AD.t -> AD.t

module type P = sig
  val n : int
  val m : int
  val dyn : k:int -> u:AD.t -> x:AD.t -> AD.t
  val final_loss : final_loss
  val running_loss : running_loss
end

module Make (P : P) = struct
  include P

  let dyn_u ~k ~x ~u = AD.jacobian (fun u -> dyn ~k ~x ~u) u |> AD.Maths.transpose
  let dyn_x ~k ~x ~u = AD.jacobian (fun x -> dyn ~k ~x ~u) x |> AD.Maths.transpose
  let l_u ~k ~x ~u = AD.grad (fun u -> running_loss ~k ~x ~u) u
  let l_x ~k ~x ~u = AD.grad (fun x -> running_loss ~k ~x ~u) x
  let l_uu ~k ~x ~u = AD.jacobian (fun u -> l_u ~k ~x ~u) u |> AD.Maths.transpose
  let l_xx ~k ~x ~u = AD.jacobian (fun x -> l_x ~k ~x ~u) x |> AD.Maths.transpose
  let l_ux ~k ~x ~u = AD.jacobian (fun x -> l_u ~k ~x ~u) x

  let forward x0 us =
    List.fold_left
      (fun (k, x, xs, us) u ->
        let xs = x :: xs in
        let us = u :: us in
        let x = dyn ~k ~x ~u in
        succ k, x, xs, us)
      (0, x0, [], [])
      us


  let forward_for_backward x0 us =
    List.fold_left
      (fun (k, x, tape) u ->
        let a = dyn_x ~k ~x ~u
        and b = dyn_u ~k ~x ~u
        and lx = l_x ~k ~x ~u
        and lu = l_u ~k ~x ~u
        and lxx = l_xx ~k ~x ~u
        and luu = l_uu ~k ~x ~u
        and lux = l_ux ~k ~x ~u in
        let s = Lqr.{ x; u; a; b; lx; lu; lxx; luu; lux } in
        let x = dyn ~k ~x ~u in
        succ k, x, s :: tape)
      (0, x0, [])
      us


  let update x0 us =
    (* xf, xs, us are in reverse *)
    let kf, xf, tape = forward_for_backward x0 us in
    let acc, (df1, df2) =
      let vxxf, vxf =
        let g x = AD.grad (fun x -> final_loss ~k:kf ~x) x in
        AD.jacobian g xf |> AD.Maths.transpose, g xf
      in
      Lqr.backward vxxf vxf tape
    in
    fun alpha ->
      let _, _, uhats =
        List.fold_left
          (fun (k, xhat, uhats) (x, u, (_K, _k)) ->
            let dx = AD.Maths.(xhat - x) in
            let du = AD.Maths.((dx *@ _K) + (AD.F alpha * _k)) in
            let uhat = AD.Maths.(u + du) in
            let uhats = uhat :: uhats in
            let xhat = dyn ~k ~x:xhat ~u:uhat in
            succ k, xhat, uhats)
          (0, x0, [])
          acc
      in
      let df = (alpha *. df1) +. (0.5 *. alpha *. alpha *. df2) in
      List.rev uhats, df


  let trajectory x0 us =
    let _, xf, xs, _ = forward x0 us in
    let xs = List.rev xs |> Array.of_list |> AD.Maths.concatenate ~axis:0 in
    AD.Maths.concatenate ~axis:0 [| xs; xf |]


  let loss x0 us =
    let kf, xf, xs, us = forward x0 us in
    let fl = final_loss ~k:kf ~x:xf in
    let _, rl =
      List.fold_left2
        (fun (k, rl) x u -> pred k, AD.Maths.(rl + running_loss ~k ~x ~u))
        (kf - 1, AD.F 0.)
        xs
        us
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
          let us, df = update alpha in
          let fv = loss x0 us in
          fv, Some df, us
        in
        match Linesearch.backtrack f0 f with
        | Some us -> loop (succ iter) us
        | None    -> failwith "linesearch did not converge ")
    in
    loop 0 us
end
