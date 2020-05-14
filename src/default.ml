open Owl
module AD = Algodiff.D

type t = k:int -> x:AD.t -> u:AD.t -> AD.t
type final_loss = k:int -> x:AD.t -> AD.t
type running_loss = k:int -> x:AD.t -> u:AD.t -> AD.t

let forward_for_backward
    ?dyn_x
    ?dyn_u
    ?l_uu
    ?l_xx
    ?l_ux
    ?l_u
    ?l_x
    ~dyn
    ~running_loss
    ~final_loss
    ()
  =
  let dyn_u =
    let default ~k ~x ~u = AD.jacobian (fun u -> dyn ~k ~x ~u) u |> AD.Maths.transpose in
    Option.value dyn_u ~default
  in
  let dyn_x =
    let default ~k ~x ~u = AD.jacobian (fun x -> dyn ~k ~x ~u) x |> AD.Maths.transpose in
    Option.value dyn_x ~default
  in
  let l_u =
    let default ~k ~x ~u = AD.grad (fun u -> running_loss ~k ~x ~u) u in
    Option.value l_u ~default
  in
  let l_x =
    let default ~k ~x ~u = AD.grad (fun x -> running_loss ~k ~x ~u) x in
    Option.value l_x ~default
  in
  let l_uu =
    let default ~k ~x ~u = AD.jacobian (fun u -> l_u ~k ~x ~u) u |> AD.Maths.transpose in
    Option.value l_uu ~default
  in
  let l_xx =
    let default ~k ~x ~u = AD.jacobian (fun x -> l_x ~k ~x ~u) x |> AD.Maths.transpose in
    Option.value l_xx ~default
  in
  let l_ux =
    let default ~k ~x ~u = AD.jacobian (fun x -> l_u ~k ~x ~u) x in
    Option.value l_ux ~default
  in
  fun x0 us ->
    let kf, xf, tape =
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
    in
    let vxxf, vxf =
      let g x = AD.grad (fun x -> final_loss ~k:kf ~x) x in
      AD.jacobian g xf |> AD.Maths.transpose, g xf
    in
    vxxf, vxf, tape


module type P = sig
  val n : int
  val m : int
  val dyn : t
  val final_loss : final_loss
  val running_loss : running_loss
  val dyn_x : t option
  val dyn_u : t option
  val l_uu : t option
  val l_xx : t option
  val l_ux : t option
  val l_u : t option
  val l_x : t option
end

module Make (P : P) = struct
  include P

  let forward x0 us =
    List.fold_left
      (fun (k, x, xs, us) u ->
        let xs = x :: xs in
        let us = u :: us in
        let x = dyn ~k ~x ~u in
        succ k, x, xs, us)
      (0, x0, [], [])
      us


  let update =
    let forward_for_backward =
      forward_for_backward
        ?dyn_x
        ?dyn_u
        ?l_uu
        ?l_xx
        ?l_ux
        ?l_u
        ?l_x
        ~dyn
        ~running_loss
        ~final_loss
        ()
    in
    fun x0 us ->
      (* xf, xs, us are in reverse *)
      let vxxf, vxf, tape = forward_for_backward x0 us in
      let acc, (df1, df2) = Lqr.backward vxxf vxf tape in
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
