open Owl
module AD = Algodiff.D

module Fl = struct
  type s =
    { q : AD.t
    ; x : AD.t
    }

  type t = s option

  let create ?qx () =
    match qx with
    | Some (q, x) -> Some { q; x }
    | None        -> None
end

module Rl = struct
  type t =
    { q : AD.t option
    ; r : AD.t option
    }

  let create ?q ?r () = { q; r }
end

module type P = sig
  val n : int
  val m : int
  val dyn : u:AD.t -> x:AD.t -> AD.t
  val final_loss : Fl.t
  val running_loss : Rl.t
end

module Make (P : P) = struct
  include P

  let jac_u ~x ~u = AD.jacobian (fun u -> dyn ~x ~u) u |> AD.Maths.transpose
  let jac_x ~x ~u = AD.jacobian (fun x -> dyn ~x ~u) x |> AD.Maths.transpose

  let forward x0 us =
    List.fold_left
      (fun (x, xs, us) u ->
        let xs = x :: xs in
        let us = u :: us in
        let x = dyn ~x ~u in
        x, xs, us)
      (x0, [], [])
      us


  let add_option x y =
    match y with
    | Some y -> AD.Maths.(x + y)
    | None   -> x


  let update x0 us =
    (* xf, xs, us are in reverse *)
    let xf, xs, us = forward x0 us in
    let _, _, acc =
      let vxx, vx =
        match final_loss with
        | Some fl -> fl.q, AD.Maths.((xf - fl.x) *@ fl.q)
        | None    -> AD.Mat.zeros n n, AD.Mat.zeros 1 n
      in
      List.fold_left2
        (fun (vxx, vx, acc) x u ->
          let a = jac_x ~x ~u in
          let b = jac_u ~x ~u in
          let quu = add_option AD.Maths.(b *@ vxx *@ transpose b) running_loss.r in
          let kv = AD.(Maths.transpose (Linalg.linsolve quu b)) in
          let ku =
            match running_loss.r with
            | Some r -> AD.(Maths.(transpose (Linalg.linsolve quu r)))
            | None   -> AD.Mat.zeros 1 m
          in
          let _K = AD.Maths.(a *@ vxx *@ kv) in
          let _k = AD.Maths.((vx *@ kv) + (u *@ ku)) in
          let acl = AD.Maths.(a - (_K *@ b)) in
          let vxx = add_option AD.Maths.(acl *@ vxx *@ transpose a) running_loss.q in
          let vx =
            let v =
              match running_loss.r with
              | Some r -> AD.Maths.((vx *@ transpose acl) - (u *@ r *@ transpose _K))
              | None   -> AD.Maths.(vx *@ transpose acl)
            in
            match running_loss.q with
            | Some q -> AD.Maths.(v + (x *@ q))
            | None   -> v
          in
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
            let du = AD.Maths.(neg (dx *@ _K) - (AD.F alpha * _k)) in
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
    let final =
      match final_loss with
      | Some fl ->
        let dxf = AD.Maths.(xf - fl.x) in
        AD.Maths.(sum' (dxf *@ fl.q *@ transpose dxf))
      | None    -> AD.F 0.
    in
    let xcost =
      match running_loss.q with
      | Some q ->
        let xs = xs |> Array.of_list |> AD.Maths.concatenate ~axis:0 in
        AD.Maths.(sum' (xs *@ q * xs))
      | None   -> AD.F 0.
    in
    let ucost =
      match running_loss.r with
      | Some r ->
        let us = us |> Array.of_list |> AD.Maths.concatenate ~axis:0 in
        AD.Maths.(sum' (us *@ r * us))
      | None   -> AD.F 0.
    in
    AD.Maths.(F 0.5 * (final + xcost + ucost)) |> AD.unpack_flt


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