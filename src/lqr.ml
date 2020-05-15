open Owl
module AD = Algodiff.D

type t =
  { x : AD.t
  ; u : AD.t
  ; a : AD.t
  ; b : AD.t
  ; rlx : AD.t
  ; rlu : AD.t
  ; rlxx : AD.t
  ; rluu : AD.t
  ; rlux : AD.t
  }

let backward flxx flx tape =
  let n = AD.(shape flx).(1) in
  let kf = List.length tape in
  let k, _, _, df1, df2, acc =
    let rec backward (delta, mu) (k, vxx, vx, df1, df2, acc) = function
      | { x; u; a; b; rlx; rlu; rlxx; rluu; rlux } :: tl ->
        let at = AD.Maths.transpose a in
        let bt = AD.Maths.transpose b in
        let qx = AD.Maths.(rlx + (vx *@ at)) in
        let qu = AD.Maths.(rlu + (vx *@ bt)) in
        let qxx = AD.Maths.(rlxx + (a *@ vxx *@ at)) in
        let quu = AD.Maths.(rluu + (b *@ vxx *@ bt)) in
        let qtuu = AD.Maths.(quu + (b *@ (AD.F mu * AD.Mat.(eye n)) *@ bt)) in
        if not (Owl.Linalg.D.is_posdef (AD.unpack_arr quu))
        then (
          Printf.printf "NOT POSDEF\n%!";
          backward
            (Regularisation.increase (delta, mu))
            (kf - 1, flxx, flx, AD.F 0., AD.F 0., [])
            tape)
        else (
          let qux = AD.Maths.(rlux + (b *@ vxx *@ at)) in
          let qtux = AD.Maths.(qux + (b *@ (AD.F mu * AD.Mat.(eye n)) *@ at)) in
          let _K = AD.Linalg.(linsolve qtuu qtux) |> AD.Maths.transpose |> AD.Maths.neg in
          let _k =
            AD.Linalg.(linsolve qtuu (AD.Maths.transpose qu))
            |> AD.Maths.transpose
            |> AD.Maths.neg
          in
          let vxx = AD.Maths.(qxx + transpose (_K *@ qux)) in
          let vx = AD.Maths.(qx + (qu *@ transpose _K)) in
          let acc = (x, u, (_K, _k)) :: acc in
          let df1 = AD.Maths.(df1 + sum' (_k *@ quu *@ transpose _k)) in
          let df2 = AD.Maths.(df2 + sum' (_k *@ transpose quu)) in
          backward
            (Regularisation.decrease (delta, mu))
            (k - 1, vxx, vx, df1, df2, acc)
            tl)
      | [] -> k, vxx, vx, df1, df2, acc
    in
    backward (1., 0.) (kf - 1, flxx, flx, AD.F 0., AD.F 0., []) tape
  in
  assert (k = -1);
  acc, (AD.unpack_flt df1, AD.unpack_flt df2)
