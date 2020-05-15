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

val backward
  :  AD.t
  -> AD.t
  -> t list
  -> (AD.t * AD.t * (AD.t * AD.t)) list * (float * float)
