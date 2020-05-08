open Owl
module AD = Algodiff.D

type t =
  { x : AD.t
  ; u : AD.t
  ; a : AD.t
  ; b : AD.t
  ; lx : AD.t
  ; lu : AD.t
  ; lxx : AD.t
  ; luu : AD.t
  ; lux : AD.t
  }

val backward
  :  AD.t
  -> AD.t
  -> t list
  -> (AD.t * AD.t * (AD.t * AD.t)) list * (float * float)
