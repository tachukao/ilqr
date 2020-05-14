open Owl
module AD = Algodiff.D

type t = k:int -> x:AD.t -> u:AD.t -> AD.t
type final_loss = k:int -> x:AD.t -> AD.t
type running_loss = k:int -> x:AD.t -> u:AD.t -> AD.t

val forward_for_backward
  :  ?dyn_x:t
  -> ?dyn_u:t
  -> ?l_uu:t
  -> ?l_xx:t
  -> ?l_ux:t
  -> ?l_u:t
  -> ?l_x:t
  -> dyn:t
  -> running_loss:running_loss
  -> final_loss:final_loss
  -> unit
  -> AD.t
  -> AD.t list
  -> AD.t * AD.t * Lqr.t list

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

module Make (P : P) : sig
  val trajectory : AD.t -> AD.t list -> AD.t
  val loss : AD.t -> AD.t list -> float
  val learn : stop:(int -> AD.t list -> bool) -> AD.t -> AD.t list -> AD.t list
end
