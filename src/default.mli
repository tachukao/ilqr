open Owl
module AD = Algodiff.D

type dyn = k:int -> x:AD.t -> u:AD.t -> AD.t
type final_loss = k:int -> x:AD.t -> AD.t
type running_loss = k:int -> x:AD.t -> u:AD.t -> AD.t

val forward_for_backward
  :  dyn:dyn
  -> running_loss:running_loss
  -> final_loss:final_loss
  -> unit
  -> AD.t
  -> AD.t list
  -> AD.t * AD.t * Lqr.t list

module type P = sig
  val n : int
  val m : int
  val dyn : dyn
  val final_loss : final_loss
  val running_loss : running_loss
end

module Make (P : P) : sig
  val trajectory : AD.t -> AD.t list -> AD.t
  val loss : AD.t -> AD.t list -> float
  val learn : stop:(int -> AD.t list -> bool) -> AD.t -> AD.t list -> AD.t list
end
