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

module Make (P : P) : sig
  val trajectory : AD.t -> AD.t list -> AD.t
  val loss : AD.t -> AD.t list -> float
  val learn : stop:(int -> AD.t list -> bool) -> AD.t -> AD.t list -> AD.t list
end
