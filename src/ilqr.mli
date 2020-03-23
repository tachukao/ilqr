open Owl
module AD = Algodiff.D

val linsearch
  :  ?alpha:float
  -> ?alpha_min:float
  -> ?tau:float
  -> 'a
  -> (float -> 'a * 'b)
  -> 'b option

(* module final loss *)
module Fl : sig
  type t

  val create : ?qx:AD.t * AD.t -> unit -> t
end

(* module running loss *)
module Rl : sig
  type t

  val create : ?q:AD.t -> ?r:AD.t -> unit -> t
end

module type P = sig
  val n : int
  val m : int
  val dyn : u:AD.t -> x:AD.t -> AD.t
  val final_loss : Fl.t
  val running_loss : Rl.t
end

module Make (P : P) : sig
  val trajectory : AD.t -> AD.t list -> AD.t
  val loss : AD.t -> AD.t list -> float
  val learn : stop:(int -> AD.t list -> bool) -> AD.t -> AD.t list -> AD.t list
end
