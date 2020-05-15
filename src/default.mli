open Owl
module AD = Algodiff.D

type t = k:int -> x:AD.t -> u:AD.t -> AD.t
type s = k:int -> x:AD.t -> AD.t
type final_loss = k:int -> x:AD.t -> AD.t
type running_loss = k:int -> x:AD.t -> u:AD.t -> AD.t

val forward_for_backward
  :  ?dyn_x:t
  -> ?dyn_u:t
  -> ?rl_uu:t
  -> ?rl_xx:t
  -> ?rl_ux:t
  -> ?rl_u:t
  -> ?rl_x:t
  -> ?fl_xx:s
  -> ?fl_x:s
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
  val rl_uu : t option
  val rl_xx : t option
  val rl_ux : t option
  val rl_u : t option
  val rl_x : t option
  val fl_xx : s option
  val fl_x : s option
end

module Make (P : P) : sig
  val trajectory : AD.t -> AD.t list -> AD.t
  val loss : AD.t -> AD.t list -> float
  val learn : stop:(int -> AD.t list -> bool) -> AD.t -> AD.t list -> AD.t list
end
