open! Core
open! Async
open! Import

exception End_of_combat

type t

val create : string list -> t

(** raises {!End_of_combat} if a unit cannot find a target *)
val perform_round : t -> unit

val sum_of_hit_points : t -> int
