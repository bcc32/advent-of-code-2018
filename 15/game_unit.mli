open! Core
open! Async
open! Import

type t

[@@deriving sexp_of]
val create : loc:Loc.t -> hit_points:int -> attack_power:int -> team:Team.t -> t
val loc : t -> Loc.t
val set_loc : t -> Loc.t -> unit
val hit_points : t -> int
val attack_power : t -> int
val team : t -> Team.t
val receive_damage : t -> points:int -> [ `Dead | `Alive ]
val is_alive : t -> bool
