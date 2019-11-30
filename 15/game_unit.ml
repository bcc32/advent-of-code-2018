open! Core
open! Async
open! Import

type t =
  { mutable loc : Loc.t
  ; mutable hit_points : int
  ; attack_power : int
  ; team : Team.t
  }
[@@deriving fields]

let create = Fields.create

let receive_damage t ~points =
  t.hit_points <- t.hit_points - points;
  if t.hit_points <= 0 then `Dead else `Alive
;;
