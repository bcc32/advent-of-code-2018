open! Core
open! Async
open! Import

(** Compare in reading order. *)
type t =
  { x : int
  ; y : int
  }

include Hashable.S_plain with type t := t
