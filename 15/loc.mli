open! Core
open! Async
open! Import

(** Compare in reading order. *)
type t =
  { x : int
  ; y : int
  }
[@@deriving compare, equal, sexp_of]

include Hashable.S_plain with type t := t

val adjacent : t -> height:int -> width:int -> t list
