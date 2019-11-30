open! Core
open! Async
open! Import

module T = struct
  type t =
    { x : int
    ; y : int
    }
  [@@deriving compare, hash, sexp_of]
end

include T
include Hashable.Make_plain (T)
