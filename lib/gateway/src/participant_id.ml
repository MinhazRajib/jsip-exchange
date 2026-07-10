open! Core

type t = int [@@deriving compare, equal, hash, sexp_of]

include functor Hashable.Make_plain

let of_int = Fn.id
