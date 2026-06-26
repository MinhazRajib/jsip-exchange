open! Core

module T = struct
  type t = int [@@deriving sexp, bin_io, compare, equal, hash, string]
end

include T
include Comparable.Make (T)
include Hashable.Make (T)

module Generator = struct
  type t = { mutable id : int } [@@deriving sexp_of]

  let create num = { id = num }
end

let to_int t = t
let of_int t = t
