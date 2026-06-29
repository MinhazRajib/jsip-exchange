open! Core

type t [@@deriving sexp, bin_io, compare, equal, hash]

include Comparable.S with type t := t
include Hashable.S with type t := t

module Generator : sig
  type client_order_id := t
  type t [@@deriving sexp_of]

  val create : unit -> t
  val next : t -> client_order_id
end

val to_int : t -> int
val of_int : int -> t
