(** A compact integer id for a trading symbol.

    A {!Symbol.t} is a human-readable name like ["AAPL"]. A [Symbol_id.t] is
    the small integer the exchange uses in its place. The exchange fixes the
    list of symbols it trades at startup and gives the [i]th symbol the id
    [i], so an id is also an index into the matching engine's array of order
    books. That is what makes a symbol lookup an array index rather than a
    string hash.

    The id is the thing that travels on the wire, which is why this module
    sits beside the other wire types and derives [bin_io]. Contrast
    [Participant_id] over in the gateway: that id is interned server-side,
    never leaves the process, and so needs no [bin_io] at all.

    The type is [private int] rather than abstract on purpose. Code outside
    this module can read the integer inside an id — enough to bounds-check it
    — but cannot build an id except through {!of_int}. That bounds check is
    not optional. [bin_io] will happily decode whatever integer arrives over
    the network, so a buggy or hostile client can send an id that names no
    symbol. [Matching_engine.book] is where those ids get rejected. *)

open! Core

type t = private int [@@deriving sexp, bin_io, compare, equal, hash]

include Comparable.S with type t := t
include Hashable.S with type t := t

(** Build an id from an integer. This does {e not} check that the exchange
    trades a symbol with this id; only whoever owns the symbol list knows
    that. *)
val of_int : int -> t

val to_int : t -> int

(** Print the id as its integer, e.g. ["7"]. A consumer holding a symbol
    directory should resolve the id to a {!Symbol.t} and print the name
    instead. *)
val to_string : t -> string
