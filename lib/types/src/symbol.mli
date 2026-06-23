(** A trading symbol identifying a financial instrument (e.g., "AAPL",
    "TSLA").

    A production exchange would support multiple asset classes with different
    symbology formats. We represent symbols as simple uppercase strings. *)

open! Core

type t [@@deriving sexp, bin_io, compare, equal, hash, string]

include Comparable.S with type t := t
include Hashable.S with type t := t

(** function of_string checks that symbol contains only uppercase
    alphanumeric characters

    Upon detected this is not true, function automatically uppercases values
    before passing. This was chosen in order to maintain module behavior, as
    prior module did not raise errors on this occurence, reducing the amount
    of changes needed due to this addition. *)
