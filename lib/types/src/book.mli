(** A read-only snapshot of an order book.

    Contains the symbol, all resting price levels on each side (aggregated by
    price), and the BBO. *)

open! Core

type t =
  { symbol : Symbol_id.t
  ; bids : Level.t list
  ; asks : Level.t list
  ; bbo : Bbo.t
  }
[@@deriving sexp, bin_io]

(** Render the book. The symbol is carried as an id, and this module has no
    way to turn one into a name, so the caller supplies the resolver: pass
    {!Symbol_id.to_string} to print the raw id, or a directory-backed lookup
    to print ["AAPL"]. That keeps this module pure data — it never needs to
    know a symbol registry exists. *)
val to_string : symbol_to_string:(Symbol_id.t -> string) -> t -> string
