(** The mapping between symbol names and the integer ids that travel on the
    wire.

    Symbols cross the network as {!Jsip_types.Symbol_id.t} ints, but humans
    type and read names like ["AAPL"]. The directory is what lets both be
    true. It is built once, authoritatively, by the exchange from the symbol
    list it is started with — the [i]th symbol gets the id [i] — and served
    to clients over {!Rpc_protocol.symbol_directory_rpc}. Each client fetches
    it at connect and keeps a local mirror.

    Two lookups, at two different boundaries:

    - {!find_id} resolves a name to an id when a human types one
      ([BUY AAPL 100] becomes id 0). This is the {b parse} boundary.
    - {!name} resolves an id back to a name when something is displayed. This
      is the {b render} boundary.

    Note the exchange itself never needs {!name}: it does not render symbols,
    so it only serves the directory rather than consulting it. Turning an id
    back into text is entirely a consumer's job. *)

open! Core
open Jsip_types

type t

(** Build the authoritative directory, giving the [i]th symbol the id [i].
    This is what the exchange does at startup, and it is the definition of
    what a symbol id means. Raises if a symbol appears twice. *)
val of_symbols : Symbol.t list -> t

(** Rebuild a directory from the pairs served over the wire. Used by clients
    to mirror the exchange's mapping. Raises if the pairs contain a duplicate
    name or id. *)
val of_alist_exn : (Symbol.t * Symbol_id.t) list -> t

(** The (name, id) pairs, in id order. This is what the directory RPC ships. *)
val to_alist : t -> (Symbol.t * Symbol_id.t) list

(** The id this exchange uses for [name], or [None] if it does not trade it.
    Used at the parse boundary, so an unknown name is caught before an order
    is ever sent. *)
val find_id : t -> Symbol.t -> Symbol_id.t option

(** The name for an id, for display. Falls back to printing the id itself if
    the directory has never heard of it — a stale mirror should garble a
    label, not crash a client mid-render. *)
val name : t -> Symbol_id.t -> string
