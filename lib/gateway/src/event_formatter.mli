(** Renders exchange events, books, and fills as human-readable text.

    On a production exchange this would be a binary protocol like FIX. We use
    a simple text format for ease of debugging and interactive use.

    {2 The render boundary}

    Symbols travel on the wire as {!Jsip_types.Symbol_id.t} ints, but people
    read names. Every function here takes the [directory] and resolves ids
    back to names, which makes this module the one place ids become text.

    It sits on the consumer's side of the boundary: the client and the
    monitor render, and so hold a directory; the exchange does not render,
    and so never needs one.

    {2 Command format}

    See {!Exchange_command.parse} for the input side of the same boundary,
    which turns a typed name back into an id:
    {v
    BUY  <client_id> <symbol> <size> <price> [<time_in_force>] [as <name>]
    SELL <client_id> <symbol> <size> <price> [<time_in_force>] [as <name>]
    BOOK <symbol>
    SUBSCRIBE <symbol>
    CANCEL <client_id>
    v}

    Examples:
    {v
    BUY 1 AAPL 100 150.25
    SELL 2 TSLA 50 200.00 IOC
    v} *)

open! Core
open Jsip_types

(** Format an exchange event as a single line of human-readable text. *)
val format_event : directory:Symbol_directory.t -> Exchange_event.t -> string

(** Format a list of events, one per line. *)
val format_events
  :  directory:Symbol_directory.t
  -> Exchange_event.t list
  -> string

(** Format a book snapshot, naming its symbol. *)
val format_book : directory:Symbol_directory.t -> Book.t -> string

(** Format a fill from one participant's point of view ("You bought 100 AAPL
    at $150.00"), or [None] if the fill does not involve them. *)
val format_fill_for_participant
  :  directory:Symbol_directory.t
  -> Fill.t
  -> Participant.t
  -> string option
