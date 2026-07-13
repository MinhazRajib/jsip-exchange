(** A filterable, color-tagged log of exchange events for the monitor UI.

    A pure model — no Async, no Bonsai, no terminal primitives. The monitor's
    UI layer (built on bonsai_term) drives this model by feeding it events
    and setting filters, then renders [visible_styled_lines] to the screen.

    The model holds events in insertion order. [add_event] appends; queries
    return the active subset under the current [Filter.t]. *)

open! Core
open Jsip_types
open Jsip_gateway

(** Coarse-grained grouping of [Exchange_event.t] variants, suited for "show
    only orders" / "show only trades" / "show only market data" filters.
    [of_event] is the only classification helper — finer granularity is
    expressed by pattern-matching on the [Exchange_event.t] itself. *)
module Category : sig
  type t =
    | Order_lifecycle
    | Trade
    | Market_data
  [@@deriving sexp_of, compare, equal, enumerate]

  val to_string : t -> string
  val of_event : Exchange_event.t -> t
end

(** A foreground color attached to a rendered line. Matches the terminal
    primary palette so the bonsai_term layer can map it to [Attr.fg]
    directly. *)
module Color : sig
  type t =
    | Default
    | Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan
  [@@deriving sexp_of, compare, equal, enumerate]

  val to_string : t -> string

  (** Each [Exchange_event.t] constructor maps to a distinct [Color.t] so the
      event log is easy to scan. *)
  val of_event : Exchange_event.t -> t
end

(** A filter narrows the visible event set. Filters compose by intersection:
    an event is visible iff it passes every constraint. *)
module Filter : sig
  type t [@@deriving sexp_of]

  (** The identity filter: every event is visible. *)
  val all : t

  (** Show only events whose [Category.t] is in [categories]. *)
  val by_categories : Category.t list -> t

  (** Show only events whose rendered line contains [substring]
      (case-insensitive). *)
  val by_substring : string -> t

  (** [combine a b] returns a filter that requires both [a] and [b]. *)
  val combine : t -> t -> t

  (** Whether the filter would keep [event]. *)
  (** The substring predicate matches against the rendered line, so this
      needs the directory to turn symbol ids into the names a user types. *)
  val matches
    :  directory:Symbol_directory.t
    -> t
    -> Exchange_event.t
    -> bool
end

type t

val create : unit -> t

(** Append an event to the log. Also refreshes [current_bbos] when [event] is
    a [Best_bid_offer_update]. *)
val add_event : t -> Exchange_event.t -> t

(** Total number of events the log has seen, regardless of the current
    filter. *)
val event_count : t -> int

(** Most recently observed [Bbo.t] for each symbol that has produced a
    [Best_bid_offer_update] since the log was created. The map preserves the
    insertion order of first appearance: a symbol's slot is added the first
    time it produces a BBO and never reordered, even when later BBOs update
    its value. *)
val current_bbos : t -> (Symbol_id.t * Bbo.t) list

(** Replace the active filter. *)
val set_filter : t -> Filter.t -> t

(** The currently-active filter. *)
val filter : t -> Filter.t

(** Visible events in insertion order (oldest first). *)
val visible_events
  :  directory:Symbol_directory.t
  -> t
  -> Exchange_event.t list

(** Visible events rendered as text via [Format.format_event]. *)
val visible_lines : directory:Symbol_directory.t -> t -> string list

(** Visible events rendered as [(Color.t, line)] pairs, ready for a styled
    UI. *)
val visible_styled_lines
  :  directory:Symbol_directory.t
  -> t
  -> (Color.t * string) list
