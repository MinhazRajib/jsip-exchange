(** Central event-routing component for the gateway.

    Owns subscription registries:

    - **Market-data subscribers**, held in an array indexed by [Symbol_id.t].
      Each subscriber gets a pipe of [Best_bid_offer_update] and
      [Trade_report] events for the symbol they asked about. This is the
      public market-data feed.

    - **Audit subscribers**, an unfiltered firehose of every event the
      matching engine produces. Intended for the exchange operator's monitor;
      not appropriate to expose to ordinary clients.

    [dispatch] is the single place that decides "for each event, who gets
    it". *)

open! Core
open! Async
open Jsip_types

type t

(** Create a dispatcher for an exchange trading [num_symbols] symbols, whose
    ids are [0] through [num_symbols - 1]. The count is fixed here because
    market-data subscribers live in an array indexed by symbol id. *)
val create : num_symbols:int -> t

(** Subscribe to public market data for one or more symbol ids. The same pipe
    receives events for every requested symbol; the dispatcher avoids
    duplicates so a subscriber listed against multiple symbols only sees each
    event once. The pipe is removed from the dispatcher when its reader is
    closed.

    Returns an error, and subscribes to nothing, if any requested id names no
    symbol this exchange trades. Ids arrive over the wire, so they cannot be
    trusted: rejecting outright stops a client from mistyping an id and
    silently hearing nothing, and stops it from making the server hold state
    for symbols that do not exist. *)
val subscribe_market_data
  :  t
  -> Symbol_id.t list
  -> Exchange_event.t Pipe.Reader.t Or_error.t

(** Subscribe to the full unfiltered event firehose. Intended for the monitor
    / admin tools. *)
val subscribe_audit : t -> Exchange_event.t Pipe.Reader.t

(** setup / remove a session from the participant table in Dispatcher *)
val clean_up_session : t -> Session.t -> unit Deferred.t

val set_up_session : t -> Participant.t -> unit Deferred.t

(** Route each event to every interested subscriber:

    - Every event is pushed to every audit subscriber.
    - [Best_bid_offer_update] and [Trade_report] are pushed to the
      market-data subscribers that asked for the event's symbol.
    - [Order_accept], [Order_cancel], and [Order_reject] are pushed to the
      session of the order's owning participant (if logged in).
    - [Fill] is pushed to both the aggressor's and the resting party's
      session (if either is logged in).

    Each session lookup is O(1) and independent of subscriber count. *)
val dispatch : t -> Exchange_event.t list -> unit

module For_testing : sig
  val audit_subscriber_count : t -> int
end

(** helpers for login *)
val valid_participant : t -> Participant.t -> bool

val get_session : t -> Participant.t -> Session.t option
