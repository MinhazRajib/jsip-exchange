(** The matching engine: receives order requests, manages order books, and
    produces exchange events.

    The engine is the heart of the exchange. It assigns order IDs, determines
    which orders can trade against each other, executes fills, and manages
    the lifecycle of resting orders. *)

open! Core
open Jsip_types

type t [@@deriving sexp_of]

(** Create a matching engine for the given symbols. Each symbol gets its own
    order book. *)
val create : Symbol.t list -> t

(** {2 Order submission} *)

(** Submit a new order request. Returns the list of exchange events produced:
    an acceptance or rejection, followed by any fills, and possibly a
    cancellation of unfilled remainder (for IOC orders).

    The event list is always non-empty (at minimum an acceptance or
    rejection). *)
val submit : t -> Order.Request.t -> Exchange_event.t list

(** {2 Queries} *)

(** The order book for a given symbol, or [None] if the symbol is not traded
    on this engine. *)
val book : t -> Symbol.t -> Order_book.t option

(** helpers for client order id *)
val check_client_order_id
  :  t
  -> Participant.t
  -> Client_order_id.t
  -> Order.t option

(* User submits a reques to cancel an event. Returns the list of exchange
   events produced: an order_cancel or cancel_rejection, followed by any bbo
   changes.

   The event list is always non-empty (at minimum an cancel or rejection) *)
val cancel : t -> Participant.t -> Client_order_id.t -> Exchange_event.t list
