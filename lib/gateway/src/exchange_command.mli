(** Exchange commands for centralizing command parsing

    Centralizing implementation of command-line interfaces, handling buy/sell
    commands and book and subscribe commands *)

open! Core
open Jsip_types

type t =
  | Submit of Order.Request.t
  | Book of Symbol_id.t
  | Subscribe of Symbol_id.t
  | Cancel of Client_order_id.t

type verb =
  | Buy
  | Sell
  | Book
  | Subscribe
  | Cancel
[@@deriving string]

(** [{Command}] *)

(** Parse a text command into an order request. Returns [Error] with a
    human-readable message if the input is malformed. Can set
    default_participant for clients that already know their identity.

    A human types a symbol's {b name} ([BUY 1 AAPL 100 150.00]) but the wire
    carries its {b id}, so parsing needs the [directory] to resolve one to
    the other. This is the parse boundary: a symbol the exchange does not
    trade fails here, before an order is ever sent. *)
val parse
  :  ?default_participant:Participant.t
  -> directory:Symbol_directory.t
  -> string
  -> t Or_error.t
