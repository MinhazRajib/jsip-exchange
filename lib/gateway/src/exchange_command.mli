(** Exchange commands for centralizing command parsing

    Centralizing implementation of command-line interfaces, handling buy/sell
    commands and book and subscribe commands *)

open! Core
open Jsip_types

type t =
  | Submit of Order.Request.t
  | Book of Symbol.t
  | Subscribe of Symbol.t
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
    default_participant for clients that already know their identity *)
val parse : ?default_participant:Participant.t -> string -> t Or_error.t
