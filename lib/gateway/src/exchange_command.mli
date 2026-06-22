(** Exchange commands for centralizing command parsing

    Centralizing implementation of command-line interfaces, handling buy/sell
    commands and book and subscribe commands, replacing the protocol.ml and
    app/client/bin/main.ml modules *)

open! Core
open Jsip_types

type t =
  | Submit of Order.Request.t
  | Book of Symbol.t
  | Subscribe of Symbol.t

type verb =
  | Buy
  | Sell
  | Book
  | Subscribe
[@@deriving string]

(** [{Command}] *)

(** takes in an input are parses it into a Verb.t *)
val parse : ?default_participant:Participant.t -> string -> t Or_error.t
