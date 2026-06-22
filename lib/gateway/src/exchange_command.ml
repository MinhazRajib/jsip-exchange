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
[@@deriving string ~case_insensitive ~capitalize:"SCREAMING_SNAKE_CASE"]

(** Split the line on spaces, take the first word. Parse it as a Verb.t. If
    it fails, return Error. Match on the verb to parse the remaining
    arguments: Buy | Sell: parse symbol, size, price, time-in-force,
    participant (move this logic from Protocol.parse_command). Book |
    Subscribe: parse a required symbol argument. *)
let parse ?default_participant:participant line =
  let line_stripped = String.strip line in
  match line with _ -> Sell
;;
