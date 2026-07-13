open! Core
open Jsip_types

type t =
  { name : string
  ; symbols : Symbol.t list
  (** Symbol {e names}, used to start the exchange. The server assigns the
      [i]th name the id [i]; everything downstream (bots, the oracle, the
      wire) speaks in those ids. *)
  ; oracle_config : Jsip_fundamental.Fundamental_oracle.Config.t
  ; news : Jsip_news_injector.News_injector.Event.t list
  ; bots : Bot_spec.t list
  }
