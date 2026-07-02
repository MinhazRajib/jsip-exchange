(* "open" lines import tools so we can use short names. *)
open! Core
open Jsip_types
open Jsip_scenario_runner

(* Nicknames for two long module names. *)
module Cancel_storm_bot = Jsip_bots.Cancel_storm
module Fundamental_oracle = Jsip_fundamental.Fundamental_oracle

(* The name you type after "-scenario" to run this. *)
let name = "cancel-storm"

(* One-line description shown in the help text. *)
let description =
  "A crowd of cancel-storm bots hammering the submit/accept/cancel path on \
   one symbol."
;;

(* The one stock we trade, and its starting "true" price ($150.00). *)
let aapl = Symbol.of_string "AAPL"
let symbols = [ aapl ]
let fair_value_cents = 15000

(* How often each bot's timer fires: every 100 milliseconds. *)
let tick_interval = Time_ns.Span.of_ms 100.

(* The "true price" engine. We keep the price flat (no ups and downs) so the
   bots' orders stay safely away from trading and just rest on the book. *)
let oracle_config : Fundamental_oracle.Config.t =
  Symbol.Map.of_alist_exn
    [ ( aapl
      , { Fundamental_oracle.Config.initial_price_cents = fair_value_cents
        ; volatility_cents_per_sec = 0.0
        ; mean_reversion_strength = 0.0
        ; tick_interval
        } )
    ]
;;

(* Build one cancel-storm bot. Each one gets its own name, random seed, and id
   generator, so the copies run independently. 50 steps every 100ms is about
   500 order+cancel pairs per second per bot — a real storm. *)
let storm_bot ~participant ~rng_seed : Bot_spec.t =
  T
    { bot = (module Cancel_storm_bot)
    ; config =
        { Cancel_storm_bot.Config.symbols
        ; cycles_per_tick = 50
        ; order_size = 10
        ; price_offset_cents = 100
        ; client_order_ids = Client_order_id.Generator.create ()
        }
    ; participant = Participant.of_string participant
    ; symbols
    ; rng_seed
    ; tick_interval
    ; is_marketdata_consumer = false
    }
;;

(* The full recipe for the scenario: the stock, the price engine, no news
   events, and three cancel-storm bots running at once. *)
let configure () : Scenario_config.t =
  { name
  ; symbols
  ; oracle_config
  ; news = []
  ; bots =
      [ storm_bot ~participant:"Storm-1" ~rng_seed:1
      ; storm_bot ~participant:"Storm-2" ~rng_seed:2
      ; storm_bot ~participant:"Storm-3" ~rng_seed:3
      ]
  }
;;
