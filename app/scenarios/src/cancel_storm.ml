open! Core
open Jsip_types
open Jsip_scenario_runner
module Cancel_storm_bot = Jsip_bots.Cancel_storm
module Fundamental_oracle = Jsip_fundamental.Fundamental_oracle

let name = "cancel-storm"

let description =
  "A crowd of cancel-storm bots hammering the submit/accept/cancel path on \
   one symbol."
;;

let aapl = Symbol.of_string "AAPL"
let symbols = [ aapl ]
let fair_value_cents = 15000
let tick_interval = Time_ns.Span.of_ms 100.

(* A flat fundamental: the storm doesn't need price movement, and a steady
   fair value keeps its passive orders reliably non-marketable. *)
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

(* Each instance gets its own participant, RNG seed, and id generator so the
   copies are independent. 50 cycles every 100ms is ~500 submit+cancel pairs
   per second per bot — a real storm, not a trickle. *)
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
