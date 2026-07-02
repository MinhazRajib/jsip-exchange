(** Tests for the market maker.

    The bot's real behaviour ([seed_book] / [run]) needs a live server, so
    this only checks that a [Config.t] can be built and inspected. Behavioural
    coverage is a TODO. *)

open! Core
open Jsip_market_maker.Market_maker

let aapl = Jsip_test_harness.Harness.aapl
let market_maker = Jsip_test_harness.Harness.market_maker

let make_config () : Config.t =
  { participant = market_maker
  ; symbol = aapl
  ; fair_value_cents = 15000
  ; half_spread_cents = 10
  ; size_per_level = 100
  ; num_levels = 3
  }
;;

let%expect_test "config builds and round-trips through sexp" =
  print_s [%sexp (make_config () : Config.t)];
  [%expect
    {|
    ((participant Market_Maker) (symbol AAPL) (fair_value_cents 15000)
     (half_spread_cents 10) (size_per_level 100) (num_levels 3))
    |}]
;;
