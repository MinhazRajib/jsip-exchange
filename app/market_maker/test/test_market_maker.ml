(** Tests for the market maker, using a real exchange server. *)

open! Core
open! Async
open Jsip_test_harness
open Jsip_market_maker
open E2e_helpers

let default_config : Market_maker.Config.t =
  { participant = Harness.market_maker
  ; symbol = Harness.aapl
  ; fair_value_cents = 15000
  ; half_spread_cents = 10
  ; size_per_level = 100
  ; num_levels = 3
  }
;;

let%expect_test "seed_book: places symmetric bids and asks around fair value"
  =
  with_server ~symbols:[ Harness.aapl ] (fun ~server:_ ~port ->
    let%bind mm = connect_as ~port Harness.market_maker in
    let%bind () = Market_maker.seed_book default_config (connection mm) in
    [%expect
      {| |}];
    return ())
;;
