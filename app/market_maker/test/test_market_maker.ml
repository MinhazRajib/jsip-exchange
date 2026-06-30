(** Tests for the market maker, using a real exchange server. *)

open! Core
open! Async
open Jsip_types
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
  ; client_id_manager = Client_order_id.Generator.create ()
  ; inventory_counter = Symbol.Table.create ()
  ; resting_client_order_ids = Client_order_id.Table.create ()
  }
;;

let%expect_test "seed_book: places symmetric bids and asks around fair value"
  =
  with_server ~symbols:[ Harness.aapl ] (fun ~server:_ ~port ->
    let%bind mm = connect_as ~port Harness.market_maker in
    let%bind () = Market_maker.seed_book default_config (connection mm) in
    [%expect
      {|
      [for MarketMaker] ACCEPTED client-id=1 id=1 AAPL BUY 100@$149.90 DAY
      [for MarketMaker] ACCEPTED client-id=2 id=2 AAPL SELL 100@$150.10 DAY
      [for MarketMaker] ACCEPTED client-id=3 id=3 AAPL BUY 100@$149.89 DAY
      [for MarketMaker] ACCEPTED client-id=4 id=4 AAPL SELL 100@$150.11 DAY
      [for MarketMaker] ACCEPTED client-id=5 id=5 AAPL BUY 100@$149.88 DAY
      [for MarketMaker] ACCEPTED client-id=6 id=6 AAPL SELL 100@$150.12 DAY
      |}];
    return ())
;;

(** Dynamic Market Maker Tests *)

let%expect_test "run: correctly adapts inventory counter and resting client \
                 order ids"
  =
  with_server ~symbols:[ Harness.aapl ] (fun ~server:_ ~port ->
    let test_config : Market_maker.Config.t =
      { participant = Harness.market_maker
      ; symbol = Harness.aapl
      ; fair_value_cents = 15000
      ; half_spread_cents = 10
      ; size_per_level = 100
      ; num_levels = 3
      ; client_id_manager = Client_order_id.Generator.create ()
      ; inventory_counter = Symbol.Table.create ()
      ; resting_client_order_ids = Client_order_id.Table.create ()
      }
    in
    let%bind mm = connect_as ~port Harness.market_maker in
    let%bind alice = connect_as ~port Harness.alice in
    let%bind () =
      rpc_submit
        alice
        (Harness.sell ~price_cents:14980 ~participant:Harness.alice ())
    in
    [%expect
      {| [for Alice] ACCEPTED client-id=0 id=1 AAPL SELL 100@$149.80 DAY |}];
    let%bind () = Market_maker.run test_config (connection mm) in
    [%expect
      {| [for Alice] FILL fill_id=1 AAPL $149.80 x100 aggressor=2 (client-id=1) (MarketMaker) BUY resting=1 (client-id=0) (Alice) |}];
    (* print resulting inventory and client_order_ids *)
    Hashtbl.iteri test_config.inventory_counter ~f:(fun ~key ~data ->
      printf "Key: %s, Value: %d\n" (Symbol.to_string key) (Size.to_int data));
    [%expect {| Key: AAPL, Value: 100 |}];
    Hashtbl.iteri test_config.resting_client_order_ids ~f:(fun ~key ~data ->
      printf
        "Key: %s, Value: %d\n"
        (Client_order_id.to_string key)
        (Size.to_int data));
    [%expect
      {|
      Key: 5, Value: 100
      Key: 4, Value: 100
      Key: 6, Value: 100
      Key: 2, Value: 100
      Key: 3, Value: 100
      |}];
    return ())
;;
