(** Unit tests for the [Market_maker_bot].

    These drive the bot through the bot runtime with recording [submit] /
    [cancel] closures instead of a live server, mirroring the pattern in
    [app/bots/test/test_bots.ml]: build a runtime around the bot with mock RPC
    closures that record every request, drive the bot's callbacks through a
    [For_testing] context, and inspect what it did. *)

open! Core
open! Async
open Jsip_types
open Jsip_bot_runtime
open Jsip_market_maker.Market_maker
module Fundamental_oracle = Jsip_fundamental.Fundamental_oracle

let aapl = Jsip_test_harness.Harness.aapl
let market_maker = Jsip_test_harness.Harness.market_maker
let alice = Jsip_test_harness.Harness.alice

(* A flat, non-moving oracle: the market maker under test doesn't read the
   fundamental, but the runtime requires one. *)
let oracle () =
  let config =
    Symbol.Map.of_alist_exn
      [ ( aapl
        , { Fundamental_oracle.Config.initial_price_cents = 15000
          ; volatility_cents_per_sec = 0.0
          ; mean_reversion_strength = 0.0
          ; tick_interval = Time_ns.Span.of_sec 1.0
          } )
      ]
  in
  Fundamental_oracle.create config ~seed:42
;;

let make_config ?(inventory_skew_cents_per_share = 1) () : Config.t =
  { symbol = aapl
  ; fair_value_cents = 15000
  ; half_spread_cents = 10
  ; size_per_level = 100
  ; num_levels = 3
  ; client_id_manager = Client_order_id.Generator.create ()
  ; inventory_skew_cents_per_share
  ; inventory_counter = Symbol.Table.create ()
  ; resting_client_order_ids = Client_order_id.Table.create ()
  }
;;

(* Wire the bot to recording closures. Returns the bot, a [For_testing]
   context to call its callbacks against, and the two mutable logs of what it
   submitted / cancelled. *)
let recording_bot (config : Config.t) =
  let submitted = ref [] in
  let cancelled = ref [] in
  let submit request =
    submitted := request :: !submitted;
    return (Ok ())
  in
  let cancel client_order_id =
    cancelled := client_order_id :: !cancelled;
    return (Ok ())
  in
  let bot =
    Bot_runtime.create
      (module Market_maker_bot)
      config
      ~participant:market_maker
      ~oracle:(oracle ())
      ~rng:(Splittable_random.of_int 7)
      ~submit
      ~cancel
      ~tick_interval:(Time_ns.Span.of_sec 1.0)
  in
  bot, Bot_runtime.For_testing.context_of bot, submitted, cancelled
;;

let print_requests (requests : Order.Request.t list) =
  List.iter requests ~f:(fun req ->
    printf
      !"client-id=%{Client_order_id} %{Side} %d@%{Price#dollar}\n"
      req.client_order_id
      req.side
      (Size.to_int req.size)
      req.price)
;;

(* Feed an [Order_accept] for each request so the bot records its resting
   orders. [seed_book] only submits; the resting table is populated from the
   accept events, exactly as it is at runtime. *)
let accept_all bot (requests : Order.Request.t list) =
  Deferred.List.iter ~how:`Sequential requests ~f:(fun request ->
    Bot_runtime.feed_event
      bot
      (Order_accept { order_id = Order_id.For_testing.of_int 0; request }))
;;

(* A fill in which [market_maker] is the resting party of [request] — i.e.
   someone aggressed against the bot's resting order, consuming [size]
   shares. *)
let fill_against (request : Order.Request.t) ~size : Exchange_event.t =
  let aggressor_side : Side.t =
    match request.side with
    | Buy -> Sell
    | Sell -> Buy
  in
  Fill
    { fill_id = 1
    ; symbol = request.symbol
    ; price = request.price
    ; size
    ; aggressor_order_id = Order_id.For_testing.of_int 99
    ; aggressor_client_order_id = Client_order_id.of_int 999
    ; aggressor_participant = alice
    ; aggressor_side
    ; resting_order_id = Order_id.For_testing.of_int 0
    ; resting_client_order_id = request.client_order_id
    ; resting_participant = market_maker
    }
;;

let%expect_test "on_start seeds a symmetric ladder around fair value" =
  let config = make_config () in
  let _bot, context, submitted, _cancelled = recording_bot config in
  let%bind () = Market_maker_bot.on_start config context in
  print_requests (List.rev !submitted);
  [%expect
    {|
    client-id=1 BUY 100@$149.90
    client-id=2 SELL 100@$150.10
    client-id=3 BUY 100@$149.89
    client-id=4 SELL 100@$150.11
    client-id=5 BUY 100@$149.88
    client-id=6 SELL 100@$150.12
    |}];
  return ()
;;

let%expect_test "a fill updates inventory, cancels the book, and re-quotes \
                 skewed"
  =
  let config = make_config ~inventory_skew_cents_per_share:1 () in
  let bot, context, submitted, cancelled = recording_bot config in
  let%bind () = Market_maker_bot.on_start config context in
  let initial = List.rev !submitted in
  let%bind () = accept_all bot initial in
  (* Isolate the reaction to the fill from the initial ladder. *)
  submitted := [];
  cancelled := [];
  (* Someone sells 100 into the bot's best bid (client-id 1): the bot buys,
     so its inventory goes long by 100. *)
  let best_bid = List.hd_exn initial in
  let%bind () =
    Bot_runtime.feed_event bot (fill_against best_bid ~size:best_bid.size)
  in
  printf "inventory:\n";
  Hashtbl.iteri config.inventory_counter ~f:(fun ~key ~data ->
    printf !"  %{Symbol} = %d\n" key (Size.to_int data));
  printf "cancelled:\n";
  List.iter
    (List.sort ~compare:Client_order_id.compare !cancelled)
    ~f:(fun id -> printf !"  client-id=%{Client_order_id}\n" id);
  printf "re-quoted ladder:\n";
  print_requests (List.rev !submitted);
  [%expect
    {|
    inventory:
      AAPL = 100
    cancelled:
      client-id=2
      client-id=3
      client-id=4
      client-id=5
      client-id=6
    re-quoted ladder:
    client-id=7 BUY 100@$148.90
    client-id=8 SELL 100@$149.10
    client-id=9 BUY 100@$148.89
    client-id=10 SELL 100@$149.11
    client-id=11 BUY 100@$148.88
    client-id=12 SELL 100@$149.12
    |}];
  return ()
;;

(* Compute the mid of a re-quoted ladder so we can compare where successive
   skews land relative to the configured 15000 fair value. *)
let mid_of_ladder (requests : Order.Request.t list) : int =
  let prices_on side =
    List.filter_map requests ~f:(fun req ->
      Option.some_if (Side.equal req.side side) (Price.to_int_cents req.price))
  in
  let best_bid = List.max_elt (prices_on Buy) ~compare:Int.compare in
  let best_ask = List.min_elt (prices_on Sell) ~compare:Int.compare in
  match best_bid, best_ask with
  | Some bid, Some ask -> (bid + ask) / 2
  | _ ->
    raise_s
      [%message
        "mid_of_ladder: ladder is missing a side"
          (requests : Order.Request.t list)]
;;

let%expect_test "inventory skew is symmetric around fair value" =
  (* A long position and an equal short position should skew the re-quoted
     ladder by equal and opposite amounts around the 15000 fair value. Each
     scenario starts from a fresh, flat bot. *)
  let requote_after ~fill_side =
    let config = make_config ~inventory_skew_cents_per_share:1 () in
    let bot, context, submitted, _cancelled = recording_bot config in
    let%bind () = Market_maker_bot.on_start config context in
    let initial = List.rev !submitted in
    let%bind () = accept_all bot initial in
    submitted := [];
    (* Fully fill one of the bot's resting orders on [fill_side]. *)
    let order =
      List.find_exn initial ~f:(fun req -> Side.equal req.side fill_side)
    in
    let%bind () =
      Bot_runtime.feed_event bot (fill_against order ~size:order.size)
    in
    return (mid_of_ladder (List.rev !submitted))
  in
  (* A fill on the bid makes the bot long (skew down); a fill on the ask makes
     it short (skew up). *)
  let%bind mid_when_long = requote_after ~fill_side:Buy in
  let%bind mid_when_short = requote_after ~fill_side:Sell in
  printf "mid when long:  %d\n" mid_when_long;
  printf "mid when short: %d\n" mid_when_short;
  [%expect {|
    mid when long:  14900
    mid when short: 15100
    |}];
  return ()
;;
