(** Scaffolding for bot tests. *)

open! Core
open! Async
open Jsip_types
open Jsip_fundamental
open Jsip_bot_runtime
open! Jsip_bots

let aapl = Symbol.of_string "AAPL"
let alice = Participant.of_string "Alice"

let oracle_config ~initial_price_cents =
  Symbol.Map.of_alist_exn
    [ ( aapl
      , { Fundamental_oracle.Config.initial_price_cents
        ; volatility_cents_per_sec = 0.0
        ; mean_reversion_strength = 0.0
        ; tick_interval = Time_ns.Span.of_sec 1.0
        } )
    ]
;;

(* Build a runtime around a bot module with a mock submit/cancel that records
   what the bot does. *)
let make_recording_bot
  (type cfg)
  (bot_module : (module Bot_runtime.Bot with type Config.t = cfg))
  (config : cfg)
  ?(initial_price_cents = 15000)
  ()
  =
  let submitted = ref [] in
  let cancelled = ref [] in
  let submit request =
    submitted := request :: !submitted;
    return (Ok ())
  in
  let cancel order_id =
    cancelled := order_id :: !cancelled;
    return (Ok ())
  in
  let oracle =
    Fundamental_oracle.create (oracle_config ~initial_price_cents) ~seed:42
  in
  let bot =
    Bot_runtime.create
      bot_module
      config
      ~participant:alice
      ~oracle
      ~rng:(Splittable_random.of_int 7)
      ~submit
      ~cancel
      ~tick_interval:(Time_ns.Span.of_sec 1.0)
  in
  bot, submitted, cancelled
;;

let print_submitted (submitted : Order.Request.t list ref) =
  let recent = List.rev !submitted in
  List.iter recent ~f:(fun req ->
    printf
      !"%{Side} %{Symbol} %d@%{Price#dollar} %{Time_in_force}\n"
      req.side
      req.symbol
      (Size.to_int req.size)
      req.price
      req.time_in_force)
;;

(* Smoke test: drive the do-nothing reference bot through one event so the
   runtest target exercises the helpers above. Replace or extend with
   bot-specific tests as concrete strategies are added to [Jsip_bots]. *)
module Inert_bot = struct
  module Config = struct
    type t = unit
  end

  let name = "inert"
  let on_start () _ctx = return ()
  let on_tick () _ctx = return ()
  let on_event () _ctx _event = return ()
end

(* A helper that builds a config for the bot, so each test doesn't repeat it.
   [~cycles_per_tick] is left for the caller to choose. *)
let cancel_storm_config ~cycles_per_tick : Cancel_storm.Config.t =
  { symbols = [ aapl ]
  ; cycles_per_tick
  ; order_size = 10
  ; price_offset_cents = 100
  ; client_order_ids = Client_order_id.Generator.create ()
  }
;;

(* This test checks the three things the bot must get right:
   1. every order gets a brand-new id (no repeats),
   2. every order we send, we also cancel,
   3. no order is priced to actually trade (buys below, sells above). We work
      these out by hand and compare, instead of trusting the bot. *)
let%expect_test "cancel storm submits-then-cancels with fresh ids each cycle"
  =
  (* 3 steps per tick. *)
  let config = cancel_storm_config ~cycles_per_tick:3 in
  (* A fake bot: instead of talking to a real exchange, it just records every
     order it submits and every id it cancels into these two lists. *)
  let bot, submitted, cancelled =
    make_recording_bot
      (module Cancel_storm)
      config
      ~initial_price_cents:15000
      ()
  in
  let ctx = Bot_runtime.For_testing.context_of bot in
  (* Fire the timer twice: 3 steps + 3 steps = 6 orders expected. *)
  let%bind () = Cancel_storm.on_tick config ctx in
  let%bind () = Cancel_storm.on_tick config ctx in
  (* The lists are recorded newest-first, so reverse them back to time order. *)
  let submits = List.rev !submitted in
  let cancels = List.rev !cancelled in
  printf
    "submits: %d, cancels: %d\n"
    (List.length submits)
    (List.length cancels);
  (* Pull just the ids out of each submitted order. *)
  let submitted_ids = List.map submits ~f:(fun r -> r.client_order_id) in
  printf !"submitted ids: %{sexp: Client_order_id.t list}\n" submitted_ids;
  printf !"cancelled ids: %{sexp: Client_order_id.t list}\n" cancels;
  (* Check 1: no id appears twice. *)
  printf
    "ids all distinct: %b\n"
    (List.contains_dup submitted_ids ~compare:Client_order_id.compare |> not);
  (* Check 2: the set of ids we cancelled matches the set we submitted. *)
  printf
    "every submit was cancelled: %b\n"
    (List.equal
       Client_order_id.equal
       (List.sort submitted_ids ~compare:Client_order_id.compare)
       (List.sort cancels ~compare:Client_order_id.compare));
  (* Check 3: each buy is priced below the true price, each sell above it. *)
  let non_marketable =
    List.for_all submits ~f:(fun r ->
      let fundamental = 15000 in
      match r.side with
      | Buy -> Price.to_int_cents r.price < fundamental
      | Sell -> Price.to_int_cents r.price > fundamental)
  in
  printf "all orders non-marketable: %b\n" non_marketable;
  (* [%expect] holds the output we expect. If the code's output ever changes,
     the test fails and shows the difference. *)
  [%expect
    {|
    submits: 6, cancels: 6
    submitted ids: (1 2 3 4 5 6)
    cancelled ids: (1 2 3 4 5 6)
    ids all distinct: true
    every submit was cancelled: true
    all orders non-marketable: true
    |}];
  return ()
;;

let%expect_test "make_recording_bot wires up a runnable bot" =
  let bot, submitted, _cancelled =
    make_recording_bot (module Inert_bot) () ()
  in
  let%bind () =
    Bot_runtime.feed_event
      bot
      (Order_accept
         { order_id = Order_id.For_testing.of_int 1
         ; request =
             { client_order_id = Client_order_id.of_int 0
             ; symbol = aapl
             ; participant = alice
             ; side = Buy
             ; price = Price.of_int_cents 15000
             ; size = Size.of_int 10
             ; time_in_force = Day
             }
         })
  in
  print_submitted submitted;
  [%expect {| |}];
  return ()
;;
