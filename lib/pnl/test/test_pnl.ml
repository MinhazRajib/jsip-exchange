open! Core
open Jsip_types
open Jsip_pnl
open Jsip_test_harness

(* Build a fill by hand. The [fill_id] and order ids are irrelevant to P&L,
   so they are fixed; what matters is who is on each side, the price, and the
   size. [aggressor_side] is the incoming order's side; the resting order is
   on the opposite side. *)
let fill ~aggressor ~aggressor_side ~resting ~price_cents ~size : Fill.t =
  { fill_id = 1
  ; symbol = Harness.aapl_id
  ; price = Price.of_int_cents price_cents
  ; size = Size.of_int size
  ; aggressor_order_id = Order_id.For_testing.of_int 1
  ; aggressor_client_order_id = Client_order_id.of_int 0
  ; aggressor_participant = aggressor
  ; aggressor_side
  ; resting_order_id = Order_id.For_testing.of_int 2
  ; resting_client_order_id = Client_order_id.of_int 0
  ; resting_participant = resting
  }
;;

let print_summary t participant =
  print_endline [%string "%{participant#Participant}:"];
  print_endline (Pnl.Summary.to_string (Pnl.summary t participant))
;;

let%expect_test "opening a position and marking to market" =
  (* Alice lifts Bob's offer: Alice buys 100, Bob sells 100, both at $150. *)
  let t =
    Pnl.empty
    |> fun t ->
    Pnl.apply_fill
      t
      (fill
         ~aggressor:Harness.alice
         ~aggressor_side:Buy
         ~resting:Harness.bob
         ~price_cents:15000
         ~size:100)
  in
  (* The market then prints a trade at $152, moving the reference price. *)
  let t =
    Pnl.apply_trade_report
      t
      { symbol = Harness.aapl_id
      ; price = Price.of_int_cents 15200
      ; size = Size.of_int 100
      }
  in
  print_summary t Harness.alice;
  print_summary t Harness.bob;
  [%expect
    {|
    Alice:
      0: inv=100 avg=$150.00 realized=$0.00 unrealized=$200.00 total=$200.00
      TOTAL: realized=$0.00 unrealized=$200.00 total=$200.00
    Bob:
      0: inv=-100 avg=$150.00 realized=$0.00 unrealized=-$200.00 total=-$200.00
      TOTAL: realized=$0.00 unrealized=-$200.00 total=-$200.00
    |}]
;;

let%expect_test "adding to a position blends the average entry" =
  let t =
    Pnl.empty
    |> fun t ->
    Pnl.apply_fill
      t
      (fill
         ~aggressor:Harness.alice
         ~aggressor_side:Buy
         ~resting:Harness.bob
         ~price_cents:15000
         ~size:100)
  in
  let t =
    Pnl.apply_fill
      t
      (fill
         ~aggressor:Harness.alice
         ~aggressor_side:Buy
         ~resting:Harness.bob
         ~price_cents:16000
         ~size:100)
  in
  print_summary t Harness.alice;
  [%expect
    {|
    Alice:
      0: inv=200 avg=$155.00 realized=$0.00 unrealized=$0.00 total=$0.00
      TOTAL: realized=$0.00 unrealized=$0.00 total=$0.00
    |}]
;;

let%expect_test "closing a position realizes cash" =
  (* Alice buys 100 @ $150, then sells all 100 @ $151: a clean $1/share gain
     on 100 shares = $100 realized, flat afterwards. *)
  let t =
    Pnl.empty
    |> fun t ->
    Pnl.apply_fill
      t
      (fill
         ~aggressor:Harness.alice
         ~aggressor_side:Buy
         ~resting:Harness.bob
         ~price_cents:15000
         ~size:100)
  in
  let t =
    Pnl.apply_fill
      t
      (fill
         ~aggressor:Harness.alice
         ~aggressor_side:Sell
         ~resting:Harness.charlie
         ~price_cents:15100
         ~size:100)
  in
  print_summary t Harness.alice;
  [%expect
    {|
    Alice:
      0: inv=0 avg=-- realized=$100.00 unrealized=$0.00 total=$100.00
      TOTAL: realized=$100.00 unrealized=$0.00 total=$100.00
    |}]
;;

let%expect_test "partial close splits total into realized and unrealized" =
  (* Alice buys 100 @ $150; the market marks up to $160; she sells 40 @ $160.
     Her whole 100-lot has appreciated $10/share = $1000 of total P&L. That
     splits into $400 realized (the 40 shares she sold) and $600 unrealized
     (the 60 she still holds). Note [realized] alone is only $400 — it is
     [total] that is the true mark-to-market P&L. *)
  let t =
    Pnl.empty
    |> fun t ->
    Pnl.apply_fill
      t
      (fill
         ~aggressor:Harness.alice
         ~aggressor_side:Buy
         ~resting:Harness.bob
         ~price_cents:15000
         ~size:100)
  in
  let t =
    Pnl.apply_trade_report
      t
      { symbol = Harness.aapl_id
      ; price = Price.of_int_cents 16000
      ; size = Size.of_int 10
      }
  in
  let t =
    Pnl.apply_fill
      t
      (fill
         ~aggressor:Harness.alice
         ~aggressor_side:Sell
         ~resting:Harness.charlie
         ~price_cents:16000
         ~size:40)
  in
  print_summary t Harness.alice;
  [%expect
    {|
    Alice:
      0: inv=60 avg=$150.00 realized=$400.00 unrealized=$600.00 total=$1000.00
      TOTAL: realized=$400.00 unrealized=$600.00 total=$1000.00
    |}]
;;

let%expect_test "flipping through zero closes then reopens" =
  (* Alice is long 100 @ $150, then sells 150 @ $151: 100 shares close for a
     $100 realized gain, and the extra 50 open a new short at $151. A print
     at $152 then marks that short down $1/share = -$50 unrealized. *)
  let t =
    Pnl.empty
    |> fun t ->
    Pnl.apply_fill
      t
      (fill
         ~aggressor:Harness.alice
         ~aggressor_side:Buy
         ~resting:Harness.bob
         ~price_cents:15000
         ~size:100)
  in
  let t =
    Pnl.apply_fill
      t
      (fill
         ~aggressor:Harness.alice
         ~aggressor_side:Sell
         ~resting:Harness.charlie
         ~price_cents:15100
         ~size:150)
  in
  let t =
    Pnl.apply_trade_report
      t
      { symbol = Harness.aapl_id
      ; price = Price.of_int_cents 15200
      ; size = Size.of_int 150
      }
  in
  print_summary t Harness.alice;
  [%expect
    {|
    Alice:
      0: inv=-50 avg=$151.00 realized=$100.00 unrealized=-$50.00 total=$50.00
      TOTAL: realized=$100.00 unrealized=-$50.00 total=$50.00
    |}]
;;
