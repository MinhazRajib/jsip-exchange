open! Core
open Jsip_types
open Jsip_pnl
open Jsip_test_harness

(* Hand-roll a fill between an [aggressor] (who traded on [aggressor_side])
   and a [resting] counterparty. Order ids are irrelevant to P&L, so we just
   derive them from [id]. *)
let fill
  ~id
  ?(symbol = Harness.aapl)
  ~price_cents
  ~size
  ~aggressor
  ~aggressor_side
  ~resting
  ()
  : Fill.t
  =
  { fill_id = id
  ; symbol
  ; price = Price.of_int_cents price_cents
  ; size = Size.of_int size
  ; aggressor_order_id = Order_id.For_testing.of_int id
  ; aggressor_participant = aggressor
  ; aggressor_side
  ; resting_order_id = Order_id.For_testing.of_int (id + 1000)
  ; resting_participant = resting
  }
;;

let apply_fills fills = List.fold fills ~init:Pnl.empty ~f:Pnl.apply_fill

let show t participant =
  print_endline [%string "%{participant#Participant}:"];
  print_endline (Pnl.Summary.to_string_hum (Pnl.summary t participant))
;;

let alice = Harness.alice
let bob = Harness.bob
let charlie = Harness.charlie

(* Alice buys 100 @ $10.00 then sells all 100 @ $10.50: she books a $50 gain
   and Bob, the counterparty on both fills, books the mirror-image $50 loss.
   Both end flat, so there is no unrealized component. *)
let%expect_test "round trip realizes P&L for both sides" =
  let t =
    apply_fills
      [ fill
          ~id:1
          ~price_cents:1000
          ~size:100
          ~aggressor:alice
          ~aggressor_side:Buy
          ~resting:bob
          ()
      ; fill
          ~id:2
          ~price_cents:1050
          ~size:100
          ~aggressor:alice
          ~aggressor_side:Sell
          ~resting:bob
          ()
      ]
  in
  show t alice;
  show t bob;
  [%expect
    {|
    Alice:
      AAPL: inv=0 avg=- ref=- realized=$50.00 unrealized=$0.00 total=$50.00
      TOTAL: realized=$50.00 unrealized=$0.00 total=$50.00
    Bob:
      AAPL: inv=0 avg=- ref=- realized=-$50.00 unrealized=$0.00 total=-$50.00
      TOTAL: realized=-$50.00 unrealized=$0.00 total=-$50.00
    |}]
;;

(* An open position has no realized P&L, but a trade print marks it to
   market: Alice is long 200 @ $30.00, the print at $31.50 shows a $300 paper
   gain and Bob (short 200) the opposite. *)
let%expect_test "unrealized P&L marks an open position to a trade print" =
  let t =
    apply_fills
      [ fill
          ~id:1
          ~price_cents:3000
          ~size:200
          ~aggressor:alice
          ~aggressor_side:Buy
          ~resting:bob
          ()
      ]
  in
  let t =
    Pnl.apply_trade_report
      t
      ~symbol:Harness.aapl
      ~price:(Price.of_int_cents 3150)
  in
  show t alice;
  show t bob;
  [%expect
    {|
    Alice:
      AAPL: inv=200 avg=$30.00 ref=$31.50 realized=$0.00 unrealized=$300.00 total=$300.00
      TOTAL: realized=$0.00 unrealized=$300.00 total=$300.00
    Bob:
      AAPL: inv=-200 avg=$30.00 ref=$31.50 realized=$0.00 unrealized=-$300.00 total=-$300.00
      TOTAL: realized=$0.00 unrealized=-$300.00 total=-$300.00
    |}]
;;

(* Two buys at different prices average into a single cost basis: 100 @
   $20.00 and 100 @ $21.00 give an average entry of $20.50, and a print at
   $22.00 marks the 200-share position up by $300. *)
let%expect_test "average entry price across multiple buys" =
  let t =
    apply_fills
      [ fill
          ~id:1
          ~symbol:Harness.tsla
          ~price_cents:2000
          ~size:100
          ~aggressor:alice
          ~aggressor_side:Buy
          ~resting:bob
          ()
      ; fill
          ~id:2
          ~symbol:Harness.tsla
          ~price_cents:2100
          ~size:100
          ~aggressor:alice
          ~aggressor_side:Buy
          ~resting:bob
          ()
      ]
  in
  let t =
    Pnl.apply_trade_report
      t
      ~symbol:Harness.tsla
      ~price:(Price.of_int_cents 2200)
  in
  show t alice;
  [%expect
    {|
    Alice:
      TSLA: inv=200 avg=$20.50 ref=$22.00 realized=$0.00 unrealized=$300.00 total=$300.00
      TOTAL: realized=$0.00 unrealized=$300.00 total=$300.00
    |}]
;;

(* Selling more than you hold flips you from long to short. Charlie is long
   100 @ $50.00, sells 150 @ $55.00: the first 100 close for a $500 realized
   gain and the extra 50 open a new short at $55.00. A later print at $54.00
   marks the short up $50 (shorts gain as the price falls). *)
let%expect_test "flipping from long to short" =
  let t =
    apply_fills
      [ fill
          ~id:1
          ~price_cents:5000
          ~size:100
          ~aggressor:charlie
          ~aggressor_side:Buy
          ~resting:bob
          ()
      ; fill
          ~id:2
          ~price_cents:5500
          ~size:150
          ~aggressor:charlie
          ~aggressor_side:Sell
          ~resting:bob
          ()
      ]
  in
  let t =
    Pnl.apply_trade_report
      t
      ~symbol:Harness.aapl
      ~price:(Price.of_int_cents 5400)
  in
  show t charlie;
  [%expect
    {|
    Charlie:
      AAPL: inv=-50 avg=$55.00 ref=$54.00 realized=$500.00 unrealized=$50.00 total=$550.00
      TOTAL: realized=$500.00 unrealized=$50.00 total=$550.00
    |}]
;;
