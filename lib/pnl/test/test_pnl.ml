open! Core
open Jsip_types
open Jsip_pnl
open Jsip_test_harness
open Harness

(* Hand-roll a fill: [aggressor] hits [resting] for [size] @ [price_cents] on
   AAPL. Order ids are irrelevant to P&L, so we derive throwaway ones from the
   fill id. *)
let fill ~id ~price_cents ~size ~aggressor ~aggressor_side ~resting : Fill.t =
  { fill_id = id
  ; symbol = aapl
  ; price = Price.of_int_cents price_cents
  ; size = Size.of_int size
  ; aggressor_order_id = Order_id.For_testing.of_int (id * 2)
  ; aggressor_participant = aggressor
  ; aggressor_side
  ; resting_order_id = Order_id.For_testing.of_int ((id * 2) + 1)
  ; resting_participant = resting
  }
;;

let trade_print ~price_cents : Exchange_event.t =
  Trade_report
    { symbol = aapl; price = Price.of_int_cents price_cents; size = Size.of_int 1 }
;;

(* Render cents as a signed dollar string: [-60000] -> ["-$600.00"]. *)
let money cents =
  let sign = if cents < 0 then "-" else "" in
  let abs = Int.abs cents in
  sprintf "%s$%d.%02d" sign (abs / 100) (abs % 100)
;;

let show label pnl participant =
  let summary = Pnl.summary pnl participant in
  printf "%s\n" label;
  List.iter summary.per_symbol ~f:(fun line ->
    printf
      "  %s: inv=%d avg=%s ref=%s realized=%s unrealized=%s\n"
      (Symbol.to_string line.symbol)
      line.inventory
      (Option.value_map
         line.average_entry_price
         ~default:"-"
         ~f:Price.to_string_dollar)
      (Option.value_map
         line.reference_price
         ~default:"-"
         ~f:Price.to_string_dollar)
      (money line.realized_cents)
      (money line.unrealized_cents));
  printf
    "  total: realized=%s unrealized=%s\n"
    (money summary.total_realized_cents)
    (money summary.total_unrealized_cents)
;;

(* Alice accumulates a long over two fills at different prices, then sells part
   of it into a higher market; Bob is the counterparty on every fill and so
   ends up with the exact mirror-image book. A final trade print marks both
   open positions to market. *)
let%expect_test "accumulate, partial close, and mark to market" =
  let pnl =
    Pnl.empty
    |> fun p ->
    Pnl.apply_fill
      p
      (fill
         ~id:1
         ~price_cents:15000
         ~size:100
         ~aggressor:alice
         ~aggressor_side:Buy
         ~resting:bob)
    |> fun p ->
    Pnl.apply_fill
      p
      (fill
         ~id:2
         ~price_cents:15200
         ~size:100
         ~aggressor:alice
         ~aggressor_side:Buy
         ~resting:bob)
    |> fun p ->
    Pnl.apply_fill
      p
      (fill
         ~id:3
         ~price_cents:15500
         ~size:150
         ~aggressor:alice
         ~aggressor_side:Sell
         ~resting:bob)
    |> fun p -> Pnl.apply_trade_report p (trade_print ~price_cents:15600)
  in
  show "alice" pnl alice;
  show "bob" pnl bob;
  [%expect
    {|
    alice
      AAPL: inv=50 avg=$151.00 ref=$156.00 realized=$600.00 unrealized=$250.00
      total: realized=$600.00 unrealized=$250.00
    bob
      AAPL: inv=-50 avg=$151.00 ref=$156.00 realized=-$600.00 unrealized=-$250.00
      total: realized=-$600.00 unrealized=-$250.00
    |}]
;;

(* One fill flips Charlie from long 100 to short 200: the first 100 shares
   close the long (realizing profit), and the remaining 200 open a fresh short
   at the sale price. No trade print, so the position is unmarked. *)
let%expect_test "position flips through zero in a single fill" =
  let pnl =
    Pnl.empty
    |> fun p ->
    Pnl.apply_fill
      p
      (fill
         ~id:1
         ~price_cents:10000
         ~size:100
         ~aggressor:charlie
         ~aggressor_side:Buy
         ~resting:market_maker)
    |> fun p ->
    Pnl.apply_fill
      p
      (fill
         ~id:2
         ~price_cents:11000
         ~size:300
         ~aggressor:charlie
         ~aggressor_side:Sell
         ~resting:market_maker)
  in
  show "charlie" pnl charlie;
  [%expect
    {|
    charlie
      AAPL: inv=-200 avg=$110.00 ref=- realized=$1000.00 unrealized=$0.00
      total: realized=$1000.00 unrealized=$0.00
    |}]
;;
