open! Core
open Jsip_types
open Jsip_order_book
open Jsip_gateway

let print_parse line =
  match Exchange_command.parse line with
  | Error msg -> print_endline [%string "ERROR: %{Error.to_string_hum msg}"]
  | Ok (Exchange_command.Submit req) ->
    print_endline [%string "%{req#Order.Request}"]
  | _ -> print_endline "Subscribe / Book was inputted!"
;;

(* --- Successful parsing --- *)

let%expect_test "parse: basic buy" =
  print_parse "BUY AAPL 100 150.25";
  [%expect {| BUY AAPL 100@$150.25 DAY as anonymous |}]
;;

let%expect_test "parse: basic sell" =
  print_parse "SELL TSLA 50 200.00";
  [%expect {| SELL TSLA 50@$200.00 DAY as anonymous |}]
;;

let%expect_test "parse: case insensitive side" =
  print_parse "buy AAPL 100\n   150.00";
  print_parse "Buy AAPL 100 150.00";
  [%expect
    {|
    BUY AAPL 100@$150.00 DAY as anonymous
    BUY AAPL 100@$150.00 DAY as anonymous
    |}]
;;

let%expect_test "parse: with IOC time-in-force" =
  print_parse "BUY AAPL\n   100 150.00 IOC";
  [%expect {| BUY AAPL 100@$150.00 IOC as anonymous |}]
;;

let%expect_test "parse: with explicit DAY" =
  print_parse "SELL AAPL 200\n   151.00 DAY";
  [%expect {| SELL AAPL 200@$151.00 DAY as anonymous |}]
;;

let%expect_test "parse: with participant" =
  print_parse "BUY AAPL 100\n   150.00 as Alice";
  [%expect {| BUY AAPL 100@$150.00 DAY as Alice |}]
;;

let%expect_test "parse: with TIF and participant" =
  print_parse "SELL GOOG\n   75 2800.50 IOC as Bob";
  [%expect {| SELL GOOG 75@$2800.50 IOC as Bob |}]
;;

let%expect_test "parse: symbol is uppercased" =
  print_parse "BUY aapl 100\n   150.00";
  [%expect {| BUY aapl 100@$150.00 DAY as anonymous |}]
;;

let%expect_test "parse: extra whitespace is ignored" =
  print_parse " BUY\n   AAPL 100 150.00 ";
  [%expect {| BUY AAPL 100@$150.00 DAY as anonymous |}]
;;

let%expect_test "parse: price with dollar sign" =
  print_parse "BUY AAPL\n   100 $150.25";
  [%expect {| BUY AAPL 100@$150.25 DAY as anonymous |}]
;;

(* --- Parse errors --- *)

let%expect_test "parse error: empty string" =
  print_parse "";
  print_parse " ";
  [%expect {|
    ERROR: empty command
    ERROR: empty command
    |}]
;;

let%expect_test "parse error: unknown command" =
  print_parse "HOLD AAPL\n   100 150.00";
  [%expect
    {| ERROR: unknown command: HOLD (expected BUY/SELL/BOOK/SUBSCRIBE) |}]
;;

let%expect_test "parse error: missing fields" =
  print_parse "BUY AAPL";
  print_parse "BUY";
  [%expect
    {|
    ERROR: expected: BUY|SELL <symbol> <size> <price> [ DAY|IOC] [as <name>]
    ERROR: expected: BUY|SELL <symbol> <size> <price> [ DAY|IOC] [as <name>]
    |}]
;;

let%expect_test "parse error: invalid size" =
  print_parse "BUY AAPL abc\n   150.00";
  print_parse "BUY AAPL 0 150.00";
  print_parse "BUY AAPL -5\n   150.00";
  [%expect
    {|
    ERROR: invalid size: abc
    ERROR: size must be positive
    ERROR: size must be positive
    |}]
;;

let%expect_test "parse error: invalid price" =
  print_parse "BUY AAPL 100\n   xyz";
  [%expect
    {|
    ERROR: invalid price: xyz
    exception: (Invalid_argument "Float.of_string xyz")
    |}]
;;

let%expect_test "parse error: unknown time-in-force" =
  print_parse "BUY\n   AAPL 100 150.00 QQQ";
  [%expect {| ERROR: unknown time-in-force: QQQ (expected DAY|IOC) |}]
;;

(* --- parse_command_with_default_participant --- *)

let%expect_test "default participant: used when none specified" =
  let default = Participant.of_string "DefaultTrader" in
  let _ =
    match
      Exchange_command.parse
        "BUY AAPL 100 150.00"
        ~default_participant:default
    with
    | Error msg ->
      print_endline [%string "ERROR: %{Error.to_string_hum msg}"]
    | Ok (Exchange_command.Submit request) ->
      print_endline
        [%string "participant=%{request.participant#Participant}"]
    | Ok _ -> print_endline "WRONG COMMAND"
  in
  [%expect {| participant=DefaultTrader |}]
;;

let%expect_test "default participant: overridden by explicit 'as'" =
  let default = Participant.of_string "DefaultTrader" in
  let _ =
    match
      Exchange_command.parse
        "BUY AAPL 100 150.00 as Alice"
        ~default_participant:default
    with
    | Error msg ->
      print_endline [%string "ERROR: %{Error.to_string_hum msg}"]
    | Ok (Exchange_command.Submit request) ->
      print_endline
        [%string "participant=%{request.participant#Participant}"]
    | Ok _ -> print_endline "WRONG COMMAND"
  in
  [%expect {| participant=Alice |}]
;;

(* --- Round-trip: parse then format --- *)

let%expect_test "round-trip: parse a command, submit, format result" =
  let open Jsip_test_harness in
  let t = Harness.create () in
  (* Place a resting sell *)
  Harness.submit_
    t
    (Harness.sell ~price_cents:15000 ~participant:Harness.bob ());
  (* Parse a buy command from text and submit it *)
  let _ =
    match Exchange_command.parse "BUY AAPL 100 150.00 as Alice" with
    | Error msg ->
      print_endline [%string "ERROR: %{Error.to_string_hum msg}"]
    | Ok (Exchange_command.Submit request) ->
      let events = Matching_engine.submit (Harness.engine t) request in
      print_endline (Event_formatter.format_events events)
    | Ok _ -> print_endline "WRONG COMMAND"
  in
  [%expect
    {|
    ACCEPTED id=1 AAPL SELL 100@$150.00 DAY
    BBO AAPL bid=- ask=$150.00 x100
    ACCEPTED id=2 AAPL BUY 100@$150.00 DAY
    FILL fill_id=1 AAPL $150.00 x100 aggressor=2(Alice) BUY resting=1(Bob)
    TRADE AAPL $150.00 x100
    BBO AAPL bid=- ask=-
    |}]
;;
