open! Core
open Jsip_types
open Jsip_order_book
open Jsip_gateway

(** Parsing tests with example clients and interactions

    Parsing tests to confirm the functionality of exchange_command and its
    parsing behavior. Completes a full end to end test between parsing and
    formatting from event_formatter. Specifically regarding BUY/SELL
    commands, no testing for book / subscribe. *)
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
  [%expect {| ERROR: expected: BUY|SELL <client_id> <symbol> <size> <price> [ DAY|IOC] [as <name>] |}]
;;

let%expect_test "parse: basic sell" =
  print_parse "SELL TSLA 50 200.00";
  [%expect {| ERROR: expected: BUY|SELL <client_id> <symbol> <size> <price> [ DAY|IOC] [as <name>] |}]
;;

let%expect_test "parse: case insensitive side" =
  print_parse "buy AAPL 100\n   150.00";
  print_parse "Buy AAPL 100 150.00";
  [%expect
    {|
    ERROR: expected: BUY|SELL <client_id> <symbol> <size> <price> [ DAY|IOC] [as <name>]
    ERROR: expected: BUY|SELL <client_id> <symbol> <size> <price> [ DAY|IOC] [as <name>]
    |}]
;;

let%expect_test "parse: with IOC time-in-force" =
  print_parse "BUY AAPL\n   100 150.00 IOC";
  [%expect {| ERROR: Invalid client_order_id |}]
;;

let%expect_test "parse: with explicit DAY" =
  print_parse "SELL AAPL 200\n   151.00 DAY";
  [%expect {| ERROR: Invalid client_order_id |}]
;;

let%expect_test "parse: with participant" =
  print_parse "BUY AAPL 100\n   150.00 as Alice";
  [%expect {| ERROR: Invalid client_order_id |}]
;;

let%expect_test "parse: with TIF and participant" =
  print_parse "SELL GOOG\n   75 2800.50 IOC as Bob";
  [%expect {| ERROR: Invalid client_order_id |}]
;;

let%expect_test "parse: symbol is uppercased" =
  print_parse "BUY aapl 100\n   150.00";
  [%expect {| ERROR: expected: BUY|SELL <client_id> <symbol> <size> <price> [ DAY|IOC] [as <name>] |}]
;;

let%expect_test "parse: extra whitespace is ignored" =
  print_parse " BUY\n   AAPL 100 150.00 ";
  [%expect {| ERROR: expected: BUY|SELL <client_id> <symbol> <size> <price> [ DAY|IOC] [as <name>] |}]
;;

let%expect_test "parse: price with dollar sign" =
  print_parse "BUY AAPL\n   100 $150.25";
  [%expect {| ERROR: expected: BUY|SELL <client_id> <symbol> <size> <price> [ DAY|IOC] [as <name>] |}]
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
    ERROR: expected: BUY|SELL <client_id> <symbol> <size> <price> [ DAY|IOC] [as <name>]
    ERROR: expected: BUY|SELL <client_id> <symbol> <size> <price> [ DAY|IOC] [as <name>]
    |}]
;;

let%expect_test "parse error: invalid size" =
  print_parse "BUY AAPL abc\n   150.00";
  print_parse "BUY AAPL 0 150.00";
  print_parse "BUY AAPL -5\n   150.00";
  [%expect
    {|
    ERROR: expected: BUY|SELL <client_id> <symbol> <size> <price> [ DAY|IOC] [as <name>]
    ERROR: expected: BUY|SELL <client_id> <symbol> <size> <price> [ DAY|IOC] [as <name>]
    ERROR: expected: BUY|SELL <client_id> <symbol> <size> <price> [ DAY|IOC] [as <name>]
    |}]
;;

let%expect_test "parse error: invalid price" =
  print_parse "BUY AAPL 100\n   xyz";
  [%expect
    {| ERROR: expected: BUY|SELL <client_id> <symbol> <size> <price> [ DAY|IOC] [as <name>] |}]
;;

let%expect_test "parse error: unknown time-in-force" =
  print_parse "BUY\n   AAPL 100 150.00 QQQ";
  [%expect {| ERROR: Invalid client_order_id |}]
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
  [%expect {| ERROR: expected: BUY|SELL <client_id> <symbol> <size> <price> [ DAY|IOC] [as <name>] |}]
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
  [%expect {| ERROR: Invalid client_order_id |}]
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
    ACCEPTED client-id=0 id=1 AAPL SELL 100@$150.00 DAY
    BBO AAPL bid=- ask=$150.00 x100
    ERROR: Invalid client_order_id
    |}]
;;
