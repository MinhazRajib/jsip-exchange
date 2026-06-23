open! Core
open Jsip_types
open Expect_test_helpers_core

(* -- Passing Test Cases -- *)
let test_symbol = Symbol.of_string "AAPL"

let%expect_test "of_string: alphanum capitalized does not raise" =
  [%test_result: Symbol.t] (Symbol.of_string "AAPL") ~expect:test_symbol
;;

let%expect_test "of_string: alphanum uncapitalized changes, does not raise" =
  [%test_result: Symbol.t] (Symbol.of_string "AapL") ~expect:test_symbol;
  [%test_result: Symbol.t] (Symbol.of_string "aapl") ~expect:test_symbol;
  [%test_result: Symbol.t] (Symbol.of_string "Aapl") ~expect:test_symbol;
  [%test_result: Symbol.t] (Symbol.of_string "AAPl") ~expect:test_symbol
;;

(* -- Failing Test Cases -- *)
let%expect_test "of_string: empty string raises" =
  require_does_raise (fun () -> Symbol.of_string "");
  [%expect {| "Symbol.of_string: symbol must be non-empty" |}]
;;

let%expect_test "of_string: special character raises" =
  require_does_raise (fun () -> Symbol.of_string "A!pl");
  [%expect
    {| "Symbol.of_string: symbol must contain only alphanumeric characters" |}];
  require_does_raise (fun () -> Symbol.of_string "A pl");
  [%expect
    {| "Symbol.of_string: symbol must contain only alphanumeric characters" |}];
  require_does_raise (fun () -> Symbol.of_string "\n   ");
  [%expect
    {| "Symbol.of_string: symbol must contain only alphanumeric characters" |}];
  require_does_raise (fun () -> Symbol.of_string "Aapl\n\t");
  [%expect
    {| "Symbol.of_string: symbol must contain only alphanumeric characters" |}];
  require_does_raise (fun () -> Symbol.of_string "A@pl\n\t");
  [%expect
    {| "Symbol.of_string: symbol must contain only alphanumeric characters" |}]
;;
