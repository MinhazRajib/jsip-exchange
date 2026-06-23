open! Core
open Jsip_types

let%expect_test "notional_cents: price * size" =
  let fill =
    ({ fill_id = 1
     ; symbol = Symbol.of_string "AAPL"
     ; price = Price.of_int_cents 15025
     ; size = Size.of_int 100
     ; aggressor_order_id = Order_id.of_string "1"
     ; aggressor_participant = Participant.of_string "Alice"
     ; aggressor_side = Buy
     ; resting_order_id = Order_id.of_string "2"
     ; resting_participant = Participant.of_string "Bob"
     }
     : Fill.t)
  in
  [%test_result: int] (Fill.notional_cents fill) ~expect:1502500
;;

let%expect_test "to_participant_view: participant is aggressor" =
  let participant = Participant.of_string "Alice" in
  let fill =
    ({ fill_id = 1
     ; symbol = Symbol.of_string "AAPL"
     ; price = Price.of_int_cents 15000
     ; size = Size.of_int 250
     ; aggressor_order_id = Order_id.of_string "1"
     ; aggressor_participant = Participant.of_string "Alice"
     ; aggressor_side = Buy
     ; resting_order_id = Order_id.of_string "2"
     ; resting_participant = Participant.of_string "Bob"
     }
     : Fill.t)
  in
  [%test_result: string option]
    (Fill.to_participant_view fill participant)
    ~expect:(Some "You bought 250 AAPL at $150.00");
  let fill' =
    ({ fill_id = 1
     ; symbol = Symbol.of_string "AAPL"
     ; price = Price.of_int_cents 15000
     ; size = Size.of_int 250
     ; aggressor_order_id = Order_id.of_string "1"
     ; aggressor_participant = Participant.of_string "Alice"
     ; aggressor_side = Sell
     ; resting_order_id = Order_id.of_string "2"
     ; resting_participant = Participant.of_string "Bob"
     }
     : Fill.t)
  in
  [%test_result: string option]
    (Fill.to_participant_view fill' participant)
    ~expect:(Some "You sold 250 AAPL at $150.00")
;;

let%expect_test "to_participant_view: participant is resting" =
  let participant = Participant.of_string "Alice" in
  let fill =
    ({ fill_id = 1
     ; symbol = Symbol.of_string "AAPL"
     ; price = Price.of_int_cents 15000
     ; size = Size.of_int 250
     ; aggressor_order_id = Order_id.of_string "1"
     ; aggressor_participant = Participant.of_string "Bob"
     ; aggressor_side = Buy
     ; resting_order_id = Order_id.of_string "2"
     ; resting_participant = Participant.of_string "Alice"
     }
     : Fill.t)
  in
  [%test_result: string option]
    (Fill.to_participant_view fill participant)
    ~expect:(Some "You sold 250 AAPL at $150.00");
  let fill' =
    ({ fill_id = 1
     ; symbol = Symbol.of_string "AAPL"
     ; price = Price.of_int_cents 15000
     ; size = Size.of_int 250
     ; aggressor_order_id = Order_id.of_string "1"
     ; aggressor_participant = Participant.of_string "Bob"
     ; aggressor_side = Sell
     ; resting_order_id = Order_id.of_string "2"
     ; resting_participant = Participant.of_string "Alice"
     }
     : Fill.t)
  in
  [%test_result: string option]
    (Fill.to_participant_view fill' participant)
    ~expect:(Some "You bought 250 AAPL at $150.00")
;;

let%expect_test "to_participant_view: participant not in Fill" =
  let participant = Participant.of_string "John" in
  let fill =
    ({ fill_id = 1
     ; symbol = Symbol.of_string "AAPL"
     ; price = Price.of_int_cents 15000
     ; size = Size.of_int 250
     ; aggressor_order_id = Order_id.of_string "1"
     ; aggressor_participant = Participant.of_string "Bob"
     ; aggressor_side = Buy
     ; resting_order_id = Order_id.of_string "2"
     ; resting_participant = Participant.of_string "Alice"
     }
     : Fill.t)
  in
  [%test_result: string option]
    (Fill.to_participant_view fill participant)
    ~expect:None
;;
