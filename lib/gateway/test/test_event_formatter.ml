open! Core
open Jsip_types
open Jsip_gateway

(** Event formating with example clients and interactions

    Tests confirm the reliability of format_event, specifically regarding
    BUY/SELL commands, no testing for book / subscribe. *)
(* --- Event formatting --- *)

let%expect_test "format_event: all event types" =
  let events =
    [ Exchange_event.Order_accept
        { order_id = Order_id.of_string "1"
        ; request =
            { client_order_id = Client_order_id.of_int 0
            ; symbol = Symbol.of_string "AAPL"
            ; participant = Participant.of_string "Alice"
            ; side = Buy
            ; price = Price.of_int_cents 15000
            ; size = Size.of_int 100
            ; time_in_force = Day
            }
        }
    ; Fill
        { fill_id = 1
        ; symbol = Symbol.of_string "AAPL"
        ; price = Price.of_int_cents 15000
        ; size = Size.of_int 100
        ; aggressor_order_id = Order_id.of_string "2"
        ; aggressor_client_order_id = Client_order_id.of_int 0
        ; aggressor_participant = Participant.of_string "Alice"
        ; aggressor_side = Buy
        ; resting_order_id = Order_id.of_string "1"
        ; resting_client_order_id = Client_order_id.of_int 0
        ; resting_participant = Participant.of_string "Bob"
        }
    ; Order_cancel
        { client_order_id = Client_order_id.of_int 0
        ; order_id = Order_id.of_string "3"
        ; participant = Participant.of_string "Charlie"
        ; symbol = Symbol.of_string "TSLA"
        ; remaining_size = Size.of_int 50
        ; reason = Ioc_remainder
        }
    ; Order_reject
        { request =
            { client_order_id = Client_order_id.of_int 0
            ; symbol = Symbol.of_string "GOOG"
            ; participant = Participant.of_string "Alice"
            ; side = Sell
            ; price = Price.of_int_cents 28000
            ; size = Size.of_int 10
            ; time_in_force = Day
            }
        ; reason = "unknown symbol"
        }
    ; Best_bid_offer_update
        { symbol = Symbol.of_string "AAPL"
        ; bbo =
            { bid =
                Some
                  { price = Price.of_int_cents 14990
                  ; size = Size.of_int 200
                  }
            ; ask =
                Some
                  { price = Price.of_int_cents 15010
                  ; size = Size.of_int 100
                  }
            }
        }
    ; Best_bid_offer_update
        { symbol = Symbol.of_string "AAPL"; bbo = Bbo.empty }
    ; Trade_report
        { symbol = Symbol.of_string "AAPL"
        ; price = Price.of_int_cents 15000
        ; size = Size.of_int 100
        }
    ]
  in
  List.iter events ~f:(fun e ->
    print_endline (Event_formatter.format_event e));
  [%expect
    {|
    ACCEPTED client-id=0 id=1 AAPL BUY 100@$150.00 DAY
    FILL fill_id=1 AAPL $150.00 x100 aggressor=2 (client-id=0) (Alice) BUY resting=1 (client-id=0) (Bob)
    CANCELLED client_id=0 id=3 TSLA remaining=50 reason=IOC_REMAINDER
    REJECTED client-id=0 GOOG SELL 10@$280.00 reason=unknown symbol
    BBO AAPL bid=$149.90 x200 ask=$150.10 x100
    BBO AAPL bid=- ask=-
    TRADE AAPL $150.00 x100
    |}]
;;
