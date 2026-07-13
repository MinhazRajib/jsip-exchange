open! Core
open Jsip_types
open Jsip_order_book
open Jsip_gateway

(* --- Constants --- *)

let aapl = Symbol.of_string "AAPL"
let tsla = Symbol.of_string "TSLA"
let goog = Symbol.of_string "GOOG"

(* The exchange gives the [i]th symbol it is told about the id [i], so these
   ids are the ones the default symbol list below hands out. Tests that build
   orders want the id; only tests that start a real server want the name. *)
let aapl_id = Symbol_id.of_int 0
let tsla_id = Symbol_id.of_int 1
let goog_id = Symbol_id.of_int 2

(* The directory for the default symbol list, for tests that render events
   without building a whole harness. *)
let default_directory = Symbol_directory.of_symbols [ aapl; tsla; goog ]
let alice = Participant.of_string "Alice"
let bob = Participant.of_string "Bob"
let charlie = Participant.of_string "Charlie"
let market_maker = Participant.of_string "MarketMaker"

(* --- Harness --- *)

type t =
  { engine : Matching_engine.t
  ; directory : Symbol_directory.t
  }

let create ?(symbols = [ aapl; tsla; goog ]) () =
  { engine = Matching_engine.create ~num_symbols:(List.length symbols)
  ; directory = Symbol_directory.of_symbols symbols
  }
;;

let engine t = t.engine
let directory t = t.directory

(* --- Builders --- *)

let make_request
  ~side
  ~price_cents
  ?(size = 100)
  ?(client_id = Client_order_id.of_int 0)
  ?(symbol = aapl_id)
  ?(participant = alice)
  ?(time_in_force = Time_in_force.Day)
  ()
  : Order.Request.t
  =
  { client_order_id = client_id
  ; symbol
  ; participant
  ; side
  ; price = Price.of_int_cents price_cents
  ; size = Size.of_int size
  ; time_in_force
  }
;;

let buy ~price_cents ?size ?client_id ?symbol ?participant ?time_in_force () =
  make_request
    ~side:Buy
    ~price_cents
    ?size
    ?client_id
    ?symbol
    ?participant
    ?time_in_force
    ()
;;

let sell ~price_cents ?size ?client_id ?symbol ?participant ?time_in_force ()
  =
  make_request
    ~side:Sell
    ~price_cents
    ?size
    ?client_id
    ?symbol
    ?participant
    ?time_in_force
    ()
;;

(* --- Formatting --- *)

module Show = struct
  type t = Exchange_event.t -> bool

  let all _ = true
  let only f = f
  let no_market_data event = not (Exchange_event.is_market_data event)
end

let print_events t ?(show = Show.all) events =
  List.iter events ~f:(fun event ->
    if show event
    then
      print_endline
        (Event_formatter.format_event ~directory:t.directory event))
;;

let print_event t event =
  print_endline (Event_formatter.format_event ~directory:t.directory event)
;;

let submit t request =
  let events = Matching_engine.submit t.engine request in
  print_events t events;
  events
;;

let submit_ t request = ignore (submit t request : Exchange_event.t list)
let submit_quiet t request = Matching_engine.submit (engine t) request

let sample_events : Exchange_event.t list =
  let order_request : Order.Request.t =
    { client_order_id =
        Client_order_id.of_int
          0 (* sample order_id, change for redundant order id testing *)
    ; symbol = aapl_id
    ; participant = alice
    ; side = Buy
    ; price = Price.of_int_cents 15000
    ; size = Size.of_int 100
    ; time_in_force = Day
    }
  in
  [ Order_accept
      { order_id = Order_id.For_testing.of_int 1; request = order_request }
  ; Fill
      { fill_id = 1
      ; symbol = aapl_id
      ; price = Price.of_int_cents 15000
      ; size = Size.of_int 100
      ; aggressor_order_id = Order_id.For_testing.of_int 2
      ; aggressor_client_order_id = Client_order_id.of_int 0
      ; aggressor_participant = alice
      ; aggressor_side = Buy
      ; resting_order_id = Order_id.For_testing.of_int 1
      ; resting_client_order_id = Client_order_id.of_int 0
      ; resting_participant = bob
      }
  ; Order_cancel
      { client_order_id = Client_order_id.of_int 0
      ; order_id = Order_id.For_testing.of_int 1
      ; participant = alice
      ; symbol = aapl_id
      ; remaining_size = Size.of_int 50
      ; reason = Ioc_remainder
      }
  ; Order_reject { request = order_request; reason = "unknown symbol" }
  ; Best_bid_offer_update
      { symbol = aapl_id
      ; bbo =
          { bid =
              Some
                { price = Price.of_int_cents 14990; size = Size.of_int 100 }
          ; ask =
              Some
                { price = Price.of_int_cents 15010; size = Size.of_int 200 }
          }
      }
  ; Trade_report
      { symbol = aapl_id
      ; price = Price.of_int_cents 15000
      ; size = Size.of_int 100
      }
  ]
;;

let submit_quiet_ t request =
  ignore (submit_quiet t request : Exchange_event.t list)
;;

let print_book t symbol =
  let name = Symbol_directory.name t.directory symbol in
  match Matching_engine.book t.engine symbol with
  | None -> print_endline [%string "unknown symbol %{name}"]
  | Some book ->
    Order_book.snapshot book
    |> Event_formatter.format_book ~directory:t.directory
    |> print_endline
;;

let print_bbo t symbol =
  let name = Symbol_directory.name t.directory symbol in
  match Matching_engine.book t.engine symbol with
  | None -> print_endline [%string "BBO %{name}: unknown symbol"]
  | Some book ->
    let bbo = Order_book.best_bid_offer book |> Bbo.to_string in
    print_endline [%string "BBO %{name}: %{bbo}"]
;;
