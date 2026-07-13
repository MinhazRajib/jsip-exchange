open! Core
open Jsip_types

module Participant_client_order_ids = struct
  type t =
    { participant : Participant.t
    ; client_order_id : Client_order_id.t
    }
  [@@deriving sexp, compare, sexp_of, hash]

  include functor Comparable.Make_plain
  include functor Hashable.Make_plain
end

(* One order book per symbol, held in a flat array indexed by the symbol's
   id. Ids are assigned by whoever owns the symbol list (the exchange server)
   and are fixed for the engine's lifetime, so the array never resizes and a
   book lookup is a bounds check plus an array index — no hashing at all.

   Before the id went on the wire, this array was paired with a
   [Symbol.Table] that hashed the incoming symbol string to its id. Now the
   client sends the id directly, so that table is gone. *)
type t =
  { books : Order_book.t array
  ; order_id_gen : Order_id.Generator.t
  ; mutable next_fill_id : int
  ; mutable participant_client_order_ids :
      Order.t Participant_client_order_ids.Table.t
  }
[@@deriving sexp_of]

let create ~num_symbols =
  { books =
      Array.init num_symbols ~f:(fun i ->
        Order_book.create (Symbol_id.of_int i))
  ; order_id_gen = Order_id.Generator.create ()
  ; next_fill_id = 1
  ; participant_client_order_ids =
      Participant_client_order_ids.Table.create ()
  }
;;

let check_client_order_id t participant client_order_id =
  Hashtbl.find
    t.participant_client_order_ids
    ({ participant; client_order_id } : Participant_client_order_ids.t)
;;

(* A symbol id arrives straight off the wire, and [bin_io] deserializes
   whatever integer it is handed — [Symbol_id.t] being a private int stops us
   building a bad id in OCaml, but it cannot stop a client sending one. So
   bounds-check before indexing. An id naming no symbol yields [None], which
   [submit] and [cancel] already turn into a rejection. *)
let book t symbol_id =
  let i = Symbol_id.to_int symbol_id in
  if i < 0 || i >= Array.length t.books then None else Some t.books.(i)
;;

(* remove an order *)
let cancel t participant client_order_id =
  match check_client_order_id t participant client_order_id with
  | None ->
    [ Exchange_event.Cancel_reject
        { participant; client_order_id; reason = "Order does not exist" }
    ]
  | Some order ->
    (match book t (Order.symbol order) with
     | None ->
       [ Exchange_event.Cancel_reject
           { participant; client_order_id; reason = "Order does not exist" }
       ]
     | Some book ->
       Hashtbl.remove
         t.participant_client_order_ids
         ({ participant; client_order_id } : Participant_client_order_ids.t);
       let bbo_before = Order_book.best_bid_offer book in
       Order_book.remove book (Order.order_id order);
       let bbo_after = Order_book.best_bid_offer book in
       let order_cancel =
         Exchange_event.Order_cancel
           { client_order_id = Order.client_order_id order
           ; order_id = Order.order_id order
           ; participant = Order.participant order
           ; symbol = Order.symbol order
           ; remaining_size = Order.size order
           ; reason = Cancel_reason.Participant_requested
           }
       in
       let bbo_events =
         if Bbo.equal bbo_before bbo_after
         then []
         else
           [ Exchange_event.Best_bid_offer_update
               { symbol = Order.symbol order; bbo = bbo_after }
           ]
       in
       List.concat [ [ order_cancel ]; bbo_events ])
;;

(** Run the matching loop: repeatedly find a compatible resting order and
    fill against it. Returns the list of Fill and Trade_report events
    produced, and the next fill_id to use. *)
let rec match_loop ~book ~order ~fill_id =
  if Size.( <= ) (Order.remaining_size order) Size.zero
  then [], fill_id
  else (
    match Order_book.find_match book order with
    | None -> [], fill_id
    | Some resting ->
      let fill_size =
        Size.min (Order.remaining_size order) (Order.remaining_size resting)
      in
      Order.fill order ~by:fill_size;
      Order.fill resting ~by:fill_size;
      if Order.is_fully_filled resting
      then Order_book.remove book (Order.order_id resting);
      let fill_event =
        Exchange_event.Fill
          { fill_id
          ; symbol = Order.symbol order
          ; price = Order.price resting
          ; size = fill_size
          ; aggressor_order_id = Order.order_id order
          ; aggressor_participant = Order.participant order
          ; aggressor_client_order_id = Order.client_order_id order
          ; aggressor_side = Order.side order
          ; resting_order_id = Order.order_id resting
          ; resting_client_order_id = Order.client_order_id resting
          ; resting_participant = Order.participant resting
          }
      in
      let trade_event =
        Exchange_event.Trade_report
          { symbol = Order.symbol order
          ; price = Order.price resting
          ; size = fill_size
          }
      in
      let remaining_events, next_fill_id =
        match_loop ~book ~order ~fill_id:(fill_id + 1)
      in
      fill_event :: trade_event :: remaining_events, next_fill_id)
;;

let submit t (request : Order.Request.t) =
  match book t request.symbol with
  | None ->
    [ Exchange_event.Order_reject { request; reason = "unknown symbol" } ]
  | Some book ->
    let order_id = Order_id.Generator.next t.order_id_gen in
    let order = Order.create request ~order_id in
    let accepted = Exchange_event.Order_accept { order_id; request } in
    (* Snapshot BBO before matching so we can detect changes. *)
    let bbo_before = Order_book.best_bid_offer book in
    (* Match *)
    let fill_events, next_fill_id =
      match_loop ~book ~order ~fill_id:t.next_fill_id
    in
    t.next_fill_id <- next_fill_id;
    (* Post-match: rest on book or cancel unfilled remainder. *)
    let post_events =
      if Size.( > ) (Order.remaining_size order) Size.zero
      then (
        match Order.time_in_force order with
        | Day ->
          Order_book.add book order;
          let key : Participant_client_order_ids.t =
            { participant = Order.participant order
            ; client_order_id = Order.client_order_id order
            }
          in
          Hashtbl.add_exn t.participant_client_order_ids ~key ~data:order;
          []
        | Ioc ->
          [ Exchange_event.Order_cancel
              { client_order_id = request.client_order_id
              ; order_id
              ; participant = Order.participant order
              ; symbol = Order.symbol order
              ; remaining_size = Order.remaining_size order
              ; reason = Ioc_remainder
              }
          ])
      else []
    in
    (* Emit BBO update if the best bid or ask changed. *)
    let bbo_after = Order_book.best_bid_offer book in
    let bbo_events =
      if Bbo.equal bbo_before bbo_after
      then []
      else
        [ Exchange_event.Best_bid_offer_update
            { symbol = Order.symbol order; bbo = bbo_after }
        ]
    in
    List.concat [ [ accepted ]; fill_events; post_events; bbo_events ]
;;
