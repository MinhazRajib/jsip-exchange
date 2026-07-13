open! Core
open Jsip_types

(* The render boundary. Events arrive carrying a symbol id; a human wants to
   read a name. Every symbol here goes through [symbol], which is the
   directory lookup. Note nothing in [lib/types] knows the directory exists —
   the renderers there take this resolver as an argument. *)
let format_event ~directory event =
  let symbol = Symbol_directory.name directory in
  match (event : Exchange_event.t) with
  | Order_accept { order_id; request } ->
    sprintf
      "ACCEPTED client-id=%s id=%s %s %s %d@%s %s"
      (Client_order_id.to_string request.client_order_id)
      (Order_id.to_string order_id)
      (symbol request.symbol)
      (Side.to_string request.side)
      (Size.to_int request.size)
      (Price.to_string_dollar request.price)
      (Time_in_force.to_string request.time_in_force)
  | Fill fill ->
    let fill = Fill.to_string ~symbol_to_string:symbol fill in
    [%string "FILL %{fill}"]
  | Order_cancel
      { client_order_id
      ; order_id
      ; participant = _
      ; symbol = symbol_id
      ; remaining_size
      ; reason
      } ->
    sprintf
      "CANCELLED client_id=%s id=%s %s remaining=%d reason=%s"
      (Client_order_id.to_string client_order_id)
      (Order_id.to_string order_id)
      (symbol symbol_id)
      (Size.to_int remaining_size)
      (Cancel_reason.to_string reason)
  | Order_reject { request; reason } ->
    sprintf
      "REJECTED client-id=%s %s %s %d@%s reason=%s"
      (Client_order_id.to_string request.client_order_id)
      (symbol request.symbol)
      (Side.to_string request.side)
      (Size.to_int request.size)
      (Price.to_string_dollar request.price)
      reason
  | Best_bid_offer_update { symbol = symbol_id; bbo } ->
    let name = symbol symbol_id in
    let bid = Level.opt_to_string bbo.bid in
    let ask = Level.opt_to_string bbo.ask in
    [%string "BBO %{name} bid=%{bid} ask=%{ask}"]
  | Trade_report { symbol = symbol_id; price; size } ->
    let name = symbol symbol_id in
    let size = Size.to_int size in
    [%string "TRADE %{name} %{price#Price} x%{size#Int}"]
  | Cancel_reject { participant; client_order_id; reason } ->
    sprintf
      "REJECTED Cancel Request client-id:%s (%s) reason=%s"
      (Client_order_id.to_string client_order_id)
      (Participant.to_string participant)
      reason
;;

let format_events ~directory events =
  List.map events ~f:(format_event ~directory) |> String.concat ~sep:"\n"
;;

let format_book ~directory book =
  Book.to_string ~symbol_to_string:(Symbol_directory.name directory) book
;;

let format_fill_for_participant ~directory fill participant =
  Fill.to_participant_view
    ~symbol_to_string:(Symbol_directory.name directory)
    fill
    participant
;;
