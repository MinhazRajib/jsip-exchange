open! Core
open! Async
open Jsip_types
open Jsip_gateway
open Jsip_bot_runtime

module Config = struct
  type t =
    { participant : Participant.t
    ; symbol : Symbol.t
    ; fair_value_cents : int
    ; half_spread_cents : int
    ; size_per_level : int
    ; num_levels : int
    ; client_id_manager : Client_order_id.Generator.t
    ; mutable inventory_counter : Size.t Symbol.Table.t
    ; mutable resting_client_order_ids :
        Order.Request.t Client_order_id.Table.t
    }
  [@@deriving sexp_of]
end

(** helpers for event handling *)
let get_inventory_change (fill : Fill.t) market_participant =
  let is_aggressor =
    Participant.( = ) market_participant fill.aggressor_participant
  in
  match fill.aggressor_side with
  | Buy -> if is_aggressor then fill.size else Size.( * ) fill.size (-1)
  | Sell -> if is_aggressor then Size.( * ) fill.size (-1) else fill.size
;;

let get_client_order_id (fill : Fill.t) market_participant =
  let is_aggressor =
    Participant.( = ) market_participant fill.aggressor_participant
  in
  if is_aggressor
  then fill.aggressor_client_order_id
  else fill.resting_client_order_id
;;

(* let cancel_symbol_orders (fill : Fill.t) (resting_orders : Order.Request.t
   Client_order_id.Table.t) (cancel_function : Client_order_id.t -> unit
   Deferred.Or_error.t) = Hashtbl.iter resting_orders ~f:(fun key data -> let
   symbol = Order.Request.symbol data in ()) ;; *)

module Market_maker_bot : Jsip_bot_runtime.Bot_runtime.Bot = struct
  module Config = struct
    type t = Config.t
  end

  open Jsip_bot_runtime.Bot_runtime

  let name = "market_maker_bot"

  let seed_book (config : Config.t) (context : Context.t) =
    let submit request =
      let%bind result = (Context.submit context) request in
      match result with
      | Ok () -> Async.return ()
      | Error msg ->
        Async.return
          [%log.error
            "market_maker: submit failed"
              (request : Order.Request.t)
              (msg : Error.t)]
    in
    Deferred.List.iter
      ~how:`Parallel
      (List.init config.num_levels ~f:Fn.id)
      ~f:(fun level ->
        let offset = config.half_spread_cents + level in
        let%bind () =
          submit
            ({ client_order_id =
                 Client_order_id.of_int
                   (Client_order_id.Generator.next config.client_id_manager)
             ; symbol = config.symbol
             ; participant = config.participant
             ; side = Buy
             ; price = Price.of_int_cents (config.fair_value_cents - offset)
             ; size = Size.of_int config.size_per_level
             ; time_in_force = Day
             }
             : Order.Request.t)
        and () =
          submit
            ({ client_order_id =
                 Client_order_id.of_int
                   (Client_order_id.Generator.next config.client_id_manager)
             ; symbol = config.symbol
             ; participant = config.participant
             ; side = Sell
             ; price = Price.of_int_cents (config.fair_value_cents + offset)
             ; size = Size.of_int config.size_per_level
             ; time_in_force = Day
             }
             : Order.Request.t)
        in
        Deferred.unit)
  ;;

  let on_start (config : Config.t) (context : Context.t) : unit Deferred.t =
    let%bind () = seed_book config context in
    Deferred.unit
  ;;

  let on_tick (_config : Config.t) (_context : Context.t) : unit Deferred.t =
    (* only requotes in response to fill *)
    Deferred.unit
  ;;

  let on_event
    (config : Config.t)
    (context : Context.t)
    (event : Exchange_event.t)
    : unit Deferred.t
    =
    let participant = Context.participant context in
    (match event with
     | Order_accept order_accept ->
       Hashtbl.set
         config.resting_client_order_ids
         ~key:order_accept.request.client_order_id
         ~data:order_accept.request
     | Order_cancel order_cancel ->
       Hashtbl.remove
         config.resting_client_order_ids
         order_cancel.client_order_id
     | Fill fill ->
       let inventory_change = get_inventory_change fill participant in
       (match Hashtbl.find config.inventory_counter fill.symbol with
        | Some value ->
          let new_inventory = Size.( + ) inventory_change value in
          Hashtbl.set
            config.inventory_counter
            ~key:fill.symbol
            ~data:new_inventory
        | None ->
          Hashtbl.set
            config.inventory_counter
            ~key:fill.symbol
            ~data:inventory_change);
       (* remove order if whole order size is consumed *)
       let client_order_id = get_client_order_id fill participant in
       (match
          Hashtbl.find config.resting_client_order_ids client_order_id
        with
        | Some client_order_request ->
          (* remove if fully filled, reduce size if partial fill *)
          if Size.( = ) fill.size client_order_request.size
          then Hashtbl.remove config.resting_client_order_ids client_order_id
          else
            Hashtbl.set
              config.resting_client_order_ids
              ~key:client_order_id
              ~data:
                { client_order_request with
                  size = Size.( - ) client_order_request.size fill.size
                }
        | None -> ())
       (* react to fills *)
     | Order_reject _ | Cancel_reject _ | Best_bid_offer_update _
     | Trade_report _ ->
       ());
    Deferred.unit
  ;;
end
