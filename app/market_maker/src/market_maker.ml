open! Core
open! Async
open Jsip_types

module Config = struct
  type t =
    { symbol : Symbol.t
    ; fair_value_cents : int
    ; half_spread_cents : int
    ; size_per_level : int
    ; num_levels : int
    ; client_id_manager : Client_order_id.Generator.t
    ; inventory_skew_cents_per_share : int
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

let cancel_symbol_orders
  (fill : Fill.t)
  (resting_orders : Order.Request.t Client_order_id.Table.t)
  cancel_function
  =
  let ids_to_cancel =
    Hashtbl.filteri resting_orders ~f:(fun ~key:_ ~data ->
      Symbol.( = ) fill.symbol data.symbol)
    |> Hashtbl.keys
  in
  Deferred.List.iter ~how:`Parallel ids_to_cancel ~f:(fun client_order_id ->
    match%bind cancel_function client_order_id with
    | Ok () -> Deferred.unit
    | Error msg ->
      [%log.error
        "market_maker: cancel failed"
          (client_order_id : Client_order_id.t)
          (msg : Error.t)];
      Deferred.unit)
;;

let get_skew (config : Config.t) (symbol : Symbol.t) =
  let symbol_inventory =
    Size.to_int (Hashtbl.find_exn config.inventory_counter symbol)
  in
  config.fair_value_cents
  - (symbol_inventory * config.inventory_skew_cents_per_share)
;;

module Market_maker_bot :
  Jsip_bot_runtime.Bot_runtime.Bot with type Config.t = Config.t = struct
  module Config = struct
    type t = Config.t
  end

  open Jsip_bot_runtime.Bot_runtime

  let name = "market_maker_bot"

  let seed_book (config : Config.t) (context : Context.t) =
    let submit side offset =
      let price =
        match side with
        | Side.Buy -> Price.of_int_cents (config.fair_value_cents - offset)
        | Sell -> Price.of_int_cents (config.fair_value_cents + offset)
      in
      let request =
        ({ client_order_id =
             Client_order_id.of_int
               (Client_order_id.Generator.next config.client_id_manager)
         ; symbol = config.symbol
         ; participant = Context.participant context
         ; side
         ; price
         ; size = Size.of_int config.size_per_level
         ; time_in_force = Day
         }
         : Order.Request.t)
      in
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
        let%bind () = submit Buy offset
        and () = submit Sell offset in
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
    match event with
    | Order_accept order_accept ->
      Hashtbl.set
        config.resting_client_order_ids
        ~key:order_accept.request.client_order_id
        ~data:order_accept.request;
      Deferred.unit
    | Order_cancel order_cancel ->
      Hashtbl.remove
        config.resting_client_order_ids
        order_cancel.client_order_id;
      Deferred.unit
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
       | None -> ());
      (* react to fills *)
      let%bind () =
        cancel_symbol_orders
          fill
          config.resting_client_order_ids
          (Context.cancel context)
      in
      let new_config =
        { config with fair_value_cents = get_skew config fill.symbol }
      in
      seed_book new_config context
    | Order_reject _ | Cancel_reject _ | Best_bid_offer_update _
    | Trade_report _ ->
      Deferred.unit
  ;;
end
