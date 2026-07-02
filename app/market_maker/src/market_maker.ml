open! Core
open! Async
open Jsip_types
open Jsip_gateway

module Config = struct
  type t =
    { participant : Participant.t
    ; symbol : Symbol.t
    ; fair_value_cents : int
    ; half_spread_cents : int
    ; size_per_level : int
    ; num_levels : int
    }
  [@@deriving sexp_of]
end

let seed_book (config : Config.t) conn =
  let submit request =
    let%map result =
      Rpc.Rpc.dispatch_exn Rpc_protocol.submit_order_rpc conn request
    in
    match result with
    | Ok () -> ()
    | Error msg ->
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
          ({ symbol = config.symbol
           ; participant = config.participant
           ; side = Buy
           ; price = Price.of_int_cents (config.fair_value_cents - offset)
           ; size = Size.of_int config.size_per_level
           ; time_in_force = Day
           ; client_order_id = Client_order_id.of_int 0
           }
           : Order.Request.t)
      and () =
        submit
          ({ symbol = config.symbol
           ; participant = config.participant
           ; side = Sell
           ; price = Price.of_int_cents (config.fair_value_cents + offset)
           ; size = Size.of_int config.size_per_level
           ; time_in_force = Day
           ; client_order_id = Client_order_id.of_int 0
           }
           : Order.Request.t)
      in
      Deferred.unit)
;;

(* --- Dynamic market maker (Exercise 2) -------------------------------- *)

(* One order we currently believe is resting on the book.

   We key these by the exchange-assigned [Order_id.t], because that is what
   [Fill] and [Order_cancel] events reference. But we also remember the
   [client_order_id] we chose ourselves, because the cancel RPC identifies an
   order by its *client* id, not its exchange id. [Order_accept] is the only
   event that shows us both ids together, so that is where we learn the
   pairing. *)
module Resting_order = struct
  type t =
    { client_order_id : Client_order_id.t
    ; symbol : Symbol.t
    ; mutable remaining_size : Size.t
    }
  [@@deriving sexp_of]
end

(* All the mutable state a running market maker carries. Kept out of the
   [.mli]: callers only ever see [run]. *)
type t =
  { config : Config.t
  ; conn : Rpc.Connection.t
  ; client_order_ids : Client_order_id.Generator.t
  ; resting_orders : Resting_order.t Order_id.Table.t
  ; inventory : int Symbol.Table.t
  }

(* Our net position in [symbol] moves by [+size] when we buy and [-size] when
   we sell. [Side.sign] is exactly that (+1 for Buy, -1 for Sell), so the
   whole update is one multiply. *)
let apply_fill_to_inventory t ~symbol ~(side : Side.t) ~size =
  let delta = Side.sign side * Size.to_int size in
  Hashtbl.update t.inventory symbol ~f:(function
    | None -> delta
    | Some current -> current + delta)
;;

let handle_fill t (fill : Fill.t) =
  let me = t.config.participant in
  (* A fill always has two sides. Work out which one (if either) is us, and
     which side we traded. If we were the aggressor, our side is
     [aggressor_side]; if we were the resting order, we traded the opposite
     side. Self-trade prevention means we can never be both at once. *)
  let ours =
    if Participant.equal fill.aggressor_participant me
    then Some (fill.aggressor_side, fill.aggressor_order_id)
    else if Participant.equal fill.resting_participant me
    then Some (Side.flip fill.aggressor_side, fill.resting_order_id)
    else None
  in
  match ours with
  | None -> ()
  | Some (side, order_id) ->
    apply_fill_to_inventory t ~symbol:fill.symbol ~side ~size:fill.size;
    (* Reduce the remaining size of the order this fill hit. If nothing is
       left, the order is gone from the book, so drop it from our map. *)
    (match Hashtbl.find t.resting_orders order_id with
     | None -> ()
     | Some resting ->
       let remaining = Size.( - ) resting.remaining_size fill.size in
       if Size.to_int remaining <= 0
       then Hashtbl.remove t.resting_orders order_id
       else resting.remaining_size <- remaining)
;;

let handle_event t (event : Exchange_event.t) =
  match event with
  | Order_accept { order_id; request } ->
    Hashtbl.set
      t.resting_orders
      ~key:order_id
      ~data:
        { Resting_order.client_order_id = request.client_order_id
        ; symbol = request.symbol
        ; remaining_size = request.size
        }
  | Fill fill -> handle_fill t fill
  | Order_cancel { order_id; _ } -> Hashtbl.remove t.resting_orders order_id
  | Order_reject { request; reason } ->
    [%log.error
      "market_maker: order rejected"
        (request : Order.Request.t)
        (reason : string)]
  | Cancel_reject { client_order_id; reason; participant = _ } ->
    [%log.error
      "market_maker: cancel rejected"
        (client_order_id : Client_order_id.t)
        (reason : string)]
  | Best_bid_offer_update _ | Trade_report _ -> ()
;;

(* Submit one order over the connection, logging any submission failure. *)
let submit t (request : Order.Request.t) =
  let%map result =
    Rpc.Rpc.dispatch_exn Rpc_protocol.submit_order_rpc t.conn request
  in
  match result with
  | Ok () -> ()
  | Error msg ->
    [%log.error
      "market_maker: submit failed"
        (request : Order.Request.t)
        (msg : Error.t)]
;;

(* Place the initial ladder, just like [seed_book], but hand every order a
   fresh [client_order_id] from the generator so we can cancel it later. We
   record nothing here: the [Order_accept] events that come back on the
   session feed are what populate [resting_orders]. *)
let seed_ladder t =
  let config = t.config in
  Deferred.List.iter
    ~how:`Parallel
    (List.init config.num_levels ~f:Fn.id)
    ~f:(fun level ->
      let offset = config.half_spread_cents + level in
      let order ~(side : Side.t) ~price_cents : Order.Request.t =
        { symbol = config.symbol
        ; participant = config.participant
        ; side
        ; price = Price.of_int_cents price_cents
        ; size = Size.of_int config.size_per_level
        ; time_in_force = Day
        ; client_order_id =
            Client_order_id.of_int (Client_order_id.Generator.next t.client_order_ids)
        }
      in
      let%bind () =
        submit
          t
          (order ~side:Buy ~price_cents:(config.fair_value_cents - offset))
      and () =
        submit
          t
          (order ~side:Sell ~price_cents:(config.fair_value_cents + offset))
      in
      Deferred.unit)
;;

let run (config : Config.t) conn =
  let t =
    { config
    ; conn
    ; client_order_ids = Client_order_id.Generator.create ()
    ; resting_orders = Order_id.Table.create ()
    ; inventory = Symbol.Table.create ()
    }
  in
  (* Subscribe to the session feed *before* seeding, so we don't miss the
     [Order_accept] (and possibly [Fill]) events produced by our own initial
     orders. *)
  let%bind session_feed, _metadata =
    Rpc.Pipe_rpc.dispatch_exn Rpc_protocol.session_feed_rpc conn ()
  in
  don't_wait_for
    (Pipe.iter_without_pushback session_feed ~f:(handle_event t));
  let%bind () = seed_ladder t in
  Deferred.never ()
;;
