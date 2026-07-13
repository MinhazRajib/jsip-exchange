(* These "open" lines let us use short names like [Symbol] and [Deferred]
   instead of long ones. Think of it like importing tools. *)
open! Core
open! Async
open Jsip_types

(* A shorter nickname for a long module name, so we can just write [Context]. *)
module Context = Jsip_bot_runtime.Bot_runtime.Context

(* The settings for this bot. The scenario fills these in. Each field is
   explained in cancel_storm.mli. *)
module Config = struct
  type t =
    { symbols : Symbol_id.t list
    ; cycles_per_tick : int
    ; order_size : int
    ; price_offset_cents : int
    ; client_order_ids : Client_order_id.Generator.t
    }
  [@@deriving sexp_of]
end

(* The bot's name, used in logs. *)
let name = "cancel-storm"

(* The lowest price we'll ever use: 1 cent. *)
let cent_floor = 1

(* Pick a price that will NOT trade, so the order just sits on the book
   waiting for us to cancel it.
   - A buy priced below the real price won't match.
   - A sell priced above the real price won't match. *)
let passive_price ~(side : Side.t) ~fundamental ~offset_cents =
  let cents = Price.to_int_cents fundamental in
  let target =
    match side with
    | Buy -> cents - offset_cents
    | Sell -> cents + offset_cents
  in
  (* Never go below 1 cent. *)
  Price.of_int_cents (Int.max cent_floor target)
;;

(* Do ONE "place an order, then cancel it" step. This is the whole point of
   the bot, repeated over and over. *)
let submit_then_cancel (config : Config.t) ctx =
  (* Our own random-number source. Using this keeps runs repeatable. *)
  let rng = Context.random ctx in
  (* Pick one of our symbols at random. *)
  let symbol =
    let n = List.length config.symbols in
    List.nth_exn config.symbols (Splittable_random.int rng ~lo:0 ~hi:(n - 1))
  in
  (* Randomly choose to buy or sell. *)
  let side : Side.t = if Splittable_random.bool rng then Buy else Sell in
  (* Get a fresh, never-used-before order id. This matters: the exchange
     rejects an id it has seen before, so reusing one would block all our
     later orders. *)
  let client_order_id =
    Client_order_id.of_int
      (Client_order_id.Generator.next config.client_order_ids)
  in
  (* Build the order we're about to send. *)
  let request : Order.Request.t =
    { client_order_id
    ; symbol
    ; participant = Context.participant ctx
    ; side
    ; price =
        passive_price
          ~side
          ~fundamental:(Context.fundamental ctx symbol)
          ~offset_cents:config.price_offset_cents
    ; size = Size.of_int config.order_size
    ; time_in_force = Day
    }
  in
  (* Send the order and wait for it to be accepted. ("match%bind" means: wait
     for the result, then look at it.) We wait first so the cancel comes
     AFTER the order, not before it. *)
  match%bind Context.submit ctx request with
  | Error err ->
    (* Sending failed. Log it and stop this step. *)
    [%log.error
      "cancel_storm: submit failed"
        (request : Order.Request.t)
        (err : Error.t)];
    return ()
  | Ok () ->
    (* Order went through, so now cancel that same order by its id. *)
    (match%map Context.cancel ctx client_order_id with
     | Ok () -> ()
     | Error err ->
       [%log.error
         "cancel_storm: cancel failed"
           (client_order_id : Client_order_id.t)
           (err : Error.t)])
;;

(* Runs once at the very start. This bot needs no setup, so it does nothing.
   ("return ()" just means "done, nothing to report".) *)
let on_start (_ : Config.t) _ctx = return ()

(* Runs over and over on a timer. Each time, we fire off many
   submit-then-cancel steps at once. That burst is what stresses the
   exchange. (One step per timer tick would be too gentle to be a "storm".) *)
let on_tick (config : Config.t) ctx =
  (* Make a list [0; 1; 2; ...] with one entry per step, then run a step for
     each entry, all at the same time. *)
  Deferred.List.iter
    ~how:`Parallel
    (List.init config.cycles_per_tick ~f:Fn.id)
    ~f:(fun (_ : int) -> submit_then_cancel config ctx)
;;

(* The exchange sends us messages here. This bot ignores most of them, but it
   logs any rejection, because a rejection usually means something is wrong
   (for example, our orders are accidentally trading instead of resting). *)
let on_event (_ : Config.t) _ctx (event : Exchange_event.t) =
  (match event with
   | Order_reject { request; reason } ->
     [%log.error
       "cancel_storm: order rejected"
         (request : Order.Request.t)
         (reason : string)]
   | Cancel_reject { client_order_id; reason; participant = _ } ->
     [%log.error
       "cancel_storm: cancel rejected"
         (client_order_id : Client_order_id.t)
         (reason : string)]
   (* We don't act on these; just ignore them. *)
   | Order_accept _ | Order_cancel _ | Fill _ | Best_bid_offer_update _
   | Trade_report _ ->
     ());
  return ()
;;
