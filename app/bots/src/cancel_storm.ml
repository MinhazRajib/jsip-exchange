open! Core
open! Async
open Jsip_types
module Context = Jsip_bot_runtime.Bot_runtime.Context

module Config = struct
  type t =
    { symbols : Symbol.t list
    ; cycles_per_tick : int
    ; order_size : int
    ; price_offset_cents : int
    ; client_order_ids : Client_order_id.Generator.t
    }
  [@@deriving sexp_of]
end

let name = "cancel-storm"

(* A resting order priced [offset] away from the fundamental never crosses,
   so the buy sits below and the sell above the true price. We clamp to a
   floor of one cent so a large offset on a cheap symbol can't produce a
   non-positive price. *)
let cent_floor = 1

let passive_price ~(side : Side.t) ~fundamental ~offset_cents =
  let cents = Price.to_int_cents fundamental in
  let target =
    match side with
    | Buy -> cents - offset_cents
    | Sell -> cents + offset_cents
  in
  Price.of_int_cents (Int.max cent_floor target)
;;

(* One submit-then-cancel cycle. We wait for the submit to be enqueued before
   cancelling so the cancel lands after its order on the request queue, which
   is what keeps the cancel targeting a real resting order rather than racing
   ahead of it. *)
let submit_then_cancel (config : Config.t) ctx =
  let rng = Context.random ctx in
  let symbol =
    let n = List.length config.symbols in
    List.nth_exn config.symbols (Splittable_random.int rng ~lo:0 ~hi:(n - 1))
  in
  let side : Side.t = if Splittable_random.bool rng then Buy else Sell in
  let client_order_id =
    Client_order_id.of_int
      (Client_order_id.Generator.next config.client_order_ids)
  in
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
  match%bind Context.submit ctx request with
  | Error err ->
    [%log.error "cancel_storm: submit failed" (request : Order.Request.t) (err : Error.t)];
    return ()
  | Ok () ->
    (match%map Context.cancel ctx client_order_id with
     | Ok () -> ()
     | Error err ->
       [%log.error
         "cancel_storm: cancel failed"
           (client_order_id : Client_order_id.t)
           (err : Error.t)])
;;

let on_start (_ : Config.t) _ctx = return ()

(* Fire the whole burst at once: every cycle's synchronous prefix (id
   allocation, the random draws) runs in list order before any awaits, so the
   RNG is consumed deterministically even though the cycles overlap in
   flight. *)
let on_tick (config : Config.t) ctx =
  Deferred.List.iter
    ~how:`Parallel
    (List.init config.cycles_per_tick ~f:Fn.id)
    ~f:(fun (_ : int) -> submit_then_cancel config ctx)
;;

(* The storm fires blindly and does not react to events. We surface only
   rejects, which during review are the tell that something is
   misconfigured (e.g. duplicate-id detection tripping, or orders crossing
   and filling instead of resting). *)
let on_event (_ : Config.t) _ctx (event : Exchange_event.t) =
  (match event with
   | Order_reject { request; reason } ->
     [%log.error
       "cancel_storm: order rejected" (request : Order.Request.t) (reason : string)]
   | Cancel_reject { client_order_id; reason; participant = _ } ->
     [%log.error
       "cancel_storm: cancel rejected"
         (client_order_id : Client_order_id.t)
         (reason : string)]
   | Order_accept _ | Order_cancel _ | Fill _ | Best_bid_offer_update _
   | Trade_report _ -> ());
  return ()
;;
