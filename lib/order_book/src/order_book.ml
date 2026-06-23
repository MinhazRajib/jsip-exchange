open! Core
open Jsip_types
open Async_log_kernel.Ppx_log_syntax

type t =
  { symbol : Symbol.t
  ; mutable bids : Order.t list
  ; mutable asks : Order.t list
  }
[@@deriving sexp_of]

let create symbol = { symbol; bids = []; asks = [] }
let symbol t = t.symbol

let side_list t side =
  match (side : Side.t) with Buy -> t.bids | Sell -> t.asks
;;

let set_side_list t side orders =
  match (side : Side.t) with
  | Buy -> t.bids <- orders
  | Sell -> t.asks <- orders
;;

let add t order =
  let side = Order.side order in
  set_side_list t side (order :: side_list t side)
;;

let remove' t order_id =
  let remove_from t side order_id =
    let orders = side_list t side in
    match
      List.partition_tf orders ~f:(fun o ->
        Order_id.equal (Order.order_id o) order_id)
    with
    | [], _ -> None
    | [ found ], rest ->
      set_side_list t side rest;
      Some found
    | matches, _ ->
      [%log.info
        "BUG: More than one order matching order_id found when removing"
          (order_id : Order_id.t)
          (matches : Order.t list)
          (t.symbol : Symbol.t)
          (side : Side.t)];
      None
  in
  match remove_from t Buy order_id with
  | Some _ as result -> result
  | None -> remove_from t Sell order_id
;;

let remove t order_id = ignore (remove' t order_id : Order.t option)

let find t order_id =
  let find_in side =
    List.find (side_list t side) ~f:(fun o ->
      Order_id.equal (Order.order_id o) order_id)
  in
  match find_in Buy with Some _ as result -> result | None -> find_in Sell
;;

(* NOTE: This walks the list front-to-back and returns the *first* tradable
   order, not the best-priced one. Orders are in reverse insertion order
   (newest first), so this matches against whatever was most recently added,
   regardless of price. See test_matching_engine.ml for a test that
   demonstrates why this is wrong. *)
let find_match t incoming =
  let incoming_side = Order.side incoming in
  let opposite_side = Side.flip incoming_side in
  let resting_orders = side_list t opposite_side in
  let marketable =
    List.filter resting_orders ~f:(fun resting ->
      Price.is_marketable
        incoming_side
        ~price:(Order.price incoming)
        ~resting_price:(Order.price resting))
  in
  match marketable with
  | [] -> None
  | first :: rest ->
    Some
      (List.fold rest ~init:first ~f:(fun previous_best next_order ->
         if Price.is_more_aggressive
              opposite_side
              ~price:(Order.price next_order)
              ~than:(Order.price previous_best)
         then next_order
         else previous_best))
;;

(* List.find resting_orders ~f:(fun resting -> Price.is_marketable
   incoming_side ~price:(Order.price incoming) ~resting_price:(Order.price
   resting)) ;; *)

let orders_on_side t side = side_list t side
let is_empty t = List.is_empty t.bids && List.is_empty t.asks
let count t side = List.length (side_list t side)

let best_price t side =
  let priceList = List.map (side_list t side) ~f:Order.price in
  List.reduce priceList ~f:(fun best price ->
    if Price.is_more_aggressive side ~price ~than:best then price else best)
;;

let best_level t side : Level.t option =
  match best_price t side with
  | None -> None
  | Some price ->
    let total_size =
      List.fold (side_list t side) ~init:Size.zero ~f:(fun acc order ->
        if Price.equal (Order.price order) price
        then Size.( + ) acc (Order.remaining_size order)
        else acc)
    in
    Some { price; size = total_size }
;;

let best_bid_offer t : Bbo.t =
  { bid = best_level t Buy; ask = best_level t Sell }
;;

(* Update snapshot_side so the snapshot lists levels in the same order that
   matching would visit them: bids highest-price-first, asks
   lowest-price-first, with ties broken by arrival time. The snapshot is what
   clients see when they query the book, so it should reflect the real
   matching order rather than insertion order. Update the expect output in
   the affected tests, including the price For snapshot_side: the current
   code maps orders to Level.t ([{ price; size }]) and sorts them with
   Level.compare, which only knows about price and size — it has no notion of
   arrival time. Sort the underlying Order.t list first, with a comparator
   built from Price.is_more_aggressive and Order_id.compare (lower order ID =
   arrived first), then map to Level.t. That keeps the snapshot c onsistent
   with the matching order. *)

let snapshot_side t (side : Side.t) =
  let orders = side_list t side in
  let sorted_orders =
    List.sort orders ~compare:(fun a b ->
      if Price.is_more_aggressive
           side
           ~price:(Order.price a)
           ~than:(Order.price b)
      then -1
      else if Price.equal (Order.price a) (Order.price b)
      then Order_id.compare (Order.order_id a) (Order.order_id b)
      else 1)
  in
  List.map sorted_orders ~f:Level.of_order
;;

(* let compare = match side with | Buy -> Comparable.reverse Level.compare |
   Sell -> Level.compare in orders_on_side t side |> List.map
   ~f:Level.of_order |> List.sort ~compare *)

let snapshot t =
  { Book.symbol = symbol t
  ; bids = snapshot_side t Buy
  ; asks = snapshot_side t Sell
  ; bbo = best_bid_offer t
  }
;;

module For_testing = struct
  let remove = remove'
end
