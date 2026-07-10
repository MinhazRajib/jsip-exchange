open! Core
open Jsip_types

module Price_id_key = struct
  type t =
    { price : Price.t
    ; order_id : Order_id.t
    }
  [@@deriving sexp, compare, sexp_of]

  include functor Comparable.Make_plain
end

type t =
  { symbol : Symbol.t
  ; mutable bids : Order.t Price_id_key.Map.t
  ; mutable asks : Order.t Price_id_key.Map.t
  ; mutable reverse_index : Price_id_key.t Order_id.Map.t
  }
[@@deriving sexp_of]

let create symbol =
  { symbol
  ; bids = Price_id_key.Map.empty
  ; asks = Price_id_key.Map.empty
  ; reverse_index = Order_id.Map.empty
  }
;;

let symbol t = t.symbol

let side_map t side =
  match (side : Side.t) with Buy -> t.bids | Sell -> t.asks
;;

let get_key order : Price_id_key.t =
  match Order.side order with
  | Sell -> { price = Order.price order; order_id = Order.order_id order }
  | Buy ->
    { price =
        Price.( * )
          (Order.price order)
          ~-1 (* set price as negative to get best order time and price *)
    ; order_id = Order.order_id order
    }
;;

let set_reverse_index t (orders : Order.t Price_id_key.Map.t) =
  let new_reverse_index =
    Map.fold
      orders
      ~init:t.reverse_index
      ~f:(fun ~key:price_id_key ~data:order acc ->
        Map.set acc ~key:(Order.order_id order) ~data:price_id_key)
  in
  t.reverse_index <- new_reverse_index
;;

let set_side_map t side orders =
  set_reverse_index t orders;
  match (side : Side.t) with
  | Buy -> t.bids <- orders
  | Sell -> t.asks <- orders
;;

let set_side_map' t side new_map new_reverse_index =
  t.reverse_index <- new_reverse_index;
  match (side : Side.t) with
  | Buy -> t.bids <- new_map
  | Sell -> t.asks <- new_map
;;

let add t order =
  let side = Order.side order in
  let side_map = side_map t side in
  let new_side_map = Map.add_exn side_map ~key:(get_key order) ~data:order in
  set_side_map t side new_side_map
;;

let remove' t order_id =
  let remove_from t side key =
    let orders = side_map t side in
    let new_reverse_index = Map.remove t.reverse_index order_id in
    let new_side_map = Map.remove orders key in
    set_side_map' t side new_side_map new_reverse_index;
    Map.find orders key
  in
  match Map.find t.reverse_index order_id with
  | Some key ->
    (match Map.find t.asks key with
     | Some _ -> remove_from t Side.Sell key
     | None -> remove_from t Side.Buy key)
  | None -> None
;;

let remove t order_id = ignore (remove' t order_id : Order.t option)

let find t order_id =
  let find_in side =
    match Map.find t.reverse_index order_id with
    | Some key -> Map.find (side_map t side) key
    | None -> None
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
  let resting_orders = side_map t opposite_side in
  let marketable_orders =
    Map.filter resting_orders ~f:(fun outgoing_order ->
      Price.is_marketable
        incoming_side
        ~price:(Order.price incoming)
        ~resting_price:(Order.price outgoing_order))
  in
  match Map.min_elt marketable_orders with
  | Some (_, best_bid) -> Some best_bid
  | None -> None
;;

let orders_on_side t side = Map.data (side_map t side)
let is_empty t = Map.is_empty t.bids && Map.is_empty t.asks
let count t side = Map.length (side_map t side)

let best_price t side =
  let orders = side_map t side in
  match Map.min_elt orders with
  | None -> None
  | Some (_, order) -> Some (Order.price order)
;;

let best_level t side : Level.t option =
  match best_price t side with
  | None -> None
  | Some price ->
    let total_size =
      Map.fold
        (side_map t side)
        ~init:Size.zero
        ~f:(fun ~key:_ ~data:order acc ->
          if Price.equal (Order.price order) price
          then Size.( + ) acc (Order.remaining_size order)
          else acc)
    in
    Some { price; size = total_size }
;;

let best_bid_offer t : Bbo.t =
  { bid = best_level t Buy; ask = best_level t Sell }
;;

let snapshot_side t side =
  (* Orders already come out best-price-first (asks ascending; bids are keyed
     on a negated price), so same-price orders are adjacent. One linear pass
     folds each run of them into a single aggregated level. *)
  orders_on_side t side
  |> List.fold ~init:[] ~f:(fun (sorted_orders : Level.t list) order ->
    let price = Order.price order in
    let size = Order.remaining_size order in
    match sorted_orders with
    | { price = prev; size = total } :: rest when Price.( = ) price prev ->
      { Level.price; size = Size.( + ) total size } :: rest
    | _ -> { Level.price; size } :: sorted_orders)
    (* refrain from using pipes if too complex and makes code less readable *)
  |> List.rev
;;

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
