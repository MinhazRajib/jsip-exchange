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
  ; mutable bids : Order.t Map.M(Price_id_key).t
  ; mutable asks : Order.t Map.M(Price_id_key).t
  ; mutable reverse_index : Price_id_key.t Map.M(Order_id).t
  }
[@@deriving sexp_of]

let create symbol =
  { symbol
  ; bids = Map.empty (module Price_id_key)
  ; asks = Map.empty (module Price_id_key)
  ; reverse_index = Map.empty (module Order_id)
  }
;;

let symbol t = t.symbol

let side_map t side =
  match (side : Side.t) with Buy -> t.bids | Sell -> t.asks
;;

let get_key order : Price_id_key.t =
  { price = Order.price order; order_id = Order.order_id order }
;;

let set_side_map t side orders =
  t.reverse_index
  <- Map.of_alist_exn
       (module Order_id)
       (List.map orders ~f:(fun order -> Order.order_id order, get_key order));
  match (side : Side.t) with
  | Buy ->
    t.bids
    <- Map.of_alist_exn
         (module Price_id_key)
         (List.map orders ~f:(fun order -> get_key order, order))
  | Sell ->
    t.asks
    <- Map.of_alist_exn
         (module Price_id_key)
         (List.map orders ~f:(fun order -> get_key order, order))
;;

let set_side_map' t side new_map =
  match (side : Side.t) with
  | Buy -> t.bids <- new_map
  | Sell -> t.asks <- new_map
;;

let add t order =
  let side = Order.side order in
  let new_side_list = order :: Map.data (side_map t side) in
  set_side_map t side new_side_list
;;

let remove' t order_id =
  let remove_from t side order_id =
    let orders = side_map t side in
    let key = Map.find t.reverse_index order_id in
    match key with
    | None -> None
    | Some key ->
      let new_side_map = Map.remove orders key in
      set_side_map' t side new_side_map;
      t.reverse_index <- Map.remove t.reverse_index order_id;
      Map.find orders key
  in
  match remove_from t Buy order_id with
  | Some _ as result -> result
  | None -> remove_from t Sell order_id
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
  match opposite_side with
  | Side.Buy ->
    (match Map.max_elt marketable_orders with
     | Some (_, best_bid) -> Some best_bid
     | None -> None)
  | Side.Sell ->
    (match Map.min_elt marketable_orders with
     | Some (_, best_bid) -> Some best_bid
     | None -> None)
;;

let orders_on_side t side = Map.data (side_map t side)
let is_empty t = Map.is_empty t.bids && Map.is_empty t.asks
let count t side = Map.length (side_map t side)

let best_price t side =
  let orders = side_map t side in
  match side with
  | Side.Buy ->
    (match Map.max_elt orders with
     | None -> None
     | Some (_, order) -> Some (Order.price order))
  | Side.Sell ->
    (match Map.min_elt orders with
     | None -> None
     | Some (_, order) -> Some (Order.price order))
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

let snapshot_side t (side : Side.t) =
  let compare (level1 : Level.t) (level2 : Level.t) =
    let price = level1.price in
    let than = level2.price in
    if Price.is_more_aggressive side ~price ~than
    then -1 (* higher precedence *)
    else if Price.( = ) price than
    then 0
    else 1
  in
  orders_on_side t side |> List.map ~f:Level.of_order |> List.sort ~compare
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
