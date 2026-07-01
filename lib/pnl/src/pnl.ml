open! Core
open Jsip_types

(* The open position for a single [(participant, symbol)] pair.

   [inventory] is signed (positive long, negative short). [average_entry_cents]
   is the average entry price per share of the open position, in cents; it is
   meaningless when [inventory = 0] and callers must not read it in that case.
   [realized_cents] accumulates cash P&L from every reduction/close.

   The market reference price is deliberately {e not} stored here — it is a
   property of the symbol, shared across all participants, so it lives in
   {!t.reference_prices}. Keeping it out of the position avoids having to
   rewrite every participant's book on each trade print. *)
module Position = struct
  type t =
    { inventory : int
    ; average_entry_cents : int
    ; realized_cents : int
    }

  let empty = { inventory = 0; average_entry_cents = 0; realized_cents = 0 }
end

type t =
  { positions : Position.t Symbol.Map.t Participant.Map.t
  ; reference_prices : Price.t Symbol.Map.t
  }

let empty =
  { positions = Participant.Map.empty; reference_prices = Symbol.Map.empty }
;;

(* Apply one execution to one participant's position for [symbol]: [qty] signed
   shares (buy positive, sell negative) at [px] cents. This is the accounting
   core; see {!update_position}. *)
let update_position (pos : Position.t) ~qty ~px : Position.t =
  let { Position.inventory; average_entry_cents; realized_cents } = pos in
  let new_inventory = inventory + qty in
  (* Same side (or opening from flat) iff the signed quantities don't oppose. *)
  let is_growing = inventory = 0 || inventory * qty > 0 in
  if is_growing
  then (
    (* Blend [px] into the average over the combined share count. *)
    let average_entry_cents =
      ((abs inventory * average_entry_cents) + (abs qty * px))
      / abs new_inventory
    in
    { Position.inventory = new_inventory; average_entry_cents; realized_cents })
  else (
    (* Opposite sign: the overlap with the existing position is closed and
       realizes cash; any excess flips through zero and opens fresh at [px]. *)
    let closed = Int.min (abs inventory) (abs qty) in
    let long_short_direction = if inventory > 0 then 1 else -1 in
    let realized_cents =
      realized_cents + (closed * (px - average_entry_cents) * long_short_direction)
    in
    let average_entry_cents =
      if inventory * new_inventory > 0
      then average_entry_cents (* position shrank but kept its side *)
      else px (* flat, or flipped through zero and reopened at [px] *)
    in
    { Position.inventory = new_inventory; average_entry_cents; realized_cents })
;;

(* Route one leg of a trade to [participant]'s book. *)
let apply_one t ~participant ~symbol ~side ~price ~size =
  let qty = Side.sign side * Size.to_int size in
  let px = Price.to_int_cents price in
  let by_symbol =
    Map.find t.positions participant
    |> Option.value ~default:Symbol.Map.empty
  in
  let pos =
    Map.find by_symbol symbol |> Option.value ~default:Position.empty
  in
  let pos = update_position pos ~qty ~px in
  let by_symbol = Map.set by_symbol ~key:symbol ~data:pos in
  { t with
    positions = Map.set t.positions ~key:participant ~data:by_symbol
  }
;;

let apply_fill t (fill : Fill.t) =
  let { Fill.symbol
      ; price
      ; size
      ; aggressor_participant
      ; aggressor_side
      ; resting_participant
      ; fill_id = _
      ; aggressor_order_id = _
      ; resting_order_id = _
      }
    =
    fill
  in
  let t =
    apply_one
      t
      ~participant:aggressor_participant
      ~symbol
      ~side:aggressor_side
      ~price
      ~size
  in
  apply_one
    t
    ~participant:resting_participant
    ~symbol
    ~side:(Side.flip aggressor_side)
    ~price
    ~size
;;

let apply_trade_report t (event : Exchange_event.t) =
  match event with
  | Trade_report { symbol; price; size = _ } ->
    { t with
      reference_prices = Map.set t.reference_prices ~key:symbol ~data:price
    }
  | Order_accept _
  | Fill _
  | Order_cancel _
  | Order_reject _
  | Best_bid_offer_update _
  | Cancel_reject _ -> t
;;

module Summary = struct
  type per_symbol =
    { symbol : Symbol.t
    ; inventory : int
    ; average_entry_price : Price.t option
    ; reference_price : Price.t option
    ; realized_cents : int
    ; unrealized_cents : int
    }
  [@@deriving sexp_of]

  type t =
    { per_symbol : per_symbol list
    ; total_realized_cents : int
    ; total_unrealized_cents : int
    }
  [@@deriving sexp_of]
end

let summary t participant : Summary.t =
  let by_symbol =
    Map.find t.positions participant
    |> Option.value ~default:Symbol.Map.empty
  in
  let per_symbol =
    Map.to_alist by_symbol
    |> List.map ~f:(fun (symbol, (pos : Position.t)) ->
      let reference_price = Map.find t.reference_prices symbol in
      let average_entry_price =
        if pos.inventory = 0
        then None
        else Some (Price.of_int_cents pos.average_entry_cents)
      in
      let unrealized_cents =
        match reference_price with
        | None -> 0
        | Some reference_price ->
          pos.inventory
          * (Price.to_int_cents reference_price - pos.average_entry_cents)
      in
      { Summary.symbol
      ; inventory = pos.inventory
      ; average_entry_price
      ; reference_price
      ; realized_cents = pos.realized_cents
      ; unrealized_cents
      })
  in
  let total_realized_cents =
    List.sum (module Int) per_symbol ~f:(fun s -> s.Summary.realized_cents)
  in
  let total_unrealized_cents =
    List.sum (module Int) per_symbol ~f:(fun s -> s.Summary.unrealized_cents)
  in
  { per_symbol; total_realized_cents; total_unrealized_cents }
;;
