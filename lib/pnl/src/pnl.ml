open! Core
open Jsip_types

(* The per-(participant, symbol) state we accumulate. [inventory] is a signed
   share count; [cost_basis_cents] is the total cents tied up in the open
   position, carrying the same sign as [inventory] so that
   [cost_basis_cents / inventory] is the (positive) average entry price. We
   keep the exact accumulated cost rather than a rounded average so that
   [inventory * reference - cost_basis] is an exact mark. *)
type position =
  { inventory : int
  ; cost_basis_cents : int
  ; realized_cents : int
  }
[@@deriving sexp_of]

let empty_position =
  { inventory = 0; cost_basis_cents = 0; realized_cents = 0 }
;;

type t =
  { positions : position Symbol.Map.t Participant.Map.t
  ; reference_prices : Price.t Symbol.Map.t
  }
[@@deriving sexp_of]

let empty =
  { positions = Participant.Map.empty; reference_prices = Symbol.Map.empty }
;;

(* Apply a single trade of [signed_qty] shares (positive = bought, negative =
   sold) at [price_cents] to one position, using the average-cost method.
   There are three shapes: growing a position in its current direction,
   shrinking it without crossing zero, and flipping through zero to the other
   side. *)
let apply_to_position position ~signed_qty:q ~price_cents:p =
  let { inventory = inv; cost_basis_cents = cb; realized_cents } =
    position
  in
  (* [inv_sign] is only used in branches where [inv <> 0]. *)
  let inv_sign = if inv > 0 then 1 else -1 in
  if inv = 0 || Bool.equal (inv > 0) (q > 0)
  then
    (* Opening or adding: fold the shares into the cost basis, no
       realization. *)
    { inventory = inv + q; cost_basis_cents = cb + (q * p); realized_cents }
  else if abs q <= abs inv
  then (
    (* Reducing or fully closing, without switching sides. Release the cost
       basis of the closed shares proportionally and realize the difference
       against the trade price. *)
    let closed = abs q in
    let cost_removed = cb * closed / abs inv in
    let realized_delta = (inv_sign * closed * p) - cost_removed in
    { inventory = inv + q
    ; cost_basis_cents = cb - cost_removed
    ; realized_cents = realized_cents + realized_delta
    })
  else (
    (* Flipping sides: close the whole existing position (releasing all of
       its cost basis), then open the remainder at [p]. *)
    let realized_delta = (inv * p) - cb in
    let new_inv = inv + q in
    { inventory = new_inv
    ; cost_basis_cents = new_inv * p
    ; realized_cents = realized_cents + realized_delta
    })
;;

let update_position t ~participant ~symbol ~signed_qty ~price_cents =
  let by_symbol =
    Map.find t.positions participant
    |> Option.value ~default:Symbol.Map.empty
  in
  let position =
    Map.find by_symbol symbol |> Option.value ~default:empty_position
  in
  let position = apply_to_position position ~signed_qty ~price_cents in
  let by_symbol = Map.set by_symbol ~key:symbol ~data:position in
  { t with positions = Map.set t.positions ~key:participant ~data:by_symbol }
;;

let apply_fill t (fill : Fill.t) =
  let price_cents = Price.to_int_cents fill.price in
  let size = Size.to_int fill.size in
  let book t participant side =
    update_position
      t
      ~participant
      ~symbol:fill.symbol
      ~signed_qty:(Side.sign side * size)
      ~price_cents
  in
  let t = book t fill.aggressor_participant fill.aggressor_side in
  book t fill.resting_participant (Side.flip fill.aggressor_side)
;;

let apply_trade_report t ~symbol ~price =
  { t with
    reference_prices = Map.set t.reference_prices ~key:symbol ~data:price
  }
;;

module Position_summary = struct
  type t =
    { inventory : int
    ; average_entry_price : Price.t option
    ; reference_price : Price.t option
    ; realized_cents : int
    ; unrealized_cents : int
    ; total_cents : int
    }
  [@@deriving sexp_of, compare, equal]
end

module Summary = struct
  type t =
    { per_symbol : (Symbol.t * Position_summary.t) list
    ; realized_cents : int
    ; unrealized_cents : int
    ; total_cents : int
    }
  [@@deriving sexp_of]

  let dollars_of_cents cents =
    let sign = if cents < 0 then "-" else "" in
    sprintf "%s$%d.%02d" sign (abs cents / 100) (abs cents % 100)
  ;;

  let symbol_line (symbol, (s : Position_summary.t)) =
    let price_or_dash = function
      | None -> "-"
      | Some price -> Price.to_string_dollar price
    in
    let avg = price_or_dash s.average_entry_price in
    let reference = price_or_dash s.reference_price in
    let realized = dollars_of_cents s.realized_cents in
    let unrealized = dollars_of_cents s.unrealized_cents in
    let total = dollars_of_cents s.total_cents in
    [%string
      "  %{symbol#Symbol}: inv=%{s.inventory#Int} avg=%{avg} \
       ref=%{reference} realized=%{realized} unrealized=%{unrealized} \
       total=%{total}"]
  ;;

  let to_string_hum t =
    let total_line =
      let realized = dollars_of_cents t.realized_cents in
      let unrealized = dollars_of_cents t.unrealized_cents in
      let total = dollars_of_cents t.total_cents in
      [%string
        "  TOTAL: realized=%{realized} unrealized=%{unrealized} \
         total=%{total}"]
    in
    List.map t.per_symbol ~f:symbol_line @ [ total_line ]
    |> String.concat ~sep:"\n"
  ;;
end

let position_summary t ~symbol (position : position) : Position_summary.t =
  let { inventory; cost_basis_cents; realized_cents } = position in
  let reference_price = Map.find t.reference_prices symbol in
  let average_entry_price =
    if inventory = 0
    then None
    else Some (Price.of_int_cents (cost_basis_cents / inventory))
  in
  let unrealized_cents =
    match reference_price with
    | None -> 0
    | Some reference ->
      (inventory * Price.to_int_cents reference) - cost_basis_cents
  in
  { inventory
  ; average_entry_price
  ; reference_price
  ; realized_cents
  ; unrealized_cents
  ; total_cents = realized_cents + unrealized_cents
  }
;;

let summary t participant : Summary.t =
  let by_symbol =
    Map.find t.positions participant
    |> Option.value ~default:Symbol.Map.empty
  in
  let per_symbol =
    Map.to_alist by_symbol
    |> List.map ~f:(fun (symbol, position) ->
      symbol, position_summary t ~symbol position)
  in
  let sum ~f = List.sum (module Int) per_symbol ~f:(fun (_, s) -> f s) in
  let realized_cents = sum ~f:(fun s -> s.realized_cents) in
  let unrealized_cents = sum ~f:(fun s -> s.unrealized_cents) in
  { per_symbol
  ; realized_cents
  ; unrealized_cents
  ; total_cents = realized_cents + unrealized_cents
  }
;;
