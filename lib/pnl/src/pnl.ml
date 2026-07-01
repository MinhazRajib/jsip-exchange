open! Core
open Jsip_types

module Trade_report = struct
  type t =
    { symbol : Symbol.t
    ; price : Price.t
    ; size : Size.t
    }
  [@@deriving sexp_of]
end

(** A single participant's position in a single symbol.

    [inventory] is signed shares ([+] long, [-] short). [cost_basis_cents] is
    the total signed cost of the {e currently open} position, so the average
    entry price is [cost_basis_cents / inventory] and unrealized P&L against
    a reference price [r] is [inventory * r - cost_basis_cents]. *)
module Position = struct
  type t =
    { inventory : int
    ; cost_basis_cents : int
    ; realized_cents : int
    }
  [@@deriving sexp_of]

  let zero = { inventory = 0; cost_basis_cents = 0; realized_cents = 0 }

  (* Apply one fill to a position, from the perspective of the participant on
     [side] at [price] for [size] shares. [side] is that participant's own
     side of the trade (a buyer's inventory goes up, a seller's down).

     The trick that keeps this branch-free is to split the fill into two
     signed pieces: [closing_delta], the shares that reduce the existing
     position toward (or through) zero, and [opening_delta], the shares that
     grow the position or start a fresh one on the other side. Every case —
     opening, adding, trimming, closing flat, flipping — is just some mix of
     the two, so a single formula covers them all:

     - realized P&L is booked only on the closing shares, and
     - the cost basis moves with inventory: closing shares leave at the old
       average entry, opening shares arrive at the fill price. *)
  let apply_fill t ~(side : Side.t) ~price ~size =
    let price_cents = Price.to_int_cents price in
    (* Signed shares this fill adds to inventory: [+] for a buy, [-] a sell. *)
    let delta = Side.sign side * Size.to_int size in
    let inventory = t.inventory in
    let average_entry_cents =
      if inventory = 0 then 0 else t.cost_basis_cents / inventory
    in
    (* A same-direction fill (or one onto a flat book) closes nothing. An
       opposite-direction fill closes up to the whole position; any overshoot
       spills into [opening_delta] and flips the position. *)
    let is_reducing =
      inventory <> 0
      && not (Sign.equal (Int.sign delta) (Int.sign inventory))
    in
    let closing_delta =
      if not is_reducing
      then 0
      else if Int.abs delta <= Int.abs inventory
      then delta
      else -inventory
    in
    let opening_delta = delta - closing_delta in
    (* Closing shares entered at [average_entry_cents] and exit at
       [price_cents]. [closing_delta] already carries the sign — negative
       when selling off a long, positive when buying back a short — so the
       cash comes out right with no extra sign factor. *)
    let realized = closing_delta * (average_entry_cents - price_cents) in
    (* Adds have [closing_delta = 0] and stay exact; only the closing shares
       pass through the [average_entry_cents] rounding. *)
    let new_cost_basis_cents =
      t.cost_basis_cents
      + (average_entry_cents * closing_delta)
      + (price_cents * opening_delta)
    in
    { inventory = inventory + delta
    ; cost_basis_cents = new_cost_basis_cents
    ; realized_cents = t.realized_cents + realized
    }
  ;;
end

type t =
  { positions : Position.t Symbol.Map.t Participant.Map.t
  ; reference_prices : Price.t Symbol.Map.t
  }
[@@deriving sexp_of]

let empty =
  { positions = Participant.Map.empty; reference_prices = Symbol.Map.empty }
;;

let find_position t ~participant ~symbol =
  match Map.find t.positions participant with
  | None -> Position.zero
  | Some by_symbol ->
    Map.find by_symbol symbol |> Option.value ~default:Position.zero
;;

let set_position t ~participant ~symbol position =
  let by_symbol =
    Map.find t.positions participant
    |> Option.value ~default:Symbol.Map.empty
  in
  let by_symbol = Map.set by_symbol ~key:symbol ~data:position in
  { t with positions = Map.set t.positions ~key:participant ~data:by_symbol }
;;

let update_position t ~participant ~symbol ~side ~price ~size =
  let position = find_position t ~participant ~symbol in
  let position = Position.apply_fill position ~side ~price ~size in
  set_position t ~participant ~symbol position
;;

let apply_fill t (fill : Fill.t) =
  let update_position_side t (fill : Fill.t) participant side =
    update_position
      t
      ~participant
      ~symbol:fill.symbol
      ~side
      ~price:fill.price
      ~size:fill.size
  in
  update_position_side t fill fill.aggressor_participant fill.aggressor_side
  |> fun t ->
  update_position_side
    t
    fill
    fill.resting_participant
    (Side.flip fill.aggressor_side)
;;

let apply_trade_report t (trade_report : Trade_report.t) =
  { t with
    reference_prices =
      Map.set
        t.reference_prices
        ~key:trade_report.symbol
        ~data:trade_report.price
  }
;;

module Summary = struct
  type row =
    { symbol : Symbol.t
    ; inventory : int
    ; average_entry_price : Price.t option
    ; realized_cents : int
    ; unrealized_cents : int
    ; total_cents : int
    }
  [@@deriving sexp_of]

  type t =
    { per_symbol : row list
    ; total_realized_cents : int
    ; total_unrealized_cents : int
    ; total_cents : int
    }
  [@@deriving sexp_of]

  let to_string t =
    let row_line (row : row) =
      let avg =
        match row.average_entry_price with
        | None -> "--"
        | Some price -> Price.to_string_dollar price
      in
      let dollars cents =
        Price.to_string_dollar (Price.of_int_cents cents)
      in
      [%string
        "  %{row.symbol#Symbol}: inv=%{row.inventory#Int} avg=%{avg} \
         realized=%{dollars row.realized_cents} unrealized=%{dollars \
         row.unrealized_cents} total=%{dollars row.total_cents}"]
    in
    let dollars cents = Price.to_string_dollar (Price.of_int_cents cents) in
    let body =
      List.map t.per_symbol ~f:row_line |> String.concat ~sep:"\n"
    in
    [%string
      "%{body}\n\
      \  TOTAL: realized=%{dollars t.total_realized_cents} \
       unrealized=%{dollars t.total_unrealized_cents} total=%{dollars \
       t.total_cents}"]
  ;;
end

let summary t participant : Summary.t =
  let by_symbol =
    Map.find t.positions participant
    |> Option.value ~default:Symbol.Map.empty
  in
  let per_symbol =
    Map.to_alist by_symbol
    |> List.map ~f:(fun (symbol, position) ->
      let inventory = position.Position.inventory in
      let cost_basis = position.cost_basis_cents in
      let realized_cents = position.realized_cents in
      let average_entry_price =
        if inventory = 0
        then None
        else Some (Price.of_int_cents (cost_basis / inventory))
      in
      let unrealized_cents =
        match Map.find t.reference_prices symbol with
        | None -> 0
        | Some reference ->
          (inventory * Price.to_int_cents reference) - cost_basis
      in
      { Summary.symbol
      ; inventory
      ; average_entry_price
      ; realized_cents
      ; unrealized_cents
      ; total_cents = realized_cents + unrealized_cents
      })
  in
  let total_realized_cents =
    List.sum (module Int) per_symbol ~f:(fun row -> row.realized_cents)
  in
  let total_unrealized_cents =
    List.sum (module Int) per_symbol ~f:(fun row -> row.unrealized_cents)
  in
  { per_symbol
  ; total_realized_cents
  ; total_unrealized_cents
  ; total_cents = total_realized_cents + total_unrealized_cents
  }
;;
