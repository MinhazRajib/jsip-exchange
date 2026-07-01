(** Per-participant profit-and-loss (P&L) tracking.

    A [Pnl.t] is an accumulator over the exchange's fill stream. For each
    participant and symbol it remembers three things:

    - the current signed {b inventory} (positive = long, negative = short),
    - the {b cost basis} of the open position (total cents paid for the
      shares still held), from which the average entry price is derived, and
    - the {b realized} cash from positions that have been closed out.

    P&L is marked to market against a per-symbol {b reference price}, which
    is refreshed from public trade prints (see {!apply_trade_report}). For a
    single symbol:

    - {b realized} P&L is the cash locked in by closing trades. Closing a
      long books [shares * (sell_price - average_entry_price)]; closing a
      short books the mirror image.
    - {b unrealized} P&L is the paper mark on the still-open position:
      [inventory * (reference_price - average_entry_price)].
    - {b total} P&L is [realized + unrealized].

    A single {!Jsip_types.Fill.t} is a trade between two participants, so
    {!apply_fill} updates {b both} of them: the aggressor trades on
    [aggressor_side] and the resting participant on the opposite side.

    Typical use: fold the exchange's {!Jsip_types.Exchange_event.t} stream,
    calling {!apply_fill} on [Fill] events and {!apply_trade_report} on
    [Trade_report] events, then ask for a {!summary} per participant. *)

open! Core
open Jsip_types

type t [@@deriving sexp_of]

(** A tracker that has seen no fills and no trade prints. *)
val empty : t

(** Book a fill against both of its participants. The aggressor's inventory
    moves by [Side.sign fill.aggressor_side * fill.size]; the resting
    participant's moves the opposite way. Realized cash and cost basis are
    updated using the average-cost method. *)
val apply_fill : t -> Fill.t -> t

(** Refresh the reference price used to mark [symbol] to market. Pass the
    price from a public trade print (an [Exchange_event.Trade_report]); it
    affects unrealized P&L only, never inventory or realized cash. *)
val apply_trade_report : t -> symbol:Symbol.t -> price:Price.t -> t

(** The P&L of one participant in one symbol. All cash figures are signed
    integer cents (negative = a loss / cash outflow). *)
module Position_summary : sig
  type t =
    { inventory : int
    (** Signed share count: positive is long, negative is short, zero is
        flat. This is not a {!Jsip_types.Size.t} precisely because it can be
        negative. *)
    ; average_entry_price : Price.t option
    (** Average price paid for the open position, or [None] when flat. *)
    ; reference_price : Price.t option
    (** Last trade print for the symbol, or [None] if none seen yet. *)
    ; realized_cents : int
    ; unrealized_cents : int
    ; total_cents : int (** [realized_cents + unrealized_cents]. *)
    }
  [@@deriving sexp_of, compare, equal]
end

(** A participant's P&L broken down per symbol, plus the totals across all
    symbols they have traded. *)
module Summary : sig
  type t =
    { per_symbol : (Symbol.t * Position_summary.t) list
    (** One entry per symbol the participant has traded, sorted by symbol.
        Symbols they have fully closed out still appear (with zero inventory)
        so their realized P&L is visible. *)
    ; realized_cents : int
    ; unrealized_cents : int
    ; total_cents : int
    }
  [@@deriving sexp_of]

  (** Renders the summary as human-readable lines, one per symbol plus a
      [TOTAL] line, with cash shown in dollars. Handy in expect tests. *)
  val to_string_hum : t -> string
end

(** The P&L breakdown for [participant]. A participant with no fills yet gets
    an empty [per_symbol] list and zero totals. *)
val summary : t -> Participant.t -> Summary.t
