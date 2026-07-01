(** Per-participant profit-and-loss (P&L) accounting.

    Tracks, for every [(participant, symbol)] pair, three quantities:

    - {b inventory}: the signed position in shares. Positive is long (you own
      shares), negative is short (you owe shares).
    - {b average entry price}: the average price at which the currently-open
      position was accumulated. This is the cost basis used to decide how
      much of a subsequent trade is profit vs. return of capital.
    - {b realized cash}: cumulative cash P&L (in cents) locked in whenever a
      position is reduced or closed.

    A [Pnl.t] is fed two kinds of input:

    - {!apply_fill} consumes a private {!Jsip_types.Fill.t}. Because a fill
      names both counterparties, it updates {e both} the aggressor's and the
      resting participant's books, on opposite sides.
    - {!apply_trade_report} consumes the public trade print carried by an
      {!Jsip_types.Exchange_event.t}. It refreshes the market reference price
      used to mark open positions to market; it never changes any inventory.

    Unrealized P&L for an open position is
    [inventory * (reference_price - average_entry_price)] — the paper gain you
    would realize by closing at the current mark. See {!summary}.

    Values of type [t] are immutable; every [apply_*] returns a new [t], so a
    stream of events folds naturally:
    {[
      List.fold events ~init:Pnl.empty ~f:(fun pnl event ->
        match event with
        | Fill fill -> Pnl.apply_fill pnl fill
        | other -> Pnl.apply_trade_report pnl other)
    ]} *)

open! Core
open Jsip_types

type t

(** An empty ledger: no positions, no realized cash, no reference prices. *)
val empty : t

(** Record a fill. Updates the aggressor at [fill.aggressor_side] and the
    resting participant at the flipped side, each for [fill.size] shares at
    [fill.price]. *)
val apply_fill : t -> Fill.t -> t

(** Refresh the reference price used to mark positions in a symbol. Only
    [Trade_report] events carry a print; every other event is ignored (returns
    the ledger unchanged). Does not touch inventory or realized cash. *)
val apply_trade_report : t -> Exchange_event.t -> t

(** A snapshot of one participant's P&L, broken down per symbol plus totals.
    All cash quantities are in integer cents. *)
module Summary : sig
  (** One symbol's line in a participant's P&L. *)
  type per_symbol =
    { symbol : Symbol.t
    ; inventory : int (** Signed position: positive long, negative short. *)
    ; average_entry_price : Price.t option
    (** Cost basis of the open position, or [None] when flat. *)
    ; reference_price : Price.t option
    (** Latest trade print for the symbol, or [None] if none seen. *)
    ; realized_cents : int (** Cash locked in from closed quantity. *)
    ; unrealized_cents : int
    (** Mark-to-market on the open position; [0] when flat or unmarked. *)
    }
  [@@deriving sexp_of]

  type t =
    { per_symbol : per_symbol list
    ; total_realized_cents : int
    ; total_unrealized_cents : int
    }
  [@@deriving sexp_of]
end

(** Summarize [participant]'s P&L. The [per_symbol] breakdown covers every
    symbol the participant has traded (in [Symbol] order); a participant that
    is flat but has traded still appears, with [inventory = 0]. *)
val summary : t -> Participant.t -> Summary.t
