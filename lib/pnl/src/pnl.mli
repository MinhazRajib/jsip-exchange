(** Per-participant, per-symbol profit-and-loss tracking.

    A single {!t} tracks every participant's book at once. For each
    [(participant, symbol)] pair it holds:

    - [inventory]: signed share count ([+] long, [-] short),
    - a running {e cost basis} (the total signed cost of the currently open
      position, from which an average entry price is derived), and
    - [realized_cents]: cash locked in by closing shares.

    P&L splits into two halves:

    - {e Realized} P&L is cash from positions that have been closed out. It
      only moves when a fill reduces (or flips) an existing position.
    - {e Unrealized} P&L marks the open position to a reference price:
      [inventory * (reference_price - average_entry_price)]. The reference
      price comes from public trade prints via {!apply_trade_report}.

    This module is pure: every update returns a fresh {!t}. It is the natural
    consumer of the events a {!Jsip_types.Exchange_event} stream carries —
    {!apply_fill} for private [Fill]s, {!apply_trade_report} for public
    [Trade_report]s. See [app/market_maker] for the sign conventions on who
    gains inventory from a fill. *)

open! Core
open Jsip_types

(** A public trade print: the price at which the market last traded a symbol.

    Unlike {!Jsip_types.Fill.t}, a print names no participants — it is what
    the whole market sees, and here it only serves to refresh the reference
    price used for unrealized P&L. Mirrors the [Trade_report] variant of
    {!Jsip_types.Exchange_event.t}. *)
module Trade_report : sig
  type t =
    { symbol : Symbol.t
    ; price : Price.t
    ; size : Size.t
    }
  [@@deriving sexp_of]
end

(** A point-in-time P&L breakdown for a single participant, as produced by
    {!summary}.

    Displays a single row per trade of a symbol the participant makes and
    that symbol's position. Shows the per symbol breakdown plus the total. *)
module Summary : sig
  type row =
    { symbol : Symbol.t
    ; inventory : int
    ; average_entry_price : Price.t option (** [None] when [inventory = 0] *)
    ; realized_cents : int
    ; unrealized_cents : int (** [0] when no reference price has been seen *)
    ; total_cents : int (** [realized_cents + unrealized_cents]. *)
    }
  [@@deriving sexp_of]

  type t =
    { per_symbol : row list
    ; total_realized_cents : int
    ; total_unrealized_cents : int
    ; total_cents : int
    }
  [@@deriving sexp_of]

  (** formats table into a string *)
  val to_string : t -> string
end

type t [@@deriving sexp_of]

(** A tracker with no positions and no reference prices. *)
val empty : t

(** Fold a private fill into the tracker, updating {e both} the aggressor's
    and the resting participant's positions for the fill's symbol. *)
val apply_fill : t -> Fill.t -> t

(** Refresh the reference price used to mark open positions to market. Does
    not touch inventory, cost basis, or realized cash. *)
val apply_trade_report : t -> Trade_report.t -> t

(** The per-symbol breakdown and totals for one participant. A participant
    with no recorded positions yields an empty, all-zero summary. *)
val summary : t -> Participant.t -> Summary.t
