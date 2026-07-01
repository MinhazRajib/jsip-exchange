(** A simple market-making bot.

    A market maker provides liquidity by continuously quoting both a bid
    (buy) and an ask (sell) price. They profit from the spread between the
    two prices, but take risk if the market moves against their inventory.

    This bot places a fixed set of resting orders on both sides of the book
    around a configured "fair value" price. It does not dynamically adjust
    its quotes in response to fills -- that is left as an extension. *)

open! Core
open! Async
open Jsip_types

(** Configuration for the market maker. *)
module Config : sig
  type t =
    { participant : Participant.t
    ; symbol : Symbol.t
    ; fair_value_cents : int
    (** The market maker's estimate of the true price, in cents. *)
    ; half_spread_cents : int
    (** Half-spread in cents. The bot will bid at [fair_value - half_spread]
        and offer at [fair_value + half_spread]. *)
    ; size_per_level : int (** Number of shares at each price level. *)
    ; num_levels : int
    (** Number of price levels on each side. The bot places orders at
        [fair_value +/- spread], [fair_value +/- (spread + tick)], etc. *)
    }
  [@@deriving sexp_of]
end

(** Submit the market maker's initial set of resting orders over the given
    open [Rpc.Connection.t]. The connection must already be logged in as
    [config.participant]. [submit_order_rpc] is one-way, so this function
    only returns success/failure of the submission attempt; the actual
    matching-engine response (acceptance, fills, rejection) arrives on the
    participant's session feed. *)
val seed_book : Config.t -> Rpc.Connection.t -> unit Deferred.t

(** Run the market maker as a long-lived bot over the given open, logged-in
    [Rpc.Connection.t].

    Unlike {!seed_book}, which places the initial ladder and returns, [run]
    keeps going. It subscribes to the participant's session feed and, as
    [Order_accept], [Fill], and [Order_cancel] events arrive, maintains its
    own view of

    - its net {e inventory} (position) in each symbol — up on buys, down on
      sells — and
    - the orders it currently has resting on the book, so it can cancel and
      re-quote them.

    The returned [Deferred.t] is never determined: the bot runs until the
    connection closes or the process exits. *)
val run : Config.t -> Rpc.Connection.t -> unit Deferred.t
