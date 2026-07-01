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
open Jsip_bot_runtime

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
    ; client_id_manager : Client_order_id.Generator.t
    (** handles client ids *)
    ; mutable inventory_counter : Size.t Symbol.Table.t
    ; mutable resting_client_order_ids :
        Order.Request.t Client_order_id.Table.t
    }
  [@@deriving sexp_of]
end

module Market_maker_bot : Jsip_bot_runtime.Bot_runtime.Bot
