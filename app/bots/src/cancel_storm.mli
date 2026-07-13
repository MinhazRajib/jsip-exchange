(** A pathological bot that hammers the cancel path.

    On every tick the cancel storm runs a burst of {e submit-then-cancel}
    cycles. Each cycle:

    - allocates a {e fresh} {!Jsip_types.Client_order_id.t} (so the
      exchange's duplicate-client-order-id bookkeeping never short-circuits
      the submit — a stale id would make every submit after the first bounce
      off duplicate detection, and no cancel traffic would be generated);
    - submits a passive Day order priced away from the current fundamental so
      it rests on the book rather than filling;
    - immediately cancels that same order by its client-order id.

    The bot never intends to trade. Its purpose is to exercise the
    submit/accept/cancel event flow and the per-participant duplicate-id
    bookkeeping as fast as the runtime will drive it. Point several copies at
    one exchange (different participant names and RNG seeds) to amplify the
    pressure.

    This bot satisfies {!Jsip_bot_runtime.Bot_runtime.Bot}. The companion
    scenario is [Jsip_scenarios.Cancel_storm]. *)

(* This ".mli" file is the public "menu" for the bot: it lists the names,
   types, and docs that other code is allowed to use. The matching ".ml" file
   holds the actual code. Anything not listed here stays private. *)

open! Core
open! Async
open Jsip_types

module Config : sig
  type t =
    { symbols : Symbol_id.t list
    (** Symbols the storm operates on. Must be non-empty; each cycle picks
        one uniformly at random from this list using the bot's
        {!Jsip_bot_runtime.Bot_runtime.Context.random} source, so runs stay
        reproducible. *)
    ; cycles_per_tick : int
    (** Number of submit-then-cancel cycles fired on each tick. This is the
        intensity knob: raise it (or shorten the runtime's tick interval) to
        turn a trickle into a storm. *)
    ; order_size : int
    (** Shares per order, in whole units. Size does not much matter for the
        cancel path, but keep it positive so the order is well-formed. *)
    ; price_offset_cents : int
    (** How far from the current fundamental to place each order, in cents. A
        buy rests at [fundamental - price_offset_cents] and a sell at
        [fundamental + price_offset_cents]; keep this large enough that the
        orders stay non-marketable and rest instead of filling. *)
    ; client_order_ids : Client_order_id.Generator.t
    (** Per-instance source of fresh client-order ids. Construct one with
        [Client_order_id.Generator.create ()] when building the config; each
        cycle draws the next id from it, which is what keeps duplicate
        detection from blocking the storm. *)
    }
  [@@deriving sexp_of]
end

(* Every bot must provide these. The runtime calls them for us: [on_start]
   once at the beginning, [on_tick] on a timer, and [on_event] each time the
   exchange sends a message. *)
val name : string

val on_start
  :  Config.t
  -> Jsip_bot_runtime.Bot_runtime.Context.t
  -> unit Deferred.t

val on_tick
  :  Config.t
  -> Jsip_bot_runtime.Bot_runtime.Context.t
  -> unit Deferred.t

val on_event
  :  Config.t
  -> Jsip_bot_runtime.Bot_runtime.Context.t
  -> Exchange_event.t
  -> unit Deferred.t
