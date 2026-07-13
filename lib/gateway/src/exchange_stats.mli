(** Infrastructure metrics for the exchange process.

    This is deliberately kept separate from {!Exchange_event} and the audit
    log. The audit log records what the exchange {e did} (accepts, fills,
    cancels); this module records how the exchange process is {e doing}
    (memory use, request latency, book depth). Mixing the two would be a
    layering mistake.

    The server keeps one collector {!t}. RPC handlers feed it latency samples
    as requests are processed; a once-per-second driver reads a {!Snapshot.t}
    out of it and pushes that to every dashboard subscribed via
    [stats_feed_rpc]. See {!Rpc_protocol.stats_feed_rpc}. *)

open! Core
open! Async
open Jsip_types

(** Percentile summary of a batch of latencies. [count] is how many samples
    the percentiles were computed over; when [count = 0] the percentiles are
    all [Time_ns.Span.zero]. *)
module Latency_summary : sig
  type t =
    { p50 : Time_ns.Span.t
    ; p90 : Time_ns.Span.t
    ; p99 : Time_ns.Span.t
    ; count : int
    }
  [@@deriving sexp, bin_io]
end

(** Depth of one symbol's book: the best bid/offer plus the total resting
    size and order count on each side. Enough for the dashboard to show live
    BBO and how much interest is stacked up behind it. *)
module Book_depth : sig
  type t =
    { symbol : Symbol_id.t
    ; bbo : Bbo.t
    ; total_bid_size : Size.t
    ; total_ask_size : Size.t
    ; bid_count : int
    ; ask_count : int
    }
  [@@deriving sexp, bin_io]
end

(** A slice of [Gc.stat ()], the runtime's memory numbers. [live_words] is
    the OCaml-managed memory the exchange is using right now; the rest give
    context on heap size and collector activity. *)
module Memory : sig
  type t =
    { live_words : int
    ; heap_words : int
    ; top_heap_words : int
    ; minor_collections : int
    ; major_collections : int
    ; promoted_words : int
    }
  [@@deriving sexp, bin_io]
end

(** One per-second snapshot: everything a dashboard pane needs, in one
    record. This is the value streamed over [stats_feed_rpc]. *)
module Snapshot : sig
  type t =
    { memory : Memory.t
    ; submit_latency : Latency_summary.t
    ; cancel_latency : Latency_summary.t
    ; books : Book_depth.t list
    }
  [@@deriving sexp, bin_io]
end

(** The server-side collector. Not sent over the wire. *)
type t

(** A fresh collector with no samples and no subscribers. *)
val create : unit -> t

(** Record how long a submit- or cancel-order request took (ingress to
    "matching engine has handled it"). Called once per request. *)
val record_submit : t -> Time_ns.Span.t -> unit

val record_cancel : t -> Time_ns.Span.t -> unit

(** Register a new dashboard subscriber and return the reader end of its
    pipe; {!publish} writes to it until the reader closes. Mirrors
    {!Dispatcher.subscribe_audit}. *)
val subscribe : t -> Snapshot.t Pipe.Reader.t

(** Percentile summaries of the submit and cancel samples collected since the
    last call, then reset the sample buffers. Returns [(submit, cancel)]. The
    once-per-second driver calls this to build the next {!Snapshot.t}. *)
val take_latency_summaries : t -> Latency_summary.t * Latency_summary.t

(** Push a snapshot to every live subscriber. *)
val publish : t -> Snapshot.t -> unit
