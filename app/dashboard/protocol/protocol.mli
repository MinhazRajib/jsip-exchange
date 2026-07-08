(** The wire protocol between the dashboard's web server and its browser
    client.

    Deliberately flat and primitive (ints/floats/strings), so it can be
    compiled to JavaScript without depending on [jsip_gateway] (which pulls
    in native-only [Tcp.Server]) or the domain types. The web server converts
    the exchange's [Exchange_stats.Snapshot.t] into a {!Sample.t}; the
    browser polls {!recent_stats_rpc} for the recent window and renders it.

    See [app/dashboard/server] for the conversion and [app/dashboard/client]
    for the rendering. *)

open! Core
open! Async_rpc_kernel

(** Submit- or cancel-latency percentiles for one one-second window, in
    microseconds. [count] is how many requests the percentiles were computed
    over; [0] means "no traffic that second". *)
module Latency : sig
  type t =
    { p50_us : float
    ; p90_us : float
    ; p99_us : float
    ; count : int
    }
  [@@deriving sexp, bin_io, equal]
end

(** One symbol's book depth, pre-formatted for display. [bid]/[ask] are the
    best prices as strings (e.g. ["$150.00"]), [None] when that side is
    empty. Sizes and counts are the totals resting on each side. *)
module Book_row : sig
  type t =
    { symbol : string
    ; bid : string option
    ; ask : string option
    ; total_bid_size : int
    ; total_ask_size : int
    ; bid_count : int
    ; ask_count : int
    }
  [@@deriving sexp, bin_io, equal]
end

(** One per-second sample. [at] is epoch seconds, stamped by the web server
    when it received the underlying snapshot — it is the x-axis of the
    rolling charts and the key the client dedupes on. *)
module Sample : sig
  type t =
    { at : float
    ; live_words : int
    ; heap_words : int
    ; top_heap_words : int
    ; minor_collections : int
    ; major_collections : int
    ; promoted_words : int
    ; submit : Latency.t
    ; cancel : Latency.t
    ; books : Book_row.t list
    }
  [@@deriving sexp, bin_io, equal]
end

(** Polled once a second by the browser. Returns the web server's buffered
    recent samples, oldest first. A plain [Rpc.Rpc] (not a [Pipe_rpc]): the
    client drives the cadence, so a backgrounded tab simply stops asking. *)
val recent_stats_rpc : (unit, Sample.t list) Rpc.Rpc.t
