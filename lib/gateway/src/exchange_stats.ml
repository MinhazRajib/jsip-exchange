open! Core
open! Async
open Jsip_types

module Latency_summary = struct
  type t =
    { p50 : Time_ns.Span.t
    ; p90 : Time_ns.Span.t
    ; p99 : Time_ns.Span.t
    ; count : int
    }
  [@@deriving sexp, bin_io]

  let empty =
    { p50 = Time_ns.Span.zero
    ; p90 = Time_ns.Span.zero
    ; p99 = Time_ns.Span.zero
    ; count = 0
    }
  ;;

  (* Nearest-rank percentile: pick the sample at position [fraction] of the
     way through the sorted array. Simple and good enough for a dashboard; no
     interpolation between neighbours. *)
  let percentile sorted ~fraction =
    let n = Array.length sorted in
    let index =
      Float.iround_nearest_exn (fraction *. Float.of_int (n - 1))
    in
    sorted.(Int.clamp_exn index ~min:0 ~max:(n - 1))
  ;;

  let of_samples samples =
    match samples with
    | [] -> empty
    | _ :: _ ->
      let sorted = Array.of_list samples in
      Array.sort sorted ~compare:Time_ns.Span.compare;
      { p50 = percentile sorted ~fraction:0.50
      ; p90 = percentile sorted ~fraction:0.90
      ; p99 = percentile sorted ~fraction:0.99
      ; count = Array.length sorted
      }
  ;;
end

module Book_depth = struct
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

module Memory = struct
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

module Snapshot = struct
  type t =
    { memory : Memory.t
    ; submit_latency : Latency_summary.t
    ; cancel_latency : Latency_summary.t
    ; books : Book_depth.t list
    }
  [@@deriving sexp, bin_io]
end

type t =
  { submit_samples : Time_ns.Span.t Queue.t
  ; cancel_samples : Time_ns.Span.t Queue.t
  ; subscribers : Snapshot.t Pipe.Writer.t Bag.t
  }

let create () =
  { submit_samples = Queue.create ()
  ; cancel_samples = Queue.create ()
  ; subscribers = Bag.create ()
  }
;;

let record_submit t span = Queue.enqueue t.submit_samples span
let record_cancel t span = Queue.enqueue t.cancel_samples span

let subscribe t =
  let reader, writer = Pipe.create () in
  let elt = Bag.add t.subscribers writer in
  don't_wait_for
    (let%map () = Pipe.closed writer in
     Bag.remove t.subscribers elt);
  reader
;;

let take_latency_summaries t =
  let submit = Latency_summary.of_samples (Queue.to_list t.submit_samples) in
  let cancel = Latency_summary.of_samples (Queue.to_list t.cancel_samples) in
  Queue.clear t.submit_samples;
  Queue.clear t.cancel_samples;
  submit, cancel
;;

let publish t snapshot =
  Bag.iter t.subscribers ~f:(fun writer ->
    Pipe.write_without_pushback_if_open writer snapshot)
;;
