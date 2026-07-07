open! Core
open! Async_rpc_kernel

module Latency = struct
  type t =
    { p50_us : float
    ; p90_us : float
    ; p99_us : float
    ; count : int
    }
  [@@deriving sexp, bin_io, equal]
end

module Book_row = struct
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

module Sample = struct
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

let recent_stats_rpc =
  Rpc.Rpc.create
    ~name:"recent-stats"
    ~version:1
    ~bin_query:Unit.bin_t
    ~bin_response:[%bin_type_class: Sample.t list]
    ~include_in_error_count:Only_on_exn
;;
