open! Core
open! Async
open Jsip_types
open Jsip_order_book

module Requests = struct
  (* What the client asked for. *)
  type kind =
    | Order of Order.Request.t
    | Cancel of
        { participant : Participant.t
        ; client_order_id : Client_order_id.t
        }
  [@@deriving sexp, bin_io]

  (* A queued request, tagged with when it entered the server. [submitted_at]
     is stamped at ingress ({!handle_write}) so the matching loop can measure
     how long the request waited plus took to process. *)
  type t =
    { kind : kind
    ; submitted_at : Time_ns.t
    }
  [@@deriving sexp, bin_io]
end

type t =
  { engine : Matching_engine.t
  ; dispatcher : Dispatcher.t
  ; request_writer : Requests.t Pipe.Writer.t
  ; tcp_server : (Socket.Address.Inet.t, int) Tcp.Server.t
  ; port : int
  }

module Connection_state = struct
  type t = { mutable session : Session.t option }

  let session t = t.session
end

(* Bound how many client requests can sit in the queue waiting for the
   matching engine. Once the queue is full, [Pipe.write] returns a pending
   deferred and the [submit_order_rpc] handler blocks until the engine has
   processed enough requests to free up space — clients get backpressure
   without the server's memory growing unboundedly. *)
let request_queue_size_budget = 1024

let handle_write ~request_writer (kind : Requests.kind) =
  let request = { Requests.kind; submitted_at = Time_ns.now () } in
  let%map () = Pipe.write_if_open request_writer request in
  Ok ()
;;

let start_matching_loop ~engine ~dispatcher ~stats request_reader =
  (*=
  let participant = Session.participant session in
                 let cancel_attempt =
                   Matching_engine.cancel engine participant client_order_id
                 in
                 List.iter cancel_attempt ~f:(fun event ->
                   Session.push session event);
  *)
  let filter_bad_client_order_ids engine (request : Order.Request.t) =
    match
      Matching_engine.check_client_order_id
        engine
        request.participant
        request.client_order_id
    with
    | Some _ ->
      [ Exchange_event.Order_reject
          { request; reason = "client order id already exits" }
      ]
    | None -> Matching_engine.submit engine request
  in
  let handle_cancel_requests
    engine
    (participant : Participant.t)
    (client_order_id : Client_order_id.t)
    =
    Matching_engine.cancel engine participant client_order_id
  in
  don't_wait_for
    (Pipe.iter_without_pushback
       request_reader
       ~f:(fun (request : Requests.t) ->
         let events =
           match request.kind with
           | Requests.Order order -> filter_bad_client_order_ids engine order
           | Cancel cancel ->
             handle_cancel_requests
               engine
               cancel.participant
               cancel.client_order_id
         in
         (* The request is now handled; record how long it took (ingress to
            here) in the submit or cancel bucket. *)
         let latency = Time_ns.diff (Time_ns.now ()) request.submitted_at in
         (match request.kind with
          | Requests.Order _ -> Exchange_stats.record_submit stats latency
          | Cancel _ -> Exchange_stats.record_cancel stats latency);
         Dispatcher.dispatch dispatcher events))
;;

(* Sample one symbol's book: best bid/offer plus total resting size and order
   count on each side. [None] if the engine does not trade [symbol]. *)
let book_depth_of_symbol engine symbol : Exchange_stats.Book_depth.t option =
  match Matching_engine.book engine symbol with
  | None -> None
  | Some book ->
    let side_totals side =
      let orders = Order_book.orders_on_side book side in
      let total =
        List.fold orders ~init:Size.zero ~f:(fun acc order ->
          Size.( + ) acc (Order.remaining_size order))
      in
      total, List.length orders
    in
    let total_bid_size, bid_count = side_totals Side.Buy in
    let total_ask_size, ask_count = side_totals Side.Sell in
    Some
      { symbol
      ; bbo = Order_book.best_bid_offer book
      ; total_bid_size
      ; total_ask_size
      ; bid_count
      ; ask_count
      }
;;

(* Once per second, build a snapshot from the latency buckets, [Gc.stat ()],
   and current book depth, then push it to every dashboard subscriber. *)
let start_stats_loop ~engine ~stats ~symbols =
  Clock_ns.every (Time_ns.Span.of_sec 1.) (fun () ->
    let submit_latency, cancel_latency =
      Exchange_stats.take_latency_summaries stats
    in
    let gc = Gc.stat () in
    let memory =
      { Exchange_stats.Memory.live_words = gc.live_words
      ; heap_words = gc.heap_words
      ; top_heap_words = gc.top_heap_words
      ; minor_collections = gc.minor_collections
      ; major_collections = gc.major_collections
      ; promoted_words = Float.iround_nearest_exn gc.promoted_words
      }
    in
    let books = List.filter_map symbols ~f:(book_depth_of_symbol engine) in
    Exchange_stats.publish
      stats
      { memory; submit_latency; cancel_latency; books })
;;

let start ~symbols ~port () =
  let engine = Matching_engine.create symbols in
  let dispatcher = Dispatcher.create () in
  let stats = Exchange_stats.create () in
  let request_reader, request_writer = Pipe.create () in
  Pipe.set_size_budget request_writer request_queue_size_budget;
  start_matching_loop ~engine ~dispatcher ~stats request_reader;
  start_stats_loop ~engine ~stats ~symbols;
  let implementations =
    Rpc.Implementations.create_exn
      ~implementations:
        [ Rpc.Rpc.implement
            Rpc_protocol.login_rpc
            (fun (state : Connection_state.t) participant_name ->
               if String.is_empty participant_name
                  || String.for_all participant_name ~f:Char.is_whitespace
               then
                 Async.return
                   (Or_error.error_string
                      "login_rpc: Invalid submitted name, no whitespace / \
                       empty names allowed")
               else (
                 let participant = Participant.of_string participant_name in
                 match state.session with
                 | Some _ ->
                   Async.return
                     (Or_error.error_string
                        "login_rpc: user already logged in")
                 | None ->
                   if Dispatcher.valid_participant dispatcher participant
                   then
                     Async.return
                       (Or_error.error_string
                          "login_rpc: user already exists in dispatch")
                   else (
                     let%bind () =
                       Dispatcher.set_up_session dispatcher participant
                     in
                     state.session
                     <- Dispatcher.get_session dispatcher participant;
                     Async.return (Ok participant))))
        ; Rpc.Rpc.implement
            Rpc_protocol.submit_order_rpc
            (fun state request ->
               match Connection_state.session state with
               | None ->
                 Async.return
                   (Or_error.error_string "submit_order_rpc: not logged in")
               | Some session ->
                 let participant = Session.participant session in
                 handle_write
                   ~request_writer
                   (Order { request with participant }))
        ; Rpc.Rpc.implement' Rpc_protocol.book_query_rpc (fun state symbol ->
            ignore state;
            Matching_engine.book engine symbol
            |> Option.map ~f:Order_book.snapshot)
        ; Rpc.Rpc.implement
            Rpc_protocol.cancel_order_rpc
            (fun state client_order_id ->
               match Connection_state.session state with
               | None ->
                 Async.return
                   (Or_error.error_string "submit_order_rpc: not logged in")
               | Some session ->
                 let participant = Session.participant session in
                 handle_write
                   ~request_writer
                   (Cancel { participant; client_order_id }))
        ; Rpc.Pipe_rpc.implement
            Rpc_protocol.market_data_rpc
            (fun state symbols ->
               ignore state;
               let reader =
                 Dispatcher.subscribe_market_data dispatcher symbols
               in
               return (Ok reader))
        ; Rpc.Pipe_rpc.implement Rpc_protocol.audit_log_rpc (fun state () ->
            ignore state;
            let reader = Dispatcher.subscribe_audit dispatcher in
            return (Ok reader))
        ; Rpc.Pipe_rpc.implement
            Rpc_protocol.session_feed_rpc
            (fun (state : Connection_state.t) () ->
               match state.session with
               | None -> return (Or_error.error_string "not logged in")
               | Some session ->
                 let reader = Session.reader session in
                 return (Ok reader))
        ; Rpc.Pipe_rpc.implement Rpc_protocol.stats_feed_rpc (fun state () ->
            ignore state;
            let reader = Exchange_stats.subscribe stats in
            return (Ok reader))
        ]
      ~on_unknown_rpc:`Close_connection
      ~on_exception:Log_on_background_exn
  in
  let initial_connection_state _ conn =
    let state = ({ session = None } : Connection_state.t) in
    let () =
      don't_wait_for
        (let%bind () = Rpc.Connection.close_finished conn in
         match state.session with
         | None -> Async.return ()
         | Some session -> Dispatcher.clean_up_session dispatcher session)
    in
    state
  in
  let%map tcp_server =
    Rpc.Connection.serve
      ~implementations
      ~initial_connection_state
      ~where_to_listen:(Tcp.Where_to_listen.of_port port)
      ()
  in
  let actual_port = Tcp.Server.listening_on tcp_server in
  { engine; dispatcher; request_writer; tcp_server; port = actual_port }
;;

let port t = t.port

let close t =
  Pipe.close t.request_writer;
  Tcp.Server.close t.tcp_server
;;

let close_finished t = Tcp.Server.close_finished t.tcp_server
