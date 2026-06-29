open! Core
open! Async
open Jsip_types
open Jsip_order_book

type t =
  { engine : Matching_engine.t
  ; dispatcher : Dispatcher.t
  ; request_writer : Order.Request.t Pipe.Writer.t
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

let handle_submit ~request_writer (request : Order.Request.t) =
  let%map () = Pipe.write_if_open request_writer request in
  Ok ()
;;

let start_matching_loop ~engine ~dispatcher request_reader =
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
  don't_wait_for
    (Pipe.iter_without_pushback request_reader ~f:(fun request ->
       let events = filter_bad_client_order_ids engine request in
       Dispatcher.dispatch dispatcher events))
;;

let start ~symbols ~port () =
  let engine = Matching_engine.create symbols in
  let dispatcher = Dispatcher.create () in
  let request_reader, request_writer = Pipe.create () in
  Pipe.set_size_budget request_writer request_queue_size_budget;
  start_matching_loop ~engine ~dispatcher request_reader;
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
                 handle_submit ~request_writer { request with participant })
        ; Rpc.Rpc.implement' Rpc_protocol.book_query_rpc (fun state symbol ->
            ignore state;
            Matching_engine.book engine symbol
            |> Option.map ~f:Order_book.snapshot)
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
