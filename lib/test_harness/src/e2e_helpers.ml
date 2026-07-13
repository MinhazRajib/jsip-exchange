open! Core
open! Async
open Jsip_gateway
open Jsip_types

let with_server ~symbols f =
  let%bind server = Exchange_server.start ~symbols ~port:0 () in
  let port = Exchange_server.port server in
  Monitor.protect
    (fun () -> f ~server ~port)
    ~finally:(fun () -> Exchange_server.close server)
;;

type client =
  { conn : Rpc.Connection.t
  ; directory : Symbol_directory.t
  }

(* Like any real client, fetch the name<->id mapping once at connect. This
   also means the e2e tests exercise the directory RPC rather than
   sidestepping it. *)
let fetch_directory conn =
  let%map pairs =
    Rpc.Rpc.dispatch_exn Rpc_protocol.symbol_directory_rpc conn ()
  in
  Symbol_directory.of_alist_exn pairs
;;

let connect_as ~port _participant =
  let where =
    Tcp.Where_to_connect.of_host_and_port { host = "localhost"; port }
  in
  let%bind conn = Rpc.Connection.client where >>| Result.ok_exn in
  let%bind (_ : Participant.t Or_error.t) =
    Rpc.Rpc.dispatch_exn
      Rpc_protocol.login_rpc
      conn
      (Participant.to_string _participant)
  in
  let%bind directory = fetch_directory conn in
  let%bind session_feed, _metadata =
    Rpc.Pipe_rpc.dispatch_exn Rpc_protocol.session_feed_rpc conn ()
  in
  don't_wait_for
    (Pipe.iter_without_pushback session_feed ~f:(fun event ->
       let e = Event_formatter.format_event ~directory event in
       print_endline [%string "[for %{_participant#Participant}] %{e}"]));
  Async.return { conn; directory }
;;

let connect_as_no_login ~port _participant =
  let where =
    Tcp.Where_to_connect.of_host_and_port { host = "localhost"; port }
  in
  let%bind conn = Rpc.Connection.client where >>| Result.ok_exn in
  let%bind directory = fetch_directory conn in
  let%bind session_feed, _metadata =
    Rpc.Pipe_rpc.dispatch_exn Rpc_protocol.session_feed_rpc conn ()
  in
  don't_wait_for
    (Pipe.iter_without_pushback session_feed ~f:(fun event ->
       let e = Event_formatter.format_event ~directory event in
       print_endline [%string "[for %{_participant#Participant}] %{e}"]));
  Async.return { conn; directory }
;;

let connection client = client.conn
let directory client = client.directory

let rpc_submit client request =
  Rpc.Rpc.dispatch_exn Rpc_protocol.submit_order_rpc client.conn request
  >>| ok_exn
;;

let rpc_cancel client client_order_id =
  Rpc.Rpc.dispatch_exn
    Rpc_protocol.cancel_order_rpc
    client.conn
    client_order_id
  >>| ok_exn
;;

let rpc_book client symbol =
  Rpc.Rpc.dispatch_exn Rpc_protocol.book_query_rpc client.conn symbol
;;
