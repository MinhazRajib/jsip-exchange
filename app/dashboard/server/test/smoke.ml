(* Headless smoke check for the dashboard relay: connect to a running
   dashboard server over a websocket, poll [recent_stats_rpc] once, and print
   what came back. Not an inline test (it needs a live server on a port); the
   verify script starts the servers and runs this against them. *)

open! Core
open! Async
module Protocol = Jsip_dashboard_protocol.Protocol

let main ~port () =
  let uri = Uri.of_string (sprintf "ws://localhost:%d" port) in
  match%bind Rpc_websocket.Rpc.client uri with
  | Error error ->
    print_s [%message "connect failed" (error : Error.t)];
    return ()
  | Ok connection ->
    (match%bind Rpc.Rpc.dispatch Protocol.recent_stats_rpc connection () with
     | Error error ->
       print_s [%message "dispatch failed" (error : Error.t)];
       return ()
     | Ok samples ->
       printf "got %d samples\n" (List.length samples);
       (match List.last samples with
        | None -> printf "no samples buffered yet\n"
        | Some sample -> print_s [%sexp (sample : Protocol.Sample.t)]);
       return ())
;;

let () =
  Command.async
    ~summary:"dispatch recent_stats_rpc against a running dashboard server"
    (let%map_open.Command port =
       flag
         "-port"
         (optional_with_default 8080 int)
         ~doc:"PORT dashboard port"
     in
     fun () -> main ~port ())
  |> Command_unix.run
;;
