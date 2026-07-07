open! Core
open! Async
open Jsip_dashboard_server

(* When the student runs `dune exec` from the repo root after `dune build`,
   the compiled bundle lives here. Overridable with -js-bundle. *)
let default_js_bundle = "_build/default/app/dashboard/client/main.bc.js"

let command =
  Command.async
    ~summary:
      "Dashboard web server: relays a JSIP exchange's stats feed to a \
       bonsai_web client, served over HTTP + websocket."
    (let%map_open.Command exchange_host =
       flag
         "-exchange-host"
         (optional_with_default "localhost" string)
         ~doc:"HOST exchange server hostname (default localhost)"
     and exchange_port =
       flag
         "-exchange-port"
         (optional_with_default 12345 int)
         ~doc:"PORT exchange server port (default 12345)"
     and http_port =
       flag
         "-http-port"
         (optional_with_default 8080 int)
         ~doc:"PORT port to serve the dashboard on (default 8080)"
     and js_bundle_path =
       flag
         "-js-bundle"
         (optional_with_default default_js_bundle string)
         ~doc:"FILE path to the compiled client bundle (default in _build)"
     in
     fun () ->
       let%bind () =
         Dashboard_server.start
           ~exchange_host
           ~exchange_port
           ~http_port
           ~js_bundle_path
           ()
       in
       Deferred.never ())
    ~behave_nicely_in_pipeline:false
;;

let () = Command_unix.run command
