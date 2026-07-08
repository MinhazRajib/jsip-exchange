open! Core
open! Async
open Jsip_types
open Jsip_gateway
module Fundamental_oracle = Jsip_fundamental.Fundamental_oracle
module News_injector = Jsip_news_injector.News_injector
module Bot_runtime = Jsip_bot_runtime.Bot_runtime

(* Bring up one bot end-to-end: open its own RPC connection, subscribe to the
   market-data stream for the symbols listed in the spec, and run the bot.
   Once the session feed exists (week 2 exercise 1) this is also where each
   bot will log in and subscribe to its session-feed RPC, so its [on_event]
   handler can react to the matching engine's responses to its own orders and
   to fills against its resting orders. *)
let start_bot ~where_to_connect ~oracle (Bot_spec.T spec) =
  let%bind connection =
    Rpc.Connection.client where_to_connect
    >>| Result.map_error ~f:Error.of_exn
    >>| ok_exn
  in
  let%bind _ =
    Rpc.Rpc.dispatch_exn
      Rpc_protocol.login_rpc
      connection
      (Participant.to_string spec.participant)
  in
  let%bind session_feed, (_metadata : Rpc.Pipe_rpc.Metadata.t) =
    Rpc.Pipe_rpc.dispatch_exn Rpc_protocol.session_feed_rpc connection ()
  in
  let submit request =
    Rpc.Rpc.dispatch_exn Rpc_protocol.submit_order_rpc connection request
  in
  let cancel client_order_id =
    Rpc.Rpc.dispatch_exn
      Rpc_protocol.cancel_order_rpc
      connection
      client_order_id
  in
  let bot =
    Bot_runtime.create
      spec.bot
      spec.config
      ~participant:spec.participant
      ~oracle
      ~rng:(Splittable_random.of_int spec.rng_seed)
      ~submit
      ~cancel
      ~tick_interval:spec.tick_interval
  in
  (* Always drain the session feed. Every bot subscribes to it above, and the
     server pushes each bot's accept/cancel/fill events into that pipe with
     [write_without_pushback_if_open] — so if we don't read it, the pipe
     buffer grows without bound (the cancel-storm memory leak measured in
     doc/dashboard-calibration.md). Market-data consumers additionally
     interleave the market-data stream into the same drain. *)
  let%bind event_feed =
    match spec.is_marketdata_consumer with
    | false -> return session_feed
    | true ->
      let%map md_pipe, metadata =
        Rpc.Pipe_rpc.dispatch_exn
          Rpc_protocol.market_data_rpc
          connection
          spec.symbols
      in
      don't_wait_for
        (match%map Rpc.Pipe_rpc.close_reason metadata with
         | Rpc.Pipe_close_reason.Closed_locally
         | Rpc.Pipe_close_reason.Closed_remotely ->
           ()
         | Rpc.Pipe_close_reason.Error err ->
           [%log.error "marketdata pipe closed with error" (err : Error.t)]);
      Pipe.interleave [ session_feed; md_pipe ]
  in
  don't_wait_for (Pipe.iter event_feed ~f:(Bot_runtime.feed_event bot));
  print_endline
    [%string "[scenario] starting bot %{spec.participant#Participant}"];
  don't_wait_for (Bot_runtime.start bot);
  return ()
;;

let run (config : Scenario_config.t) ~port ~seed =
  print_endline
    [%string
      "[scenario] starting %{config.name} on port %{port#Int} \
       (seed=%{seed#Int})"];
  let%bind server = Exchange_server.start ~symbols:config.symbols ~port () in
  let where_to_connect =
    Tcp.Where_to_connect.of_host_and_port
      { Host_and_port.host = "localhost"; port }
  in
  let oracle = Fundamental_oracle.create config.oracle_config ~seed in
  let injector = News_injector.create oracle config.news in
  (* Background tasks. *)
  don't_wait_for (Fundamental_oracle.start oracle);
  don't_wait_for (News_injector.start injector);
  let%bind () =
    Deferred.List.iter
      ~how:`Parallel
      config.bots
      ~f:(start_bot ~where_to_connect ~oracle)
  in
  Exchange_server.close_finished server
;;
