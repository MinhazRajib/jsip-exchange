(** Exchange client.

    Connects to a running exchange server and provides an interactive
    command-line interface for submitting orders and querying the book.

    Run with: dune exec app/client/bin/main.exe -- -host localhost -port
    12345 -name Alice *)

open! Core
open! Async
open Jsip_types
open Jsip_gateway

let run_client ~host ~port ~participant_name =
  let where_to_connect =
    Tcp.Where_to_connect.of_host_and_port { host; port }
  in
  let%bind conn = Rpc.Connection.client where_to_connect >>| Result.ok_exn in
  (** login to server and dispatch session feed*)
  let%bind login = Rpc.Rpc.dispatch_exn Rpc_protocol.login_rpc conn participant_name in
  let participant = Or_error.ok_exn login in
  (* Symbols travel on the wire as ints. Fetch the exchange's name<->id
     mapping once, up front, so we can turn a name the user types into an id
     to send, and an id we receive back into a name to show them. The
     exchange's symbol set is fixed at startup, so this never goes stale. *)
  let%bind pairs =
    Rpc.Rpc.dispatch_exn Rpc_protocol.symbol_directory_rpc conn ()
  in
  let directory = Symbol_directory.of_alist_exn pairs in
  let symbols =
    List.map pairs ~f:(fun (name, _id) -> Symbol.to_string name)
    |> String.concat ~sep:", "
  in
  print_endline [%string
      {|
  Connected to exchange at %{host}:%{port#Int} as %{participant#Participant}
  Commands: BUY|SELL <client-id> <symbol> <size> <price> [IOC|DAY]
            BOOK <symbol>
            SUBSCRIBE <symbol>  (stream market data)
            CANCEL <client-id>

  Trading: %{symbols}|}];
  let%bind session_feed, _metadata = (Rpc.Pipe_rpc.dispatch_exn Rpc_protocol.session_feed_rpc conn ()) 
  in
  (** dispatch session feed *)
  don't_wait_for
  (Pipe.iter_without_pushback session_feed ~f:(fun event ->
     match event with 
    | Fill fill ->
      (match
         Event_formatter.format_fill_for_participant
           ~directory
           fill
           participant
       with
       | Some msg -> print_endline msg
       | None -> ())
    | _ ->
      print_endline (Event_formatter.format_event ~directory event)));
  let rec loop () = 
    print_string "> ";
    match%bind Reader.read_line (Lazy.force Reader.stdin) with
    | `Eof ->
      print_endline "\nDisconnected.";
      Deferred.Or_error.ok_unit
    | `Ok line ->
      let line = String.strip line in
      if String.is_empty line
      then loop ()
      else (
        match
          Exchange_command.parse
            line
            ~default_participant:participant
            ~directory
        with
        | Error msg ->
          print_endline [%string "ERROR: %{Error.to_string_hum msg}"];
          loop ()
        | Ok (Exchange_command.Submit request) ->
          let%bind.Deferred.Or_error () =
            Rpc.Rpc.dispatch_exn Rpc_protocol.submit_order_rpc conn request
          in
          loop ()
        | Ok (Exchange_command.Cancel client_order_id) ->
            let%bind.Deferred.Or_error () = Rpc.Rpc.dispatch_exn Rpc_protocol.cancel_order_rpc conn client_order_id in
          loop ()
        | Ok (Exchange_command.Book symbol) ->
          let%bind result =
            Rpc.Rpc.dispatch_exn Rpc_protocol.book_query_rpc conn symbol
          in
          let name = Symbol_directory.name directory symbol in
          (match result with
           | None -> print_endline [%string "No book available for %{name}"]
           | Some result ->
             print_endline (Event_formatter.format_book ~directory result));
          loop ()
        | Ok (Exchange_command.Subscribe symbol) ->
          let%bind result =
            Rpc.Pipe_rpc.dispatch
              Rpc_protocol.market_data_rpc
              conn
              [ symbol ]
          in
          (match result with
           | Error err | Ok (Error err) ->
             print_endline
               [%string "ERROR subscribing: %{Error.to_string_hum err}"];
             loop ()
           | Ok (Ok (reader, _id)) ->
             let name = Symbol_directory.name directory symbol in
             print_endline
               [%string
                 {|
Subscribed to %{name} market data. Updates will appear below.
Continue entering commands as normal.|}];
             (* Read market data in the background; the command loop
                continues running concurrently. *)
             don't_wait_for
               (Pipe.iter_without_pushback reader ~f:(fun event ->
                  let e = Event_formatter.format_event ~directory event in
                  print_endline [%string "[MD] %{e}"]));
             loop ()))
  in
  loop ()
;;

let () =
  Command.async_or_error
    ~summary:"JSIP Exchange client"
    (let%map_open.Command host =
       flag
         "-host"
         (optional_with_default "localhost" string)
         ~doc:"HOST server hostname"
     and port = flag "-port" (required int) ~doc:"PORT server port"
     and participant_name =
       flag
         "-name"
         (optional_with_default (Core_unix.getlogin ()) string)
         ~doc:"NAME participant name"
     in
     fun () -> run_client ~host ~port ~participant_name)
  |> Command_unix.run
;;
