open! Core
open! Async
open Jsip_types

type t =
  { (* One bag of subscribers per symbol, indexed by the symbol's id. The
       exchange's symbol set is fixed at startup, so this is a plain array —
       the same shape as the matching engine's array of books, for the same
       reason. *)
    market_data_subscribers : Exchange_event.t Pipe.Writer.t Bag.t array
  ; audit_subscribers : Exchange_event.t Pipe.Writer.t Bag.t
  ; registry : Participant_registry.t
  ; participants : Session.t Participant_id.Table.t
  }

let create ~num_symbols =
  { market_data_subscribers =
      Array.init num_symbols ~f:(fun (_ : int) -> Bag.create ())
  ; audit_subscribers = Bag.create ()
  ; registry = Participant_registry.create ()
  ; participants = Participant_id.Table.create ()
  }
;;

let is_known_symbol t symbol_id =
  let i = Symbol_id.to_int symbol_id in
  i >= 0 && i < Array.length t.market_data_subscribers
;;

let subscribe_market_data t symbol_ids =
  (* The ids came off the wire, so check them before using one as an index.
     Rejecting the whole subscription (rather than quietly dropping the bad
     ids) means a client that mistypes an id hears about it, and cannot make
     the server allocate state for symbols that do not exist. *)
  match List.find symbol_ids ~f:(Fn.non (is_known_symbol t)) with
  | Some unknown ->
    Or_error.error_s
      [%message
        "subscribe_market_data: unknown symbol id"
          (unknown : Symbol_id.t)
          ~num_symbols:(Array.length t.market_data_subscribers : int)]
  | None ->
    let reader, writer = Pipe.create () in
    (* Register the same writer in every requested symbol's bag. A per-symbol
       publish iterates a single bag, so a subscriber listed in multiple bags
       receives each event exactly once — only via whichever bag matches the
       event's symbol. *)
    let elts =
      List.map symbol_ids ~f:(fun symbol_id ->
        let subscribers =
          t.market_data_subscribers.(Symbol_id.to_int symbol_id)
        in
        subscribers, Bag.add subscribers writer)
    in
    don't_wait_for
      (let%map () = Pipe.closed writer in
       List.iter elts ~f:(fun (subscribers, elt) ->
         Bag.remove subscribers elt));
    Ok reader
;;

let subscribe_audit t =
  let reader, writer = Pipe.create () in
  let elt = Bag.add t.audit_subscribers writer in
  don't_wait_for
    (let%map () = Pipe.closed writer in
     Bag.remove t.audit_subscribers elt);
  reader
;;

(* Events come from the matching engine, which only ever names symbols it
   trades, so the guard here is belt-and-braces rather than a real check. *)
let push_market_data t event symbol_id =
  if is_known_symbol t symbol_id
  then
    Bag.iter
      t.market_data_subscribers.(Symbol_id.to_int symbol_id)
      ~f:(fun writer -> Pipe.write_without_pushback_if_open writer event)
;;

let push_audit t event =
  Bag.iter t.audit_subscribers ~f:(fun writer ->
    Pipe.write_without_pushback_if_open writer event)
;;

let clean_up_session t session =
  let name = Session.participant session in
  match Participant_registry.find_id t.registry name with
  | None -> Async.return ()
  | Some id ->
    (match Hashtbl.find t.participants id with
     | Some _ ->
       Hashtbl.remove t.participants id;
       Async.return (Session.close session)
     | None -> Async.return ())
;;

let set_up_session t participant =
  let id = Participant_registry.intern t.registry participant in
  let%bind () =
    match Hashtbl.find t.participants id with
    | None -> Async.return ()
    | Some session -> clean_up_session t session
  in
  Async.return
    (Hashtbl.add_exn
       t.participants
       ~key:id
       ~data:(Session.create participant))
;;

let push_to_session t participant event =
  match Participant_registry.find_id t.registry participant with
  | None -> ()
  | Some id ->
    (match Hashtbl.find t.participants id with
     | Some session -> Session.push session event
     | None -> ())
;;

let dispatch_event t (event : Exchange_event.t) =
  push_audit t event;
  match event with
  | Best_bid_offer_update { symbol; bbo = _ } ->
    push_market_data t event symbol
  | Trade_report { symbol; price = _; size = _ } ->
    push_market_data t event symbol
  | Order_accept { order_id = _; request }
  | Order_reject { request; reason = _ } ->
    push_to_session t request.participant event
  | Cancel_reject { participant; client_order_id = _; reason = _ } ->
    push_to_session t participant event
  | Order_cancel
      { client_order_id = _
      ; order_id = _
      ; participant
      ; symbol = _
      ; remaining_size = _
      ; reason = _
      } ->
    push_to_session t participant event
  | Fill
      { fill_id = _
      ; symbol = _
      ; price = _
      ; size = _
      ; aggressor_order_id = _
      ; aggressor_client_order_id = _
      ; aggressor_participant
      ; aggressor_side = _
      ; resting_order_id = _
      ; resting_client_order_id = _
      ; resting_participant
      } ->
    push_to_session t aggressor_participant event;
    push_to_session t resting_participant event
;;

let dispatch t events = List.iter events ~f:(dispatch_event t)

module For_testing = struct
  let audit_subscriber_count t = Bag.length t.audit_subscribers
end

let valid_participant t participant =
  match Participant_registry.find_id t.registry participant with
  | None -> false
  | Some id -> Hashtbl.mem t.participants id
;;

let get_session t participant =
  match Participant_registry.find_id t.registry participant with
  | None -> None
  | Some id -> Hashtbl.find t.participants id
;;
