open! Core
open Jsip_types

type t =
  { name_to_id : Participant_id.t Participant.Table.t
  ; id_to_name : Participant.t Participant_id.Table.t
  ; mutable next_id : int
  }

let create () =
  { name_to_id = Participant.Table.create ()
  ; id_to_name = Participant_id.Table.create ()
  ; next_id = 0
  }
;;

let intern t name =
  match Hashtbl.find t.name_to_id name with
  | Some id -> id
  | None ->
    let id = Participant_id.of_int t.next_id in
    t.next_id <- t.next_id + 1;
    Hashtbl.add_exn t.name_to_id ~key:name ~data:id;
    Hashtbl.add_exn t.id_to_name ~key:id ~data:name;
    id
;;

let find_id t name = Hashtbl.find t.name_to_id name
let to_name t id = Hashtbl.find_exn t.id_to_name id
