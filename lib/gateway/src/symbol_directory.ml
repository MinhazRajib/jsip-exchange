open! Core
open Jsip_types

type t =
  { ids : Symbol_id.t Symbol.Map.t
  ; names : Symbol.t Symbol_id.Map.t
  }

let of_alist_exn pairs =
  { ids = Symbol.Map.of_alist_exn pairs
  ; names =
      List.map pairs ~f:(fun (name, id) -> id, name)
      |> Symbol_id.Map.of_alist_exn
  }
;;

let of_symbols symbols =
  List.mapi symbols ~f:(fun i symbol -> symbol, Symbol_id.of_int i)
  |> of_alist_exn
;;

let to_alist t = Map.to_alist t.names |> List.map ~f:(fun (id, n) -> n, id)
let find_id t name = Map.find t.ids name

let name t id =
  match Map.find t.names id with
  | Some name -> Symbol.to_string name
  | None -> Symbol_id.to_string id
;;
