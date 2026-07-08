open! Core

module List_seq = struct
  (* TODO: replace the definition of type t and the implementations of
     create, set, and get *)
  type t = int list ref

  let create () = ref []

  let set t ~key ~data =
    let len_list = List.length !t in
    if key < 0 || key > len_list
    then raise (Invalid_argument "key out of bounds");
    if key = len_list
    then t := !t @ [ data ]
    else (
      let updated_list =
        List.mapi !t ~f:(fun i x -> if i = key then data else x)
      in
      t := updated_list)
  ;;

  let get t key =
    if key < 0 || key >= List.length !t then None else List.nth !t key
  ;;
end

module Dynarray_seq = struct
  (* TODO: replace the definition of type t and the implementations of
     create, set, and get *)
  type t = int Dynarray.t

  let create () = Dynarray.create ()

  let set t ~key ~data =
    let len_arr = Dynarray.length t in
    if key = len_arr
    then Dynarray.add_last t data
    else if key < 0 || key > len_arr
    then raise (Invalid_argument "key out of bounds")
    else Dynarray.set t key data
  ;;

  let get t key =
    if key < 0 || key >= Dynarray.length t
    then None
    else Some (Dynarray.get t key)
  ;;
  (* error that it doesnt return an int, so i needed to wrap it in Some , why *)
end
