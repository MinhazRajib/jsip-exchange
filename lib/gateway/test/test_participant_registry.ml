open! Core
open Jsip_types
open Jsip_gateway

let%expect_test "registry interns names to stable, additive ids" =
  let registry = Participant_registry.create () in
  let alice = Participant.of_string "Alice" in
  let bob = Participant.of_string "Bob" in
  (* An unseen name has no id yet. *)
  print_s
    [%sexp
      (Participant_registry.find_id registry alice : Participant_id.t option)];
  [%expect {| () |}];
  (* First intern assigns id 0; interning again returns the same id. *)
  let alice_id = Participant_registry.intern registry alice in
  let alice_id_again = Participant_registry.intern registry alice in
  print_s
    [%message
      "" (alice_id : Participant_id.t) (alice_id_again : Participant_id.t)];
  [%expect {| ((alice_id 0) (alice_id_again 0)) |}];
  (* A different name gets a distinct id. *)
  print_s
    [%sexp (Participant_registry.intern registry bob : Participant_id.t)];
  [%expect {| 1 |}];
  (* The reverse direction round-trips id -> name. *)
  print_endline
    (Participant_registry.to_name registry alice_id |> Participant.to_string);
  print_endline
    (Participant_registry.to_name registry (Participant_id.of_int 1)
     |> Participant.to_string);
  [%expect {|
    Alice
    Bob
    |}]
;;
