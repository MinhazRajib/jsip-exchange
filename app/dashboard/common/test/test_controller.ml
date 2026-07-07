open! Core
open Jsip_dashboard_common
open Jsip_dashboard_protocol

(* A minimal sample at time [at] carrying a distinguishing [live_words], so
   we can see exactly which samples survived folding. The other fields don't
   matter for the windowing logic under test. *)
let sample ~at ~live_words : Protocol.Sample.t =
  let no_latency : Protocol.Latency.t =
    { p50_us = 0.; p90_us = 0.; p99_us = 0.; count = 0 }
  in
  { at
  ; live_words
  ; heap_words = 0
  ; top_heap_words = 0
  ; minor_collections = 0
  ; major_collections = 0
  ; promoted_words = 0
  ; submit = no_latency
  ; cancel = no_latency
  ; books = []
  }
;;

(* Show the surviving window as (at, live_words) pairs — compact and enough
   to verify ordering, dedup, and trimming. *)
let show t =
  let { Controller.Display.samples; latest } = Controller.display t in
  let pairs =
    List.map samples ~f:(fun s -> s.Protocol.Sample.at, s.live_words)
  in
  print_s
    [%message
      ""
        (pairs : (float * int) list)
        ~latest_live_words:
          (Option.map latest ~f:(fun s -> s.live_words) : int option)]
;;

let%expect_test "feed orders samples oldest-first and tracks latest" =
  let t = Controller.empty () in
  let t = Controller.feed t [ sample ~at:2. ~live_words:200 ] in
  let t = Controller.feed t [ sample ~at:1. ~live_words:100 ] in
  let t = Controller.feed t [ sample ~at:3. ~live_words:300 ] in
  show t;
  [%expect
    {| ((pairs ((1 100) (2 200) (3 300))) (latest_live_words (300))) |}]
;;

let%expect_test "overlapping polls dedup on [at]" =
  let t = Controller.empty () in
  let window =
    [ sample ~at:1. ~live_words:100; sample ~at:2. ~live_words:200 ]
  in
  let t = Controller.feed t window in
  (* Poll again, overlapping the previous window and adding one new sample. *)
  let t =
    Controller.feed
      t
      [ sample ~at:2. ~live_words:200; sample ~at:3. ~live_words:300 ]
  in
  show t;
  [%expect
    {| ((pairs ((1 100) (2 200) (3 300))) (latest_live_words (300))) |}]
;;

let%expect_test "samples older than the window are trimmed" =
  let t = Controller.empty ~window:(Time_ns.Span.of_sec 60.) () in
  (* t=0 is 100s before the newest (t=100), so it must fall outside a 60s
     window; t=50 and t=100 are within 60s of t=100 and survive. *)
  let t =
    Controller.feed
      t
      [ sample ~at:0. ~live_words:1
      ; sample ~at:50. ~live_words:50
      ; sample ~at:100. ~live_words:100
      ]
  in
  show t;
  [%expect {| ((pairs ((50 50) (100 100))) (latest_live_words (100))) |}]
;;
