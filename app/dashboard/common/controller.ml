open! Core
open Jsip_dashboard_protocol

module Display = struct
  type t =
    { samples : Protocol.Sample.t list
    ; latest : Protocol.Sample.t option
    }
  [@@deriving sexp_of, equal]
end

(* One minute of history by default: enough for the "flat vs linear vs
   exponential" read the memory pane is for. *)
let default_window = Time_ns.Span.of_sec 60.

type t =
  { samples : Protocol.Sample.t list (* oldest first *)
  ; window_sec : float
  }

let empty ?(window = default_window) () =
  { samples = []; window_sec = Time_ns.Span.to_sec window }
;;

let feed t new_samples =
  (* [dedup_and_sort] with a compare that only looks at [at] both sorts
     oldest-first and collapses samples that share a timestamp, so re-polling
     an overlapping window is a no-op. *)
  let merged =
    List.dedup_and_sort (t.samples @ new_samples) ~compare:(fun a b ->
      Float.compare a.Protocol.Sample.at b.at)
  in
  match List.last merged with
  | None -> { t with samples = [] }
  | Some newest ->
    let cutoff = newest.at -. t.window_sec in
    { t with
      samples =
        List.filter merged ~f:(fun s ->
          Float.( >= ) s.Protocol.Sample.at cutoff)
    }
;;

let display t = { Display.samples = t.samples; latest = List.last t.samples }
