(** The dashboard's pure state machine: it folds the samples the client polls
    into a rolling window, ready for the view to render.

    Like [app/monitor]'s [Controller], this holds no Bonsai or Async types,
    so it can be unit-tested with plain values (see
    [test/test_controller.ml]). The Bonsai layer in [app/dashboard/client]
    drives it: each poll response is {!feed} into the model, and the view
    renders {!display}. *)

open! Core
open Jsip_dashboard_protocol

(** Everything the panes need, derived from the current window. *)
module Display : sig
  type t =
    { samples : Protocol.Sample.t list (** in the window, oldest first *)
    ; latest : Protocol.Sample.t option (** most recent sample, if any *)
    }
  [@@deriving sexp_of, equal]
end

type t

(** An empty window. [window] is how much history to keep (default 60s); the
    memory and latency charts show this span. *)
val empty : ?window:Time_ns.Span.t -> unit -> t

(** Fold newly-polled samples into the window: merge with what we have, drop
    duplicates (same [at]), keep them sorted oldest-first, and trim anything
    older than [window] before the newest sample. Idempotent on repeats,
    which matters because successive polls return overlapping windows. *)
val feed : t -> Protocol.Sample.t list -> t

val display : t -> Display.t
