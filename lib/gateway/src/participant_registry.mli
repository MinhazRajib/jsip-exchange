(** Server-global mapping between participant names and interned
    {!Participant_id.t}s.

    Built once per server and shared across every connection, so an id means
    the same participant to everyone who sees it (both parties named in a
    [Fill], for instance). It is {b additive}: a name keeps its id for the
    whole run — including across reconnects — and ids are never reused. That
    lifetime is why it is separate from the dispatcher's session table, which
    tracks who is {e currently} connected and is pruned on disconnect. *)

open! Core
open Jsip_types

type t

val create : unit -> t

(** The id for [name], assigning a fresh one the first time the name is seen. *)
val intern : t -> Participant.t -> Participant_id.t

(** The id [name] was interned under, or [None] if it has never logged in. *)
val find_id : t -> Participant.t -> Participant_id.t option

(** The name an id was interned from. Raises if the id is not from this
    registry. *)
val to_name : t -> Participant_id.t -> Participant.t
