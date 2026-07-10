(** A server-local integer id for a participant.

    Interned from a participant's name at login by {!Participant_registry}
    and used to key the gateway's own session tables. It never crosses the
    wire: clients only ever see the {!Jsip_types.Participant} name. Keeping
    it out of [lib/types] is deliberate — the wire types stay pure, and all
    the statefulness lives in the registry. *)

open! Core

type t = private int [@@deriving compare, equal, hash, sexp_of]

include Hashable.S_plain with type t := t

val of_int : int -> t
