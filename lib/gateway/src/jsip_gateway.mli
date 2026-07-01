open Jsip_types

(** Gateway layer for the JSIP exchange... *)

module Protocol = Protocol
module Rpc_protocol = Rpc_protocol
module Session = Session
module Dispatcher = Dispatcher
module Exchange_server = Exchange_server

module Exchange_command : sig
  module Verb : sig
    type t =
      | Buy
      | Sell
      | Book
      | Subscribe

    val of_string : string -> (t, string) Result.t
  end

  type t =
    | Submit of Order.Request.t
    | Book of Symbol.t
    | Subscribe of Symbol.t
    | Cancel of Client_order_id.t

  val parse
    :  ?default_participant:Participant.t
    -> string
    -> (t, string) Result.t
end
