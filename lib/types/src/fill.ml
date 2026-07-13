open! Core

type t =
  { fill_id : int
  ; symbol : Symbol_id.t
  ; price : Price.t
  ; size : Size.t
  ; aggressor_order_id : Order_id.t
  ; aggressor_client_order_id : Client_order_id.t
  ; aggressor_participant : Participant.t
  ; aggressor_side : Side.t
  ; resting_order_id : Order_id.t
  ; resting_client_order_id : Client_order_id.t
  ; resting_participant : Participant.t
  }
[@@deriving sexp, bin_io]

let to_string
  ~symbol_to_string
  ({ fill_id
   ; symbol
   ; price
   ; size
   ; aggressor_order_id
   ; aggressor_client_order_id
   ; aggressor_participant
   ; aggressor_side
   ; resting_order_id
   ; resting_client_order_id
   ; resting_participant
   } :
    t)
  =
  sprintf
    "fill_id=%d %s %s x%d aggressor=%s (client-id=%d) (%s) %s resting=%s \
     (client-id=%d) (%s)"
    fill_id
    (symbol_to_string symbol)
    (Price.to_string_dollar price)
    (Size.to_int size)
    (Order_id.to_string aggressor_order_id)
    (Client_order_id.to_int aggressor_client_order_id)
    (Participant.to_string aggressor_participant)
    (Side.to_string aggressor_side)
    (Order_id.to_string resting_order_id)
    (Client_order_id.to_int resting_client_order_id)
    (Participant.to_string resting_participant)
;;

let notional_cents t = Price.to_int_cents t.price * Size.to_int t.size

let to_participant_view ~symbol_to_string t participant =
  if Participant.( <> ) t.aggressor_participant participant
     && Participant.( <> ) t.resting_participant participant
  then None
  else (
    let side =
      match Participant.( = ) t.aggressor_participant participant with
      | true -> t.aggressor_side
      | false ->
        (match t.aggressor_side with
         | Side.Buy -> Side.Sell
         | Side.Sell -> Side.Buy)
    in
    match side with
    | Buy ->
      Some
        (sprintf
           "You bought %d %s at %s"
           (Size.to_int t.size)
           (symbol_to_string t.symbol)
           (Price.to_string_dollar t.price))
    | Sell ->
      Some
        (sprintf
           "You sold %d %s at %s"
           (Size.to_int t.size)
           (symbol_to_string t.symbol)
           (Price.to_string_dollar t.price)))
;;
