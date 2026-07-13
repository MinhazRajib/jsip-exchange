open! Core
open Jsip_types

type t =
  | Submit of Order.Request.t
  | Book of Symbol_id.t
  | Subscribe of Symbol_id.t
  | Cancel of Client_order_id.t

type verb =
  | Buy
  | Sell
  | Book
  | Subscribe
  | Cancel
[@@deriving string ~case_insensitive ~capitalize:"SCREAMING_SNAKE_CASE"]

(* Default participant when no "as <name>" is specified in the command, adding the optional argument
`default_participant` overrides this with the caller-supplied default. *)
let system_default_participant = "anonymous"

(* The parse boundary: a human types a name, the wire carries an id. This is
   where the directory turns one into the other, so an unknown symbol is
   caught here rather than becoming an order the exchange has to reject. *)
let parse_symbol_id ~directory symbol_str =
  let%bind.Or_error symbol =
    try Ok (Symbol.of_string symbol_str) with
    | exn ->
      let exn_str = Exn.to_string exn in
      Or_error.error_string
        [%string "invalid symbol: %{symbol_str}\nexception: %{exn_str}"]
  in
  match Symbol_directory.find_id directory symbol with
  | Some id -> Ok id
  | None ->
    Or_error.error_string
      [%string "unknown symbol: %{symbol_str} (not traded on this exchange)"]
;;

let parse ?default_participant:participant ~directory line =
  let line_stripped = String.strip line |> String.filter ~f:(fun c -> not (Char.equal c '\n')) in
  if String.is_empty line_stripped
  then Or_error.error_string "empty command"
  else
    let parts =
      String.split line_stripped ~on:' ' |> List.filter ~f:(Fn.non String.is_empty)
    in
    match parts with
    | [] -> Or_error.error_string "empty command"
    | verb :: remaining_arguments -> 
      let open Result.Let_syntax in
      let%bind command = match String.uppercase verb with 
        | "BUY" -> Ok Buy
        | "SELL" -> Ok Sell
        | "BOOK" -> Ok Book
        | "SUBSCRIBE" -> Ok Subscribe
        | "CANCEL" -> Ok Cancel
        | other -> Or_error.error_string [%string "unknown command: %{other} (expected BUY/SELL/BOOK/SUBSCRIBE)"]
      in

      (match command with 
        | Buy| Sell -> (
          match remaining_arguments with 
            | client_order_id_str :: symbol_str :: size_str :: price_str :: rest ->
                let%bind side =
                  match command with
                  | Buy -> Ok Side.Buy
                  | Sell -> Ok Side.Sell
                  | _ ->
                    Or_error.error_string [%string "unknown command: this should not occur"]
                in
                let%bind client_order_id =
                  match (Int.of_string_opt client_order_id_str) with
                  | Some n  -> Ok n
                  | None -> Or_error.error_string "Invalid client_order_id"
                in
                let%bind size =
                  match Int.of_string_opt size_str with
                  | Some n when n > 0 -> Ok n
                  | Some _ -> Or_error.error_string "size must be positive"
                  | None -> Or_error.error_string [%string "invalid size: %{size_str}"]
                in
                let%bind price =
                  try Ok (Price.of_string price_str) with
                  | exn ->
                    let exn_str = Exn.to_string exn in
                    Or_error.error_string
                      [%string "invalid price: %{price_str}\nexception: %{exn_str}"]
                in
                let%bind symbol = parse_symbol_id ~directory symbol_str in
                let%bind time_in_force, rest =
                  match rest with
                  | tif_str :: rest' ->(
                    if String.(=) tif_str "as" (** handle when arg has no given time_in_force*)
                    then Ok(Time_in_force.Day, rest)
                    else
                    ( try 
                        (match Time_in_force.of_string tif_str with
                          | Ioc -> Ok (Ioc, rest')
                          | Day -> Ok (Day, rest'))
                      with 
                        | _ -> Or_error.error_string 
                        [%string "unknown time-in-force: %{tif_str} (expected %{Time_in_force.all_str})"]))
                  | [] -> Ok (Day, [])
                in
                let%bind participant =
                  match rest with
                  | "as" :: name :: _ | "AS" :: name :: _ -> Ok (Participant.of_string name)
                  | [] -> (match participant with 
                    | Some name -> Ok name 
                    | None -> Ok (Participant.of_string system_default_participant))
                  | _ ->
                    let trailing = String.concat ~sep:" " rest in
                    Or_error.error_string [%string "unexpected trailing arguments: %{trailing}"]
                in
                Ok
                   (Submit 
                   ({ client_order_id = Client_order_id.of_int client_order_id
                    ; symbol
                    ; participant
                    ; side
                    ; price
                    ; size = Size.of_int size
                    ; time_in_force
                    }: Order.Request.t))
            | _ -> Or_error.error_string
                [%string "expected: BUY|SELL <client_id> <symbol> <size> <price> [ %{Time_in_force.all_str}] [as <name>]"]
            )
          
        | Book | Subscribe ->(
          match remaining_arguments with 
          | symbol_str::_ ->
              let%bind symbol = parse_symbol_id ~directory symbol_str in
              (match command with
                | Book -> Ok (Book symbol : t)
                | Subscribe -> Ok (Subscribe symbol)
                | _ -> Or_error.error_string "UNEXPECTED ERROR: should be caught by earlier errors")
          | [] ->
              Or_error.error_string
                "expected: BOOK|SUBSCRIBE <symbol>"
        )
        | Cancel -> (
          match remaining_arguments with 
          | client_order_id_str::_ -> 
            let%bind client_id =
                match (Int.of_string_opt client_order_id_str) with
                  | Some n  -> Ok n
                  | None -> Or_error.error_string "Invalid client_order_id"
            in
            Ok (Cancel (Client_order_id.of_int client_id) : t)
          | [] -> Or_error.error_string
                "expected: CANCEL <client_id>"
        )
      )
;;
