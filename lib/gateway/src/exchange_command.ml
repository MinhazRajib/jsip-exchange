open! Core
open Jsip_types

type t =
  | Submit of Order.Request.t
  | Book of Symbol.t
  | Subscribe of Symbol.t

type verb =
  | Buy
  | Sell
  | Book
  | Subscribe
[@@deriving string ~case_insensitive ~capitalize:"SCREAMING_SNAKE_CASE"]

(*explain*)
let parse ?default_participant:participant line =
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
      let%bind first_word = match String.uppercase verb with 
        | "BUY" -> Ok Buy
        | "SELL" -> Ok Sell
        | "BOOK" -> Ok Book
        | "SUBSCRIBE" -> Ok Subscribe
        | other -> Or_error.error_string [%string "unknown command: %{other} (expected BUY/SELL/BOOK/SUBSCRIBE)"]
      in

      (** match on verb to parse remaining arguments*)
      (match first_word with 
        (** match Buy and Sell arguments*)
        | Buy| Sell -> (
          match remaining_arguments with 
            | symbol_str :: size_str :: price_str :: rest ->
                let%bind side =
                  match first_word with
                  | Buy -> Ok Side.Buy
                  | Sell -> Ok Side.Sell
                  | _ ->
                    Or_error.error_string [%string "unknown command: this should not occur"]
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
                let%bind symbol =
                  try Ok (Symbol.of_string symbol_str) with
                  | exn ->
                    let exn_str = Exn.to_string exn in
                    Or_error.error_string
                      [%string
                        "invalid symbol: %{symbol_str}\nexception: %{exn_str}"]
                in
                let%bind time_in_force, rest =
                  match rest with
                  | tif_str :: rest' ->(
                    if String.(=) tif_str "as"
                    then Ok(Time_in_force.Day, rest)
                    else
                    (**adjust to handle use of_string to parse values, and enumerate to handle error messages*)
                    (**is there a prettier way to do this?*)
                    try 
                      (match Time_in_force.of_string tif_str with
                        | Ioc -> Ok (Ioc, rest')
                        | Day -> Ok (Day, rest'))
                    with 
                      | _ -> Or_error.error_string [%string "unknown time-in-force: %{tif_str} (expected %{Time_in_force.all_str})"])
                  | [] -> Ok (Day, [])
                in
                let%bind participant =
                  match rest with
                  | "as" :: name :: _ | "AS" :: name :: _ -> Ok (Participant.of_string name)
                  (**if given default participant, overrides the no name, otherwise default to anonymous*)
                  | [] -> (match participant with 
                    | Some name -> Ok name 
                    | None -> Ok (Participant.of_string "anonymous"))
                  | _ ->
                    let trailing = String.concat ~sep:" " rest in
                    Or_error.error_string [%string "unexpected trailing arguments: %{trailing}"]
                in
                Ok
                   (Submit ({symbol
                    ; participant
                    ; side
                    ; price
                    ; size = Size.of_int size
                    ; time_in_force
                    }: Order.Request.t))
            | _ -> Or_error.error_string
                [%string "expected: BUY|SELL <symbol> <size> <price> [ %{Time_in_force.all_str}] [as <name>]"]
            )
          
        (** match Book and Subscribe arguments*)
        | Book | Subscribe ->(
          match remaining_arguments with 
          | symbol_str::_ ->
              let%bind symbol =
                try Ok (Symbol.of_string symbol_str) with
                | exn ->
                let exn_str = Exn.to_string exn in
                Or_error.error_string
                  [%string
                  "invalid symbol: %{symbol_str}\nexception: %{exn_str}"]
              in
              (match first_word with 
                | Book -> Ok (Book symbol : t)
                | Subscribe -> Ok (Subscribe symbol)
                | _ -> Or_error.error_string "UNEXPECTED ERROR: should be caught by earlier errors")
          | [] ->
              Or_error.error_string
                "expected: BOOK|SUBSCRIBE <symbol>"
        )
        (** unplanned type, clean up this formatting later*)
      )
;;
