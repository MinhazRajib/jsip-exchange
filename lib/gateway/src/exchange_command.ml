open! Core
open Jsip_types

module Verb = struct
  type t =
    | Buy
    | Sell
    | Book
    | Subscribe
  [@@deriving equal, sexp]

  let of_string s =
    match String.uppercase s with
    | "BUY" -> Ok Buy
    | "SELL" -> Ok Sell
    | "BOOK" -> Ok Book
    | "SUBSCRIBE" -> Ok Subscribe
    | other -> Error [%string "unknown verb: %{other}"]
  ;;
end

type t =
  | Submit of Order.Request.t
  | Book of Symbol.t
  | Subscribe of Symbol.t

let parse ?(default_participant = Participant.of_string "default") line =
  let line = String.strip line in
  if String.is_empty line
  then Error "empty command"
  else (
    let parts =
      String.split line ~on:' ' |> List.filter ~f:(Fn.non String.is_empty)
    in
    match parts with
    | [] -> Error "empty command"
    | verb_str :: rest ->
      let open Result.Let_syntax in
      let%bind verb =
        match String.uppercase verb_str with
        | "BUY" -> Ok Verb.Buy
        | "SELL" -> Ok Verb.Sell
        | "BOOK" -> Ok Verb.Book
        | "SUBSCRIBE" -> Ok Verb.Subscribe
        | other ->
          Error
            [%string "unknown command: %{other} (expected valid command)"]
      in
      (match verb with
       | Verb.Book | Verb.Subscribe ->
         (match rest with
          | [] -> Error "expected: BOOK|SUBSCRIBE <symbol>"
          | symbol_str :: _ ->
            let%bind symbol =
              try Ok (Symbol.of_string symbol_str) with
              | exn ->
                let exn_str = Exn.to_string exn in
                Error
                  [%string
                    "invalid symbol: %{symbol_str}\nexception: %{exn_str}"]
            in
            (match verb with
             | Verb.Book -> Ok (Book symbol)
             | Verb.Subscribe -> Ok (Subscribe symbol)
             | _ -> Error "unexpected error"))
       | Verb.Buy | Verb.Sell ->
         (match rest with
          | symbol_str :: size_str :: price_str :: rest ->
            let%bind size =
              match Int.of_string_opt size_str with
              | Some n when n > 0 -> Ok n
              | Some _ -> Error "size must be positive"
              | None -> Error [%string "invalid size: %{size_str}"]
            in
            let%bind side =
              match verb with
              | Verb.Buy -> Ok Side.Buy
              | Verb.Sell -> Ok Side.Sell
              | _ -> Error [%string "invalid side for command: %{verb_str}"]
            in
            let%bind price =
              try Ok (Price.of_string price_str) with
              | exn ->
                let exn_str = Exn.to_string exn in
                Error
                  [%string
                    "invalid price: %{price_str}\nexception: %{exn_str}"]
            in
            let%bind symbol =
              try Ok (Symbol.of_string symbol_str) with
              | exn ->
                let exn_str = Exn.to_string exn in
                Error
                  [%string
                    "invalid symbol: %{symbol_str}\nexception: %{exn_str}"]
            in
            (* When moving the order-parsing logic, also fix the
               time-in-force parsing: Protocol.parse_command hardcodes "IOC",
               "DAY", etc. as string literals, but Time_in_force already has
               a case-insensitive of_string derived from [@@deriving string].
               Use it instead.

               Similarly, these abbreviations are hard-coded in error
               messages and usage strings, meaning this have to be manually
               updated every time the variant changes. Fortunately,
               [@@deriving enumerate] provides a val all : t list of the
               variant tags that you can use along with List.map and
               String.concat to add val all_str : string to Time_in_force.
               Use it in the error message for unrecognized values, so any
               new time-in-force variants will automatically appear.

               Apply the same principle to the usage string — use
               Time_in_force.all_str rather than writing "[DAY|IOC]". *)
            let%bind time_in_force, rest =
              match rest with
              | tif_str :: rest' ->
                (match
                   Or_error.try_with (fun () ->
                     Time_in_force.of_string tif_str)
                 with
                 | Ok tif -> Ok (tif, rest')
                 | Error _ ->
                   if String.equal (String.uppercase tif_str) "AS"
                   then Ok (Day, rest)
                   else
                     Error
                       [%string
                         "unknown time-in-force: %{tif_str} (expected valid \
                          time-in-force)"])
              | [] -> Ok (Day, [])
            in
            let%bind participant =
              match rest with
              | "as" :: name :: _ | "AS" :: name :: _ ->
                Ok (Participant.of_string name)
              | [] -> Ok default_participant
              | _ ->
                let trailing = String.concat ~sep:" " rest in
                Error [%string "unexpected trailing arguments: %{trailing}"]
            in
            Ok
              (Submit
                 { symbol
                 ; participant
                 ; side
                 ; price
                 ; size = Size.of_int size
                 ; time_in_force
                 })
          | _ ->
            Error
              "expected: BUY|SELL <symbol> <size> <price> [DAY|IOC] [as \
               <name>]")))
;;
