(* Pure view helpers for the dashboard panes. No Bonsai or Async here — every
   function takes plain values and returns a [Vdom.Node.t], so the layout is
   easy to reason about and the wiring in [main.ml] stays small. Styling is
   by CSS class; the classes and design tokens live in the served index.html. *)

open! Core
open! Bonsai_web
module Sample = Jsip_dashboard_protocol.Protocol.Sample
module Latency = Jsip_dashboard_protocol.Protocol.Latency
module Book_row = Jsip_dashboard_protocol.Protocol.Book_row

let words_hum n = Int.to_string_hum n ~delimiter:','

(* One OCaml word is 8 bytes; show the live heap in MB for a human-readable
   companion to the raw word count. *)
let words_to_mb words = Float.of_int (words * 8) /. 1_000_000.

(* A minimal inline-SVG line chart. The viewBox is the data space; CSS
   stretches it to the panel width, and [non-scaling-stroke] keeps the line
   crisp. *)
let sparkline ~stroke (values : float list) : Vdom.Node.t =
  let width = 1000. in
  let height = 100. in
  let svg tag attrs children = Vdom.Node.create_svg tag ~attrs children in
  let frame =
    [ Vdom.Attr.create "viewBox" (sprintf "0 0 %.0f %.0f" width height)
    ; Vdom.Attr.create "preserveAspectRatio" "none"
    ; Vdom.Attr.class_ "spark"
    ]
  in
  match values with
  | [] | [ _ ] -> svg "svg" frame []
  | _ :: _ :: _ ->
    let n = List.length values in
    let ymin = List.reduce_exn values ~f:Float.min in
    let ymax = List.reduce_exn values ~f:Float.max in
    (* Guard a flat series (ymax = ymin) so we don't divide by zero; it
       renders as a centred horizontal line. *)
    let yrange = Float.max 1e-9 (ymax -. ymin) in
    let points =
      List.mapi values ~f:(fun i v ->
        let x = Float.of_int i /. Float.of_int (n - 1) *. width in
        let y = height -. ((v -. ymin) /. yrange *. height) in
        sprintf "%.2f,%.2f" x y)
      |> String.concat ~sep:" "
    in
    svg
      "svg"
      frame
      [ svg
          "polyline"
          [ Vdom.Attr.create "points" points
          ; Vdom.Attr.create "fill" "none"
          ; Vdom.Attr.create "stroke" stroke
          ; Vdom.Attr.create "stroke-width" "1.5"
          ; Vdom.Attr.create "vector-effect" "non-scaling-stroke"
          ]
          []
      ]
;;

let memory (samples : Sample.t list) (latest : Sample.t option) : Vdom.Node.t
  =
  let series = List.map samples ~f:(fun s -> Float.of_int s.live_words) in
  let live, heap, promoted, minor, major =
    match latest with
    | None -> "—", "—", "—", "—", "—"
    | Some s ->
      ( [%string "%{words_hum s.live_words}"]
      , [%string "%{words_hum s.heap_words}"]
      , [%string "%{words_hum s.promoted_words}"]
      , Int.to_string s.minor_collections
      , Int.to_string s.major_collections )
  in
  let mb =
    match latest with
    | None -> "—"
    | Some s -> sprintf "%.1f MB" (words_to_mb s.live_words)
  in
  {%html|
    <section class="panel panel-wide">
      <h2>Process memory <span class="unit">live_words, last 60s</span></h2>
      <div class="metric-row">
        <div class="metric">
          <div class="metric-value mono">#{live}</div>
          <div class="metric-label">live words (#{mb})</div>
        </div>
      </div>
      %{sparkline ~stroke:"var(--color-accent-hover)" series}
      <div class="submetrics mono">
        <span>heap #{heap}</span>
        <span>promoted #{promoted}</span>
        <span>minor GC #{minor}</span>
        <span>major GC #{major}</span>
      </div>
    </section>
  |}
;;

let latency
  ~title
  ~(stroke : string)
  (samples : Sample.t list)
  (get : Sample.t -> Latency.t)
  : Vdom.Node.t
  =
  let series = List.map samples ~f:(fun s -> (get s).p99_us) in
  let latest = List.last samples |> Option.map ~f:get in
  let cell label value =
    let text =
      match latest with
      | Some (l : Latency.t) when l.count > 0 -> sprintf "%.0f µs" value
      | _ -> "—"
    in
    {%html|
      <div class="lat-cell">
        <div class="lat-label">#{label}</div>
        <div class="lat-value mono">#{text}</div>
      </div>
    |}
  in
  let p50, p90, p99, count =
    match latest with
    | None -> 0., 0., 0., 0
    | Some l -> l.p50_us, l.p90_us, l.p99_us, l.count
  in
  {%html|
    <section class="panel">
      <h2>#{title} <span class="unit">%{count#Int} req/s</span></h2>
      <div class="lat-grid">
        %{cell "p50" p50}
        %{cell "p90" p90}
        %{cell "p99" p99}
      </div>
      %{sparkline ~stroke series}
    </section>
  |}
;;

let books (rows : Book_row.t list) : Vdom.Node.t =
  let opt = function None -> "—" | Some s -> s in
  let row (b : Book_row.t) =
    {%html|
      <tr>
        <td class="mono sym">#{b.symbol}</td>
        <td class="mono num">#{opt b.bid}</td>
        <td class="mono num">#{opt b.ask}</td>
        <td class="mono num">%{b.total_bid_size#Int}</td>
        <td class="mono num">%{b.total_ask_size#Int}</td>
        <td class="mono num dim">%{b.bid_count#Int}</td>
        <td class="mono num dim">%{b.ask_count#Int}</td>
      </tr>
    |}
  in
  let body =
    match rows with
    | [] ->
      let colspan = Vdom.Attr.create "colspan" "7" in
      {%html|<tr><td class="empty" %{colspan}>no symbols</td></tr>|}
    | _ :: _ -> {%html|<>*{List.map rows ~f:row}</>|}
  in
  {%html|
    <section class="panel">
      <h2>Order book depth</h2>
      <table class="book">
        <thead>
          <tr>
            <th class="sym">Symbol</th>
            <th class="num">Bid</th>
            <th class="num">Ask</th>
            <th class="num">Bid sz</th>
            <th class="num">Ask sz</th>
            <th class="num dim">Bids</th>
            <th class="num dim">Asks</th>
          </tr>
        </thead>
        <tbody>%{body}</tbody>
      </table>
    </section>
  |}
;;

(* Freshness/connection indicator for the top bar. [age] is seconds since the
   newest sample (None = nothing received yet). *)
let status ~(age : float option) : Vdom.Node.t =
  let state_class, label =
    match age with
    | None -> "connecting", "connecting…"
    | Some age when Float.( > ) age 3. ->
      "stale", sprintf "stale · %.0fs ago" age
    | Some age -> "live", sprintf "live · updated %.0fs ago" age
  in
  let dot = Vdom.Attr.classes [ "dot"; "dot-" ^ state_class ] in
  {%html|
    <div class="status">
      <span %{dot}></span>
      <span class="status-label">#{label}</span>
    </div>
  |}
;;
