open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
module Protocol = Jsip_dashboard_protocol.Protocol
module Controller = Jsip_dashboard_common.Controller

let poll_every = Time_ns.Span.of_sec 1.

(* Convert a Bonsai "now" into epoch seconds, to compare against a sample's
   [at] (which the server also stamped in epoch seconds). *)
let now_seconds now = Time_ns.to_span_since_epoch now |> Time_ns.Span.to_sec

let dashboard ~(display : Controller.Display.t) ~now_sec : Vdom.Node.t =
  let age =
    Option.map display.latest ~f:(fun s -> now_sec -. s.Protocol.Sample.at)
  in
  let books = match display.latest with None -> [] | Some s -> s.books in
  {%html|
    <div class="app">
      <header class="topbar">
        <div class="brand">JSIP Exchange <span class="brand-sub">live dashboard</span></div>
        %{Panes.status ~age}
      </header>
      <main class="grid">
        %{Panes.memory display.samples display.latest}
        %{Panes.latency
            ~title:"Submit latency"
            ~stroke:"var(--color-accent-hover)"
            display.samples
            (fun s -> s.Protocol.Sample.submit)}
        %{Panes.latency
            ~title:"Cancel latency"
            ~stroke:"var(--color-warn)"
            display.samples
            (fun s -> s.Protocol.Sample.cancel)}
        %{Panes.books books}
      </main>
    </div>
  |}
;;

let app (local_ graph) : Vdom.Node.t Bonsai.t =
  let samples =
    Rpc_effect.Rpc.poll
      Protocol.recent_stats_rpc
      ~equal_query:[%equal: Unit.t]
      ~every:(Bonsai.return poll_every)
      ~output_type:Last_ok_response
      (Bonsai.return ())
      graph
  in
  let now = Bonsai.Clock.approx_now ~tick_every:poll_every graph in
  let%arr samples and now in
  let now_sec = now_seconds now in
  match samples with
  | None ->
    (* No successful poll yet. *)
    dashboard ~display:{ samples = []; latest = None } ~now_sec
  | Some window ->
    let display =
      Controller.display (Controller.feed (Controller.empty ()) window)
    in
    dashboard ~display ~now_sec
;;

let () = Bonsai_web.Start.start ~bind_to_element_with_id:"app" app
