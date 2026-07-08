open! Core
open! Async
open Jsip_types
module Protocol = Jsip_dashboard_protocol.Protocol

(* Keep about two minutes of history so a browser that was backgrounded can
   catch up on refocus; the client trims to its own (shorter) display window. *)
let buffer_size = 120

type t =
  { samples : Protocol.Sample.t Queue.t
  ; js_bundle : string option
  }

(* Flatten the exchange's rich snapshot into the primitive, JS-safe wire
   type. [at] is stamped here, on arrival, and is the x-axis of the client's
   charts. *)
let sample_of_snapshot (snapshot : Jsip_gateway.Exchange_stats.Snapshot.t)
  : Protocol.Sample.t
  =
  let at =
    Time_ns.now () |> Time_ns.to_span_since_epoch |> Time_ns.Span.to_sec
  in
  let latency (l : Jsip_gateway.Exchange_stats.Latency_summary.t)
    : Protocol.Latency.t
    =
    let us s = Time_ns.Span.to_us s in
    { p50_us = us l.p50
    ; p90_us = us l.p90
    ; p99_us = us l.p99
    ; count = l.count
    }
  in
  let price_string p = sprintf "$%.2f" (Price.to_float p) in
  let book (b : Jsip_gateway.Exchange_stats.Book_depth.t)
    : Protocol.Book_row.t
    =
    { symbol = Symbol.to_string b.symbol
    ; bid = Bbo.price b.bbo Buy |> Option.map ~f:price_string
    ; ask = Bbo.price b.bbo Sell |> Option.map ~f:price_string
    ; total_bid_size = Size.to_int b.total_bid_size
    ; total_ask_size = Size.to_int b.total_ask_size
    ; bid_count = b.bid_count
    ; ask_count = b.ask_count
    }
  in
  let memory = snapshot.memory in
  { at
  ; live_words = memory.live_words
  ; heap_words = memory.heap_words
  ; top_heap_words = memory.top_heap_words
  ; minor_collections = memory.minor_collections
  ; major_collections = memory.major_collections
  ; promoted_words = memory.promoted_words
  ; submit = latency snapshot.submit_latency
  ; cancel = latency snapshot.cancel_latency
  ; books = List.map snapshot.books ~f:book
  }
;;

let record t snapshot =
  Queue.enqueue t.samples (sample_of_snapshot snapshot);
  while Queue.length t.samples > buffer_size do
    ignore (Queue.dequeue_exn t.samples : Protocol.Sample.t)
  done
;;

let implementations t =
  Rpc.Implementations.create_exn
    ~implementations:
      [ Rpc.Rpc.implement' Protocol.recent_stats_rpc (fun (_ : unit) () ->
          Queue.to_list t.samples)
      ]
    ~on_unknown_rpc:`Close_connection
    ~on_exception:Log_on_background_exn
;;

(* The page chrome: design tokens + component classes live here in one place;
   the bonsai_web client (main.bc.js) just sets class names. Dark, dense,
   monochrome with a single accent — the developer-tool aesthetic. *)
let index_html =
  {html|<!doctype html>
<html>
  <head>
    <meta charset="utf-8" />
    <title>JSIP Exchange Dashboard</title>
    <style>
      :root {
        --font-sans: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif;
        --font-mono: ui-monospace, SFMono-Regular, Menlo, Consolas, monospace;
        --color-bg-0: lch(3% 1 260);
        --color-bg-1: lch(12% 1.5 280);
        --color-text-primary: lch(97.5% 0.5 240);
        --color-text-secondary: lch(86% 4 250);
        --color-text-tertiary: lch(61% 4 260);
        --color-text-quaternary: lch(44% 3 260);
        --color-border-1: lch(16% 2 260);
        --color-accent-hover: lch(62% 50 275);
        --color-warn: lch(72% 60 75);
        --color-ok: lch(66% 45 150);
      }
      * { box-sizing: border-box; }
      html, body { height: 100%; margin: 0; }
      body {
        background: var(--color-bg-0);
        color: var(--color-text-primary);
        font-family: var(--font-sans);
        font-size: 13px;
        color-scheme: dark;
        font-variant-numeric: tabular-nums;
        scrollbar-width: thin;
      }
      .app { display: flex; flex-direction: column; height: 100vh; }
      .topbar {
        display: flex; align-items: center; justify-content: space-between;
        padding: 10px 16px; border-bottom: 1px solid var(--color-border-1);
        background: var(--color-bg-1);
      }
      .brand { font-weight: 600; font-size: 15px; }
      .brand-sub { color: var(--color-text-tertiary); font-weight: 400; margin-left: 8px; font-size: 13px; }
      .status { display: flex; align-items: center; gap: 8px; color: var(--color-text-secondary); }
      .status-label { font-size: 12px; }
      .dot { width: 8px; height: 8px; border-radius: 9999px; display: inline-block; }
      .dot-live { background: var(--color-ok); }
      .dot-stale { background: var(--color-warn); }
      .dot-connecting { background: var(--color-text-quaternary); }
      .grid {
        flex: 1; min-height: 0; display: grid; gap: 12px; padding: 12px;
        grid-template-columns: repeat(3, 1fr); grid-template-rows: 1fr 1fr;
      }
      .panel {
        background: var(--color-bg-1); border: 1px solid var(--color-border-1);
        border-radius: 8px; padding: 16px; display: flex; flex-direction: column;
        gap: 10px; min-height: 0;
      }
      .panel-wide { grid-column: 1 / -1; }
      .panel h2 {
        margin: 0; font-size: 12px; font-weight: 600; text-transform: uppercase;
        letter-spacing: 0; color: var(--color-text-tertiary);
        display: flex; justify-content: space-between; align-items: baseline;
      }
      .unit { font-size: 11px; color: var(--color-text-quaternary); text-transform: none; font-weight: 400; }
      .metric-row { display: flex; gap: 24px; }
      .metric-value { font-size: 28px; font-weight: 600; color: var(--color-text-primary); }
      .metric-label { font-size: 12px; color: var(--color-text-tertiary); }
      .mono { font-family: var(--font-mono); font-variant-numeric: tabular-nums; }
      .spark { width: 100%; height: 80px; display: block; flex: 1; min-height: 0; }
      .submetrics { display: flex; gap: 16px; font-size: 12px; color: var(--color-text-tertiary); }
      .lat-grid { display: grid; grid-template-columns: repeat(3, 1fr); gap: 12px; }
      .lat-cell { display: flex; flex-direction: column; gap: 2px; }
      .lat-label { font-size: 11px; color: var(--color-text-quaternary); }
      .lat-value { font-size: 22px; font-weight: 600; }
      table.book { width: 100%; border-collapse: collapse; font-size: 12px; }
      table.book th {
        text-align: right; color: var(--color-text-quaternary); font-weight: 500;
        padding: 4px 8px; border-bottom: 1px solid var(--color-border-1); font-size: 11px;
      }
      table.book th.sym, table.book td.sym { text-align: left; }
      table.book td { padding: 4px 8px; border-bottom: 1px solid var(--color-border-1); }
      table.book td.num { text-align: right; }
      .dim { color: var(--color-text-quaternary); }
      .empty { text-align: center; color: var(--color-text-quaternary); padding: 12px; }
    </style>
  </head>
  <body>
    <div id="app"></div>
    <script defer src="/main.bc.js"></script>
  </body>
</html>
|html}
;;

let respond ~content_type body =
  let headers = Cohttp.Header.init_with "Content-Type" content_type in
  Cohttp_async.Server.respond_string ~headers body
;;

let http_handler t ~body:_ (_ : Socket.Address.Inet.t) request =
  match Uri.path (Cohttp.Request.uri request) with
  | "/" | "/index.html" ->
    respond ~content_type:"text/html; charset=utf-8" index_html
  | "/main.bc.js" ->
    (match t.js_bundle with
     | Some js -> respond ~content_type:"text/javascript; charset=utf-8" js
     | None ->
       Cohttp_async.Server.respond_string
         ~status:`Service_unavailable
         "client bundle not found; run `dune build app/dashboard/client`")
  | _ -> Cohttp_async.Server.respond_string ~status:`Not_found "not found"
;;

let connect_to_exchange ~host ~port =
  let%map result =
    Rpc.Connection.client
      (Tcp.Where_to_connect.of_host_and_port { host; port })
  in
  match result with
  | Ok conn -> conn
  | Error exn ->
    raise_s
      [%message
        "dashboard: failed to connect to exchange"
          (host : string)
          (port : int)
          (exn : Exn.t)]
;;

let subscribe_stats ~connection ~host ~port =
  match%map
    Rpc.Pipe_rpc.dispatch
      Jsip_gateway.Rpc_protocol.stats_feed_rpc
      connection
      ()
  with
  | Error err | Ok (Error err) ->
    raise_s
      [%message
        "dashboard: stats-feed subscription failed"
          (host : string)
          (port : int)
          (err : Error.t)]
  | Ok (Ok (pipe, _md)) -> pipe
;;

let load_js_bundle path =
  match%map Monitor.try_with (fun () -> Reader.file_contents path) with
  | Ok contents -> Some contents
  | Error _ -> None
;;

let start ~exchange_host ~exchange_port ~http_port ~js_bundle_path () =
  let%bind js_bundle = load_js_bundle js_bundle_path in
  let t = { samples = Queue.create (); js_bundle } in
  let%bind connection =
    connect_to_exchange ~host:exchange_host ~port:exchange_port
  in
  let%bind pipe =
    subscribe_stats ~connection ~host:exchange_host ~port:exchange_port
  in
  don't_wait_for (Pipe.iter_without_pushback pipe ~f:(fun s -> record t s));
  let%bind (_ : (Socket.Address.Inet.t, int) Cohttp_async.Server.t) =
    Rpc_websocket.Rpc.serve
      ~where_to_listen:(Tcp.Where_to_listen.of_port http_port)
      ~implementations:(implementations t)
      ~initial_connection_state:(fun () _from _addr _conn -> ())
      ~http_handler:(fun () -> http_handler t)
      ()
  in
  Async.return ()
;;
