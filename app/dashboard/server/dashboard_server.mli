(** The dashboard's relay/web server.

    Connects to a running exchange as an RPC client, subscribes to its
    [stats_feed_rpc] (Piece 1), converts each [Exchange_stats.Snapshot.t]
    into a flat {!Jsip_dashboard_protocol.Sample.t}, and buffers a rolling
    window of recent samples. It also serves the bonsai_web client
    (index.html + the JS bundle) over HTTP and exposes
    {!Jsip_dashboard_protocol.recent_stats_rpc} over a websocket, which the
    browser polls once a second.

    The exchange's pipe stays server-to-server; the browser polls this relay,
    so a backgrounded tab simply stops asking rather than piling up a pushed
    stream. *)

open! Core
open! Async

(** Connect to the exchange, start relaying its stats into a buffer, and
    serve the dashboard on [http_port]. The returned deferred becomes
    determined once the HTTP/websocket server is listening; the server keeps
    running after that. [js_bundle_path] is where to read the compiled client
    bundle. *)
val start
  :  exchange_host:string
  -> exchange_port:int
  -> http_port:int
  -> js_bundle_path:string
  -> unit
  -> unit Deferred.t
