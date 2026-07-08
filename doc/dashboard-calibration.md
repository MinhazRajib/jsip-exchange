# Dashboard calibration notes (Part 3, Section 2c)

Observations from running the dashboard (`app/dashboard/`) against each
scenario. These are the raw material for the debrief and for choosing which
problems to fix in Section 3 — the point is to fix the *specific* problems we
measured here, not generic ones.

## How these were gathered

The dashboard renders in a browser; open it and watch the panes:

```sh
dune build
# terminal 1 — exchange + load (swap in the scenario you want):
dune exec app/scenario_runner/bin/main.exe -- -scenario cancel-storm -port 12345
# terminal 2 — dashboard:
dune exec app/dashboard/server/bin/main.exe -- -exchange-port 12345 -http-port 8080
# open http://localhost:8080
```

The numbers below were captured **headlessly** by polling the dashboard's
`recent_stats_rpc` over ~30s (via `app/dashboard/server/test/smoke.exe`), so
they characterise the trend without a browser. **Confirm the shapes visually**
— that's the part a test can't do for you.

`live_words` is the exchange process's live OCaml heap (from `Gc.stat`);
latency is submit/cancel p99 over each 1s window; `n/s` is requests/second.

## Baseline — idle exchange (no bots)

`dune exec app/server/bin/main.exe -- -port <p>` with nothing trading.

| t (s) | live_words | submit p99 | cancel p99 |
| ----- | ---------- | ---------- | ---------- |
| 0     | 650,198    | —          | —          |
| 3     | 650,198    | —          | —          |
| 6     | 650,198    | —          | —          |
| 9     | 650,198    | —          | —          |

- **Memory:** flat (bounded). This is the reference — anything above this
  under load is attributable to the load.
- **Latency:** no traffic, so no samples.

## Cancel-storm

`-scenario cancel-storm` — 3 storm bots, ~500 order+cancel pairs/sec each.

| t (s) | live_words | submit p99 (µs) | submit n/s | cancel p99 (µs) | cancel n/s |
| ----- | ---------- | --------------- | ---------- | --------------- | ---------- |
| 0     | 876,833    | 1965            | 1500       | 936             | 1500       |
| 3     | 989,933    | 2541            | 1500       | 855             | 1500       |
| 6     | 1,127,609  | 1715            | 1500       | 935             | 1500       |
| 9     | 1,240,709  | 1800            | 1500       | 992             | 1500       |
| 12    | 1,402,961  | 1794            | 1500       | 908             | 1500       |
| 15    | 1,516,061  | 1915            | 1500       | 797             | 1500       |
| 18    | 1,629,161  | 1807            | 1500       | 1025            | 1500       |
| 21    | 1,742,261  | 1707            | 1500       | 888             | 1500       |
| 24    | 1,855,361  | 1908            | 1500       | 962             | 1500       |

- **Memory: linear, unbounded growth.** ~+37,700 words/s (~300 KB/s); more
  than doubled in 24s with no plateau. Straight line, not a curve — so
  **linear**, not exponential, and not bounded. Left running it would exhaust
  memory (~1 GB/hour).
- **Latency: bounded.** Submit p99 hovers ~1.7–2.5 ms, cancel p99 ~0.8–1.0 ms,
  both roughly flat. So this scenario is a **memory leak, not a throughput
  collapse** — the engine keeps up (steady 1500 req/s each) but leaks.
- **Submit is ~2x slower than cancel** (p99 ~1.8 ms vs ~0.9 ms) — submit does
  the matching work; cancel is a lookup + two removes.
- **Extras (book depth):** uninformative here — AAPL's book stays near-empty
  because orders are cancelled/filled as fast as they arrive. The depth pane
  isn't the right lens for this scenario; **pipe occupancy** (an extra we did
  not build) would more directly show buffer buildup. Worth adding if Section
  3 targets a slow-consumer problem.

### First hypothesis for Section 3 — WRONG (kept as a lesson)

Our first guess was the matching engine's `participant_client_order_ids` table
(`lib/order_book/src/matching_engine.ml:19`): **cancel** removes an order's
entry but **fill** did not, so filled orders would leak table entries "under a
storm with crossing trades."

Running the dashboard against the actual bot disproved this:

- The cancel-storm bot is built to **never cross**. It runs a flat fundamental
  (`volatility_cents_per_sec = 0.0`) and prices every order 100¢ away, so orders
  rest and are cancelled — the exchange log shows **0 fills** over a whole run.
- So the fill path is never taken here, and since every order is submitted
  (adds a table entry) then cancelled (removes it), the id table stays
  **bounded**. Fixing the fill path did **not** move the memory line at all.

We still committed the fill-path fix (`matching_engine.ml`) with a regression
test: it closes a genuine *latent* leak that would bite a filling storm. But it
is not the fix for what the dashboard measured. **Lesson: a green unit test does
not mean you fixed the thing you measured — you have to run the app and watch
the pane move.**

### Actual root cause — found & fixed (verified)

The leak was in the **scenario runner**, not the engine. In
`app/scenario_runner/src/runner.ml`, every bot subscribes to its `session_feed`
pipe, but the old code only *drained* that pipe for market-data consumers
(`is_marketdata_consumer = true`). The cancel-storm bots are not market-data
consumers, so each one subscribed to a session feed and **never read it** — while
the server pushed ~1000 accept/cancel events per second per bot into that pipe
with `write_without_pushback_if_open` (`lib/gateway/src/session.ml:18`), which
never blocks. The unread pipe buffer grew without bound; three bots produced the
~60k words/s slope.

**Fix:** always drain the session feed, and interleave market data on top only
when the bot is a market-data consumer.

**Verified:** after the fix, `live_words` is flat at ~665k for the whole run
(idle baseline is ~650k), versus ~5.9M and climbing before. Same throughput
(1500 req/s each), same latency — memory just stops growing.

The server-side `write_without_pushback_if_open` (flagged in `CLAUDE.md`) is the
deeper *enabler*: a well-behaved exchange would bound the buffer or drop/kick a
slow consumer instead of buffering forever. That is the right follow-up if a
**Slow_consumers** bot is added — the runner fix cures the self-inflicted case,
but not a genuinely slow *client* the server can't control.

## Scenarios not yet run

Only `cancel-storm` is implemented in this repo right now; the rest are still
`failwith "TODO"` stubs (`app/scenarios/src/`), and the group's other
pathological bots (e.g. Book_fill, Slow_consumers) aren't pulled in yet. Fill
these in once they exist:

- **calm-day** (baseline sanity) — expect flat memory, low latency.
- **active-day / earnings-shock / flash-crash** — memory shape? latency under
  bursty load?
- **Book_fill** — expect memory growth from ever-growing resting books; watch
  the book-depth pane.
- **Slow_consumers** — expect the audit/session pipe backpressure smell
  (`Pipe.write_without_pushback_if_open`); the **pipe occupancy** extra is the
  right lens. This is the case the depth pane can't show.
