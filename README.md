# NSDL

A parser and simulation model for the Network Systems Description
Language. The grammar (`lib/parser.mly`) still follows
`../docs/Older Versions/Network_Systems_Description_Language_Proposal_v0_2.docx`;
the runtime is migrating to the stricter contract in
`../docs/NSDL_Executable_Semantic_Core_Proposal_v0_3.docx`, which
supersedes v0.2 and drives everything in "v0.3 migration" below.

The parser (text in, `Ast.program` out) is done. On top of it sits a
simulation model (`lib/sim.ml`) that's grown past "just a bag of
fields": `instance NAME : TYPE` binds to a matching `object TYPE { }`
definition, instances have a real lifecycle state that starts at the
type's declared `initial` state, `transition`/`after EXPR -> STATE`
actually move an instance between states, entering a state runs that
state's `in STATE { }` body, and handlers can be dispatched by trigger
name (matched against the instance's current state) via a new `Invoke`
action. Port/message dispatch (`emit`/`on TRIGGER at PORT`/`schedule`)
is real now too (see "Port/message dispatch" below). There's still no
workflow success evaluation, no deterministic/seeded randomness, and no
name resolution, type checking, or IR lowering — see "Known
limitations" and "Next up" below for exactly where the line is now.

## v0.3 migration

v0.3 is a much stricter executable contract than v0.2: only events may
change canonical state (derived facts like network status become
read-only projections), topology-affecting events run as an atomic
multi-step transaction, deliveries get revalidated against a
topology/medium epoch at arrival time (no "ghost packets" after a
disconnect), same-timestamp ordering follows normative priority classes
(physical mutation → derived recompute → invalidation → protocol
reactions → application work → observation → analytics), and DHCP must
be a real message-driven Discover/Offer/Request/Ack exchange rather
than a shortcut. It also names a concrete reference vertical slice
(power source, gateway, switch, workstation, printer, three Cat6 media)
with 8 acceptance criteria, and a 6-phase implementation order this
migration follows:

| Phase | Deliverable | Exit condition | Status |
| --- | --- | --- | --- |
| 1 | Event queue, stable identity, canonical state transaction, snapshots | Deterministic replay of state-only fixtures | done |
| 2 | Ports, media, lifecycle, epochs/generations | Disconnect/power tests pass without ghost deliveries | done |
| 3 | Switch forwarding and DHCP message flow | Lease causality tests pass | done |
| 4 | Observations and provenance | Projection consistency tests pass | done |
| 5 | Gateway fidelity profile + print workflow | Reference vertical slice passes end-to-end | done |
| 6 | World/embodiment bindings | Alternate clients preserve canonical outcomes | done |

**Phase 1 (done):** `scheduled_event` now carries a `priority` (0–6,
named `Sim.priority_physical` .. `Sim.priority_analytics`), and
`advance`'s same-timestamp sort is `(due, priority, seq)` instead of
just `(due, seq)` — every event this module currently schedules is
`priority_physical` until phases 2+ actually populate the other
classes, but the ordering contract is now normative rather than
insertion-order-only. `Sim.snapshot`/`Sim.restore` capture and rebuild
a world's full mutable state (instances, fields, lifecycle states,
connections, clock, pending queue) for the deterministic-replay
property this phase is named for: restoring a snapshot and replaying
the same actions reproduces identical state, proven in
`test/harness.ml`'s `test_snapshot_restore_replay` even though
`random(...)` itself isn't seeded yet — the trick is that a random
draw is already baked into a concrete `due` time by the point you
snapshot, so replay from that point is deterministic regardless.

**Phase 2 (done):** the grammar now accepts v0.3's new forms —
`state { field: TYPE }` blocks (`TYPE` can be a union `up | down` and/or
optional `medium_id?`, see `Ast.state_type`), `emits`/`receives`
declarations, `endpoints: exactly<N, T>` arity constraints, `capability`
declarations, top-level `profile { }` blocks, and a looser `inject`
shape (`inject KIND on TARGET` or `inject KIND(args) on TARGET`,
replacing the old mandatory-bare-amount form — see the `SInject`
semantics note below). `object_def` now captures all of these
(`state_fields`/`emits`/`receives`/`endpoints`/`capabilities`), though
none of it is deeply interpreted yet — a `state` field's declared type
isn't checked against what actually gets written to it, `emits`/
`receives` aren't matched against anything, `capability` doesn't gate
anything. That's honestly out of scope for "parses and is stored";
Phase 3 is where ports/media start actually mattering to behavior.

The real runtime addition: `world.topology_epoch` (global) plus a
per-medium `generation` field (an ordinary instance field, bumped by
`disconnect_medium`), and `scheduled_event` gained an optional
`delivery : delivery_check option`. A delivery sent via
`Sim.send_via_medium` stamps the medium's generation and the epoch at
send time; `advance` revalidates both at fire time and drops the
delivery (`dropped_due_to_link_loss`, logged) instead of executing it
if either changed — this is what rules out ghost packets. Proven in
`test/harness.ml`: a positive control (delivery succeeds with no
disconnect) and the actual exit condition (disconnecting the medium
before the delivery's due time drops it).

`SInject`'s new semantics: `inject KIND on TARGET` sets
`TARGET.KIND = true` (a fault/condition marker); `inject KIND(arg) on
TARGET` uses the first arg's value instead. `disconnect` additionally
calls `disconnect_medium`. The v0.3 doc's own examples (`inject
disconnect on X`, `inject impairment(loss = 0.35) on Y`) don't fully
specify what an injection *does* beyond that shape, so this is a
documented interpretation, not a derived fact — see the comment at
`SInject`'s case in `lib/sim.ml`.

**Phase 3 (done):** `Sim.dhcp_discover` runs a reduced but causal
Discover/Offer/Request/Ack handshake between a named client and server
— each of the four hops is `send_via_path`, so each is independently
revalidated against every medium on its resolved route's
generation/epoch at its own fire time, same as any other delivery. All
four hops are pre-scheduled at invocation time rather than dynamically
chained hop-by-hop (a documented "reduced" simplification), but lease
installation happens entirely inside the fourth hop's (the Ack's)
body, so it only runs if that specific delivery survives revalidation
— which is what the doc's actual DHCP causality property needs,
regardless of what happened to the earlier hops. Exposed as a new
`DhcpDiscover` action (`client`, `server`, `address`, `lease_seconds`
— no `medium`: the route is resolved automatically, see below); the
lease itself is just ordinary instance fields on the client
(`dhcp_address`, `dhcp_server`, `dhcp_starts_at`, `dhcp_expires_at`,
`dhcp_state`), not a new record type — consistent with how everything
else in `Sim` is stored. Four new tests in `test/harness.ml` prove each
of the doc's own "DHCP causality property" bullets: no lease from a
bare `address = dhcp` field with no handshake invoked; a full handshake
installs a lease matching the delivered Ack; a disconnect before the
Ack's due time drops it and installs no lease (the phase's actual exit
condition); and a disconnect *after* the Ack doesn't retroactively
remove an already-installed lease.

**Switch forwarding (closed, added after Phase 6):** `Sim.resolve_path`
does a breadth-first search over `world.connections`, treated as an
undirected graph of *instances* (each `connect A -> B via M` statement
becomes an edge between A's and B's leading instance names, labeled
with medium `M`), to find the ordered chain of media connecting any
two instances — not just ones sharing one directly-named medium.
`Sim.send_via_path` wraps this: it resolves the route, stamps every
medium along it with its current generation (so revalidation at fire
time checks the *whole* path, not just the last hop), and schedules
the delivery; it returns `false` (and logs `no route: A -> B`, schedules
nothing) if the instances aren't connected at all. `dhcp_discover` now
calls this instead of naming one medium directly, so a handshake
genuinely routes through an intermediate `ethernet_switch` instance
per the doc's own reference topology — `test/fixtures/switch_topology.nsdl`
is a real two-hop client → switch → gateway topology (no medium directly
joins client and gateway) with four new tests: the two-hop handshake
succeeds and installs a lease; disconnecting *either* the client-side or
the gateway-side link before the Ack's due time drops the delivery
(multi-hop revalidation, not just single-hop); and discovering from a
client with no path to the server reports an error immediately instead
of silently scheduling a handshake that can never complete. This closes
the gap this section used to describe as deferred, and was deliberately
kept deterministic and simple — plain BFS over declared connections, no
MAC-address learning or per-port forwarding tables — since the doc's
exit condition only asks that a message can reach its destination
through the declared topology, not that switching be modeled with
full fidelity.

**Phase 4 (done):** the proposal's `NetworkStatus` struct (physical
attachment, carrier, L2 reachability, IPv4, default route, DNS, service
readiness, and an `overall` summary) is implemented as a genuinely
derived, read-only projection — `Sim.network_status_field` computes any
of its eight facts on demand from whatever canonical state actually
exists, and nothing stores them. `inspect NAME.overall` (and the other
seven names) route through this same function, so `Inspect` *is* the
"project(canonical_state, observer_context)" the proposal's projection
-consistency property asks for: one code path, so there's no way for
two observers to see disagreeing values. Writing a derived field
directly — via `Configure` or an authored `set`/bare assignment — is
now rejected rather than silently accepted-and-ignored, per the
proposal's "derived state is read-only" invariant.

Honest gap: several of the eight facts (`carrier`, `l2_reachability`,
`dns`, `service_readiness`) have no real causal mechanism behind them
yet, so they report a fixed "nothing modeled" default (`down`,
`unavailable`, `unavailable`, `unavailable`) rather than fabricating
something that merely *looks* derived. `physical_attachment`, `ipv4`,
`default_route`, and `overall` are genuinely computed from real
canonical facts (a medium's `physical_state`, a client's `dhcp_state`).
`l2_reachability` stays in this list even after switch forwarding
landed (see below): `resolve_path` can answer "is instance A reachable
from instance B," but `network_status_field` is a per-instance
projection with no second endpoint to check against, so wiring this up
for real means changing this function's shape, not just calling
`resolve_path` — left honest rather than picking an arbitrary target.
As DNS and services get built in later phases, this is where their
results should start actually feeding the remaining facts.

Also added: `Sim.provenance_for` — every log entry mentioning a given
instance, in order. Deliberately simple (log-grepping, not a
structured causal-chain graph matching the proposal's `explain(...)
-> ProvenanceGraph`) — an honest, if modest, answer to "why is this
instance in the state it's in," built entirely from facts already
recorded. The proposal's full headless API shape
(`submit_action`/`observe`/`query`/`explain` as a formal typed
surface) was not built as its own layer this phase; `Sim.perform`/
`Sim.snapshot`/`Sim.restore`/`Sim.provenance_for` cover the same ground
without the wrapper.

New tests: disconnecting a medium changes `physical_attachment` and
`overall` together, consistently, from the same underlying fact; a
DHCP lease changes `ipv4` and `overall` together the same way; writing
a derived field via `Configure` and via an authored assignment are both
rejected; `provenance_for` returns relevant entries.

**Phase 5 (done):** `profile { }` blocks are now real.
`world.profiles` registers each one (field name -> its *unevaluated*
expr, not a pre-computed value — so a field like
`wan_acquisition = random(10s .. 20s, ...)` is freshly sampled every
time it's actually referenced, not once at load time, which matters
once more than one instance shares a profile). `after
PROFILE.field -> STATE` resolves against this registry via
`eval_duration_like`, recursing if the field is itself `random(...)`.

`test/fixtures/consumer_gateway.nsdl` demonstrates the doc's own
composed-startup idea directly: a lifecycle chain
(`off → booting → lan_ready → dhcp_ready → wan_training → stabilizing →
online`) where each transition's delay comes from
`consumer_cable_gateway_startup`'s fields — not "one generic online
event." Verified against real timing: `dhcp_ready` at exactly
`bootloader + lan_activation` (2.2s), `wan_training` at
`+ dhcp_start` (3.2s), `online` only once `+ wan_acquisition +
stabilization` has fully elapsed. `DhcpDiscover` now also checks
`server_ready_for_dhcp` — a gateway still `off`/`booting` rejects a
discover outright — proving the doc's "capability gating": DHCP
succeeds once the gateway is merely past `booting` (`dhcp_ready`),
*well* before it reaches `online`, exactly the "LAN carrier before WAN
readiness" acceptance criterion.

**A real bug found and fixed along the way**: building this chain
surfaced a genuine clock-ordering bug in `advance` present since
Phase 1 — it jumped `world.clock` straight to the batch's target time
*before* firing any due events, so a chained `after ... -> STATE`
computed its new delay from the wrong (future) clock value while
firing, and drift compounded with each hop. Never surfaced earlier
because nothing before this chained multiple hops inside one `advance`
call. Fixed: `advance` now sets `world.clock` to each event's own `due`
time immediately before firing it, and loops — re-partitioning
`world.pending` each round — until nothing more is due by the target,
so a single `advance` call correctly walks through several chained
states at once (bounded at 100,000 iterations against a pathological
self-rescheduling chain). All prior tests still pass unchanged, since
the bug only manifests with multiple hops inside a single `advance`.

**Closing out the reference vertical slice:** the remaining gap was four
new `Sim` actions plus one bug fix `Ping` exposed. `PrintJob { client;
printer; content }` gates on whether `client.print_target` (if set)
matches `printer_current_address` — a stale target fails immediately with
no delivery scheduled — otherwise routes via `send_via_path` and, on
arrival, sets `label_printed`/`label_content_valid`, which is what makes
`stale_printer_target.nsdl`'s incident (which already set exactly this
field mismatch) causally do something for the first time instead of only
being an inert state overlay, and wires `clinic_printer.nsdl`'s own
`workflow patient_label { success = label_printed && label_content_valid }`
to something real. `Ping { from_inst; to_inst; via_gateway }` is a plain
LAN round trip when `via_gateway` is `None`; when it's `Some gateway`, the
actual delivery runs `from_inst <-> gateway` (there's no modeled
"internet" device — the reference slice's own inventory has none) gated
on a new `gateway_wan_ready` (lifecycle state must be `online` — the
proposal's own table names this as the point "full WAN-dependent
verification may succeed"), so a WAN-crossing ping fails at every earlier
readiness state and only `to_inst` is recorded as the (purely
descriptive) name of what was pinged. `ThreadSight`/`PacketSight` are
read-only projections, not new stored state: Thread Sight reports a
medium's own `physical_state`/`generation`/endpoints; Packet Sight reports
`resolve_path`'s current route plus the most recent matching `world.log`
entry, reusing the same `string_contains` technique `provenance_for`
already uses.

**A bug `Ping`/`PrintJob` exposed and fixed**: `resolve_path`'s BFS
walked `world.connections` with no regard for whether a medium was
currently attached. This was invisible before now because the only thing
calling `send_via_path` (`dhcp_discover`) was only ever exercised with a
disconnect *after* a delivery was already scheduled, which the existing
generation/epoch check in `advance` already catches correctly. But a
*new* send issued *after* a medium is already disconnected would still
find a route (nothing checked `physical_state`) and, since nothing
changes its generation again before it fires, would wrongly succeed — a
live delivery through a cut cable, in direct violation of the doc's own
"a disconnected cable must not coexist with successful packet delivery
through the old path." Fixed by filtering `resolve_path`'s edges to
media that are currently `medium_attached` (default `true` when a medium
has no recorded `physical_state` at all, same default
`network_status_field` already used). This is also what makes acceptance
criterion 6 ("path queries... agree") hold without extra bookkeeping —
`resolve_path`/`PacketSight` immediately report "no route" through a cut
cable rather than only discovering it once a doomed delivery fires.

Reconnect is the mirror case: `reconnect_medium` (reachable via a new
`inject reconnect on TARGET`, parallel to the existing `inject disconnect
on TARGET`) does not set `physical_state` straight back to `attached` —
it sets `training`, bumps generation/epoch (still a topology-affecting
event), and only after a fixed `link_training_delay` (3s) schedules the
actual flip. Because `medium_attached` treats anything but `attached` as
unusable, a send attempted mid-training is refused by the same
`resolve_path` filter, with no separate mechanism needed — this is the
whole proof behind "reconnect begins lawful link training... it does not
restore state instantaneously" (acceptance criterion 7).

`test/fixtures/reference_slice.nsdl` assembles the doc's own device list
— one `power_source` (included for inventory fidelity; nothing gates on
it yet), the existing `consumer_gateway` + its fidelity profile, an
`ethernet_switch`, a `clinic_client` workstation, a `label_printer`, and
exactly three `cat6_medium` instances — and `test/harness.ml` has one test
per numbered acceptance criterion (`test_criterion1_...` through
`test_criterion8_...`), quoting the doc's own criterion text, checked
against that one fixture. `workstation`/`printer`/`switch` deliberately
stay plain field-bag instances with no lifecycle object types: none of
the 8 criteria need client- or switch-level power state, and inventing
unused lifecycle for them would be exactly the kind of "merely looks
derived" fabrication this codebase's own conventions elsewhere reject.
Criterion 1 ("all devices offline at t=0") is checked the same honest way
— `gateway.state = off` (real) and `workstation`/`printer.overall <>
online` (a real projection), not a fabricated per-client power flag.
Neither `Ping`/`PrintJob`/`ThreadSight`/`PacketSight` nor `reconnect` are
wired into either TUI's command set — same scoping `DhcpDiscover` already
has (tested at the `Sim` level only).

**Phase 6 (done):** a new `world NAME { local_name = canonical_name }`
top-level construct (one new keyword, `world`; the body reuses the
same generic `stmt`/`block` grammar as everything else — no new
statement forms needed) registers a pure name-translation table:
local field/trigger name -> canonical field/trigger name. Two new
actions resolve through it — `InspectAs { world_name; instance;
local_field }` and `InvokeAs { world_name; local_trigger; target }` —
and both delegate to the *exact same* `Inspect`/`Invoke` `perform`
cases once resolved (`perform` is now `let rec` for this reason).
There is deliberately no separate storage a world binding could hold
its own opinion in — a "world" is just a lens over the one canonical
world, never a second one.

`test/fixtures/world_bindings.nsdl` declares two alternate vocabularies
over the same `communications_relay` facts — `technical` (`status`,
`wake_up`) and `merfolk` (`current_binding`, `summon_light`) — and the
new tests prove they can never disagree: both read `"off"` before
anything happens; both read `"booting"` identically after a canonical
`Invoke`; and invoking through `merfolk`'s local trigger name
(`InvokeAs`) produces the identical resulting canonical state as
invoking `power_on` directly (checked by running both from a fresh
`load_files` and comparing). An unknown world or local name errors
rather than crashing or fabricating a value.

Worth noting: `bin/tui.ml` and `bin/tui_nottui.ml` were already
unintentionally satisfying this phase's spirit before it existed —
both are alternate presentation layers that only ever call
`Sim.perform`/`Sim.advance`, never touch instance fields directly, and
so could never disagree with each other. `InspectAs`/`InvokeAs` make
that guarantee *structural and explicit* (one name-translation table,
one delegation point) rather than merely "true because neither client
happens to cheat."

Not built this phase: neither TUI has a `world`-aware command exposing
`InspectAs`/`InvokeAs` interactively (same as `DhcpDiscover` in Phase
3 — tested at the `Sim` level, not yet wired into either TUI's command
set); and the proposal's own presentation-binding example (renaming
with an attached `visual`, and a `preserve = [...]` list constraining
which fields survive translation) is more elaborate than the plain
rename table built here — this covers the causal-consistency guarantee
the exit condition asks for, not the full authoring surface.

The `v0.2`-era implementation (before this migration started) is
preserved on the `nsdlv02` branch.

## Setup

```
cd nsdl
nix develop
```

Gives you `ocaml` 5.4.1, `dune` 3.21.1, `menhir`, `ocamllex` (bundled
with `ocaml`), `findlib` (needed for dune to resolve any third-party
library — without it, only libraries bundled inside the compiler itself,
like `unix`, are visible), `notty-community`/`nottui`/`lwd` (for
`bin/tui_nottui.ml`), and `js_of_ocaml`/`js_of_ocaml-ppx`/
`js_of_ocaml-compiler`/`wasm_of_ocaml-compiler`/`binaryen` (for
`web/nsdl_web.ml`, see "WebAssembly build" below), pinned to the same
nixpkgs revision (`nixos-26.05`) as the rest of this machine's config.
Everything below assumes you're inside this shell.

If you'd rather not use the flake, any OCaml toolchain with `dune`,
`ocamllex`, and `menhir` on `PATH` works too.

## Build

```
dune build
```

You'll see:

```
Warning: 4 states have shift/reduce conflicts.
Warning: 6 shift/reduce conflicts were arbitrarily resolved.
```

This is expected — see "Known limitations" below. It's not a build
failure.

## Run

```
dune exec bin/main.exe -- path/to/file.nsdl
```

Prints the parsed `Ast.program` back out via the hand-written
pretty-printer (`lib/pretty.ml`) and a count of top-level declarations, or
a parse/lex error with byte offset and surrounding source context.

## Test

Parser tests:

```
./test/run_fixtures.sh
```

Parses every fixture in `test/fixtures/` (the proposal doc's own example
snippets, taken verbatim) and reports OK/FAIL per file. Exits non-zero if
any fixture fails.

Headless state/config tests:

```
dune exec test/harness.exe
```

Loads any number of files (via `Nsdl.Sim.load_files`) through `Nsdl.Sim`
— objects, a scenario, an incident overlay, schedule blocks, in any
combination — optionally applies a sequence of `invoke (trigger,
target)` calls and `advance` durations, then asserts on field values
via `inspect`. No Unity, no rendering, no IPC. This is the layer to keep
growing as the real network-config test suite; add new `case` entries
to `test/harness.ml` as new scenarios/incidents get written.

It also has direct unit tests (`test_*` functions, not file-based
`case`s) for exact boundaries a `.nsdl` fixture can't easily express:
the virtual clock (events don't fire before their due time, fire
exactly at the boundary and never twice, same-timestamp events fire in
stable insertion order *within* a priority class but priority overrides
insertion order *across* classes, `between ... every` expands to the
right occurrence count, `every <= 0` is rejected, `Lexer.parse_duration`
handles every malformed shape without crashing), the object/lifecycle
layer (entering a state runs its `in STATE { }` body, an unknown
`invoke` trigger errors instead of crashing, a handler's `in STATE`
guard actually gates dispatch), and snapshot/restore (replaying the
same actions from a restored snapshot reproduces identical state).

## Objects and lifecycle (lib/sim.ml)

`Sim.load_files` (used by both TUIs and the test harness) loads any
number of `.nsdl` files in two passes, so object registration never
depends on file order: every `object TYPE { }` definition across all
given files is registered first, then instances are created (the first
`scenario` found wins, every `incident` overlays, every `at`/
`between ... every` schedule block registers). `instance NAME : TYPE`
binds to a matching registered `object` — if none exists, the instance
still gets its scenario-declared fields, just no lifecycle or handlers,
same as before this layer existed.

If the bound type declares `lifecycle` states, the instance enters the
one marked `initial` immediately (running that state's `in STATE { }`
body, if any). From there:

- `transition STATE` (inside a handler or `in STATE { }` body) moves the
  instance to `STATE` immediately, and runs `STATE`'s own `in STATE { }`
  body if declared — see `communications_relay.nsdl`'s
  `in stabilizing { }`, which sets `connectivity = intermittent` and
  schedules a transition to `online` 30 seconds later.
- `after EXPR -> STATE` schedules a *delayed* transition on the same
  event queue `advance`/`live` already drive — `EXPR` can be a plain
  duration or `random(A .. B)` (sampled via unseeded `Stdlib.Random`;
  see "Known limitations").
- Bare (unqualified) field names inside a handler/on-entry body —
  `connectivity = intermittent`, not `relay.connectivity = ...` — are
  resolved relative to the instance the body is running under.
- `dhcp_discover SERVER ADDRESS LEASE` (inside a handler or `in STATE { }`
  body) fires a real DHCP Discover/Offer/Request/Ack handshake with the
  body's own instance as the client — the same handshake `DhcpDiscover`
  (the action, invocable directly) already runs, just triggered
  declaratively instead of externally. If `SERVER` isn't
  `server_ready_for_dhcp` yet, nothing fails silently and nothing is
  logged as an error that never gets resolved: the statement reschedules
  *itself* `dhcp_retry_delay` (2s) later and keeps retrying — real DHCP
  clients don't give up after one failed attempt, they keep trying until
  the network is actually up, and now this does too. `test/fixtures/
  dhcp_auto_clients.nsdl` gives `clinic_client`/`label_printer` a
  `requesting_address` lifecycle whose `in requesting_address { }` body
  is just this one statement — load it alongside a scenario using those
  types (the web playground's "Reference vertical slice" preset does)
  and the workstation/printer provision themselves the moment they're
  connected, no manual `dhcp`/`DhcpDiscover` needed. This is opt-in per
  file-set on purpose: `reference_slice.nsdl`'s own acceptance-criterion
  tests (`test/harness.ml`) still load *without* this file and still
  prove the manual-invocation path works, unchanged.

  A server also can't accidentally double-assign — `Sim.dhcp_discover`
  checks `world.dhcp_leases` (a real `(server, address) -> (client,
  expires_at)` table, included in `snapshot`/`restore` like any other
  mutable run state) before ever scheduling a handshake, and refuses a
  Discover whose address is already actively held by a *different*
  client (the same client renewing its own address is fine). This is a
  guarantee for *every* DHCP path, not just the auto-triggered one —
  `test_dhcp_prevents_double_assignment` proves it directly against the
  manual `DhcpDiscover` action.

Dispatch a handler by trigger name with the new `Invoke (trigger,
target)` action (`invoke TRIGGER TARGET` in both TUIs): it finds the
first handler on the target's object type whose `h_trigger` matches and
whose `h_in` clause (if any) matches the instance's current state, and
runs its body. `h_when` is still parsed but not evaluated. `h_at` *is*
now checked — but only by message-delivery dispatch (`dispatch_message`),
a different code path for a different kind of trigger; see "Port/message
dispatch" below. Try it against the relay fixtures:

```
dune exec bin/tui.exe -- \
  test/fixtures/communications_relay.nsdl \
  test/fixtures/relay_scenario.nsdl
```

then `inspect relay.state` (→ `off`), `invoke power_on relay` (→
`booting`, immediately), `advance 10s`, `inspect relay.state` again (→
`scanning`, once the delayed transition fires). `inspect NAME.state` is
a small special case in `Inspect` — lifecycle state isn't stored as a
regular field.

## Port/message dispatch (lib/sim.ml)

`port NAME : TYPE`, `emit EXPR through PORT`, `on TRIGGER at PORT [in
STATE] { }`, and `schedule EVENT after EXPR` have been in the grammar
since the v0.3 migration's Phase 2 — this README's own "Known
limitations" said so, unchanged, until now: *"parsed, never executed."*
The gap was entirely in the interpreter, not the language — every
construct below already parsed before this section existed; nothing
here added a keyword.

`emit MSG(key = VALUE, ...) through PORT`, executed from inside a
handler or `in STATE { }` body, resolves a destination two different
ways depending on context: if the emitting instance currently has a
`__reply_to`/`__reply_to_port` set (i.e. this emit is happening *while*
handling a just-received message), it targets that one peer's own port
directly — a reply, not a re-flood. Otherwise it's a fresh emission:
`emit_targets` floods to *every* instance reachable through that port
(over the same `medium_attached`-filtered graph `resolve_path` already
walks, generalized to enumerate everyone reachable rather than resolve
one named target) — which is, not incidentally, exactly what a real
DHCP Discover already is at the Ethernet layer. Delivery still goes
through `send_via_path` per target, so every leg is subject to the
exact same generation/epoch revalidation and disconnect-drop behavior
every other delivery in this file already has — broadcast doesn't
bypass any of that, it's N ordinary deliveries, not a new delivery
kind. Delay is scaled by hop count (`0.3 + 0.15 * hops`), so a
topologically closer recipient's message genuinely tends to arrive
first — a real, deterministic, reproducible race, not coin-flip
nondeterminism.

On arrival, `dispatch_message` writes the message's payload fields onto
the target, records `__reply_to`/`__reply_to_port`, and finds the first
handler whose trigger, **`h_at` (the port — the first thing anywhere in
this codebase to actually check it)**, and `h_in` (the same lifecycle
guard `dispatch_handler` already uses, unchanged) all match. No match —
wrong trigger, wrong port, or the instance has since moved to a state
with no matching handler — logs "dropped" and does nothing further.
That silent, structural drop *is* the entire arbitration mechanism (a
client that already accepted one offer has no `h_in`-matching handler
left for a second, later one) and the entire readiness gate (a server
not yet in its "ready" state has no matching handler for an early
discover) — this needed no new conditional-expression language feature
to build; `h_in` was already exactly that mechanism, just not put to
this use yet.

`schedule EVENT after EXPR` schedules a delayed self-dispatch (reusing
`dispatch_handler` exactly, the same function `Invoke` already uses) —
this is how a client's auto-retry loop works
(`test/fixtures/dhcp_actor_race.nsdl`'s `on retry_discover in
requesting_address { emit discover() through eth0; schedule
retry_discover after 2s }`): if the instance has since moved to a
different state, the stale retry's `h_in` guard simply fails and
nothing happens — no new "cancel this timer" mechanism was needed
either.

**`allocate_from_pool(RANGE_FIELD)`** is a specially-recognized call
form (the same precedent `random(...)` already set — one specific
shape, not a general user-definable function), usable both as an
`emit` payload value and as an ordinary assignment's RHS
(`offered_address = allocate_from_pool(dhcp.range)`, so a server can
remember what it offered for a later handler to reference). It walks a
`dhcp.range` field (a real `VRange` value now — this field already
existed, inert, in `clinic_printer.nsdl` before this) address by
address, skipping anything already an active, unexpired entry in
`world.dhcp_leases` for that server, and reserves the first free one —
the exact same table the older, still-unchanged targeted `dhcp_discover`
path's own collision guard maintains, so a pool-allocated address and a
manually-specified one can never collide either.

**A real bug this exposed and fixed**: `Sim.advance` used to drain
pending events in *rounds* — snapshot whatever's currently due, sort
that one batch, fire it; anything newly scheduled *as a side effect of
firing* only got picked up next round. Every mechanism before this one
schedules eagerly upfront (DHCP's four hops, a gateway's `after`-chained
boot sequence), so every event that would ever fire was already pending
before the first round even started — this was never a problem. A
dynamically-chained reply *is* a problem: with an independent, already
-pending, later-due timer in the same window (a client's own 2s retry,
pending from round 1, alongside a 0.9s reply that can only be scheduled
*during* round 1's firing), the retry fired at `t=2.0` before the 0.9s
reply got its turn in round 2 — `world.clock` went `0.75 → 2.0 → 0.9`,
non-monotonic. Fixed: `advance` now always fires the single globally
next-due pending event, recomputed from the *current* pending set every
time, rather than sorting one fixed batch — correct for any scheduling
pattern, dynamic or eager, and behaviorally identical to the old code
for every case that already worked (repeatedly extracting the minimum
by the same `(due, priority, seq)` comparator produces the same order a
single sort would, for events that already existed when the round
began).

**Explicitly out of scope for now**: the older, targeted `dhcp_discover`
statement / `DhcpDiscover` action (see "Objects and lifecycle" above)
is untouched — same behavior, same tests, still the way to reach one
specific named server without discovery. Migrating it, and building
`Ping`/`PrintJob`/`ThreadSight`/`PacketSight` on this same mechanism,
are separate, later steps, not folded into this one. DHCP's own deeper
protocol surface (NAK/Decline/Release, lease renewal/T1/T2 timers,
DHCPINFORM) also isn't modeled — this proves the broadcast/race/pool
-allocation properties that were actually asked for, not the entirety
of RFC 2131. And a real ordering requirement worth stating plainly
because it isn't enforced anywhere, just documented:
`Sim.load_scenario` processes a scenario's statements in file order
with no reordering pass, so an instance whose lifecycle auto-emits on
instantiation (like `dhcp_actor_client`) needs every `connect` it
depends on to already appear earlier in the same file — `.nsdl` authors
have to get this right by hand.

## TUI

```
dune exec bin/tui.exe -- test/fixtures/clinic_printer.nsdl
```

Pass any number of `.nsdl` files, in any order — typically a scenario,
optionally an incident overlay, optionally a file of `at`/`between`
schedule blocks:

```
dune exec bin/tui.exe -- \
  test/fixtures/clinic_printer.nsdl \
  test/fixtures/stale_printer_target.nsdl \
  test/fixtures/schedule_blocks.nsdl
```

Renders the loaded network (instances, a network diagram, pending
scheduled events, recent action log, and the virtual clock) and accepts
commands interactively: `inspect PATH` (also `PATH.state` for an
instance's lifecycle state), `set PATH VALUE`, `power_cycle TARGET`,
`restart_service TARGET`, `invoke TRIGGER TARGET` (see "Objects and
lifecycle" above), `advance DURATION` (e.g. `advance 2m30s`),
`live [SPEED]`, `quit`.

Any instance with an integer `ports` field (e.g.
`instance switch : ethernet_switch { ports = 8 }`) is drawn as a
rectangle with that many numbered port cells; every device connected to
one of its ports gets its own small box, with a connector line down to
the port it's plugged into and the medium noted below it:

```
+-------------------------------+
|   switch : ethernet_switch    |
+---+---+---+---+---+---+---+---+
|  1|  2|  3|  4|  5|  6|  7|  8|
+---+---+---+---+---+---+---+---+
      |           |           |
 +-----------+  +-------+  +-------+
 |workstation|  |printer|  |gateway|
 +-----------+  +-------+  +-------+
(eth0 via cat6)(eth0 via cat6)(lan via cat6)
```

This is a naming convention (`ports` as a plain int field), not
anything a type system enforces — there isn't one yet. Connections not
anchored to a rendered switch (neither endpoint matches
`<name>.port[N]`) fall back to a flat "other connections" list. A box
is pushed right instead of overlapping when it's wider than its port's
column, so it can drift from its exact port when several adjacent ports
are in use — a readability tradeoff, not a bug.

`live SPEED` switches the TUI into a real-time loop: it polls stdin
with a timeout (`Unix.select`) and, whenever idle, calls
`Sim.advance` with however much wall-clock time just passed times
`SPEED` (virtual seconds per real second; default `1.0`), then
re-renders — so the network visibly evolves on its own. It's built
entirely on top of the same deterministic `advance`, not a separate
code path, so `advance`-only and `live` sessions stay consistent with
each other. `stop`/`pause` drops back to normal one-command-at-a-time
mode; `quit`/`exit` works from either. Caveat: stdin is line-buffered,
so ticking visibly pauses while you're mid-line typing a command (it
resumes once you hit Enter) rather than dropping ticks silently.

`advance` moves the virtual clock forward and fires any event now due
on the queue — top-level `at`/`between ... every` schedule blocks, and
(since the object/lifecycle layer landed) any `after EXPR -> STATE`
delayed transition an `invoke`d handler or `in STATE { }` body
scheduled. This is how the world "lives" without you touching it. It's
driven entirely by explicit `advance` calls rather than wall-clock
time, which keeps everything deterministic and step-through-able
(matching the proposal's "Time controls" section) instead of racing a
real-time timer. What `advance` still doesn't do is fire a handler on
its own — `on TRIGGER { }` bodies only run in response to an explicit
`invoke`, never spontaneously; and it doesn't evaluate `random(...)`
deterministically yet (see "Known limitations").

## Nottui TUI (bin/tui_nottui.ml)

```
dune exec bin/tui_nottui.exe -- test/fixtures/clinic_printer.nsdl
```

A full interactive alternative to `bin/tui.ml`, built on Nottui
(`nottui`/`nottui-unix`/`lwd`) and Notty's image algebra
(`notty-community` — the maintained fork; the plain `notty` findlib
package throws a deprecation error in this nixpkgs revision) instead of
hand-rolled ANSI/`Bytes.t` rendering and a manual `Unix.select` input
loop. Same command set as `bin/tui.ml`: `inspect PATH`,
`set PATH VALUE`, `power_cycle TARGET`, `restart_service TARGET`,
`invoke TRIGGER TARGET`, `advance DURATION`, `live [SPEED]`,
`stop`/`pause`, `quit`/`exit` (Ctrl-Q and Escape also quit, courtesy of
`Nottui_unix.run`'s defaults).

What's structurally different from `bin/tui.ml`:

- **The diagram can't misalign.** Each port is one self-contained column
  image (`I.vcat` of a number box, connector, device box, annotation),
  every row `I.hsnap`-ed to that column's own max width — so the
  connector is centered on its own box by construction, not by
  computing a matching position separately (which is what caused the
  connector-drift bug fixed earlier in `bin/tui.ml`). Columns are placed
  with `I.hcat`/`<|>`, which can never overlap regardless of width, so
  there's no collision-avoidance pass to get wrong either. This part of
  Notty's composition model (`hcat`/`vcat`/`hsnap`, auto-padding on size
  mismatch) is well-suited to this row-of-columns layout specifically —
  it isn't a general graph-layout engine, and won't route a connector
  around an obstacle or handle arbitrary topology; nothing in the OCaml
  ecosystem does that out of the box.
- **Reactive, not "clear and reprint."** One `world_version : int
  Lwd.var` gets bumped after any command that mutates `Sim.world`; each
  display panel is `Lwd.map`ped from it, so touching the var is what
  causes a redraw — there's no manual full-screen clear+reprint pass.
- **`live` ticking uses `Nottui_unix.run`'s own `?tick_period`/`?tick`**
  instead of a hand-rolled `Unix.select` loop — the tick callback is a
  no-op unless `live` is active, so there's one run loop for the whole
  program's lifetime rather than nested ones for normal vs. live mode.
- **The command line doesn't block on a full line.** `bin/tui.ml`'s
  input is a blocking, line-buffered `input_line`, so `live` ticking
  visibly pauses while you're mid-line typing a command. `tui_nottui`'s
  input is `Nottui_widgets.edit_field`, driven by the same event loop as
  everything else, so ticking continues to update the screen while
  you're typing, not just between submitted lines.

Getting any of this building at all required adding `findlib` to the
flake devShell — without it, `OCAMLPATH` never gets populated by the
other packages' setup hooks, so `dune build` couldn't see *any*
third-party library, not just notty/nottui.

Verifying this by hand isn't as simple as piping stdin the way
`bin/tui.ml` allows: it's a real raw-mode terminal app, so testing it
non-interactively means driving it through an actual pseudo-terminal
(e.g. Python's `pty` module), not just piping lines into stdin.

## WebAssembly build (web/nsdl_web.ml)

```
dune build web
```

Compiles `lib/` (the parser and `Sim` — no `Unix` dependency, unlike the
TUIs) to WebAssembly via `wasm_of_ocaml` (dune's `(modes wasm)`), with a
thin JS API layer (`web/nsdl_web.ml`) on top, not a reimplementation of
anything: `Js.export "Nsdl" { loadSources }` takes a JS array of `{name,
content}` source pairs (there's no filesystem in a browser, so this goes
through the new `Sim.load_sources`/`Sim.parse_source` rather than
`Sim.load_files`) and returns a world handle exposing `inspect(path)`,
`configure(path, value)`, `invoke(trigger, target)`, and
`advance(seconds)` — the same vocabulary `bin/tui.ml`'s commands already
use, not a different one invented for the browser. Each call returns
`{kind, text}` (`kind` is `"value"`, `"ack"`, or `"error"`, mirroring
`Sim.observation`'s three constructors) rather than a bare string, so a
real caller can branch on outcome without string-sniffing.

Produces `_build/default/web/nsdl_web.bc.wasm.js` (a self-instantiating
loader — load it via `<script src="...">` in a browser, or `require()` it
under Node) plus a sibling `nsdl_web.bc.wasm.assets/` directory of `.wasm`
modules it fetches relative to itself at load time; both must ship
together. `globalThis.Nsdl` becomes available once the loader's internal
async instantiation finishes — there's no exported promise/callback to
await that completion from outside, so a caller needs to poll for
`globalThis.Nsdl` (or, in a browser, simply run code after the script has
had a moment to initialize) rather than assuming it's ready synchronously
after the script tag or `require()` call returns.

**A real trap this surfaced**: unlike js_of_ocaml's JS backend,
`wasm_of_ocaml` does not map OCaml `float` onto JS numbers automatically
— a method parameter typed as plain `float` silently receives the wrong
bit pattern and corrupts the next float operation it touches (`Sim.advance`
raised a wasm `RuntimeError: illegal cast`, not an OCaml exception, so it
doesn't fail cleanly). `advance`'s parameter is typed `Js.number Js.t` and
converted explicitly via `Js.to_float`, which is the documented fix
(`wasm_of_ocaml`'s own README: *"explicit conversions `Js.to_float` and
`Js.float` are now necessary"*). Caught by an actual Node smoke test
against `test/fixtures/relay_scenario.nsdl` (load, inspect, invoke,
advance, inspect again) before this was trusted — not just "it compiled."

Setup needs a few more devShell packages beyond the native toolchain:
`js_of_ocaml`/`js_of_ocaml-ppx` (the runtime library `nsdl_web.ml` is
written against and its `object%js`/`Js.export` ppx),
`js_of_ocaml-compiler`/`wasm_of_ocaml-compiler` (dune's `(modes wasm)`
integration shells out to `wasm_of_ocaml`, which shares tooling with the
JS-targeting compiler), and `binaryen` (`wasm_of_ocaml` shells out to its
`wasm-opt` — without it on `PATH`, the build fails with `wasm-opt: command
not found`, not a clearer error). All pinned to the same `nixos-26.05`
nixpkgs revision as everything else.

Not built: neither TUI has any wasm-related change (`web/` is a wholly
separate consumer of `lib/`), and there's no bundled HTML page/playground
yet — `web/nsdl_web.ml`'s JS API is there to be embedded by one, not a
complete UI itself.

## Layout

```
dune-project
flake.nix              -- nix develop shell (ocaml + dune + menhir)
bin/main.ml            -- CLI entry point
lib/ast.ml              -- AST types
lib/lexer.mll           -- ocamllex tokenizer
lib/parser.mly          -- menhir grammar
lib/pretty.ml           -- structural printer (no ppx/sexp dependency)
lib/sim.ml              -- simulation model: objects, lifecycle, clock, actions
bin/tui.ml              -- interactive terminal renderer over Sim
bin/tui_nottui.ml       -- full interactive TUI built on Nottui/Notty
test/fixtures/*.nsdl    -- example programs from the spec (+ relay_scenario.nsdl,
                           a minimal scenario instantiating communications_relay)
test/run_fixtures.sh    -- parses every fixture, reports pass/fail
test/harness.ml         -- headless config tests against Sim
web/nsdl_web.ml         -- JS API over Sim, compiled to WebAssembly
.github/workflows/release.yml -- builds + tests, then publishes the wasm build
```

Every brace-delimited body in the language — `object`, `scenario`,
`incident`, `workflow`, handler bodies, `in STATE { }` lifecycle blocks,
`at`/`between` schedule blocks — is parsed into the *same* `stmt list`
(see `lib/ast.ml`). The surface forms look different; the grammar treats
them uniformly, which is most of what keeps `parser.mly` small.

## Known limitations

- **Parser only.** No semantic passes yet — that's the intended next
  layer (name resolution → type checking → topology/lifecycle validation
  → IR), per the proposal's own compilation pipeline.
- **`memory` persistence marker is greedy.** `memory q : queue<job> volatile`
  followed immediately by a bare-identifier statement (no blank line, no
  leading keyword) can have that identifier swallowed as the persistence
  marker instead of starting the next statement. This is one of the
  reported shift/reduce conflicts; it fails loudly (parse error) rather
  than silently, and doesn't affect any of the five fixtures.
- **Only three keywords double as field names**: `port`, `workflow`,
  and `state` (needed for `switch.port[2]`, `workflow.patient_label`,
  and `relay.state` in the specs' own examples). Any other keyword used
  as an identifier will currently fail to parse — extend the `name`
  rule in `parser.mly` if you hit one.
- **`Sim` still isn't the full v0.3 runtime.** This bullet predates most
  of the "v0.3 migration" phases above and much of it is now stale by
  their own account (canonical/derived state, topology epochs, port/
  message dispatch, and `h_at` are all real now — see the phase table
  and "Port/message dispatch" above) — kept narrowly accurate rather
  than rewritten wholesale: there's still no workflow `success`
  evaluation, and `h_when` (unlike `h_at`) is still parsed but never
  checked. `power_cycle`/`restart_service` remain logged no-ops for a
  different reason now — they're not tied to any `on TRIGGER`/`on
  receive` name a scenario could declare, so there's nothing for them to
  dispatch to even with ports wired up; use `invoke TRIGGER TARGET` for
  a handler that actually runs ("power_on" for `communications_relay`,
  not "power_cycle"; neither proposal specifies a canonical-action-to
  -trigger-name mapping, so this codebase doesn't invent one).
- **`random(A .. B)` is not deterministic or replayable yet.** It's an
  unseeded `Stdlib.Random.float` call. Both proposals want named
  streams derived from the scenario's `seed`, specifically so replay
  reproduces identical event ordering and sampled values — see the
  `test_snapshot_restore_replay` note above for why Phase 1's
  determinism claim doesn't depend on this being fixed first.

## Next up

All 6 phases of the v0.3 migration (see the table above) are now done —
the runtime matches the executable semantic core's own reference vertical
slice, all 8 acceptance criteria included. The phase table no longer names
what's next; the honest candidates are the gaps "Known limitations" above
already flags. Most concrete: seeded/deterministic randomness — named
streams derived from a scenario's `seed`, so a run's exact event ordering
and sampled values are reproducible from the seed alone, not only from a
snapshot taken after the sampling already happened (which is what
`test_criterion8_deterministic_replay` and `test_snapshot_restore_replay`
both lean on today, honestly, rather than claiming more than that). After
that: the deferred semantic-pass layer (name resolution → type checking →
topology/lifecycle validation → IR) this README's own intro paragraph has
flagged since before the v0.3 migration began, and wiring the newer `Sim`
actions (`DhcpDiscover`, `Ping`, `PrintJob`, `ThreadSight`, `PacketSight`,
`inject reconnect`) into at least one TUI's command set, since none of
them are reachable outside the test harness today.
