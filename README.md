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
action. There's still no port/message passing, no workflow success
evaluation, no deterministic/seeded randomness, and no name resolution,
type checking, or IR lowering — see "Known limitations" and "Next up"
below for exactly where the line is now.

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
| 2 | Ports, media, lifecycle, epochs/generations | Disconnect/power tests pass without ghost deliveries | not started |
| 3 | Switch forwarding and DHCP message flow | Lease causality tests pass | not started |
| 4 | Observations and provenance | Projection consistency tests pass | not started |
| 5 | Gateway fidelity profile + print workflow | Reference vertical slice passes end-to-end | not started |
| 6 | World/embodiment bindings | Alternate clients preserve canonical outcomes | not started |

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

The `v0.2`-era implementation (before this migration started) is
preserved on the `nsdlv02` branch.

The pre-existing grammar (`lib/parser.mly`) already covers most of what
v0.2 needed; v0.3 introduces new surface forms (`state { }` blocks
inside `object` defs distinct from `lifecycle`, `emits`/`receives`
declarations, `endpoints: exactly<N, T>` arity constraints, `capability`
declarations, top-level `profile { }` blocks, and a looser `inject`
shape that doesn't always take a bare amount expression) that phases 2
and 5 will need to add to the grammar — not done yet, tracked for when
those phases start.

## Setup

```
cd nsdl
nix develop
```

Gives you `ocaml` 5.4.1, `dune` 3.21.1, `menhir`, `ocamllex` (bundled
with `ocaml`), `findlib` (needed for dune to resolve any third-party
library — without it, only libraries bundled inside the compiler itself,
like `unix`, are visible), and `notty-community`/`nottui`/`lwd` (for
`bin/tui_nottui.ml`), pinned to the same nixpkgs revision (`nixos-26.05`)
as the rest of this machine's config. Everything below assumes you're
inside this shell.

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

Dispatch a handler by trigger name with the new `Invoke (trigger,
target)` action (`invoke TRIGGER TARGET` in both TUIs): it finds the
first handler on the target's object type whose `h_trigger` matches and
whose `h_in` clause (if any) matches the instance's current state, and
runs its body. `h_at`/`h_when` guards are parsed but not evaluated yet
— they need the port/message layer below. Try it against the relay
fixtures:

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
- **Only two keywords double as field names**: `port` and `workflow`
  (needed for `switch.port[2]`, `workflow.patient_label` in the spec's
  own examples). Any other keyword used as an identifier will currently
  fail to parse — extend the `name` rule in `parser.mly` if you hit one.
- **`Sim` still isn't the full v0.3 runtime.** Objects, lifecycle
  states, `transition`/`after ... -> STATE`, priority-ordered same
  -timestamp events, and snapshot/restore are real now (Phase 1, done)
  — but there's no canonical/derived state distinction yet (any field
  can still be written directly), no port/message passing (`emit ...
  through PORT`, `on receive(...) at PORT` are parsed, never executed),
  no topology epochs/generations or delivery revalidation, no workflow
  `success` evaluation, and a handler's `h_at`/`h_when` guards are
  never checked (only `h_in`, the lifecycle-state guard, is). Since
  ports aren't wired, `power_cycle`/`restart_service` remain logged
  no-ops — use `invoke TRIGGER TARGET` for a handler that actually runs
  ("power_on" for `communications_relay`, not "power_cycle"; neither
  proposal specifies a canonical-action-to-trigger-name mapping, so
  this codebase doesn't invent one).
- **`random(A .. B)` is not deterministic or replayable yet.** It's an
  unseeded `Stdlib.Random.float` call. Both proposals want named
  streams derived from the scenario's `seed`, specifically so replay
  reproduces identical event ordering and sampled values — see the
  `test_snapshot_restore_replay` note above for why Phase 1's
  determinism claim doesn't depend on this being fixed first.

## Next up

See "v0.3 migration" above for the phase table and what each phase
needs — Phase 2 (ports, media, lifecycle, epochs/generations) is next.
That phase starts with the grammar extensions listed at the end of
that section (`state { }`, `emits`/`receives`, `endpoints:`,
`capability`, and a looser `inject` shape), since nothing in Phase 2
can be authored in a `.nsdl` file until the parser accepts it.
