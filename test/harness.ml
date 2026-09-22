(* Headless test harness: loads any number of .nsdl files (objects,
   scenario, incident overlays, schedule blocks, in any order --
   Sim.load_files handles the two-pass object-registration-before
   -instantiation ordering) through Nsdl.Sim and asserts on the
   resulting field values -- no Unity, no rendering, no IPC. This is the
   layer described in the proposal's "Testing model": healthy baseline
   tests, incident tests, and (for the `advance` clock/event queue and
   the object/lifecycle/handler layer) direct unit tests where
   file-based fixtures can't easily express the exact boundary being
   checked. *)

(* ------------------------------------------------------------------ *)
(* File-based cases: files loaded, an optional sequence of `invoke`    *)
(* (trigger, target) calls and `advance` durations applied in order,   *)
(* then assertions checked via `inspect`.                              *)
(* ------------------------------------------------------------------ *)

type expectation =
  | ExpectValue of string * Nsdl.Sim.value
  | ExpectError of string (* path must NOT resolve -- e.g. "hasn't fired yet" *)

type case = {
  name : string;
  files : string list;
  invokes : (string * string) list; (* (trigger, target) applied in order via Sim.Invoke *)
  advances : string list; (* durations applied in order via Sim.advance *)
  expectations : expectation list;
}

let build_world c =
  let world = Nsdl.Sim.load_files c.files in
  List.iter
    (fun (trigger, target) -> ignore (Nsdl.Sim.perform world (Nsdl.Sim.Invoke (trigger, target))))
    c.invokes;
  List.iter (fun dur -> Nsdl.Sim.advance world (Nsdl.Lexer.parse_duration dur)) c.advances;
  world

let check_expectation world = function
  | ExpectValue (path, expect) -> (
    match Nsdl.Sim.perform world (Nsdl.Sim.Inspect path) with
    | Nsdl.Sim.OValue v when Nsdl.Sim.value_equal v expect -> None
    | Nsdl.Sim.OValue v ->
      Some
        (Printf.sprintf "  %s: expected %s, got %s" path (Nsdl.Sim.value_to_string expect)
           (Nsdl.Sim.value_to_string v))
    | Nsdl.Sim.OError msg -> Some (Printf.sprintf "  %s: expected a value, got error: %s" path msg)
    | Nsdl.Sim.OAck _ -> Some (Printf.sprintf "  %s: unexpected ack" path))
  | ExpectError path -> (
    match Nsdl.Sim.perform world (Nsdl.Sim.Inspect path) with
    | Nsdl.Sim.OError _ -> None
    | Nsdl.Sim.OValue v ->
      Some
        (Printf.sprintf "  %s: expected no value yet, got %s" path (Nsdl.Sim.value_to_string v))
    | Nsdl.Sim.OAck _ -> Some (Printf.sprintf "  %s: unexpected ack" path))

let run_case c =
  let world = build_world c in
  let failures = List.filter_map (check_expectation world) c.expectations in
  match failures with
  | [] ->
    Printf.printf "OK   %s\n" c.name;
    true
  | fs ->
    Printf.printf "FAIL %s\n%s\n" c.name (String.concat "\n" fs);
    false

let cases =
  [
    {
      name = "clinic_printer: healthy baseline matches scenario declaration";
      files = [ "test/fixtures/clinic_printer.nsdl" ];
      invokes = [];
      advances = [];
      expectations =
        [
          ExpectValue ("gateway.lan.address", Nsdl.Sim.VIpAddr "192.168.20.1/24");
          ExpectValue ("workstation.address", Nsdl.Sim.VIdent "dhcp");
          ExpectValue ("switch.ports", Nsdl.Sim.VInt 8);
        ];
    };
    {
      name = "stale_printer_target: incident overrides only the fields it sets";
      files = [ "test/fixtures/clinic_printer.nsdl"; "test/fixtures/stale_printer_target.nsdl" ];
      invokes = [];
      advances = [];
      expectations =
        [
          ExpectValue ("workstation.print_target", Nsdl.Sim.VIpAddr "192.168.20.43");
          ExpectValue ("printer.preferred_address", Nsdl.Sim.VIpAddr "192.168.20.71");
          (* acceptance criterion from the proposal: the overlay changes
             root cause without disturbing the rest of the baseline *)
          ExpectValue ("gateway.lan.address", Nsdl.Sim.VIpAddr "192.168.20.1/24");
          ExpectValue ("workstation.address", Nsdl.Sim.VIdent "dhcp");
        ];
    };
    {
      name = "advance: nothing fires before the between-window opens (1m < 4m)";
      files = [ "test/fixtures/clinic_printer.nsdl"; "test/fixtures/schedule_blocks.nsdl" ];
      invokes = [];
      advances = [ "1m" ];
      expectations = [ ExpectError "clinic.uplink.loss" ];
    };
    {
      name = "advance: a single 6m jump fires every between-window occurrence";
      files = [ "test/fixtures/clinic_printer.nsdl"; "test/fixtures/schedule_blocks.nsdl" ];
      invokes = [];
      advances = [ "6m" ];
      expectations = [ ExpectValue ("clinic.uplink.loss", Nsdl.Sim.VFloat 0.35) ];
    };
    {
      name = "advance: three cumulative 2m steps reach the same state as one 6m jump";
      files = [ "test/fixtures/clinic_printer.nsdl"; "test/fixtures/schedule_blocks.nsdl" ];
      invokes = [];
      advances = [ "2m"; "2m"; "2m" ];
      expectations = [ ExpectValue ("clinic.uplink.loss", Nsdl.Sim.VFloat 0.35) ];
    };
    {
      name = "relay: instantiation binds object type and enters its initial lifecycle state";
      files = [ "test/fixtures/communications_relay.nsdl"; "test/fixtures/relay_scenario.nsdl" ];
      invokes = [];
      advances = [];
      expectations = [ ExpectValue ("relay.state", Nsdl.Sim.VIdent "off") ];
    };
    {
      name = "relay: invoking power_on in state off transitions immediately, then again after \
              the scheduled delay";
      files = [ "test/fixtures/communications_relay.nsdl"; "test/fixtures/relay_scenario.nsdl" ];
      invokes = [ ("power_on", "relay") ];
      (* handler body: transition booting (immediate); after random(4s..8s) -> scanning
         (delayed) -- 10s comfortably clears the top of that range *)
      advances = [ "10s" ];
      expectations = [ ExpectValue ("relay.state", Nsdl.Sim.VIdent "scanning") ];
    };
  ]

(* ------------------------------------------------------------------ *)
(* Direct unit tests: constructed in OCaml rather than parsed from a   *)
(* file, so exact clock boundaries and tie-breaking can be pinned down *)
(* precisely instead of hoping a fixture happens to land on them.      *)
(* ------------------------------------------------------------------ *)

let ok name = Printf.printf "OK   %s\n" name; true
let fail name detail = Printf.printf "FAIL %s\n  %s\n" name detail; false

let field name = Nsdl.Ast.EField (Nsdl.Ast.EIdent "x", name)

let test_parse_duration () =
  let name = "Lexer.parse_duration: valid and invalid inputs" in
  let cases =
    [
      ("30s", Some 30.0);
      ("2m30s", Some 150.0);
      ("4m", Some 240.0);
      ("1h", Some 3600.0);
      ("1h2m3s", Some 3723.0);
      ("", Some 0.0);
      ("30", None); (* digits with no unit *)
      ("abc", None); (* no digits at all *)
      ("30x", None); (* bad unit letter *)
      ("30s5", None); (* trailing digits with no unit *)
      ("0.2s", Some 0.2); (* fractional -- needed for v0.3 fidelity profiles *)
      ("2.8s", Some 2.8);
      ("2m0.5s", Some 120.5);
      ("0.s", None); (* '.' with no digits after it *)
    ]
  in
  let failures =
    List.filter_map
      (fun (input, expected) ->
        let actual =
          try Some (Nsdl.Lexer.parse_duration input) with Nsdl.Lexer.Lex_error _ -> None
        in
        let matches =
          match (expected, actual) with
          | None, None -> true
          | Some e, Some a -> Float.equal e a
          | _ -> false
        in
        if matches then None
        else
          Some
            (Printf.sprintf "parse_duration %S: expected %s, got %s" input
               (match expected with None -> "an error" | Some f -> string_of_float f)
               (match actual with None -> "an error" | Some f -> string_of_float f)))
      cases
  in
  match failures with [] -> ok name | fs -> fail name (String.concat "\n  " fs)

let test_advance_does_not_fire_early () =
  let name = "advance: an event due later stays pending and doesn't mutate state" in
  let world = Nsdl.Sim.create () in
  Nsdl.Sim.schedule_at world ~self:None ~priority:Nsdl.Sim.priority_physical ~delivery:None 100.0 "test" [ Nsdl.Ast.SAssign (field "flag", Nsdl.Ast.EInt 1) ];
  Nsdl.Sim.advance world 50.0;
  let not_fired =
    match Nsdl.Sim.perform world (Nsdl.Sim.Inspect "x.flag") with
    | Nsdl.Sim.OError _ -> true
    | _ -> false
  in
  let still_pending = List.length world.Nsdl.Sim.pending = 1 in
  if not_fired && still_pending then ok name
  else
    fail name
      (Printf.sprintf "fired_early=%b pending=%d" (not not_fired)
         (List.length world.Nsdl.Sim.pending))

let test_advance_fires_at_boundary_and_not_twice () =
  let name = "advance: fires exactly at its due time, then never again" in
  let world = Nsdl.Sim.create () in
  Nsdl.Sim.schedule_at world ~self:None ~priority:Nsdl.Sim.priority_physical ~delivery:None 100.0 "test" [ Nsdl.Ast.SAssign (field "flag", Nsdl.Ast.EInt 1) ];
  Nsdl.Sim.advance world 100.0;
  let fired_at_boundary =
    match Nsdl.Sim.perform world (Nsdl.Sim.Inspect "x.flag") with
    | Nsdl.Sim.OValue (Nsdl.Sim.VInt 1) -> true
    | _ -> false
  in
  let drained = world.Nsdl.Sim.pending = [] in
  (* set to a different value directly to prove a second advance doesn't
     re-fire the (already-consumed) event and stomp it back to 1 *)
  Nsdl.Sim.set_field world "x" "flag" (Nsdl.Sim.VInt 2);
  Nsdl.Sim.advance world 1000.0;
  let stayed_2 =
    match Nsdl.Sim.perform world (Nsdl.Sim.Inspect "x.flag") with
    | Nsdl.Sim.OValue (Nsdl.Sim.VInt 2) -> true
    | _ -> false
  in
  if fired_at_boundary && drained && stayed_2 then ok name
  else
    fail name
      (Printf.sprintf "fired_at_boundary=%b drained=%b stayed_2=%b" fired_at_boundary drained
         stayed_2)

let test_same_timestamp_stable_order () =
  let name = "advance: same-timestamp events fire in insertion order" in
  let world = Nsdl.Sim.create () in
  Nsdl.Sim.schedule_at world ~self:None ~priority:Nsdl.Sim.priority_physical ~delivery:None 50.0 "first" [ Nsdl.Ast.SAssign (field "order", Nsdl.Ast.EString "first") ];
  Nsdl.Sim.schedule_at world ~self:None ~priority:Nsdl.Sim.priority_physical ~delivery:None 50.0 "second"
    [ Nsdl.Ast.SAssign (field "order", Nsdl.Ast.EString "second") ];
  Nsdl.Sim.advance world 50.0;
  match Nsdl.Sim.perform world (Nsdl.Sim.Inspect "x.order") with
  | Nsdl.Sim.OValue (Nsdl.Sim.VString "second") -> ok name
  | Nsdl.Sim.OValue v -> fail name (Printf.sprintf "final value was %s" (Nsdl.Sim.value_to_string v))
  | _ -> fail name "inspect returned no value"

let test_priority_beats_insertion_order () =
  let name = "advance: priority class overrides insertion order at the same timestamp" in
  let world = Nsdl.Sim.create () in
  (* Inserted first but in a LATE priority class (observation) -- if
     priority weren't respected, insertion order alone would fire this
     before the physical-class event below and "B" would win instead
     of "A". *)
  Nsdl.Sim.schedule_at world ~self:None ~priority:Nsdl.Sim.priority_observation ~delivery:None
    50.0 "late-class"
    [ Nsdl.Ast.SAssign (field "order", Nsdl.Ast.EString "A") ];
  (* Inserted second but in the EARLY priority class (physical); must
     fire first despite that, so its write gets overwritten by the
     first event's -- final value should be "A". *)
  Nsdl.Sim.schedule_at world ~self:None ~priority:Nsdl.Sim.priority_physical ~delivery:None 50.0 "early-class"
    [ Nsdl.Ast.SAssign (field "order", Nsdl.Ast.EString "B") ];
  Nsdl.Sim.advance world 50.0;
  match Nsdl.Sim.perform world (Nsdl.Sim.Inspect "x.order") with
  | Nsdl.Sim.OValue (Nsdl.Sim.VString "A") -> ok name
  | Nsdl.Sim.OValue v ->
    fail name (Printf.sprintf "final value was %s, expected \"A\"" (Nsdl.Sim.value_to_string v))
  | _ -> fail name "inspect returned no value"

let test_between_expands_correct_count () =
  let name = "load_schedule: `between A..B every E` generates exactly the occurrences in range" in
  let world = Nsdl.Sim.create () in
  Nsdl.Sim.load_schedule world
    (Nsdl.Ast.TBetween
       (Nsdl.Ast.EInt 10, Nsdl.Ast.EInt 30, Nsdl.Ast.EInt 10, [ Nsdl.Ast.STransition "noop" ]));
  (* 10, 20, 30 *)
  let n = List.length world.Nsdl.Sim.pending in
  if n = 3 then ok name else fail name (Printf.sprintf "expected 3 pending events, got %d" n)

let test_between_rejects_nonpositive_every () =
  let name = "load_schedule: `every <= 0` is rejected instead of looping forever" in
  let world = Nsdl.Sim.create () in
  match
    Nsdl.Sim.load_schedule world
      (Nsdl.Ast.TBetween (Nsdl.Ast.EInt 0, Nsdl.Ast.EInt 10, Nsdl.Ast.EInt 0, []))
  with
  | () -> fail name "expected Invalid_argument, load_schedule returned normally"
  | exception Invalid_argument _ -> ok name

let relay_files =
  [ "test/fixtures/communications_relay.nsdl"; "test/fixtures/relay_scenario.nsdl" ]

let test_on_enter_runs_state_body () =
  let name = "transition_to: entering a state runs its `in STATE { }` body" in
  let world = Nsdl.Sim.load_files relay_files in
  Nsdl.Sim.transition_to world "relay" "stabilizing";
  let connectivity_set =
    match Nsdl.Sim.perform world (Nsdl.Sim.Inspect "relay.connectivity") with
    | Nsdl.Sim.OValue (Nsdl.Sim.VIdent "intermittent") -> true
    | _ -> false
  in
  let scheduled_to_online = List.length world.Nsdl.Sim.pending = 1 in
  Nsdl.Sim.advance world 30.0;
  let reached_online =
    match Nsdl.Sim.perform world (Nsdl.Sim.Inspect "relay.state") with
    | Nsdl.Sim.OValue (Nsdl.Sim.VIdent "online") -> true
    | _ -> false
  in
  if connectivity_set && scheduled_to_online && reached_online then ok name
  else
    fail name
      (Printf.sprintf "connectivity_set=%b scheduled_to_online=%b reached_online=%b"
         connectivity_set scheduled_to_online reached_online)

let observation_to_string = function
  | Nsdl.Sim.OValue v -> "OValue " ^ Nsdl.Sim.value_to_string v
  | Nsdl.Sim.OAck m -> "OAck " ^ m
  | Nsdl.Sim.OError m -> "OError " ^ m

let test_invoke_no_matching_handler () =
  let name = "invoke: an unknown trigger returns an error, not a crash" in
  let world = Nsdl.Sim.load_files relay_files in
  match Nsdl.Sim.perform world (Nsdl.Sim.Invoke ("does_not_exist", "relay")) with
  | Nsdl.Sim.OError _ -> ok name
  | o -> fail name (Printf.sprintf "expected OError, got %s" (observation_to_string o))

let test_invoke_state_guard () =
  let name = "invoke: a handler's `in STATE` guard prevents firing outside that state" in
  let world = Nsdl.Sim.load_files relay_files in
  let first = Nsdl.Sim.perform world (Nsdl.Sim.Invoke ("power_on", "relay")) in
  let second = Nsdl.Sim.perform world (Nsdl.Sim.Invoke ("power_on", "relay")) in
  match (first, second) with
  | Nsdl.Sim.OAck _, Nsdl.Sim.OError _ -> ok name
  | _ ->
    fail name
      (Printf.sprintf
         "expected the first invoke (state=off) to succeed and the second (state=booting) to \
          fail; got %s then %s"
         (observation_to_string first) (observation_to_string second))

let test_snapshot_restore_replay () =
  let name =
    "snapshot/restore: replaying the same actions from a restored snapshot reproduces identical \
     state"
  in
  let world = Nsdl.Sim.load_files relay_files in
  ignore (Nsdl.Sim.perform world (Nsdl.Sim.Invoke ("power_on", "relay")));
  (* random(4s..8s) has already been sampled and baked into the pending
     event's due time here, so replaying from this snapshot is
     deterministic even though Random itself isn't seeded yet -- this
     is the v0.3 proposal's Phase 1 exit condition: "deterministic
     replay of state-only fixtures". *)
  let snap = Nsdl.Sim.snapshot world in
  Nsdl.Sim.advance world 10.0;
  let state_a =
    match Nsdl.Sim.perform world (Nsdl.Sim.Inspect "relay.state") with
    | Nsdl.Sim.OValue v -> Nsdl.Sim.value_to_string v
    | _ -> "<none>"
  in
  let world2 = Nsdl.Sim.restore snap in
  Nsdl.Sim.advance world2 10.0;
  let state_b =
    match Nsdl.Sim.perform world2 (Nsdl.Sim.Inspect "relay.state") with
    | Nsdl.Sim.OValue v -> Nsdl.Sim.value_to_string v
    | _ -> "<none>"
  in
  if state_a = state_b && state_a = "scanning" then ok name
  else fail name (Printf.sprintf "state_a=%s state_b=%s, expected both \"scanning\"" state_a state_b)

(* ------------------------------------------------------------------ *)
(* Phase 2 (ports, media, lifecycle, epochs/generations). Exit         *)
(* condition per the v0.3 doc: "disconnect/power tests pass without    *)
(* ghost deliveries".                                                  *)
(* ------------------------------------------------------------------ *)

let medium_files =
  [ "test/fixtures/ethernet_port_cat6_medium.nsdl"; "test/fixtures/medium_scenario.nsdl" ]

let test_disconnect_bumps_generation_and_epoch () =
  let name = "disconnect_medium: bumps the medium's generation and the world's topology_epoch" in
  let world = Nsdl.Sim.load_files medium_files in
  let gen0 = Nsdl.Sim.generation_of world "link" in
  let epoch0 = world.Nsdl.Sim.topology_epoch in
  Nsdl.Sim.disconnect_medium world "link";
  let gen1 = Nsdl.Sim.generation_of world "link" in
  let epoch1 = world.Nsdl.Sim.topology_epoch in
  if gen1 = gen0 + 1 && epoch1 = epoch0 + 1 then ok name
  else fail name (Printf.sprintf "gen0=%d gen1=%d epoch0=%d epoch1=%d" gen0 gen1 epoch0 epoch1)

let test_delivery_succeeds_without_disconnect () =
  let name =
    "advance: a delivery with no intervening disconnect succeeds (positive control for the \
     ghost-delivery test below)"
  in
  let world = Nsdl.Sim.load_files medium_files in
  Nsdl.Sim.send_via_medium world ~medium:"link" ~self:None ~priority:Nsdl.Sim.priority_protocol
    ~delay:10.0 "test-delivery" [ Nsdl.Ast.SAssign (field "delivered", Nsdl.Ast.EInt 1) ];
  Nsdl.Sim.advance world 10.0;
  match Nsdl.Sim.perform world (Nsdl.Sim.Inspect "x.delivered") with
  | Nsdl.Sim.OValue (Nsdl.Sim.VInt 1) -> ok name
  | o -> fail name (Printf.sprintf "expected the delivery to succeed, got %s" (observation_to_string o))

let test_no_ghost_delivery_after_disconnect () =
  let name =
    "advance: disconnecting a medium before an in-flight delivery's due time drops it instead \
     of delivering (no ghost packets)"
  in
  let world = Nsdl.Sim.load_files medium_files in
  Nsdl.Sim.send_via_medium world ~medium:"link" ~self:None ~priority:Nsdl.Sim.priority_protocol
    ~delay:10.0 "test-delivery" [ Nsdl.Ast.SAssign (field "delivered", Nsdl.Ast.EInt 1) ];
  Nsdl.Sim.disconnect_medium world "link";
  (* still at clock=0, well before the delivery's due time of 10 *)
  Nsdl.Sim.advance world 10.0;
  match Nsdl.Sim.perform world (Nsdl.Sim.Inspect "x.delivered") with
  | Nsdl.Sim.OError _ -> ok name
  | o ->
    fail name
      (Printf.sprintf "expected the delivery to be dropped, but got %s" (observation_to_string o))

let test_inject_no_args_sets_bool_marker () =
  let name = "SInject: `inject KIND on TARGET` with no args sets TARGET.KIND = true" in
  let world = Nsdl.Sim.create () in
  Nsdl.Sim.exec_stmt world ~self:None (Nsdl.Ast.SInject ("power_loss", [], Nsdl.Ast.EIdent "gateway"));
  match Nsdl.Sim.perform world (Nsdl.Sim.Inspect "gateway.power_loss") with
  | Nsdl.Sim.OValue (Nsdl.Sim.VBool true) -> ok name
  | o -> fail name (Printf.sprintf "expected VBool true, got %s" (observation_to_string o))

let test_inject_named_arg_sets_value () =
  let name = "SInject: `inject KIND(name = value) on TARGET` sets TARGET.KIND to that value" in
  let world = Nsdl.Sim.create () in
  Nsdl.Sim.exec_stmt world ~self:None
    (Nsdl.Ast.SInject
       ( "impairment",
         [ Nsdl.Ast.ANamed ("loss", Nsdl.Ast.EFloat 0.35) ],
         Nsdl.Ast.EField (Nsdl.Ast.EIdent "clinic", "uplink") ));
  match Nsdl.Sim.perform world (Nsdl.Sim.Inspect "clinic.uplink.impairment") with
  | Nsdl.Sim.OValue (Nsdl.Sim.VFloat f) when Float.equal f 0.35 -> ok name
  | o -> fail name (Printf.sprintf "expected VFloat 0.35, got %s" (observation_to_string o))

(* ------------------------------------------------------------------ *)
(* Phase 3 (switch forwarding and DHCP message flow). Exit condition   *)
(* per the v0.3 doc: "lease causality tests pass" -- directly the      *)
(* proposal's own "DHCP causality property" conformance bullets.       *)
(* ------------------------------------------------------------------ *)

let dhcp_files = [ "test/fixtures/dhcp_scenario.nsdl" ]

let discover world =
  ignore
    (Nsdl.Sim.perform world
       (Nsdl.Sim.DhcpDiscover
          {
            client = "client";
            server = "gateway";
            medium = "link";
            address = "192.168.20.71";
            lease_seconds = 3600.0;
          }))

let test_dhcp_no_auto_lease_from_bare_field () =
  let name =
    "dhcp: `address = dhcp` in a scenario alone (no DhcpDiscover invoked) does not install a \
     lease -- \"a down server cannot assign a lease merely because scenario syntax requests \
     it\""
  in
  let world = Nsdl.Sim.load_files dhcp_files in
  match Nsdl.Sim.perform world (Nsdl.Sim.Inspect "client.dhcp_address") with
  | Nsdl.Sim.OError _ -> ok name
  | o -> fail name (Printf.sprintf "expected no lease yet, got %s" (observation_to_string o))

let test_dhcp_full_handshake_installs_lease () =
  let name =
    "dhcp: a full, undisturbed handshake installs a lease matching the delivered Ack"
  in
  let world = Nsdl.Sim.load_files dhcp_files in
  discover world;
  Nsdl.Sim.advance world 2.0;
  (* ack_delay *)
  let addr =
    match Nsdl.Sim.perform world (Nsdl.Sim.Inspect "client.dhcp_address") with
    | Nsdl.Sim.OValue (Nsdl.Sim.VIpAddr a) -> Some a
    | _ -> None
  in
  let server =
    match Nsdl.Sim.perform world (Nsdl.Sim.Inspect "client.dhcp_server") with
    | Nsdl.Sim.OValue (Nsdl.Sim.VIdent s) -> Some s
    | _ -> None
  in
  let bound =
    match Nsdl.Sim.perform world (Nsdl.Sim.Inspect "client.dhcp_state") with
    | Nsdl.Sim.OValue (Nsdl.Sim.VIdent "bound") -> true
    | _ -> false
  in
  let expiry_matches_lease_length =
    match
      ( Nsdl.Sim.perform world (Nsdl.Sim.Inspect "client.dhcp_starts_at"),
        Nsdl.Sim.perform world (Nsdl.Sim.Inspect "client.dhcp_expires_at") )
    with
    | Nsdl.Sim.OValue (Nsdl.Sim.VFloat starts), Nsdl.Sim.OValue (Nsdl.Sim.VFloat expires) ->
      Float.equal (expires -. starts) 3600.0
    | _ -> false
  in
  if addr = Some "192.168.20.71" && server = Some "gateway" && bound && expiry_matches_lease_length
  then ok name
  else
    fail name
      (Printf.sprintf "addr=%s server=%s bound=%b expiry_matches=%b"
         (Option.value addr ~default:"<none>") (Option.value server ~default:"<none>") bound
         expiry_matches_lease_length)

let test_dhcp_dropped_ack_installs_no_lease () =
  let name =
    "dhcp: disconnecting the medium before the Ack's due time drops it and installs no lease -- \
     \"a dropped or invalidated Ack cannot produce a lease\""
  in
  let world = Nsdl.Sim.load_files dhcp_files in
  discover world;
  (* Disconnect immediately, well before any of the four hops (the
     earliest is due at 0.5s) can arrive -- every hop, including the
     Ack, must be dropped. *)
  Nsdl.Sim.disconnect_medium world "link";
  Nsdl.Sim.advance world 2.0;
  match Nsdl.Sim.perform world (Nsdl.Sim.Inspect "client.dhcp_address") with
  | Nsdl.Sim.OError _ -> ok name
  | o -> fail name (Printf.sprintf "expected no lease, got %s" (observation_to_string o))

let test_dhcp_late_disconnect_after_ack_keeps_lease () =
  let name =
    "dhcp: disconnecting the medium *after* the Ack already arrived does not retroactively \
     remove the lease (persistent state, not physical state)"
  in
  let world = Nsdl.Sim.load_files dhcp_files in
  discover world;
  Nsdl.Sim.advance world 2.0;
  (* ack_delay; lease now installed *)
  Nsdl.Sim.disconnect_medium world "link";
  match Nsdl.Sim.perform world (Nsdl.Sim.Inspect "client.dhcp_address") with
  | Nsdl.Sim.OValue (Nsdl.Sim.VIpAddr "192.168.20.71") -> ok name
  | o -> fail name (Printf.sprintf "expected the lease to survive, got %s" (observation_to_string o))

let unit_tests =
  [
    test_parse_duration;
    test_advance_does_not_fire_early;
    test_advance_fires_at_boundary_and_not_twice;
    test_same_timestamp_stable_order;
    test_priority_beats_insertion_order;
    test_between_expands_correct_count;
    test_between_rejects_nonpositive_every;
    test_on_enter_runs_state_body;
    test_invoke_no_matching_handler;
    test_invoke_state_guard;
    test_snapshot_restore_replay;
    test_disconnect_bumps_generation_and_epoch;
    test_delivery_succeeds_without_disconnect;
    test_no_ghost_delivery_after_disconnect;
    test_inject_no_args_sets_bool_marker;
    test_inject_named_arg_sets_value;
    test_dhcp_no_auto_lease_from_bare_field;
    test_dhcp_full_handshake_installs_lease;
    test_dhcp_dropped_ack_installs_no_lease;
    test_dhcp_late_disconnect_after_ack_keeps_lease;
  ]

let () =
  let case_results = List.map run_case cases in
  let unit_results = List.map (fun f -> f ()) unit_tests in
  if List.for_all (fun ok -> ok) (case_results @ unit_results) then exit 0 else exit 1
