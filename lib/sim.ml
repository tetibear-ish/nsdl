(* A minimal simulation model over the parsed AST.

   This is still NOT the full discrete-event runtime the NSDL proposal
   describes -- there is no port/message passing yet (`emit ... through
   PORT`, `on receive(...) at PORT`), no workflow success evaluation,
   and randomness is an unseeded `Stdlib.Random` call rather than the
   proposal's named, seed-derived streams (so `after random(...)` is
   NOT yet deterministic/replayable -- that's the next milestone).

   What IS real as of this pass: `instance NAME : TYPE` is bound to a
   matching `object TYPE { }` definition, giving it actual `port`/
   `memory`/`lifecycle`/handler declarations instead of just a bag of
   fields; instances have a real lifecycle state, initialized to the
   type's `initial` state; `transition STATE` and `after EXPR -> STATE`
   actually move an instance between states; entering a state runs that
   state's `in STATE { }` block (if declared), exactly the way
   `communications_relay.nsdl`'s `in stabilizing { }` is written to
   behave; and handlers can be dispatched by trigger name via the new
   `Invoke` action, matched against an instance's current lifecycle
   state via a handler's `in STATE` clause. Handler `at PORT`/`when
   EXPR` guards are parsed but not evaluated -- they need the port/
   message layer this pass doesn't add.

   It exists so the headless test harness (test/harness.ml) and the TUIs
   (bin/tui.ml, bin/tui_nottui.ml) have a real, stable interface to
   exercise before the full runtime exists. That runtime should be able
   to replace this module's internals wholesale -- callers only depend
   on the signatures below, not on how state is stored or actions are
   resolved. *)

open Ast

type value =
  | VInt of int
  | VFloat of float
  | VString of string
  | VIdent of string
  | VIpAddr of string
  | VBool of bool

let value_equal a b =
  match (a, b) with
  | VInt a, VInt b -> a = b
  | VFloat a, VFloat b -> a = b
  | VString a, VString b -> String.equal a b
  | VIdent a, VIdent b -> String.equal a b
  | VIpAddr a, VIpAddr b -> String.equal a b
  | VBool a, VBool b -> a = b
  | _ -> false

let value_to_string = function
  | VInt i -> string_of_int i
  | VFloat f -> string_of_float f
  | VString s -> "\"" ^ s ^ "\""
  | VIdent s -> s
  | VIpAddr s -> s
  | VBool b -> string_of_bool b

(* Anything we don't have a direct [value] case for (calls, ranges,
   booleans expressions, ...) is kept as its printed form rather than
   dropped, so `inspect` never silently loses information. *)
let rec expr_to_value (e : expr) : value =
  match e with
  | EInt i -> VInt i
  | EFloat f -> VFloat f
  | EString s -> VString s
  | EDuration d -> VFloat d
  | EIpAddr s -> VIpAddr s
  | EIdent "true" -> VBool true
  | EIdent "false" -> VBool false
  | EIdent s -> VIdent s
  | EField _ | EIndex _ | ECall _ | ERange _ | EAnd _ | EOr _ | EDequeue _ ->
    VString (Pretty.expr_to_string e)

and path_of_expr (e : expr) : string =
  match e with
  | EIdent s -> s
  | EField (e, f) -> path_of_expr e ^ "." ^ f
  | EIndex (e, i) -> Printf.sprintf "%s[%s]" (path_of_expr e) (value_to_string (expr_to_value i))
  | _ -> Pretty.expr_to_string e

(* The registered shape of an `object TYPE { }` definition: everything
   an instance of that type needs to have real behavior instead of
   just a bag of fields. *)
type object_def = {
  ports : (string * string) list;
  memories : (string * string * string option) list; (* name, type, persistence *)
  lifecycles : (string * bool) list; (* name, is_initial *)
  handlers : handler list;
  on_enter : (string, stmt list) Hashtbl.t; (* lifecycle state -> `in STATE { }` body *)
  (* v0.3 port/media declarations (Phase 2). Parsed and stored, but not
     yet deeply interpreted -- there's no type-checking of state field
     types, and `emits`/`receives`/`capability` aren't enforced against
     anything yet. That's the honest boundary of what this phase adds. *)
  state_fields : (string * state_type) list;
  emits : string list;
  receives : string list;
  endpoints : (int * string) option; (* count, type -- from `endpoints: exactly<N, T>` *)
  capabilities : string list;
}

let build_object_def (body : stmt list) : object_def =
  let ports = ref [] and memories = ref [] and lifecycles = ref [] and handlers = ref [] in
  let on_enter = Hashtbl.create 8 in
  let state_fields = ref [] and emits = ref [] and receives = ref [] in
  let endpoints = ref None and capabilities = ref [] in
  List.iter
    (fun s ->
      match s with
      | SPort (n, t) -> ports := (n, t) :: !ports
      | SMemory (n, t, p) -> memories := (n, t, p) :: !memories
      | SLifecycleDecl (n, init) -> lifecycles := (n, init) :: !lifecycles
      | SHandler h -> handlers := h :: !handlers
      | SLifecycleBlock (st, b) -> Hashtbl.replace on_enter st b
      | SStateDecl fields -> state_fields := !state_fields @ fields
      | SEmits n -> emits := n :: !emits
      | SReceives ns -> receives := !receives @ ns
      | SEndpoints (n, t) -> endpoints := Some (n, t)
      | SCapability n -> capabilities := n :: !capabilities
      | _ -> ())
    body;
  {
    ports = List.rev !ports;
    memories = List.rev !memories;
    lifecycles = List.rev !lifecycles;
    handlers = List.rev !handlers;
    on_enter;
    state_fields = !state_fields;
    emits = List.rev !emits;
    receives = !receives;
    endpoints = !endpoints;
    capabilities = List.rev !capabilities;
  }

type instance = {
  inst_type : string;
  fields : (string, value) Hashtbl.t;
  mutable obj_type : object_def option;
  mutable lifecycle_state : string option;
}

(* Same-timestamp priority classes, normative per the v0.3 executable
   semantics proposal: physical mutations must settle before derived
   recomputation, which must settle before protocol reactions, and so
   on, regardless of insertion order. Phases 2+ (ports/media, protocol
   components, observations) are what actually populate the middle and
   upper classes -- until then, every event this module schedules is
   [priority_physical], since a lifecycle transition or a schedule
   -block's field write *is* the state mutation, not a reaction to one.
   The constants exist now so later phases assign into an already
   -normative ordering instead of retrofitting one. *)
let priority_physical = 0 (* action completion / physical mutation *)
let priority_topology = 1 (* derived topology and port recomputation *)
let priority_invalidation = 2 (* cancellation and invalidation *)
let priority_protocol = 3 (* protocol state reactions (DHCP, routing, ...) *)
let priority_application = 4 (* application and service work *)
let priority_observation = 5 (* observation materialization *)
let priority_analytics = 6 (* analytics, scoring, presentation hints *)

(* Stamped on a scheduled event that represents a message crossing a
   medium (v0.3's DeliveryIntent). At fire time, if the medium's
   generation or the world's topology_epoch no longer match what was
   captured when the event was scheduled, the delivery is dropped
   ("dropped_due_to_link_loss") instead of executing -- this is the
   mechanism that rules out "ghost packets" arriving after a disconnect
   that happened after they were sent but before they were due. *)
type delivery_check = {
  via_medium : string;
  sent_generation : int;
  sent_epoch : int;
}

(* A pending schedule-block body, delayed lifecycle transition, or
   in-flight delivery, due at an absolute virtual-clock time. Same-time
   events fire in [(priority, seq)] order -- [seq] (insertion order)
   only breaks ties *within* a priority class, it does not override
   priority. [self], when set, is the instance the body's bare
   (unqualified) field names and `transition`/`clear` statements are
   relative to -- top-level schedule blocks leave this [None] and
   require fully-qualified paths, same as before. [delivery], when set,
   makes this event subject to revalidation at fire time (see
   [delivery_check] above); non-delivery events (schedule blocks,
   lifecycle timers) leave it [None] and always fire. *)
type scheduled_event = {
  due : float;
  priority : int;
  seq : int;
  label : string;
  self : string option;
  delivery : delivery_check option;
  body : stmt list;
}

type world = {
  instances : (string, instance) Hashtbl.t;
  object_defs : (string, object_def) Hashtbl.t;
  (* Phase 5: `profile NAME { field = expr ... }` blocks, registered by
     name -> field name -> its *unevaluated* expr. Kept unevaluated
     (rather than eagerly computing each field to a float once at load
     time) so a field like `wan_acquisition = random(10s .. 20s, ...)`
     is freshly sampled every time something actually references it
     (e.g. a gateway's `after consumer_cable_gateway_startup.wan_acquisition
     -> stabilizing`), the way separate instances using the same
     hardware profile would independently roll their own timing. *)
  profiles : (string, (string, expr) Hashtbl.t) Hashtbl.t;
  (* Phase 6: `world NAME { local_name = canonical_name }` blocks --
     name -> local vocabulary name -> canonical field/trigger name.
     Purely a rename table: [InspectAs]/[InvokeAs] resolve through it
     and then delegate to the exact same [Inspect]/[Invoke] logic, so
     there is no separate store a "world" could disagree with canonical
     state about -- see the comment on [InspectAs] below. *)
  world_bindings : (string, (string, string) Hashtbl.t) Hashtbl.t;
  mutable connections : (string * string * string) list; (* from, to, medium *)
  mutable log : string list; (* most recent action first *)
  mutable clock : float; (* virtual seconds elapsed *)
  mutable pending : scheduled_event list;
  mutable next_seq : int;
  (* Bumped by [disconnect_medium] (or anything else topology-affecting
     later) alongside the affected medium's own "generation" field.
     Both are checked at delivery time -- see [delivery_check]. *)
  mutable topology_epoch : int;
}

let create () =
  {
    instances = Hashtbl.create 16;
    object_defs = Hashtbl.create 16;
    profiles = Hashtbl.create 8;
    world_bindings = Hashtbl.create 8;
    connections = [];
    log = [];
    clock = 0.0;
    pending = [];
    next_seq = 0;
    topology_epoch = 0;
  }

(* A point-in-time copy of everything mutable in a [world], for the
   v0.3 proposal's `snapshot()`/`restore(snapshot)` API and the
   deterministic-replay property it exists to prove: restoring a
   snapshot and replaying the same actions from it should reproduce
   identical state. [object_defs] is shared by reference rather than
   deep-copied -- it's the compiled type registry, populated once by
   [load_files] and never mutated afterward, not part of a run's
   canonical mutable state. Connections/log/pending are plain
   (immutable once built) OCaml lists, so sharing those by reference is
   also safe; only the instances hashtable and each instance's own
   fields hashtable need an actual copy. *)
type instance_snapshot = {
  si_inst_type : string;
  si_fields : (string * value) list;
  si_obj_type : object_def option;
  si_lifecycle_state : string option;
}

type snapshot = {
  snap_object_defs : (string, object_def) Hashtbl.t;
  snap_profiles : (string, (string, expr) Hashtbl.t) Hashtbl.t;
  snap_world_bindings : (string, (string, string) Hashtbl.t) Hashtbl.t;
  snap_instances : (string * instance_snapshot) list;
  snap_connections : (string * string * string) list;
  snap_log : string list;
  snap_clock : float;
  snap_pending : scheduled_event list;
  snap_next_seq : int;
  snap_topology_epoch : int;
}

let snapshot (world : world) : snapshot =
  {
    snap_object_defs = world.object_defs;
    snap_profiles = world.profiles;
    snap_world_bindings = world.world_bindings;
    snap_instances =
      Hashtbl.fold
        (fun name inst acc ->
          ( name,
            {
              si_inst_type = inst.inst_type;
              si_fields = Hashtbl.fold (fun k v acc -> (k, v) :: acc) inst.fields [];
              si_obj_type = inst.obj_type;
              si_lifecycle_state = inst.lifecycle_state;
            } )
          :: acc)
        world.instances [];
    snap_connections = world.connections;
    snap_log = world.log;
    snap_clock = world.clock;
    snap_pending = world.pending;
    snap_next_seq = world.next_seq;
    snap_topology_epoch = world.topology_epoch;
  }

let restore (snap : snapshot) : world =
  let instances = Hashtbl.create (List.length snap.snap_instances) in
  List.iter
    (fun (name, si) ->
      let fields = Hashtbl.create (List.length si.si_fields) in
      List.iter (fun (k, v) -> Hashtbl.replace fields k v) si.si_fields;
      Hashtbl.replace instances name
        {
          inst_type = si.si_inst_type;
          fields;
          obj_type = si.si_obj_type;
          lifecycle_state = si.si_lifecycle_state;
        })
    snap.snap_instances;
  {
    instances;
    object_defs = snap.snap_object_defs;
    profiles = snap.snap_profiles;
    world_bindings = snap.snap_world_bindings;
    connections = snap.snap_connections;
    log = snap.snap_log;
    clock = snap.snap_clock;
    pending = snap.snap_pending;
    next_seq = snap.snap_next_seq;
    topology_epoch = snap.snap_topology_epoch;
  }

let log_action world msg = world.log <- msg :: world.log

let get_instance world name = Hashtbl.find_opt world.instances name

(* Field names owned by the Phase 4 network-status projection (see
   [network_status_field] below) -- never stored, always computed, so
   attempts to write them directly (via `Configure` or an authored
   `set`/bare assignment) are rejected rather than silently accepted
   -and-ignored. Declared here, ahead of [exec_stmt], since the guard
   applies to authored assignments too, not just the `Configure`
   action. *)
let derived_field_names =
  [
    "physical_attachment";
    "carrier";
    "l2_reachability";
    "ipv4";
    "default_route";
    "dns";
    "service_readiness";
    "overall";
  ]

let is_derived_field_name name = List.mem name derived_field_names

(* [path] is "instance_name.field.subfield" or "instance_name" alone. *)
let split_path path =
  match String.index_opt path '.' with
  | None -> (path, "")
  | Some i -> (String.sub path 0 i, String.sub path (i + 1) (String.length path - i - 1))

let set_field world inst_name field value =
  match get_instance world inst_name with
  | Some inst -> Hashtbl.replace inst.fields field value
  | None ->
    let inst =
      { inst_type = "unknown"; fields = Hashtbl.create 8; obj_type = None; lifecycle_state = None }
    in
    Hashtbl.replace inst.fields field value;
    Hashtbl.replace world.instances inst_name inst

let get_field world inst_name field =
  match get_instance world inst_name with
  | None -> None
  | Some inst -> Hashtbl.find_opt inst.fields field

(* Flattens a block's [SAssign] statements into "dotted.field" -> value
   entries. Used both for an instance's inline field block and (by
   [apply_incident]) for `set X.Y = Z` overlay statements, which parse
   to the same [SAssign] constructor. *)
let collect_assigns prefix stmts acc =
  List.fold_left
    (fun acc s ->
      match s with
      | SAssign (p, e) ->
        let key = path_of_expr p in
        let key = if prefix = "" then key else prefix ^ "." ^ key in
        (key, expr_to_value e) :: acc
      | _ -> acc)
    acc stmts

let load_object world (top : top) =
  match top with
  | TObject (name, body) -> Hashtbl.replace world.object_defs name (build_object_def body)
  | _ -> invalid_arg "load_object: expected a TObject"

(* Registers a `profile NAME { field = expr ... }` block. Fields are
   kept as their raw, unevaluated exprs -- see the comment on
   [world.profiles] for why (mainly: so `random(...)` fields are
   sampled fresh per reference, not once at load time). *)
let load_profile world (top : top) =
  match top with
  | TProfile (name, body) ->
    let fields = Hashtbl.create 8 in
    List.iter
      (function SAssign (p, e) -> Hashtbl.replace fields (path_of_expr p) e | _ -> ())
      body;
    Hashtbl.replace world.profiles name fields
  | _ -> invalid_arg "load_profile: expected a TProfile"

(* Registers a `world NAME { local_name = canonical_name }` block --
   pure string -> string renames, evaluated eagerly since (unlike
   profile durations) there's no reason a vocabulary mapping would
   itself be a `random(...)` or otherwise need lazy re-evaluation. *)
let load_world_binding world (top : top) =
  match top with
  | TWorld (name, body) ->
    let bindings = Hashtbl.create 8 in
    List.iter
      (function
        | SAssign (p, e) -> Hashtbl.replace bindings (path_of_expr p) (path_of_expr e)
        | _ -> ())
      body;
    Hashtbl.replace world.world_bindings name bindings
  | _ -> invalid_arg "load_world_binding: expected a TWorld"

let expr_to_seconds (e : expr) : float =
  match e with
  | EDuration d -> d
  | EInt i -> float_of_int i
  | EFloat f -> f
  | _ -> invalid_arg (Printf.sprintf "expr_to_seconds: not a duration: %s" (Pretty.expr_to_string e))

(* Handles the duration-shaped expression forms that aren't a plain
   literal:
   - `random(A .. B)`: NOT deterministic/replayable yet -- the proposal
     wants named streams derived from the scenario seed; this is an
     honest placeholder (unseeded `Stdlib.Random`) until that lands.
   - `PROFILE.field`: looks up the named profile's field (registered by
     [load_profile]) and evaluates *that* expr recursively -- so a
     profile field that's itself `random(...)` gets freshly sampled
     here, at the point of reference, not once when the profile was
     loaded. *)
let rec eval_duration_like world (e : expr) : float =
  match e with
  | ECall (EIdent "random", APos (ERange (a, b)) :: _) ->
    let lo = expr_to_seconds a and hi = expr_to_seconds b in
    lo +. Random.float (hi -. lo)
  | EField (EIdent profile_name, field_name) -> (
    match Hashtbl.find_opt world.profiles profile_name with
    | None -> expr_to_seconds e (* not a known profile -- fall through, will likely error *)
    | Some fields -> (
      match Hashtbl.find_opt fields field_name with
      | Some inner -> eval_duration_like world inner
      | None ->
        invalid_arg (Printf.sprintf "no such profile field: %s.%s" profile_name field_name)))
  | _ -> expr_to_seconds e

let generation_of world inst_name =
  match get_field world inst_name "generation" with
  | Some (VInt n) -> n
  | _ -> 0

(* Bumps the medium's own "generation" field and the world's global
   topology_epoch -- both are checked against any in-flight
   [delivery_check] at fire time (see [scheduled_event]/[advance]).
   No-ops but logs if [medium_name] isn't a known instance, since a
   malformed incident target shouldn't crash the run. *)
let disconnect_medium world medium_name =
  match get_instance world medium_name with
  | None -> log_action world (Printf.sprintf "disconnect: no such medium %s" medium_name)
  | Some _ ->
    let gen = generation_of world medium_name + 1 in
    set_field world medium_name "generation" (VInt gen);
    set_field world medium_name "physical_state" (VIdent "detached");
    world.topology_epoch <- world.topology_epoch + 1;
    log_action world
      (Printf.sprintf "topology: %s generation -> %d, epoch -> %d" medium_name gen
         world.topology_epoch)

let schedule_at world ~self ~priority ~delivery due label body =
  let seq = world.next_seq in
  world.next_seq <- seq + 1;
  world.pending <- { due; priority; seq; label; self; delivery; body } :: world.pending

(* Schedules [body] to fire after [delay] seconds as a message crossing
   [medium] -- captures the medium's current generation and the
   world's current topology_epoch now, so [advance] can detect at fire
   time whether the medium was disconnected (or otherwise changed) in
   between and drop the delivery instead of executing a "ghost packet". *)
let send_via_medium world ~medium ~self ~priority ~delay label body =
  schedule_at world ~self ~priority
    ~delivery:
      (Some
         {
           via_medium = medium;
           sent_generation = generation_of world medium;
           sent_epoch = world.topology_epoch;
         })
    (world.clock +. delay) label body

(* [transition_to] and [exec_stmt] are mutually recursive: entering a
   state runs its `in STATE { }` body, and that body can itself contain
   a `transition` (or a `set`/`after ... -> STATE` chain) that enters
   another state. *)
let rec transition_to world inst_name new_state =
  match get_instance world inst_name with
  | None -> ()
  | Some inst ->
    inst.lifecycle_state <- Some new_state;
    log_action world (Printf.sprintf "%s: transition -> %s" inst_name new_state);
    (match inst.obj_type with
    | None -> ()
    | Some def -> (
      match Hashtbl.find_opt def.on_enter new_state with
      | None -> ()
      | Some body -> List.iter (exec_stmt world ~self:(Some inst_name)) body))

(* Applies one statement to world state. [self], when set, is the
   instance a bare (unqualified) field name, `transition`, or `clear`
   is relative to -- this is how handler/on-entry bodies can write
   `connectivity = intermittent` instead of `relay.connectivity = ...`.
   Only the statement kinds with an obvious, direct effect are modeled;
   port/message kinds (SEnqueue, SSchedule, SEmit, ...) and workflow/
   report kinds require machinery this pass doesn't add, so they're
   logged rather than silently ignored or guessed at. *)
and exec_stmt world ~self (s : stmt) =
  match s with
  | SAssign (p, e) ->
    let path = path_of_expr p in
    let inst_name, field =
      match self with
      | Some name when not (String.contains path '.') -> (name, path)
      | _ -> split_path path
    in
    if is_derived_field_name field then
      log_action world
        (Printf.sprintf "rejected: %s.%s is a derived projection, cannot be set directly"
           inst_name field)
    else set_field world inst_name field (expr_to_value e)
  | SInject (kind, args, target) ->
    (* `inject KIND on TARGET` sets TARGET.KIND = true (a fault/condition
       marker); `inject KIND(arg, ...) on TARGET` uses the first arg's
       value instead -- the v0.3 doc's own examples (`inject disconnect
       on X`, `inject impairment(loss = 0.35) on Y`) don't fully specify
       what an injection *does* beyond that shape, so this is a
       documented interpretation, not a derived fact. `disconnect`
       specifically also bumps the target's generation and the world's
       topology_epoch (see [disconnect_medium]) -- every other kind is
       just the marker/value write. *)
    let target_path = path_of_expr target in
    let value =
      match args with
      | [] -> VBool true
      | APos e :: _ -> expr_to_value e
      | ANamed (_, e) :: _ -> expr_to_value e
    in
    let inst_name, field = split_path (target_path ^ "." ^ kind) in
    set_field world inst_name field value;
    let args_str =
      if args = [] then "" else "(" ^ String.concat ", " (List.map Pretty.arg_to_string args) ^ ")"
    in
    log_action world (Printf.sprintf "inject %s%s on %s" kind args_str target_path);
    if kind = "disconnect" then disconnect_medium world target_path
  | STransition new_state -> (
    match self with
    | Some name -> transition_to world name new_state
    | None -> log_action world (Printf.sprintf "(no instance context) transition %s" new_state))
  | SClear persistence -> (
    match self with
    | None -> log_action world (Printf.sprintf "(no instance context) clear %s" persistence)
    | Some name -> (
      match get_instance world name with
      | None -> ()
      | Some inst ->
        (match inst.obj_type with
        | None -> ()
        | Some def ->
          List.iter
            (fun (mem_name, _, mem_persist) ->
              if mem_persist = Some persistence then Hashtbl.remove inst.fields mem_name)
            def.memories);
        log_action world (Printf.sprintf "%s: clear %s" name persistence)))
  | SAfterTransition (delay_expr, new_state) -> (
    match self with
    | None -> log_action world "(no instance context) after ... -> transition"
    | Some name ->
      let delay = eval_duration_like world delay_expr in
      schedule_at world ~self:(Some name) ~priority:priority_physical ~delivery:None
        (world.clock +. delay)
        (Printf.sprintf "%s -> %s" name new_state)
        [ STransition new_state ])
  | _ -> log_action world (Printf.sprintf "(unmodeled) %s" (Pretty.stmt_to_string s))

let load_scenario world (top : top) =
  match top with
  | TScenario (_, body) ->
    List.iter
      (fun s ->
        match s with
        | SInstance (name, ty, fields) ->
          let obj_type = Hashtbl.find_opt world.object_defs ty in
          let inst = { inst_type = ty; fields = Hashtbl.create 8; obj_type; lifecycle_state = None } in
          List.iter (fun (k, v) -> Hashtbl.replace inst.fields k v) (collect_assigns "" fields []);
          Hashtbl.replace world.instances name inst;
          (match obj_type with
          | None -> ()
          | Some def -> (
            match List.find_opt (fun (_, is_initial) -> is_initial) def.lifecycles with
            | Some (initial_state, _) -> transition_to world name initial_state
            | None -> ()))
        | SConnect (a, b, medium) ->
          world.connections <- (path_of_expr a, path_of_expr b, medium) :: world.connections
        | _ -> ())
      body
  | _ -> invalid_arg "load_scenario: expected a TScenario"

(* Overlays `set path = expr` statements from an incident onto existing
   instance state. Unlike [load_scenario], this does not create new
   instances -- an incident only perturbs an already-loaded scenario. *)
let apply_incident world (top : top) =
  match top with
  | TIncident (_, _, body) ->
    List.iter
      (fun s ->
        match s with
        | SAssign (p, e) ->
          let path = path_of_expr p in
          let inst_name, field = split_path path in
          set_field world inst_name field (expr_to_value e)
        | _ -> ())
      body
  | _ -> invalid_arg "apply_incident: expected a TIncident"

(* Registers a top-level `at`/`between ... every` schedule block's body
   to fire at its due time(s), with no instance context ([self] is
   [None]): its statements must use fully-qualified paths, same as
   before this pass. Does not run anything -- events only fire when
   [advance] moves the clock past their due time. *)
let load_schedule world (top : top) =
  match top with
  | TAt (d, body) ->
    schedule_at world ~self:None ~priority:priority_physical ~delivery:None (expr_to_seconds d)
      "at" body
  | TBetween (a, b, every, body) ->
    let a = expr_to_seconds a and b = expr_to_seconds b and every = expr_to_seconds every in
    if every <= 0.0 then invalid_arg "load_schedule: `every` duration must be positive";
    let t = ref a in
    while !t <= b +. 1e-9 do
      schedule_at world ~self:None ~priority:priority_physical ~delivery:None !t
        "between...every" body;
      t := !t +. every
    done
  | _ -> invalid_arg "load_schedule: expected a TAt or TBetween"

(* Advances the virtual clock by [by] seconds and fires every pending
   event now due, in (time, priority class, insertion order) -- the
   v0.3 proposal's normative same-timestamp ordering. Firing an event
   applies its body statements to world state via [exec_stmt] under
   that event's own [self] context.

   Critically, [world.clock] is set to *each event's own [due] time*
   immediately before firing it, not jumped straight to [target] before
   the batch starts. A chained transition (`after ... -> STATE` whose
   on-entry body schedules another `after ... -> STATE`) computes its
   new due time as `world.clock +. delay` *while firing* -- if the
   clock had already been jumped to the batch's final target, every
   link in the chain after the first would compute its delay from the
   wrong (future) clock value, and drift would compound with each hop.
   This only shows up once something chains multiple hops inside one
   [advance] call (a single relay transition or DHCP hop never
   triggered it) -- found via the Phase 5 gateway startup chain, which
   is exactly that.

   This also means a newly-scheduled event can itself become due within
   the same [target] window (e.g. a gateway's `off -> booting ->
   lan_ready -> dhcp_ready` chain, where each link's delay is small
   enough that a single large [advance] should walk through several
   states at once) -- so this loops, re-partitioning [world.pending]
   each round, until nothing more is due by [target]. A between/every
   block's repeated occurrences still don't self-reschedule; those are
   registered individually up front by [load_schedule], same as before.

   An event stamped with a [delivery] check (see [send_via_medium]) is
   revalidated first: if the medium's generation or the world's
   topology_epoch no longer match what was captured at send time, the
   body never executes -- this is what rules out ghost deliveries
   arriving after a disconnect that happened after the message was
   sent but before it was due. *)
let advance world (by : float) =
  let target = world.clock +. by in
  let fire ev =
    let stale =
      match ev.delivery with
      | None -> false
      | Some dc ->
        generation_of world dc.via_medium <> dc.sent_generation
        || world.topology_epoch <> dc.sent_epoch
    in
    if stale then
      log_action world
        (Printf.sprintf "t=%gs dropped_due_to_link_loss (%s) via %s" ev.due ev.label
           (match ev.delivery with Some dc -> dc.via_medium | None -> "?"))
    else (
      log_action world (Printf.sprintf "t=%gs fire (%s)" ev.due ev.label);
      List.iter (exec_stmt world ~self:ev.self) ev.body)
  in
  let rec drain iterations =
    if iterations > 100_000 then
      failwith "advance: exceeded iteration budget (a chain of events keeps rescheduling itself)";
    let due, not_due = List.partition (fun ev -> ev.due <= target +. 1e-9) world.pending in
    match due with
    | [] -> ()
    | _ ->
      let due =
        List.sort
          (fun a b ->
            if a.due <> b.due then compare a.due b.due
            else if a.priority <> b.priority then compare a.priority b.priority
            else compare a.seq b.seq)
          due
      in
      world.pending <- not_due;
      List.iter
        (fun ev ->
          world.clock <- ev.due;
          fire ev)
        due;
      drain (iterations + 1)
  in
  drain 0;
  world.clock <- target

(* Finds the first handler on [target]'s object type whose trigger name
   matches and whose `in STATE` clause (if any) matches the instance's
   current lifecycle state, and runs its body under that instance's
   context. Does not evaluate `at PORT`/`when EXPR` guards -- those need
   the port/message layer this pass doesn't add. *)
let dispatch_handler world trigger target : (string, string) result =
  match get_instance world target with
  | None -> Error (Printf.sprintf "no such instance: %s" target)
  | Some inst -> (
    match inst.obj_type with
    | None -> Error (Printf.sprintf "%s has no object-type behavior bound" target)
    | Some def -> (
      let matches h =
        String.equal h.h_trigger trigger
        &&
        match h.h_in with
        | None -> true
        | Some required_state -> inst.lifecycle_state = Some required_state
      in
      match List.find_opt matches def.handlers with
      | None ->
        Error
          (Printf.sprintf "no handler for %s on %s in state %s" trigger target
             (Option.value inst.lifecycle_state ~default:"<none>"))
      | Some h ->
        List.iter (exec_stmt world ~self:(Some target)) h.h_body;
        Ok (Printf.sprintf "invoked %s on %s" trigger target)))

(* Phase 3: a reduced but causal DHCP Discover/Offer/Request/Ack
   handshake. Each hop is delivered via [send_via_medium], so it's
   subject to the same epoch/generation revalidation as any other
   delivery -- a disconnect mid-handshake drops whichever hop is still
   in flight, same as any other message. All four hops are
   pre-scheduled here at invocation time rather than dynamically
   chained hop-by-hop (each one's own due time is computed now, not
   when the previous hop fires) -- "reduced", per the proposal's own
   framing for a reference implementation. This does NOT weaken the
   causality property that matters: lease installation happens
   entirely inside the fourth hop's (the Ack's) body, so it only runs
   if that specific delivery survives its own revalidation. A dropped
   or invalidated Ack -- for any reason, including an earlier hop never
   having arrived -- cannot produce a lease. Nothing here runs on its
   own just because a scenario declares `address = dhcp`; only an
   explicit [DhcpDiscover] starts a handshake at all. *)
(* Phase 5 capability gating: "lifecycle states enable only
   capabilities that are actually ready" -- a gateway whose lifecycle
   hasn't progressed past `off`/`booting` yet has no DHCP service
   listening, so a discover against it should fail outright rather than
   schedule a handshake that would eventually just not get answered.
   This only checks "has *any* lifecycle progress happened" (not a
   specific "dhcp_ready" state), since that's the general shape any
   object's lifecycle can express without this module hardcoding one
   particular gateway's state names. *)
let server_ready_for_dhcp world server =
  match get_instance world server with
  | None -> false
  | Some inst -> (
    match inst.lifecycle_state with
    | None -> true (* no lifecycle at all -- nothing to gate on, assume ready *)
    | Some ("off" | "booting") -> false
    | Some _ -> true)

let dhcp_discover world ~client ~server ~medium ~address ~lease_seconds =
  let discover_delay = 0.5 and offer_delay = 1.0 and request_delay = 1.5 and ack_delay = 2.0 in
  let field n v = SAssign (EField (EIdent client, n), v) in
  send_via_medium world ~medium ~self:None ~priority:priority_protocol ~delay:discover_delay
    (Printf.sprintf "dhcp discover %s -> %s" client server)
    [ SAssign (EField (EIdent server, "dhcp_last_discover"), EIdent client) ];
  send_via_medium world ~medium ~self:None ~priority:priority_protocol ~delay:offer_delay
    (Printf.sprintf "dhcp offer %s -> %s" server client)
    [ field "dhcp_offered_address" (EIpAddr address) ];
  send_via_medium world ~medium ~self:None ~priority:priority_protocol ~delay:request_delay
    (Printf.sprintf "dhcp request %s -> %s" client server)
    [ SAssign (EField (EIdent server, "dhcp_last_request"), EIdent client) ];
  send_via_medium world ~medium ~self:None ~priority:priority_protocol ~delay:ack_delay
    (Printf.sprintf "dhcp ack %s -> %s" server client)
    [
      field "dhcp_address" (EIpAddr address);
      field "dhcp_server" (EIdent server);
      field "dhcp_starts_at" (EFloat (world.clock +. ack_delay));
      field "dhcp_expires_at" (EFloat (world.clock +. ack_delay +. lease_seconds));
      field "dhcp_state" (EIdent "bound");
    ]

(* Phase 4: a derived, read-only network-status projection computed
   from canonical fields on demand -- never stored, so there is nothing
   to get out of sync between "canonical state" and "what inspect shows
   you" (the proposal's "displayed_state == project(canonical_state,
   observer_context)" conformance property). This is the proposal's
   NetworkStatus struct, function-shaped instead of field-shaped: one
   function computes any of its eight named facts on demand from
   whatever canonical state actually exists. Facts this codebase has no
   real causal basis for yet -- l2_reachability needs a forwarding
   graph, carrier needs link training, dns/service_readiness need those
   layers -- honestly report their "nothing modeled yet" default rather
   than fabricating something that merely looks derived. As ports,
   switch forwarding, DNS, and services get built in later phases, this
   is where their results should start actually feeding these facts. *)
let network_status_field world inst_name field : value option =
  match get_instance world inst_name with
  | None -> None
  | Some inst ->
    let physical_attachment =
      match Hashtbl.find_opt inst.fields "physical_state" with
      | Some (VIdent s) -> s
      | _ -> "attached" (* no disconnect recorded against this instance yet *)
    in
    let ipv4 =
      match Hashtbl.find_opt inst.fields "dhcp_state" with
      | Some (VIdent "bound") -> "leased"
      | _ -> "absent"
    in
    let default_route = if ipv4 = "leased" then "installed" else "absent" in
    let overall =
      if physical_attachment <> "attached" then "offline"
      else if ipv4 = "leased" then "online"
      else "local_only"
    in
    let value_of = function
      | "physical_attachment" -> Some physical_attachment
      | "carrier" -> Some "down" (* not modeled: no link-training mechanism built yet *)
      | "l2_reachability" -> Some "unavailable" (* not modeled: no switch forwarding yet *)
      | "ipv4" -> Some ipv4
      | "default_route" -> Some default_route
      | "dns" -> Some "unavailable" (* not modeled: no DNS mechanism built yet *)
      | "service_readiness" -> Some "unavailable" (* not modeled: no service layer yet *)
      | "overall" -> Some overall
      | _ -> None
    in
    Option.map (fun s -> VIdent s) (value_of field)

let string_contains ~needle haystack =
  let nl = String.length needle and hl = String.length haystack in
  if nl = 0 then true
  else
    let rec go i = i + nl <= hl && (String.sub haystack i nl = needle || go (i + 1)) in
    go 0

(* A lightweight provenance lookup: every log entry mentioning
   [inst_name], most recent first (same order as [world.log] itself).
   Deliberately simple -- real log-grepping, not a structured causal
   -chain graph -- but it's an honest answer to "why is this instance
   in the state it's in", built entirely from facts already recorded
   rather than reconstructed after the fact. *)
let provenance_for world inst_name =
  List.filter (string_contains ~needle:inst_name) world.log

type action =
  | Inspect of string
  | Configure of string * value
  | PowerCycle of string
  | RestartService of string
  | Invoke of string * string (* trigger, target instance *)
  | DhcpDiscover of {
      client : string;
      server : string;
      medium : string;
      address : string;
      lease_seconds : float;
    }
  | InspectAs of {
      world_name : string;
      instance : string;
      local_field : string;
    } (* resolves local_field through world_name's binding, then delegates to Inspect *)
  | InvokeAs of { world_name : string; local_trigger : string; target : string }
    (* resolves local_trigger through world_name's binding, then delegates to Invoke *)

type observation =
  | OValue of value
  | OAck of string
  | OError of string

(* [rec] because [InspectAs]/[InvokeAs] resolve a local vocabulary name
   to its canonical field/trigger name and then delegate to this same
   function's [Inspect]/[Invoke] cases -- there is deliberately no
   separate code path for "look up a value through a world binding",
   so two different worlds naming the same canonical fact can never
   disagree about it: they're reading the exact same [Inspect], just
   arriving at it via a different name. This is the mechanism behind
   the Phase 6 exit condition ("alternate clients preserve canonical
   outcomes") -- see the proposal's "world package may not... create a
   second hidden truth". *)
let rec perform world (a : action) : observation =
  match a with
  | Inspect path ->
    let inst_name, field = split_path path in
    log_action world (Printf.sprintf "inspect %s" path);
    if field = "" then (
      match get_instance world inst_name with
      | Some inst ->
        let state_suffix =
          match inst.lifecycle_state with Some s -> " state=" ^ s | None -> ""
        in
        OValue (VString (Printf.sprintf "<%s%s>" inst.inst_type state_suffix))
      | None -> OError (Printf.sprintf "no such instance: %s" inst_name))
    else if field = "state" then (
      match get_instance world inst_name with
      | None -> OError (Printf.sprintf "no such instance: %s" inst_name)
      | Some inst -> (
        match inst.lifecycle_state with
        | Some s -> OValue (VIdent s)
        | None -> OError (Printf.sprintf "%s has no lifecycle state" inst_name)))
    else if is_derived_field_name field then (
      match network_status_field world inst_name field with
      | Some v -> OValue v
      | None -> OError (Printf.sprintf "no such instance: %s" inst_name))
    else (
      match get_field world inst_name field with
      | Some v -> OValue v
      | None -> OError (Printf.sprintf "no such field: %s" path))
  | Configure (path, v) ->
    let inst_name, field = split_path path in
    log_action world (Printf.sprintf "configure %s = %s" path (value_to_string v));
    if field = "" then OError (Printf.sprintf "cannot configure a whole instance: %s" path)
    else if is_derived_field_name field then
      OError
        (Printf.sprintf
           "%s is a derived projection (computed from canonical state), it cannot be set \
            directly"
           path)
    else (
      set_field world inst_name field v;
      OAck (Printf.sprintf "configured %s" path))
  | PowerCycle target ->
    log_action world (Printf.sprintf "power_cycle %s" target);
    OAck
      (Printf.sprintf
         "power_cycle %s: acknowledged (no-op -- use `invoke TRIGGER %s` for object-defined \
          handlers, e.g. \"power_on\")"
         target target)
  | RestartService target ->
    log_action world (Printf.sprintf "restart_service %s" target);
    OAck
      (Printf.sprintf
         "restart_service %s: acknowledged (no-op -- service/port semantics not yet implemented)"
         target)
  | Invoke (trigger, target) ->
    log_action world (Printf.sprintf "invoke %s %s" trigger target);
    (match dispatch_handler world trigger target with
    | Ok msg -> OAck msg
    | Error msg -> OError msg)
  | DhcpDiscover { client; server; medium; address; lease_seconds } ->
    log_action world
      (Printf.sprintf "dhcp: %s discovering via %s (server %s, offering %s)" client medium server
         address);
    if not (server_ready_for_dhcp world server) then
      OError
        (Printf.sprintf "%s is not ready to serve DHCP yet (lifecycle state: %s)" server
           (match get_instance world server with
           | Some inst -> Option.value inst.lifecycle_state ~default:"<none>"
           | None -> "<no such instance>"))
    else (
      dhcp_discover world ~client ~server ~medium ~address ~lease_seconds;
      OAck (Printf.sprintf "dhcp handshake scheduled: %s <-> %s via %s" client server medium))
  | InspectAs { world_name; instance; local_field } -> (
    match Hashtbl.find_opt world.world_bindings world_name with
    | None -> OError (Printf.sprintf "no such world: %s" world_name)
    | Some bindings -> (
      match Hashtbl.find_opt bindings local_field with
      | None -> OError (Printf.sprintf "world %s has no local name %s" world_name local_field)
      | Some canonical_field -> perform world (Inspect (instance ^ "." ^ canonical_field))))
  | InvokeAs { world_name; local_trigger; target } -> (
    match Hashtbl.find_opt world.world_bindings world_name with
    | None -> OError (Printf.sprintf "no such world: %s" world_name)
    | Some bindings -> (
      match Hashtbl.find_opt bindings local_trigger with
      | None -> OError (Printf.sprintf "world %s has no local name %s" world_name local_trigger)
      | Some canonical_trigger -> perform world (Invoke (canonical_trigger, target))))

(* Parses and loads any number of .nsdl files into a fresh world, in two
   passes so object-type and profile registration never depend on file
   order: every `object` definition and `profile` block across all
   files is registered first, then every `scenario` (first one wins),
   `incident` (every one applied as an overlay), and `at`/`between`
   schedule block (every one registered) is processed. Raises
   [Nsdl.Lexer.Lex_error] or [Nsdl.Parser.Error] on a malformed file, or
   [Failure] if no scenario is found across all of them. *)
let parse_file path =
  let ic = open_in path in
  let lexbuf = Lexing.from_channel ic in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () -> Parser.program Lexer.token lexbuf)

let load_files (paths : string list) : world =
  let world = create () in
  let progs = List.map parse_file paths in
  List.iter
    (List.iter (function
      | TObject _ as t -> load_object world t
      | TProfile _ as t -> load_profile world t
      | TWorld _ as t -> load_world_binding world t
      | _ -> ()))
    progs;
  let scenario_loaded = ref false in
  List.iter
    (List.iter (function
      | TObject _ | TProfile _ | TWorld _ -> ()
      | TScenario _ as t ->
        if not !scenario_loaded then (
          load_scenario world t;
          scenario_loaded := true)
      | TIncident _ as t -> apply_incident world t
      | (TAt _ | TBetween _) as t -> load_schedule world t))
    progs;
  if not !scenario_loaded then
    failwith (Printf.sprintf "no scenario found across: %s" (String.concat ", " paths));
  world
