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
  | VRange of string * string (* an IP range, e.g. dhcp.range = A .. B -- see allocate_from_pool *)

let value_equal a b =
  match (a, b) with
  | VInt a, VInt b -> a = b
  | VFloat a, VFloat b -> a = b
  | VString a, VString b -> String.equal a b
  | VIdent a, VIdent b -> String.equal a b
  | VIpAddr a, VIpAddr b -> String.equal a b
  | VBool a, VBool b -> a = b
  | VRange (a1, a2), VRange (b1, b2) -> String.equal a1 b1 && String.equal a2 b2
  | _ -> false

let value_to_string = function
  | VInt i -> string_of_int i
  | VFloat f -> string_of_float f
  | VString s -> "\"" ^ s ^ "\""
  | VIdent s -> s
  | VIpAddr s -> s
  | VBool b -> string_of_bool b
  | VRange (a, b) -> a ^ " .. " ^ b

(* Anything we don't have a direct [value] case for (calls, booleans
   expressions, ...) is kept as its printed form rather than dropped, so
   `inspect` never silently loses information. The one exception is an
   IP-address range (`ERange (EIpAddr, EIpAddr)`, e.g. `dhcp.range = A ..
   B`), matched *before* the general `ERange` fallback below -- every
   other `ERange` shape (e.g. `random(10s .. 20s)`'s duration range) is
   evaluated by its own call site (`eval_duration_like`), never through
   here, so this is a narrow, additive case, not a behavior change for
   anything else. *)
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
  | ERange (EIpAddr a, EIpAddr b) -> VRange (a, b)
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

(* Stamped on a scheduled event that represents a message crossing one
   or more media (v0.3's DeliveryIntent) -- a list rather than a single
   medium so a multi-hop path through an intermediate switch instance
   (see [resolve_path]/[send_via_path]) can be revalidated as a whole:
   if *any* medium along the path changed generation, or the world's
   topology_epoch no longer matches what was captured when the event
   was scheduled, the delivery is dropped ("dropped_due_to_link_loss")
   instead of executing -- this is the mechanism that rules out "ghost
   packets" arriving after a disconnect that happened after they were
   sent but before they were due, generalized from one hop to a path. *)
type delivery_check = {
  via_media : (string * int) list; (* medium instance name, generation at send time *)
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
  (* (server, address) -> (client, expires_at) -- every actively-reserved
     DHCP lease, checked by [dhcp_discover] before it ever schedules a
     handshake, so a server can never hand the same address to two
     different, still-active clients. Keyed by server as well as address
     since two independent servers could legitimately reuse the same
     address literal on different subnets without conflict. Real,
     mutable run state (like [instances]), not a static registry (like
     [object_defs]/[profiles]) -- see [snapshot]/[restore] below. *)
  mutable dhcp_leases : (string * string, string * float) Hashtbl.t;
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
    dhcp_leases = Hashtbl.create 16;
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
  snap_dhcp_leases : (string * string, string * float) Hashtbl.t;
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
    snap_dhcp_leases = Hashtbl.copy world.dhcp_leases;
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
    (* Copied, not shared: restoring the same snapshot twice (e.g. two
       independent replays for a determinism check) must not let one
       replay's DHCP activity leak into the other's, same reasoning as
       why [instances] is deep-copied above. *)
    dhcp_leases = Hashtbl.copy snap.snap_dhcp_leases;
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

(* Whether [medium_name] currently admits traffic -- used to keep
   [resolve_path] from routing through a medium that's been disconnected
   (or is still mid-reconnect-training; see [reconnect_medium]). Defaults
   to [true] when there's no recorded `physical_state` at all, matching
   [network_status_field]'s own "never disconnected" default: a bare
   medium name that was never instantiated as a real object (e.g. the
   `cat6` label in `clinic_printer.nsdl`'s `connect ... via cat6`) has no
   generation/physical_state fields and was never disconnected, so it
   should still route. *)
let medium_attached world medium_name =
  match get_field world medium_name "physical_state" with
  | Some (VIdent s) -> s = "attached"
  | _ -> true

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

(* Real seconds a medium spends "training" after a reconnect before it's
   usable again -- deliberately a plain constant rather than a fidelity
   profile field, since there's exactly one physical-layer mechanism this
   models (link training after cable insertion), not a family of hardware
   variants the way gateway startup timing is. *)
let link_training_delay = 3.0

(* The inverse of [disconnect_medium], and the mechanism behind the
   proposal's "reconnect begins lawful link training and protocol
   recovery; it does not restore every higher-layer state instantaneously"
   conformance property. Unlike disconnect (which is instantaneous),
   reconnect does NOT set `physical_state` straight to "attached": it sets
   "training" immediately, bumps generation/epoch (same as disconnect --
   this is still a topology-affecting event, invalidating anything that
   was mid-flight expecting the old, disconnected generation), and only
   after [link_training_delay] schedules the actual flip to "attached".
   Because [medium_attached] treats anything other than "attached" as
   unusable, [resolve_path] correctly refuses to route through a medium
   that's still training -- no separate bookkeeping needed for that half
   of the property. *)
let reconnect_medium world medium_name =
  match get_instance world medium_name with
  | None -> log_action world (Printf.sprintf "reconnect: no such medium %s" medium_name)
  | Some _ ->
    let gen = generation_of world medium_name + 1 in
    set_field world medium_name "generation" (VInt gen);
    set_field world medium_name "physical_state" (VIdent "training");
    world.topology_epoch <- world.topology_epoch + 1;
    log_action world
      (Printf.sprintf "topology: %s reconnecting (generation -> %d, epoch -> %d), training for %gs"
         medium_name gen world.topology_epoch link_training_delay);
    schedule_at world ~self:None ~priority:priority_physical ~delivery:None
      (world.clock +. link_training_delay)
      (Printf.sprintf "%s: link training complete" medium_name)
      [ SAssign (EField (EIdent medium_name, "physical_state"), EIdent "attached") ]

(* Schedules [body] to fire after [delay] seconds as a message crossing
   [medium] -- captures the medium's current generation and the
   world's current topology_epoch now, so [advance] can detect at fire
   time whether the medium was disconnected (or otherwise changed) in
   between and drop the delivery instead of executing a "ghost packet". *)
let send_via_medium world ~medium ~self ~priority ~delay label body =
  schedule_at world ~self ~priority
    ~delivery:
      (Some { via_media = [ (medium, generation_of world medium) ]; sent_epoch = world.topology_epoch })
    (world.clock +. delay) label body

(* Resolves the sequence of media connecting two instances by name, via
   breadth-first search over [world.connections] treated as an
   undirected graph of instances (an edge's endpoints are the leading
   instance-name segment of each connection's two paths, so
   `workstation.eth0 -> switch.port[2] via cat6` becomes an edge
   between instances "workstation" and "switch" labeled "cat6"). This
   is deliberately simple -- no MAC-address learning, no per-port
   forwarding tables, just "is there a path in the declared topology" --
   but it's enough to route a message through an intermediate switch
   instance instead of requiring scenario authors to name one direct
   medium between every pair of devices that need to talk. Returns the
   ordered list of (instance arrived at, medium used to get there), or
   [None] if the instances aren't connected at all.

   Edges whose medium is not currently [medium_attached] are excluded from
   the graph entirely, not merely revalidated later: without this, a *new*
   send issued after a medium is already disconnected would still find a
   route (nothing else here looks at physical state), get scheduled, and --
   since nothing changes its generation again before it fires -- would
   wrongly succeed, a live delivery through a cut cable. Messages already
   in flight *before* a disconnect are unaffected by this filter (they were
   scheduled while the edge was still attached); those are still caught by
   the separate generation/epoch check in [advance]'s [fire], which is what
   the ghost-packet tests exercise. *)
let resolve_path world ~from_inst ~to_inst : (string * string) list option =
  if from_inst = to_inst then Some []
  else (
    let edges =
      List.concat_map
        (fun (a, b, medium) ->
          if not (medium_attached world medium) then []
          else (
            let ia = fst (split_path a) and ib = fst (split_path b) in
            [ (ia, ib, medium); (ib, ia, medium) ]))
        world.connections
    in
    let visited = Hashtbl.create 16 in
    Hashtbl.replace visited from_inst ();
    let rec bfs = function
      | [] -> None
      | (node, path) :: rest ->
        if node = to_inst then Some (List.rev path)
        else (
          let neighbors =
            List.filter_map
              (fun (a, b, medium) ->
                if a = node && not (Hashtbl.mem visited b) then Some (b, medium) else None)
              edges
          in
          List.iter (fun (n, _) -> Hashtbl.replace visited n ()) neighbors;
          let frontier = List.map (fun (n, medium) -> (n, (n, medium) :: path)) neighbors in
          bfs (rest @ frontier))
    in
    bfs [ (from_inst, []) ])

(* Like [send_via_medium], but resolves the medium(s) automatically by
   walking the declared topology from [from_inst] to [to_inst] instead
   of taking one named medium directly -- this is what makes DHCP (or
   anything else built on it) work through an intermediate switch
   instance. Every medium along the resolved path is stamped and later
   revalidated, same as the single-hop case generalizes to a list (see
   [delivery_check]). Returns [false] (and logs, schedules nothing) if
   no path exists, so callers can report "no route" instead of
   silently sending a message into the void. *)
let send_via_path world ~from_inst ~to_inst ~self ~priority ~delay label body : bool =
  match resolve_path world ~from_inst ~to_inst with
  | None ->
    log_action world (Printf.sprintf "no route: %s -> %s" from_inst to_inst);
    false
  | Some hops ->
    let via_media = List.map (fun (_, medium) -> (medium, generation_of world medium)) hops in
    schedule_at world ~self ~priority
      ~delivery:(Some { via_media; sent_epoch = world.topology_epoch })
      (world.clock +. delay) label body;
    true

(* Phase 3 (+ Phase 3.5/switch-forwarding, added later): a reduced but
   causal DHCP Discover/Offer/Request/Ack handshake. Each hop is routed
   via [send_via_path], so it's subject to the same epoch/generation
   revalidation as any other delivery -- a disconnect anywhere along
   the path mid-handshake drops whichever hop is still in flight, same
   as any other message, and this now genuinely routes through an
   intermediate switch instance rather than requiring client and
   server to share one directly-named medium. All four hops are
   pre-scheduled here at invocation time rather than dynamically
   chained hop-by-hop (each one's own due time is computed now, not
   when the previous hop fires) -- "reduced", per the proposal's own
   framing for a reference implementation. This does NOT weaken the
   causality property that matters: lease installation happens
   entirely inside the fourth hop's (the Ack's) body, so it only runs
   if that specific delivery survives its own revalidation. A dropped
   or invalidated Ack -- for any reason, including an earlier hop never
   having arrived, or no route existing between client and server at
   all -- cannot produce a lease. Nothing here runs on its own just
   because a scenario declares `address = dhcp`; only an explicit
   [DhcpDiscover] action or a declared `dhcp_discover` statement (see
   [exec_stmt]'s [SDhcpDiscover] case below) starts a handshake at all. *)
(* Phase 5 capability gating: "lifecycle states enable only
   capabilities that are actually ready" -- a gateway whose lifecycle
   hasn't progressed past `off`/`booting` yet has no DHCP service
   listening, so a discover against it should fail outright rather than
   schedule a handshake that would eventually just not get answered.
   This only checks "has *any* lifecycle progress happened" (not a
   specific "dhcp_ready" state), since that's the general shape any
   object's lifecycle can express without this module hardcoding one
   particular gateway's state names. Moved above [exec_stmt] (it used to
   live after it) so [SDhcpDiscover]'s case can call it directly. *)
let server_ready_for_dhcp world server =
  match get_instance world server with
  | None -> false
  | Some inst -> (
    match inst.lifecycle_state with
    | None -> true (* no lifecycle at all -- nothing to gate on, assume ready *)
    | Some ("off" | "booting") -> false
    | Some _ -> true)

(* Gates a WAN-crossing [Ping] the same way [server_ready_for_dhcp] gates a
   DHCP handshake, but stricter: per the proposal's own startup timeline,
   only the terminal "Stable online" state grants "full WAN-dependent
   verification" -- every earlier state (including `wan_training` and
   `stabilizing`) is explicitly WAN-unready or only intermittently so. This
   is what proves "WAN-dependent work fails until the gateway reaches the
   required readiness state" (acceptance criterion 5) as something other
   than a coincidence of timing. *)
let gateway_wan_ready world gateway_name =
  match get_instance world gateway_name with
  | None -> false
  | Some inst -> inst.lifecycle_state = Some "online"

(* Server-side lease-collision guard: checked before anything else, so a
   server can never hand out an address it's already actively leased to
   a *different* client -- the same client re-requesting/renewing its
   own address is not a conflict. Reservation is committed only once the
   route to the server is confirmed to exist (inside the [else] branch
   below), not here -- a discover that can't even reach the server never
   held the address in the first place. *)
let dhcp_discover world ~client ~server ~address ~lease_seconds : (unit, string) result =
  match Hashtbl.find_opt world.dhcp_leases (server, address) with
  | Some (existing_client, expires_at) when existing_client <> client && expires_at > world.clock ->
    Error
      (Printf.sprintf "%s cannot offer %s to %s: already leased to %s until t=%gs" server address
         client existing_client expires_at)
  | _ ->
    let discover_delay = 0.5 and offer_delay = 1.0 and request_delay = 1.5 and ack_delay = 2.0 in
    let field n v = SAssign (EField (EIdent client, n), v) in
    let send ~from_inst ~to_inst ~delay label body =
      send_via_path world ~from_inst ~to_inst ~self:None ~priority:priority_protocol ~delay label
        body
    in
    if
      not
        (send ~from_inst:client ~to_inst:server ~delay:discover_delay
           (Printf.sprintf "dhcp discover %s -> %s" client server)
           [ SAssign (EField (EIdent server, "dhcp_last_discover"), EIdent client) ])
    then Error (Printf.sprintf "no route from %s to %s" client server)
    else (
      (* Provisional hold for this transaction, committed at
         Discover-acceptance time -- real DHCP servers hold an offered
         address similarly, not only once a lease fully completes. *)
      Hashtbl.replace world.dhcp_leases (server, address) (client, world.clock +. lease_seconds);
      ignore
        (send ~from_inst:server ~to_inst:client ~delay:offer_delay
           (Printf.sprintf "dhcp offer %s -> %s" server client)
           [ field "dhcp_offered_address" (EIpAddr address) ]);
      ignore
        (send ~from_inst:client ~to_inst:server ~delay:request_delay
           (Printf.sprintf "dhcp request %s -> %s" client server)
           [ SAssign (EField (EIdent server, "dhcp_last_request"), EIdent client) ]);
      ignore
        (send ~from_inst:server ~to_inst:client ~delay:ack_delay
           (Printf.sprintf "dhcp ack %s -> %s" server client)
           [
             field "dhcp_address" (EIpAddr address);
             field "dhcp_server" (EIdent server);
             field "dhcp_starts_at" (EFloat (world.clock +. ack_delay));
             field "dhcp_expires_at" (EFloat (world.clock +. ack_delay +. lease_seconds));
             field "dhcp_state" (EIdent "bound");
           ]);
      Ok ())

(* Seconds between a client's own auto-retry attempts (see [exec_stmt]'s
   [SDhcpDiscover] case) while the server it's targeting isn't ready yet.
   Real DHCP clients don't give up after one failed attempt -- they keep
   trying until the network is actually up; this is that behavior. *)
let dhcp_retry_delay = 2.0

(* ------------------------------------------------------------------ *)
(* Real port/message dispatch: emit/receive/schedule, wired up for the *)
(* first time (README's "Known limitations" has said "parsed, never    *)
(* executed" since before the v0.3 migration started). Every piece     *)
(* below is genuinely generic -- proven with DHCP, but nothing here     *)
(* names DHCP specifically except [allocate_from_pool], which a        *)
(* handler body opts into by calling it, the same way [random(...)]    *)
(* is a specially-evaluated call form rather than a general feature.   *)
(* ------------------------------------------------------------------ *)

(* Dotted-quad <-> 32-bit int, for walking a dhcp.range pool address by
   address. Fails gracefully (returns [None]) on malformed input rather
   than crashing, same discipline as [Lexer.parse_duration]. *)
let ip_to_int (ip : string) : int option =
  match String.split_on_char '.' ip with
  | [ a; b; c; d ] -> (
    try
      let a = int_of_string a and b = int_of_string b and c = int_of_string c and d = int_of_string d in
      if a < 0 || a > 255 || b < 0 || b > 255 || c < 0 || c > 255 || d < 0 || d > 255 then None
      else Some (((a lsl 24) lor (b lsl 16)) lor (c lsl 8) lor d)
    with _ -> None)
  | _ -> None

let int_to_ip (n : int) : string =
  Printf.sprintf "%d.%d.%d.%d" ((n lsr 24) land 255) ((n lsr 16) land 255) ((n lsr 8) land 255)
    (n land 255)

(* Provisional hold duration when a pool address is allocated (see
   [allocate_from_pool] below) -- not re-extended to whatever lease
   length a handshake's own Ack later negotiates (the Ack step has no
   clean hook back into "confirm/extend the earlier reservation" in this
   pass). A documented, generous-enough simplification for proving the
   race/pool-separation properties, not a hidden gap. *)
let dhcp_pool_reservation_seconds = 3600.0

(* Finds the first address in [range] not already an active, unexpired
   lease on [server] -- reusing [world.dhcp_leases], the exact same table
   the targeted path's own collision guard maintains (see [dhcp_discover]
   above), so a pool-allocated address and any manually-specified one can
   never collide either; one source of truth. Reserves the address to
   [client] immediately on success (the same "hold from the moment it's
   offered, not released early if the transaction doesn't complete"
   timing [dhcp_discover] already documents). Returns [None] if [range]
   isn't a real [VRange] or the whole pool is exhausted. *)
let allocate_from_pool world ~server ~client ~(range : value) : string option =
  match range with
  | VRange (lo, hi) -> (
    match (ip_to_int lo, ip_to_int hi) with
    | Some lo_n, Some hi_n ->
      let rec try_addr n =
        if n > hi_n then None
        else
          let candidate = int_to_ip n in
          match Hashtbl.find_opt world.dhcp_leases (server, candidate) with
          | Some (_, expires_at) when expires_at > world.clock -> try_addr (n + 1)
          | _ ->
            Hashtbl.replace world.dhcp_leases (server, candidate)
              (client, world.clock +. dhcp_pool_reservation_seconds);
            Some candidate
      in
      try_addr lo_n
    | _ -> None)
  | _ -> None

(* The inverse of [expr_to_value] for the cases that round-trip cleanly
   (every [value] variant does) -- needed because [SMessageArrived]'s
   payload is carried as already-literal [expr]s, not raw [value]s (see
   the comment on it in ast.ml: Ast doesn't depend on Sim, so it can't
   reference [Sim.value] directly). *)
let expr_of_value : value -> expr = function
  | VInt i -> EInt i
  | VFloat f -> EFloat f
  | VString s -> EString s
  | VIdent s -> EIdent s
  | VIpAddr s -> EIpAddr s
  | VBool b -> EIdent (string_of_bool b)
  | VRange (a, b) -> ERange (EIpAddr a, EIpAddr b)

(* Recognizes exactly the call shape `allocate_from_pool(RANGE_FIELD)`
   evaluated relative to [self] -- shared by [exec_stmt]'s [SAssign] case
   (`offered_address = allocate_from_pool(dhcp.range)`, so a server can
   remember what it offered for a later Ack to reference) and
   [eval_emit_payload_arg] below (offering the value directly in an
   emit's payload). [~client] is who the allocation should be reserved
   to: the sender of the message currently being handled ([__reply_to]),
   or self if none is set. Returns [None] for any other expr shape, so
   callers fall back to their own normal evaluation -- this does not
   change what any *other* expression evaluates to, only this one exact
   call form, the same way `random(...)` is a specifically-recognized
   call shape elsewhere rather than a general user-definable function. *)
let try_allocate_from_pool world ~self (e : expr) : value option =
  match e with
  | ECall (EIdent "allocate_from_pool", [ APos path_expr ]) -> (
    match get_field world self (path_of_expr path_expr) with
    | None -> Some (VBool false)
    | Some range ->
      let client = match get_field world self "__reply_to" with Some (VIdent p) -> p | _ -> self in
      Some
        (match allocate_from_pool world ~server:self ~client ~range with
        | Some addr -> VIpAddr addr
        | None -> VBool false (* pool exhausted -- logged by the SEmit case, not silently offered *)))
  | _ -> None

(* Evaluates [e] relative to [self]: [allocate_from_pool(...)] is
   special-cased (see [try_allocate_from_pool]); a bare [EIdent name]
   resolves as *self's own field* if one exists (falling back to a
   literal [VIdent] otherwise) -- the same self-relative idea [SAssign]'s
   LHS path already uses for writes, extended here to RHS values;
   anything else evaluates the same way it always has
   ([expr_to_value], no change). Deliberately narrow and only used from
   two specific places -- [exec_stmt]'s [SAssign] case (so a handler can
   write `dhcp_address = address`, copying a field a message delivery
   just wrote onto it, e.g. via [dispatch_message]'s payload step) and
   [eval_emit_payload_arg] below (so a handler can re-emit a value it
   just received, `emit request(address = offered_address) through
   eth0`) -- not a general "expressions can read fields" feature. Neither
   call site is reachable from [collect_assigns] (instance field
   declarations) or [apply_incident] (`set` overlays), which use their
   own, separate, unchanged [expr_to_value] evaluation -- self-relative
   resolution only ever applies where [self] already means something
   (inside a running handler/on-entry body), never at load time. *)
let eval_self_relative_value world ~self (e : expr) : value =
  match try_allocate_from_pool world ~self e with
  | Some v -> v
  | None -> (
    match e with
    | EIdent name -> ( match get_field world self name with Some v -> v | None -> expr_to_value e)
    | _ -> expr_to_value e)

let eval_emit_payload_arg world ~self (a : arg) : string * value =
  match a with
  | ANamed (key, e) -> (key, eval_self_relative_value world ~self e)
  | APos e -> ("_", expr_to_value e)

(* Finds everyone reachable from [from_inst]'s [port_name], flooding
   outward over the same [medium_attached]-filtered graph [resolve_path]
   already walks (generalized to enumerate *everyone* reachable, with hop
   count, rather than resolving one named target). A disconnected port
   naturally yields [] -- [resolve_path]'s own filtering already makes a
   cut cable stop routing, nothing extra is needed here for that to hold
   for emit too.

   Returns each target's *own* port name alongside it, not just the
   instance -- essential, not cosmetic: a message's receiving port is
   checked against the *target's* `on TRIGGER at PORT` handlers, and two
   connected devices very often use differently-named ports (a client's
   "eth0" talking to a gateway's "lan"), so the sender's own port name
   (the [port_name] argument) is never the right thing to check on the
   far end. [world.connections] already stores each side's full path
   (e.g. "gateway1.lan"), so each edge direction carries its own
   destination port for free -- this only has to look each one up, not
   invent new topology data. *)
let emit_targets world ~from_inst ~port_name : (string * string * int) list =
  let directed_edges =
    (* (from_instance, from_port, to_instance, to_port), both directions,
       attached media only *)
    List.concat_map
      (fun (a, b, medium) ->
        if not (medium_attached world medium) then []
        else
          let ia, pa = split_path a and ib, pb = split_path b in
          [ (ia, pa, ib, pb); (ib, pb, ia, pa) ])
      world.connections
  in
  let first_hops =
    List.filter_map
      (fun (fi, fp, ti, tp) -> if fi = from_inst && fp = port_name then Some (ti, tp) else None)
      directed_edges
  in
  match first_hops with
  | [] -> []
  | _ ->
    let visited = Hashtbl.create 16 in
    Hashtbl.replace visited from_inst ();
    List.iter (fun (ti, _) -> Hashtbl.replace visited ti ()) first_hops;
    let result = ref (List.map (fun (ti, tp) -> (ti, tp, 1)) first_hops) in
    let rec bfs = function
      | [] -> ()
      | (node, hops) :: rest ->
        let neighbors =
          List.filter_map
            (fun (fi, _, ti, tp) -> if fi = node && not (Hashtbl.mem visited ti) then Some (ti, tp) else None)
            directed_edges
        in
        List.iter
          (fun (n, p) ->
            Hashtbl.replace visited n ();
            result := (n, p, hops + 1) :: !result)
          neighbors;
        bfs (rest @ List.map (fun (n, _) -> (n, hops + 1)) neighbors)
    in
    bfs (List.map (fun (ti, _) -> (ti, 1)) first_hops);
    List.rev !result

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
    else (
      (* Self-relative RHS resolution (see [eval_self_relative_value]):
         lets a handler both remember a computed value for later
         (`offered_address = allocate_from_pool(dhcp.range)`, so a
         subsequent handler like `on request` can reference the exact
         same address rather than re-allocating and getting a different
         one) and copy a value a message delivery just wrote onto self
         (`dhcp_address = address`, reading the field [dispatch_message]'s
         payload step already set). Any RHS shape that isn't one of these
         two falls through to plain [expr_to_value], unchanged. *)
      set_field world inst_name field (eval_self_relative_value world ~self:inst_name e))
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
    else if kind = "reconnect" then reconnect_medium world target_path
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
  | SDhcpDiscover (server, address_expr, lease_expr) -> (
    match self with
    | None -> log_action world "(no instance context) dhcp_discover"
    | Some client ->
      if server_ready_for_dhcp world server then (
        let address =
          match expr_to_value address_expr with VIpAddr a -> a | v -> value_to_string v
        in
        let lease_seconds = eval_duration_like world lease_expr in
        ignore (dhcp_discover world ~client ~server ~address ~lease_seconds))
      else (
        (* Real DHCP clients keep trying rather than giving up after one
           failed attempt -- reschedule the exact same statement, so
           re-entering this same [in STATE { }] body isn't needed; the
           retry loop lives entirely in this one rescheduled event. *)
        log_action world
          (Printf.sprintf "%s: %s not ready for dhcp yet, retrying in %gs" client server
             dhcp_retry_delay);
        schedule_at world ~self:(Some client) ~priority:priority_protocol ~delivery:None
          (world.clock +. dhcp_retry_delay)
          (Printf.sprintf "%s: retry dhcp_discover %s" client server)
          [ SDhcpDiscover (server, address_expr, lease_expr) ]))
  | SEmit (ECall (EIdent trigger, args), port) -> (
    match self with
    | None -> log_action world "(no instance context) emit"
    | Some sender ->
      (* Currently replying to someone (a `__reply_to`/`__reply_to_port`
         set while dispatching the message that triggered this body)?
         Target that one peer, at *its* own port (the port it originally
         sent from) -- a directed reply, not a re-flood. Otherwise this
         is a fresh emission: reach *everyone* [emit_targets] finds
         through [port], which is, not incidentally, exactly what a real
         DHCP Discover already is at the Ethernet layer. *)
      let reply_target =
        match
          (get_field world sender "__reply_to", get_field world sender "__reply_to_port")
        with
        | Some (VIdent peer), Some (VIdent peer_port) -> Some (peer, peer_port)
        | _ -> None
      in
      let payload =
        List.map (eval_emit_payload_arg world ~self:sender) args |> List.map (fun (k, v) -> (k, expr_of_value v))
      in
      let targets =
        match reply_target with
        | Some (peer, peer_port) -> [ (peer, peer_port, 0) ]
        | None -> emit_targets world ~from_inst:sender ~port_name:port
      in
      if targets = [] then
        log_action world
          (Printf.sprintf "%s: emit %s through %s reaches nobody (port not connected)" sender trigger port)
      else
        List.iter
          (fun (target, receiving_port, hops) ->
            let delay = 0.3 +. (0.15 *. float_of_int hops) in
            ignore
              (send_via_path world ~from_inst:sender ~to_inst:target ~self:None ~priority:priority_protocol
                 ~delay
                 (Printf.sprintf "%s: %s.%s -> %s.%s" trigger sender port target receiving_port)
                 [ SMessageArrived (target, trigger, receiving_port, sender, port, payload) ]))
          targets)
  | SSchedule (event_name, delay_expr) -> (
    match self with
    | None -> log_action world "(no instance context) schedule"
    | Some name ->
      let delay = eval_duration_like world delay_expr in
      schedule_at world ~self:(Some name) ~priority:priority_application ~delivery:None
        (world.clock +. delay)
        (Printf.sprintf "%s: scheduled %s" name event_name)
        [ SInvokeSelf event_name ])
  | SMessageArrived (target, trigger, receiving_port, sender, sender_port, payload) ->
    dispatch_message world ~trigger ~receiving_port ~sender ~sender_port ~target ~payload
  | SInvokeSelf trigger -> (
    match self with
    | None -> log_action world "(no instance context) invoke self"
    | Some name -> ignore (dispatch_handler world trigger name))
  | _ -> log_action world (Printf.sprintf "(unmodeled) %s" (Pretty.stmt_to_string s))

(* Finds the first handler on [target]'s object type whose trigger name
   matches and whose `in STATE` clause (if any) matches the instance's
   current lifecycle state, and runs its body under that instance's
   context. Does not evaluate `at PORT`/`when EXPR` guards -- those are
   for port-scoped message dispatch (see [dispatch_message] below), a
   different code path for a different kind of trigger (an explicit
   [Invoke]/[SInvokeSelf] has no "which port did this arrive at" to
   check). Mutually recursive with [exec_stmt] now (it wasn't before):
   [SInvokeSelf] needs to call this, and this already called [exec_stmt]
   to run a matched handler's body. *)
and dispatch_handler world trigger target : (string, string) result =
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

(* The receiving half of the port/message mechanism: writes [payload]'s
   fields onto [target], records who sent it ([__reply_to], read by a
   reply's own [emit] -- see [exec_stmt]'s [SEmit] case), finds the first
   handler on [target]'s object type whose trigger, [h_at] (the port --
   the first thing anywhere in this codebase to actually check it; it's
   been parsed and ignored since Phase 2), and `in STATE` guard (same
   [h_in] semantics [dispatch_handler] already uses, unchanged) all
   match, and runs it. No match -- wrong trigger, wrong port, or the
   instance has since moved to a state with no matching handler -- logs
   "dropped" and does nothing further; this silent, structural drop is
   the entire arbitration mechanism (a client that's already committed to
   one server's offer has no [h_in]-matching handler left for a second,
   later offer) and the entire readiness gate (a server not yet in its
   "ready" state has no matching handler for an early discover) -- see
   the plan's Context for why this needed no new conditional-expression
   language feature. *)
and dispatch_message world ~trigger ~receiving_port ~sender ~sender_port ~target ~(payload : (string * expr) list) :
    unit =
  match get_instance world target with
  | None ->
    log_action world
      (Printf.sprintf "message %s at %s: no such instance %s (dropped)" trigger receiving_port target)
  | Some inst ->
    List.iter (fun (k, e) -> set_field world target k (expr_to_value e)) payload;
    set_field world target "__reply_to" (VIdent sender);
    set_field world target "__reply_to_port" (VIdent sender_port);
    (match inst.obj_type with
    | None ->
      log_action world
        (Printf.sprintf "message %s at %s on %s: no object-type behavior bound (dropped)" trigger receiving_port
           target)
    | Some def -> (
      let matches h =
        String.equal h.h_trigger trigger
        && h.h_at = Some receiving_port
        && (match h.h_in with None -> true | Some s -> inst.lifecycle_state = Some s)
      in
      match List.find_opt matches def.handlers with
      | None ->
        log_action world
          (Printf.sprintf "no handler for %s at %s on %s in state %s (dropped)" trigger receiving_port target
             (Option.value inst.lifecycle_state ~default:"<none>"))
      | Some h -> List.iter (exec_stmt world ~self:(Some target)) h.h_body));
    (match get_instance world target with
    | Some inst2 ->
      Hashtbl.remove inst2.fields "__reply_to";
      Hashtbl.remove inst2.fields "__reply_to_port"
    | None -> ())

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
        List.exists (fun (m, gen) -> generation_of world m <> gen) dc.via_media
        || world.topology_epoch <> dc.sent_epoch
    in
    if stale then
      log_action world
        (Printf.sprintf "t=%gs dropped_due_to_link_loss (%s) via %s" ev.due ev.label
           (match ev.delivery with
           | Some dc -> String.concat "," (List.map fst dc.via_media)
           | None -> "?"))
    else (
      log_action world (Printf.sprintf "t=%gs fire (%s)" ev.due ev.label);
      List.iter (exec_stmt world ~self:ev.self) ev.body)
  in
  (* Fires the single globally next-due pending event, then repeats --
     recomputing the minimum from the *current* [world.pending] every
     time, rather than sorting one fixed batch and firing all of it. This
     used to snapshot "everything currently due" into one batch per
     round, sort just that batch, and only reconsider newly-scheduled
     events on the *next* round -- correct as long as everything a
     scenario could schedule was pre-scheduled eagerly upfront (every
     mechanism before the actor message-passing layer works this way:
     DHCP's four hops, a gateway's `after`-chained boot sequence). It
     breaks once something schedules dynamically *while also* having an
     independent, already-pending, later-due timer in the same window --
     exactly what an actor's retry loop (`schedule ... after` sitting
     alongside a message's own reply chain) does. A concrete case that
     surfaced this: a 2s retry timer already pending from round 1, and a
     reply due at 0.9s that only gets scheduled *during* round 1's firing
     (so it can't be in round 1's own batch) -- the retry fired at t=2.0
     before the 0.9s reply got its turn in round 2, i.e. world.clock went
     0.75 -> 2.0 -> 0.9, non-monotonic. Always picking the single next
     -due event avoids this by construction: a newly-scheduled event is
     immediately eligible to win the very next pick, never deferred past
     something that's actually due later. Equivalent to the old batched
     sort for every case that was already correct (repeatedly extracting
     the minimum by the same (due, priority, seq) comparator produces the
     same order a single sort would, for events that already existed;
     it's only the *dynamically added* ones that are now handled
     correctly instead of deferred). *)
  let rec drain iterations =
    if iterations > 100_000 then
      failwith "advance: exceeded iteration budget (a chain of events keeps rescheduling itself)";
    let due_now = List.filter (fun ev -> ev.due <= target +. 1e-9) world.pending in
    match due_now with
    | [] -> ()
    | first :: rest ->
      let earlier a b =
        if a.due <> b.due then a.due < b.due
        else if a.priority <> b.priority then a.priority < b.priority
        else a.seq < b.seq
      in
      let next = List.fold_left (fun best ev -> if earlier ev best then ev else best) first rest in
      world.pending <- List.filter (fun ev -> ev != next) world.pending;
      world.clock <- next.due;
      fire next;
      drain (iterations + 1)
  in
  drain 0;
  world.clock <- target

(* A printer's (or any client's) *currently usable* address: its bound
   DHCP lease if it has one, otherwise a statically configured `address`
   field -- but only if that field actually holds a real address rather
   than the unresolved `dhcp` placeholder ident a scenario writes before
   any handshake has run (e.g. `instance printer : label_printer { address
   = dhcp }` in clinic_printer.nsdl). Used by [PrintJob]'s misdirection
   check below: comparing what a client *remembers* as the target
   (`print_target`) against what the printer's address actually *is* right
   now is the whole mechanism behind the "stale address" fault --
   `stale_printer_target.nsdl` already sets exactly this mismatch, it just
   had nothing that executed against it until now. *)
let printer_current_address world printer_name : value option =
  match get_field world printer_name "dhcp_state" with
  | Some (VIdent "bound") -> get_field world printer_name "dhcp_address"
  | _ -> (
    match get_field world printer_name "address" with
    | Some (VIpAddr _ as v) -> Some v
    | _ -> None)

(* Phase 4: a derived, read-only network-status projection computed
   from canonical fields on demand -- never stored, so there is nothing
   to get out of sync between "canonical state" and "what inspect shows
   you" (the proposal's "displayed_state == project(canonical_state,
   observer_context)" conformance property). This is the proposal's
   NetworkStatus struct, function-shaped instead of field-shaped: one
   function computes any of its eight named facts on demand from
   whatever canonical state actually exists. Facts this codebase has no
   real causal basis for yet -- carrier needs link training,
   dns/service_readiness need those layers -- honestly report their
   "nothing modeled yet" default rather than fabricating something that
   merely looks derived. l2_reachability is in the same boat for a
   different reason: [resolve_path] (see below) can now answer "is
   there a route between these two instances," but that's a two-instance
   question and this projection is per-instance, so there's no second
   endpoint to check reachability against here without changing this
   function's shape -- left as "nothing modeled" rather than picking an
   arbitrary target. As DNS and services get built in later phases, this
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
      | "l2_reachability" -> Some "unavailable" (* not modeled: no second endpoint to check reachability against *)
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
      address : string;
      lease_seconds : float;
    } (* medium(s) are resolved automatically from the declared topology *)
  | InspectAs of {
      world_name : string;
      instance : string;
      local_field : string;
    } (* resolves local_field through world_name's binding, then delegates to Inspect *)
  | InvokeAs of { world_name : string; local_trigger : string; target : string }
    (* resolves local_trigger through world_name's binding, then delegates to Invoke *)
  | Ping of {
      from_inst : string;
      to_inst : string; (* descriptive only when [via_gateway] is set -- see below *)
      via_gateway : string option;
      (* [None]: a plain LAN ping, routed from_inst -> to_inst directly.
         [Some gateway]: a WAN-crossing ping -- there is no modeled
         "internet" instance (the reference slice's own device list has
         none), so the actual round trip runs from_inst <-> gateway (the
         real, physical, revalidated part of the path) gated on
         [gateway_wan_ready], while [to_inst] is recorded purely as the
         logical name of what was pinged. *)
    }
  | PrintJob of { client : string; printer : string; content : string }
  | ThreadSight of string (* medium instance name -- physical-layer facts only *)
  | PacketSight of { from_inst : string; to_inst : string }
    (* the observed route and the most recent logged outcome between them *)

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
  | DhcpDiscover { client; server; address; lease_seconds } ->
    log_action world
      (Printf.sprintf "dhcp: %s discovering (server %s, offering %s)" client server address);
    if not (server_ready_for_dhcp world server) then
      OError
        (Printf.sprintf "%s is not ready to serve DHCP yet (lifecycle state: %s)" server
           (match get_instance world server with
           | Some inst -> Option.value inst.lifecycle_state ~default:"<none>"
           | None -> "<no such instance>"))
    else (
      match dhcp_discover world ~client ~server ~address ~lease_seconds with
      | Ok () -> OAck (Printf.sprintf "dhcp handshake scheduled: %s <-> %s" client server)
      | Error msg -> OError msg)
  | Ping { from_inst; to_inst; via_gateway } ->
    log_action world
      (Printf.sprintf "ping: %s -> %s%s" from_inst to_inst
         (match via_gateway with Some g -> Printf.sprintf " (WAN, via %s)" g | None -> ""));
    let route_target = Option.value via_gateway ~default:to_inst in
    let readiness_error =
      match via_gateway with
      | None -> None
      | Some gateway ->
        if gateway_wan_ready world gateway then None
        else
          Some
            (Printf.sprintf "%s is not WAN-ready yet (lifecycle state: %s)" gateway
               (match get_instance world gateway with
               | Some inst -> Option.value inst.lifecycle_state ~default:"<none>"
               | None -> "<no such instance>"))
    in
    (match readiness_error with
    | Some msg -> OError msg
    | None ->
      let sent =
        send_via_path world ~from_inst ~to_inst:route_target ~self:None
          ~priority:priority_application ~delay:0.1
          (Printf.sprintf "ping request %s -> %s" from_inst route_target)
          [ SAssign (EField (EIdent route_target, "last_ping_from"), EIdent from_inst) ]
      in
      if not sent then OError (Printf.sprintf "no route from %s to %s" from_inst route_target)
      else (
        ignore
          (send_via_path world ~from_inst:route_target ~to_inst:from_inst ~self:None
             ~priority:priority_application ~delay:0.2
             (Printf.sprintf "ping reply %s -> %s" route_target from_inst)
             [
               SAssign (EField (EIdent from_inst, "last_ping_target"), EIdent to_inst);
               SAssign (EField (EIdent from_inst, "last_ping_result"), EIdent "success");
             ]);
        OAck (Printf.sprintf "ping scheduled: %s -> %s" from_inst to_inst)))
  | PrintJob { client; printer; content } ->
    log_action world (Printf.sprintf "print_job: %s -> %s (%s)" client printer content);
    let misdirected =
      match get_field world client "print_target" with
      | Some (VIpAddr target) -> (
        match printer_current_address world printer with
        | Some (VIpAddr actual) -> actual <> target
        | Some _ -> false
        | None -> true (* client remembers a target; printer has no current address to match it against *))
      | _ -> false
    in
    if misdirected then
      OError
        (Printf.sprintf
           "%s.print_target does not match %s's current address -- job sent to a stale address"
           client printer)
    else (
      let sent =
        send_via_path world ~from_inst:client ~to_inst:printer ~self:None
          ~priority:priority_application ~delay:0.5
          (Printf.sprintf "print job %s -> %s" client printer)
          [
            SAssign (EField (EIdent printer, "label_printed"), EIdent "true");
            SAssign
              ( EField (EIdent printer, "label_content_valid"),
                EIdent (if content = "" then "false" else "true") );
          ]
      in
      if sent then OAck (Printf.sprintf "print job scheduled: %s -> %s" client printer)
      else OError (Printf.sprintf "no route from %s to %s" client printer))
  | ThreadSight medium_name ->
    log_action world (Printf.sprintf "thread_sight %s" medium_name);
    (match get_instance world medium_name with
    | None -> OError (Printf.sprintf "no such medium: %s" medium_name)
    | Some _ ->
      let physical_state =
        match get_field world medium_name "physical_state" with
        | Some (VIdent s) -> s
        | _ -> "attached" (* never disconnected *)
      in
      let generation = generation_of world medium_name in
      let endpoints_str =
        match List.find_opt (fun (_, _, m) -> m = medium_name) world.connections with
        | Some (a, b, _) -> Printf.sprintf "%s <-> %s" a b
        | None -> "(not connected to anything)"
      in
      OValue
        (VString
           (Printf.sprintf "<%s physical_state=%s generation=%d %s>" medium_name physical_state
              generation endpoints_str)))
  | PacketSight { from_inst; to_inst } ->
    log_action world (Printf.sprintf "packet_sight %s -> %s" from_inst to_inst);
    let path_str =
      match resolve_path world ~from_inst ~to_inst with
      | None -> "no route"
      | Some hops ->
        String.concat " -> "
          (from_inst :: List.map (fun (node, medium) -> Printf.sprintf "%s(via %s)" node medium) hops)
    in
    let fate =
      List.find_opt
        (fun line -> string_contains ~needle:from_inst line && string_contains ~needle:to_inst line)
        world.log
    in
    OValue
      (VString
         (Printf.sprintf "path: %s | last: %s" path_str
            (Option.value fate ~default:"<no traffic observed yet>")))
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

(* Loads any number of already-parsed programs into a fresh world, in two
   passes so object-type and profile registration never depend on source
   order: every `object` definition and `profile` block across all
   sources is registered first, then every `scenario` (first one wins),
   `incident` (every one applied as an overlay), and `at`/`between`
   schedule block (every one registered) is processed. [source_label] is
   only used to name the sources in the "no scenario found" error message.
   Raises [Failure] if no scenario is found across all of them. Shared by
   [load_files] (paths, real filesystem) and [load_sources] (in-memory
   name/content pairs, e.g. from a browser with no filesystem) -- both
   just differ in how they produce [progs]. *)
let load_programs ~source_label (progs : program list) : world =
  let world = create () in
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
    failwith (Printf.sprintf "no scenario found across: %s" source_label);
  world

(* Parses one real file from disk. Raises [Nsdl.Lexer.Lex_error] or
   [Nsdl.Parser.Error] on a malformed file. *)
let parse_file path =
  let ic = open_in path in
  let lexbuf = Lexing.from_channel ic in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () -> Parser.program Lexer.token lexbuf)

let load_files (paths : string list) : world =
  let progs = List.map parse_file paths in
  load_programs ~source_label:(String.concat ", " paths) progs

(* Parses NSDL source text directly, with no filesystem involved -- the
   wasm/browser entry point (web/nsdl_web.ml) has no real files to open,
   only in-memory strings a page already has (e.g. fetched or pasted
   text). Same grammar entry point as [parse_file], just over
   [Lexing.from_string] instead of [Lexing.from_channel]. *)
let parse_source (content : string) : program = Parser.program Lexer.token (Lexing.from_string content)

(* Like [load_files], but from a list of (name, content) pairs instead of
   real paths -- [name] is used only for the "no scenario found" error
   message, exactly as a path would be. *)
let load_sources (sources : (string * string) list) : world =
  let progs = List.map (fun (_, content) -> parse_source content) sources in
  load_programs ~source_label:(String.concat ", " (List.map fst sources)) progs
