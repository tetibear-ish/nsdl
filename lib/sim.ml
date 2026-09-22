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
}

let build_object_def (body : stmt list) : object_def =
  let ports = ref [] and memories = ref [] and lifecycles = ref [] and handlers = ref [] in
  let on_enter = Hashtbl.create 8 in
  List.iter
    (fun s ->
      match s with
      | SPort (n, t) -> ports := (n, t) :: !ports
      | SMemory (n, t, p) -> memories := (n, t, p) :: !memories
      | SLifecycleDecl (n, init) -> lifecycles := (n, init) :: !lifecycles
      | SHandler h -> handlers := h :: !handlers
      | SLifecycleBlock (st, b) -> Hashtbl.replace on_enter st b
      | _ -> ())
    body;
  {
    ports = List.rev !ports;
    memories = List.rev !memories;
    lifecycles = List.rev !lifecycles;
    handlers = List.rev !handlers;
    on_enter;
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

(* A pending schedule-block body or delayed lifecycle transition, due at
   an absolute virtual-clock time. Same-time events fire in
   [(priority, seq)] order -- [seq] (insertion order) only breaks ties
   *within* a priority class, it does not override priority. [self],
   when set, is the instance the body's bare (unqualified) field names
   and `transition`/`clear` statements are relative to -- top-level
   schedule blocks leave this [None] and require fully-qualified paths,
   same as before. *)
type scheduled_event = {
  due : float;
  priority : int;
  seq : int;
  label : string;
  self : string option;
  body : stmt list;
}

type world = {
  instances : (string, instance) Hashtbl.t;
  object_defs : (string, object_def) Hashtbl.t;
  mutable connections : (string * string * string) list; (* from, to, medium *)
  mutable log : string list; (* most recent action first *)
  mutable clock : float; (* virtual seconds elapsed *)
  mutable pending : scheduled_event list;
  mutable next_seq : int;
}

let create () =
  {
    instances = Hashtbl.create 16;
    object_defs = Hashtbl.create 16;
    connections = [];
    log = [];
    clock = 0.0;
    pending = [];
    next_seq = 0;
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
  snap_instances : (string * instance_snapshot) list;
  snap_connections : (string * string * string) list;
  snap_log : string list;
  snap_clock : float;
  snap_pending : scheduled_event list;
  snap_next_seq : int;
}

let snapshot (world : world) : snapshot =
  {
    snap_object_defs = world.object_defs;
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
    connections = snap.snap_connections;
    log = snap.snap_log;
    clock = snap.snap_clock;
    pending = snap.snap_pending;
    next_seq = snap.snap_next_seq;
  }

let log_action world msg = world.log <- msg :: world.log

let get_instance world name = Hashtbl.find_opt world.instances name

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

let expr_to_seconds (e : expr) : float =
  match e with
  | EDuration d -> d
  | EInt i -> float_of_int i
  | EFloat f -> f
  | _ -> invalid_arg (Printf.sprintf "expr_to_seconds: not a duration: %s" (Pretty.expr_to_string e))

(* Handles the one duration-shaped expression form that isn't a plain
   literal: `random(A .. B)`. NOT deterministic/replayable yet -- the
   proposal wants named streams derived from the scenario seed; this is
   an honest placeholder (unseeded `Stdlib.Random`) until that lands. *)
let eval_duration_like (e : expr) : float =
  match e with
  | ECall (EIdent "random", APos (ERange (a, b)) :: _) ->
    let lo = expr_to_seconds a and hi = expr_to_seconds b in
    lo +. Random.float (hi -. lo)
  | _ -> expr_to_seconds e

let schedule_at world ~self ~priority due label body =
  let seq = world.next_seq in
  world.next_seq <- seq + 1;
  world.pending <- { due; priority; seq; label; self; body } :: world.pending

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
    set_field world inst_name field (expr_to_value e)
  | SInject (kind, amount, target) ->
    let inst_name, field = split_path (path_of_expr target ^ "." ^ kind) in
    let v = expr_to_value amount in
    set_field world inst_name field v;
    log_action world
      (Printf.sprintf "inject %s %s on %s" kind (value_to_string v) (path_of_expr target))
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
      let delay = eval_duration_like delay_expr in
      schedule_at world ~self:(Some name) ~priority:priority_physical (world.clock +. delay)
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
    schedule_at world ~self:None ~priority:priority_physical (expr_to_seconds d) "at" body
  | TBetween (a, b, every, body) ->
    let a = expr_to_seconds a and b = expr_to_seconds b and every = expr_to_seconds every in
    if every <= 0.0 then invalid_arg "load_schedule: `every` duration must be positive";
    let t = ref a in
    while !t <= b +. 1e-9 do
      schedule_at world ~self:None ~priority:priority_physical !t "between...every" body;
      t := !t +. every
    done
  | _ -> invalid_arg "load_schedule: expected a TAt or TBetween"

(* Advances the virtual clock by [by] seconds and fires every pending
   event now due, in (time, priority class, insertion order) -- the
   v0.3 proposal's normative same-timestamp ordering. Firing an event
   applies its body statements to world state via [exec_stmt] under
   that event's own [self] context; it does not reschedule itself, so a
   `between ... every` block's repeated occurrences must already have
   been registered individually by [load_schedule]. *)
let advance world (by : float) =
  let target = world.clock +. by in
  let due, not_due = List.partition (fun ev -> ev.due <= target +. 1e-9) world.pending in
  let due =
    List.sort
      (fun a b ->
        if a.due <> b.due then compare a.due b.due
        else if a.priority <> b.priority then compare a.priority b.priority
        else compare a.seq b.seq)
      due
  in
  world.pending <- not_due;
  world.clock <- target;
  List.iter
    (fun ev ->
      log_action world (Printf.sprintf "t=%gs fire (%s)" ev.due ev.label);
      List.iter (exec_stmt world ~self:ev.self) ev.body)
    due

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

type action =
  | Inspect of string
  | Configure of string * value
  | PowerCycle of string
  | RestartService of string
  | Invoke of string * string (* trigger, target instance *)

type observation =
  | OValue of value
  | OAck of string
  | OError of string

let perform world (a : action) : observation =
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
    else (
      match get_field world inst_name field with
      | Some v -> OValue v
      | None -> OError (Printf.sprintf "no such field: %s" path))
  | Configure (path, v) ->
    let inst_name, field = split_path path in
    log_action world (Printf.sprintf "configure %s = %s" path (value_to_string v));
    if field = "" then OError (Printf.sprintf "cannot configure a whole instance: %s" path)
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

(* Parses and loads any number of .nsdl files into a fresh world, in two
   passes so object-type registration never depends on file order:
   every `object` definition across all files is registered first, then
   every `scenario` (first one wins), `incident` (every one applied as
   an overlay), and `at`/`between` schedule block (every one
   registered) is processed. Raises [Nsdl.Lexer.Lex_error] or
   [Nsdl.Parser.Error] on a malformed file, or [Failure] if no scenario
   is found across all of them. *)
let parse_file path =
  let ic = open_in path in
  let lexbuf = Lexing.from_channel ic in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () -> Parser.program Lexer.token lexbuf)

let load_files (paths : string list) : world =
  let world = create () in
  let progs = List.map parse_file paths in
  List.iter (List.iter (function TObject _ as t -> load_object world t | _ -> ())) progs;
  let scenario_loaded = ref false in
  List.iter
    (List.iter (function
      | TObject _ -> ()
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
