(* A thin JS API over Nsdl.Sim, compiled to WebAssembly via js_of_ocaml's
   wasm_of_ocaml backend (dune's `(modes wasm)`, see web/dune). Not a
   reimplementation of anything in Sim -- every method here just adapts
   one Sim.perform call (or Sim.advance / Sim.load_sources / the two
   medium-connectivity functions that aren't Sim.action variants) to
   JS-friendly types, and mirrors bin/tui.ml's own command vocabulary
   (inspect/set/invoke/advance/power_cycle/restart_service) plus the
   newer actions (DhcpDiscover, Ping, PrintJob, ThreadSight, PacketSight,
   disconnect/reconnect) that aren't wired into either TUI yet -- this is
   the first place those become interactively reachable outside the test
   suite, using the same names Sim itself already gave them rather than
   inventing a different vocabulary for the browser.

   There is no filesystem in a browser, so this loads from in-memory
   (name, content) source pairs via Sim.load_sources rather than
   Sim.load_files -- see lib/sim.ml's comment on why that function exists.

   Convention used throughout for "no value" JS-side: an empty string,
   not null/undefined -- e.g. `ping`'s optional via-gateway argument, or
   an instance with no lifecycle state. Every real value in this API
   (instance names, medium names, lifecycle states) is a non-empty
   identifier, so this is unambiguous, and it avoids relying on
   Js.Opt/Js.Optdef conversions this module doesn't otherwise need. *)

open Js_of_ocaml

class type source_js =
  object
    method name : Js.js_string Js.t Js.readonly_prop
    method content : Js.js_string Js.t Js.readonly_prop
  end

let observation_to_js (o : Nsdl.Sim.observation) =
  let kind, text =
    match o with
    | Nsdl.Sim.OValue v -> ("value", Nsdl.Sim.value_to_string v)
    | Nsdl.Sim.OAck m -> ("ack", m)
    | Nsdl.Sim.OError m -> ("error", m)
  in
  object%js
    val kind = Js.string kind
    val text = Js.string text
  end

(* Mirrors bin/tui.ml's own parse_value -- each presentation layer over
   Sim already owns its own value parsing (tui.ml and tui_nottui.ml don't
   share this either), since it's about *this* surface's input format,
   not something Sim itself should know about. *)
let parse_value (s : string) : Nsdl.Sim.value =
  match int_of_string_opt s with
  | Some i -> Nsdl.Sim.VInt i
  | None -> (
    match float_of_string_opt s with
    | Some f -> Nsdl.Sim.VFloat f
    | None ->
      let is_ip =
        String.length s > 0
        && String.for_all (fun c -> (c >= '0' && c <= '9') || c = '.' || c = '/') s
        && String.contains s '.'
      in
      if is_ip then Nsdl.Sim.VIpAddr s
      else if s = "true" then Nsdl.Sim.VBool true
      else if s = "false" then Nsdl.Sim.VBool false
      else Nsdl.Sim.VIdent s)

(* ------------------------------------------------------------------ *)
(* state(): a read-only projection of world for the monitoring panel -- *)
(* the exact same public `world` record fields bin/tui.ml's `render`    *)
(* already reads directly, just shaped as JS values instead of printed  *)
(* text. No new Sim behavior.                                           *)
(* ------------------------------------------------------------------ *)

let field_js key value =
  object%js
    val key = Js.string key
    val value = Js.string value
  end

let instance_js name (inst : Nsdl.Sim.instance) =
  let fields =
    Hashtbl.fold (fun k v acc -> (k, Nsdl.Sim.value_to_string v) :: acc) inst.Nsdl.Sim.fields []
    |> List.sort (fun (a, _) (b, _) -> compare a b)
    |> List.map (fun (k, v) -> field_js k v)
    |> Array.of_list |> Js.array
  in
  object%js
    val name = Js.string name
    val type_ = Js.string inst.Nsdl.Sim.inst_type
    val state = Js.string (Option.value inst.Nsdl.Sim.lifecycle_state ~default:"")
    val fields = fields
  end

let connection_js (a, b, medium) =
  object%js
    val a = Js.string a
    val b = Js.string b
    val medium = Js.string medium
  end

let pending_js (ev : Nsdl.Sim.scheduled_event) =
  object%js
    val due = Js.float ev.Nsdl.Sim.due
    val label = Js.string ev.Nsdl.Sim.label
  end

(* Last 30 log lines (already most-recent-first, same as world.log itself
   and the same order bin/tui.ml prints its own "recent actions" list) --
   a plain cap so a long playground session doesn't hand the page an
   ever-growing array. *)
let log_js (world : Nsdl.Sim.world) =
  world.Nsdl.Sim.log |> List.filteri (fun i _ -> i < 30) |> List.map Js.string |> Array.of_list
  |> Js.array

let state_js (world : Nsdl.Sim.world) =
  let instances =
    Hashtbl.fold (fun name inst acc -> (name, inst) :: acc) world.Nsdl.Sim.instances []
    |> List.sort (fun (a, _) (b, _) -> compare a b)
    |> List.map (fun (name, inst) -> instance_js name inst)
    |> Array.of_list |> Js.array
  in
  let connections = world.Nsdl.Sim.connections |> List.map connection_js |> Array.of_list |> Js.array in
  let pending =
    world.Nsdl.Sim.pending
    |> List.sort (fun a b -> compare a.Nsdl.Sim.due b.Nsdl.Sim.due)
    |> List.map pending_js |> Array.of_list |> Js.array
  in
  object%js
    val clock = Js.float world.Nsdl.Sim.clock
    val instances = instances
    val connections = connections
    val pending = pending
    val log = log_js world
  end

(* ------------------------------------------------------------------ *)
(* The world handle: every command the console panel can dispatch.      *)
(* ------------------------------------------------------------------ *)

let make_world_handle (world : Nsdl.Sim.world) =
  object%js
    method inspect (path : Js.js_string Js.t) =
      observation_to_js (Nsdl.Sim.perform world (Nsdl.Sim.Inspect (Js.to_string path)))

    method configure (path : Js.js_string Js.t) (value : Js.js_string Js.t) =
      observation_to_js
        (Nsdl.Sim.perform world
           (Nsdl.Sim.Configure (Js.to_string path, parse_value (Js.to_string value))))

    method invoke (trigger : Js.js_string Js.t) (target : Js.js_string Js.t) =
      observation_to_js
        (Nsdl.Sim.perform world (Nsdl.Sim.Invoke (Js.to_string trigger, Js.to_string target)))

    method advance (seconds : Js.number Js.t) =
      (* wasm_of_ocaml, unlike js_of_ocaml's JS backend, does not map OCaml
         floats onto JS numbers automatically -- a plain `float` parameter
         here reads whatever bit pattern the wasm GC representation hands
         it, not the JS number's value, and corrupts the very next float
         operation ("illegal cast" from Sim.advance's internal arithmetic).
         Js.to_float is the documented explicit conversion -- and the same
         applies in reverse (Js.float) for every float this module hands
         back to JS below (state()'s clock/due fields, dhcpDiscover's
         lease). *)
      Nsdl.Sim.advance world (Js.to_float seconds);
      Js.string (Printf.sprintf "t = %gs" world.Nsdl.Sim.clock)

    method powerCycle (target : Js.js_string Js.t) =
      observation_to_js (Nsdl.Sim.perform world (Nsdl.Sim.PowerCycle (Js.to_string target)))

    method restartService (target : Js.js_string Js.t) =
      observation_to_js (Nsdl.Sim.perform world (Nsdl.Sim.RestartService (Js.to_string target)))

    method dhcpDiscover
        (client : Js.js_string Js.t)
        (server : Js.js_string Js.t)
        (address : Js.js_string Js.t)
        (leaseSeconds : Js.number Js.t) =
      observation_to_js
        (Nsdl.Sim.perform world
           (Nsdl.Sim.DhcpDiscover
              {
                client = Js.to_string client;
                server = Js.to_string server;
                address = Js.to_string address;
                lease_seconds = Js.to_float leaseSeconds;
              }))

    (* viaGateway: "" means a plain LAN ping (Sim.Ping's via_gateway =
       None); any other string names the gateway to gate WAN readiness
       against (via_gateway = Some ...) -- see the module-level comment
       on the empty-string convention this API uses throughout. *)
    method ping
        (fromInst : Js.js_string Js.t) (toInst : Js.js_string Js.t) (viaGateway : Js.js_string Js.t) =
      let via_gateway = match Js.to_string viaGateway with "" -> None | s -> Some s in
      observation_to_js
        (Nsdl.Sim.perform world
           (Nsdl.Sim.Ping { from_inst = Js.to_string fromInst; to_inst = Js.to_string toInst; via_gateway }))

    method printJob
        (client : Js.js_string Js.t) (printer : Js.js_string Js.t) (content : Js.js_string Js.t) =
      observation_to_js
        (Nsdl.Sim.perform world
           (Nsdl.Sim.PrintJob
              { client = Js.to_string client; printer = Js.to_string printer; content = Js.to_string content }))

    method threadSight (medium : Js.js_string Js.t) =
      observation_to_js (Nsdl.Sim.perform world (Nsdl.Sim.ThreadSight (Js.to_string medium)))

    method packetSight (fromInst : Js.js_string Js.t) (toInst : Js.js_string Js.t) =
      observation_to_js
        (Nsdl.Sim.perform world
           (Nsdl.Sim.PacketSight { from_inst = Js.to_string fromInst; to_inst = Js.to_string toInst }))

    (* disconnect/reconnect aren't Sim.action variants -- same as
       test/harness.ml, this calls Sim.disconnect_medium/reconnect_medium
       directly and synthesizes an ack-shaped observation so the console
       panel's output formatting stays uniform across every command. *)
    method disconnect (medium : Js.js_string Js.t) =
      let name = Js.to_string medium in
      Nsdl.Sim.disconnect_medium world name;
      observation_to_js (Nsdl.Sim.OAck (Printf.sprintf "disconnected %s" name))

    method reconnect (medium : Js.js_string Js.t) =
      let name = Js.to_string medium in
      Nsdl.Sim.reconnect_medium world name;
      observation_to_js (Nsdl.Sim.OAck (Printf.sprintf "reconnecting %s (link training)" name))

    method state = state_js world
  end

let load_sources (sources : source_js Js.t Js.js_array Js.t) =
  let srcs =
    Js.to_array sources |> Array.to_list
    |> List.map (fun s -> (Js.to_string s##.name, Js.to_string s##.content))
  in
  make_world_handle (Nsdl.Sim.load_sources srcs)

let () =
  Js.export "Nsdl"
    (object%js
       method loadSources sources = load_sources sources

       (* Reuses the exact same duration grammar ("2m30s", "1h", ...) the
          TUIs and test/harness.ml already parse via Nsdl.Lexer, rather
          than reimplementing it in JS -- so the console's `advance`
          command has real parity with bin/tui.ml's, not a downgraded
          seconds-only version. Raises (as a catchable JS exception) on a
          malformed duration, same as everywhere else this function is
          used. *)
       method parseDuration (s : Js.js_string Js.t) = Js.float (Nsdl.Lexer.parse_duration (Js.to_string s))
     end)
