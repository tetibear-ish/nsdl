(* A thin JS API over Nsdl.Sim, compiled to WebAssembly via js_of_ocaml's
   wasm_of_ocaml backend (dune's `(modes wasm)`, see web/dune). Not a
   reimplementation of anything in Sim -- every method here just adapts
   one Sim.perform call (or Sim.advance / Sim.load_sources) to JS-friendly
   types, and mirrors bin/tui.ml's own command vocabulary (inspect/set/
   invoke/advance) rather than inventing a different one for the browser,
   since that vocabulary is already the documented surface over Sim.

   There is no filesystem in a browser, so this loads from in-memory
   (name, content) source pairs via Sim.load_sources rather than
   Sim.load_files -- see lib/sim.ml's comment on why that function exists. *)

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
         Js.to_float is the documented explicit conversion. *)
      Nsdl.Sim.advance world (Js.to_float seconds);
      Js.string (Printf.sprintf "t = %gs" world.Nsdl.Sim.clock)
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
     end)
