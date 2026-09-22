(* Full interactive terminal UI over Nsdl.Sim's state model, built on
   Nottui (nottui/nottui-unix/lwd) + Notty's image algebra
   (notty-community), instead of bin/tui.ml's hand-rolled ANSI/Bytes.t
   rendering and manual Unix.select input-polling loop.

   Same command set and semantics as bin/tui.ml: inspect, set,
   power_cycle, restart_service, invoke, advance, live, stop/pause,
   quit -- see that file's own header comment for what each does and the
   determinism rationale behind `advance`/`live`. This file focuses on
   what's structurally different:

   - The diagram (switch/port/device boxes) is unchanged from the
     earlier tui_nottui.ml prototype: each port is one self-contained
     column image, every row [I.hsnap]-ed to the column's own max
     width, so the connector is always centered on its own box by
     construction -- see [port_column] below.
   - Everything reactive hangs off one [world_version : int Lwd.var],
     bumped after any command that mutates [world]; each display panel
     is [Lwd.map]ped from it, so only touching the var causes a
     redraw -- no manual "clear screen and reprint everything" pass.
   - `live` ticking uses [Nottui_unix.run]'s own [?tick_period]/[?tick]
     polling instead of a hand-rolled [Unix.select] loop: the tick
     callback is a no-op unless `live` is active, so there's one run
     loop for the whole program's lifetime rather than two nested ones.
   - The command line is [Nottui_widgets.edit_field], so typing no
     longer waits on a line-buffered blocking read the way bin/tui.ml's
     did -- `live` ticking updates the screen while a command is being
     typed, not just between lines.

   Usage: tui_nottui.exe <file.nsdl> [file.nsdl ...] *)

open Notty
open Notty.Infix
open Nottui

let attr = A.empty

let load_world = Nsdl.Sim.load_files

(* "switch.port[2]" -> Some ("switch", 2); anything else -> None. *)
let parse_port_ref (path : string) : (string * int) option =
  match String.index_opt path '[' with
  | None -> None
  | Some lb -> (
    match String.index_opt path ']' with
    | Some rb when rb > lb + 1 -> (
      let prefix = String.sub path 0 lb in
      let num_str = String.sub path (lb + 1) (rb - lb - 1) in
      match (String.rindex_opt prefix '.', int_of_string_opt num_str) with
      | Some dot, Some n -> Some (String.sub prefix 0 dot, n)
      | _ -> None)
    | _ -> None)

let switch_port_count (inst : Nsdl.Sim.instance) : int option =
  match Hashtbl.find_opt inst.Nsdl.Sim.fields "ports" with
  | Some (Nsdl.Sim.VInt n) when n > 0 -> Some n
  | _ -> None

let bordered_box (lines : string list) =
  let w = List.fold_left (fun w l -> max w (String.length l)) 0 lines in
  let border = I.string attr ("+" ^ String.make w '-' ^ "+") in
  let mid l = I.string attr ("|" ^ l ^ String.make (w - String.length l) ' ' ^ "|") in
  I.vcat ((border :: List.map mid lines) @ [ border ])

let device_box remote_path medium =
  let dev, rest = Nsdl.Sim.split_path remote_path in
  let annotation = Printf.sprintf "(%s%s)" (if rest = "" then "" else rest ^ " via ") medium in
  (bordered_box [ dev ], annotation)

(* One self-contained column per port: number box, always; connector,
   device box, and annotation only if something's plugged in. Every row
   is hsnapped to the column's own max width, so the connector is
   guaranteed centered over its own box -- there is no separate
   position to compute, and therefore nothing to get out of sync. *)
let port_column n remote_opt =
  let num_box = bordered_box [ string_of_int n ] in
  match remote_opt with
  | None -> num_box
  | Some (remote_path, medium) ->
    let box, annotation = device_box remote_path medium in
    let w = List.fold_left max (I.width num_box) [ I.width box; String.length annotation ] in
    let center img = I.hsnap ~align:`Middle w img in
    I.vcat [ center num_box; center (I.string attr "|"); center box; center (I.string attr annotation) ]

let switch_diagram world name (inst : Nsdl.Sim.instance) port_count =
  let remote_at = Array.make (port_count + 1) None in
  List.iter
    (fun (a, b, medium) ->
      let try_side self other =
        match parse_port_ref self with
        | Some (dev, n) when dev = name && n >= 1 && n <= port_count ->
          remote_at.(n) <- Some (other, medium)
        | _ -> ()
      in
      try_side a b;
      try_side b a)
    world.Nsdl.Sim.connections;
  let columns =
    List.init port_count (fun i -> port_column (i + 1) remote_at.(i + 1))
  in
  let gap = I.void 1 1 in
  let ports_row =
    List.fold_left (fun acc col -> if acc = I.empty then col else acc <|> gap <|> col) I.empty
      columns
  in
  let title = Printf.sprintf " %s : %s " name inst.Nsdl.Sim.inst_type in
  let title_box = I.hsnap ~align:`Middle (I.width ports_row) (bordered_box [ title ]) in
  title_box <-> ports_row

let render_network_diagram world =
  let switches =
    Hashtbl.fold
      (fun name inst acc ->
        match switch_port_count inst with Some n -> (name, inst, n) :: acc | None -> acc)
      world.Nsdl.Sim.instances []
  in
  let switches = List.sort (fun (a, _, _) (b, _, _) -> compare a b) switches in
  let diagrams = List.map (fun (name, inst, n) -> switch_diagram world name inst n) switches in
  List.fold_left (fun acc d -> if acc = I.empty then d else acc <-> I.void 0 1 <-> d) I.empty
    diagrams

let instances_image world =
  let names = Hashtbl.fold (fun k _ acc -> k :: acc) world.Nsdl.Sim.instances [] in
  let names = List.sort compare names in
  I.vcat
    (List.map
       (fun name ->
         let inst = Hashtbl.find world.Nsdl.Sim.instances name in
         let header = I.string attr (Printf.sprintf "[%s : %s]" name inst.Nsdl.Sim.inst_type) in
         let fields = Hashtbl.fold (fun k v acc -> (k, v) :: acc) inst.Nsdl.Sim.fields [] in
         let fields = List.sort (fun (a, _) (b, _) -> compare a b) fields in
         let field_lines =
           List.map
             (fun (k, v) ->
               I.string attr (Printf.sprintf "  %-24s = %s" k (Nsdl.Sim.value_to_string v)))
             fields
         in
         I.vcat (header :: field_lines))
       names)

let pending_image world =
  let pending = List.sort (fun a b -> compare a.Nsdl.Sim.due b.Nsdl.Sim.due) world.Nsdl.Sim.pending in
  if pending = [] then I.empty
  else
    I.vcat
      (I.string attr "pending events:"
      :: List.map
           (fun ev -> I.string attr (Printf.sprintf "  t=%gs  %s" ev.Nsdl.Sim.due ev.Nsdl.Sim.label))
           pending)

let log_image world =
  let recent = List.filteri (fun i _ -> i < 5) world.Nsdl.Sim.log in
  if recent = [] then I.empty
  else
    I.vcat
      (I.string attr "recent actions:" :: List.map (fun l -> I.string attr ("  " ^ l)) (List.rev recent))

let clock_image world = I.string attr (Printf.sprintf "t = %gs" world.Nsdl.Sim.clock)

let help_text =
  "commands: inspect PATH | set PATH VALUE | power_cycle TARGET | restart_service TARGET | \
   invoke TRIGGER TARGET | advance DURATION | live [SPEED] | stop | quit"

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

let () =
  let paths = List.tl (Array.to_list Sys.argv) in
  if paths = [] then (
    Printf.eprintf "usage: tui_nottui.exe <file.nsdl> [file.nsdl ...]\n";
    exit 1);
  let world =
    try load_world paths with
    | Nsdl.Lexer.Lex_error msg ->
      Printf.eprintf "lex error: %s\n" msg;
      exit 1
    | Nsdl.Parser.Error ->
      Printf.eprintf "parse error\n";
      exit 1
    | Failure msg ->
      Printf.eprintf "%s\n" msg;
      exit 1
  in

  let world_version = Lwd.var 0 in
  let bump () = Lwd.set world_version (Lwd.peek world_version + 1) in
  let status = Lwd.var "type a command, or \"live\" to start ticking" in
  let set_status s = Lwd.set status s in
  let input = Lwd.var ("", 0) in
  let quit = Lwd.var false in

  let live_on = ref false in
  let live_speed = ref 1.0 in
  let last_tick = ref (Unix.gettimeofday ()) in

  let on_tick () =
    let now = Unix.gettimeofday () in
    if !live_on then (
      let elapsed = now -. !last_tick in
      last_tick := now;
      Nsdl.Sim.advance world (elapsed *. !live_speed);
      bump ())
    else last_tick := now
  in

  let handle_command line =
    match String.split_on_char ' ' (String.trim line) with
    | [ "inspect"; path ] -> (
      match Nsdl.Sim.perform world (Inspect path) with
      | OValue v -> set_status (Printf.sprintf "=> %s" (Nsdl.Sim.value_to_string v))
      | OAck m -> set_status (Printf.sprintf "=> %s" m)
      | OError m -> set_status (Printf.sprintf "=> error: %s" m))
    | "set" :: path :: rest when rest <> [] ->
      let v = parse_value (String.concat " " rest) in
      (match Nsdl.Sim.perform world (Configure (path, v)) with
      | OValue v -> set_status (Printf.sprintf "=> %s" (Nsdl.Sim.value_to_string v))
      | OAck m -> set_status (Printf.sprintf "=> %s" m)
      | OError m -> set_status (Printf.sprintf "=> error: %s" m));
      bump ()
    | [ "power_cycle"; target ] ->
      (match Nsdl.Sim.perform world (PowerCycle target) with
      | OAck m -> set_status (Printf.sprintf "=> %s" m)
      | _ -> ());
      bump ()
    | [ "restart_service"; target ] ->
      (match Nsdl.Sim.perform world (RestartService target) with
      | OAck m -> set_status (Printf.sprintf "=> %s" m)
      | _ -> ());
      bump ()
    | [ "invoke"; trigger; target ] ->
      (match Nsdl.Sim.perform world (Invoke (trigger, target)) with
      | OAck m -> set_status (Printf.sprintf "=> %s" m)
      | OError m -> set_status (Printf.sprintf "=> error: %s" m)
      | OValue _ -> ());
      bump ()
    | [ "advance"; dur ] -> (
      try
        let seconds = Nsdl.Lexer.parse_duration dur in
        Nsdl.Sim.advance world seconds;
        set_status (Printf.sprintf "=> advanced %s (t = %gs)" dur world.Nsdl.Sim.clock);
        bump ()
      with Nsdl.Lexer.Lex_error msg -> set_status (Printf.sprintf "=> bad duration %S: %s" dur msg))
    | [ "live" ] ->
      live_on := true;
      live_speed := 1.0;
      last_tick := Unix.gettimeofday ();
      set_status "=> live: ticking at 1x real time (\"stop\" to pause)"
    | [ "live"; speed_str ] -> (
      match float_of_string_opt speed_str with
      | Some speed when speed > 0.0 ->
        live_on := true;
        live_speed := speed;
        last_tick := Unix.gettimeofday ();
        set_status (Printf.sprintf "=> live: ticking at %gx real time (\"stop\" to pause)" speed)
      | _ -> set_status "=> bad speed (expected a positive number)")
    | [ ("stop" | "pause") ] ->
      live_on := false;
      set_status "=> stopped"
    | [ ("quit" | "exit") ] -> Lwd.set quit true
    | [ "" ] -> ()
    | _ -> set_status "=> unrecognized command"
  in

  let focus = Focus.make () in
  let input_ui =
    Nottui_widgets.edit_field (Lwd.get input)
      ~focus
      ~on_change:(fun v -> Lwd.set input v)
      ~on_submit:(fun (text, _) ->
        handle_command text;
        Lwd.set input ("", 0))
  in
  Focus.request focus;

  let versioned f = Lwd.map (Lwd.get world_version) ~f:(fun _ -> Ui.atom (f world)) in
  let root =
    Nottui_widgets.vbox
      [
        versioned instances_image;
        versioned render_network_diagram;
        versioned pending_image;
        versioned log_image;
        versioned clock_image;
        Lwd.map (Lwd.get status) ~f:(fun s -> Ui.atom (I.string attr s));
        Lwd.pure (Ui.atom (I.string attr help_text));
        input_ui;
      ]
  in
  Nottui_unix.run ~tick_period:0.2 ~tick:on_tick ~quit root
