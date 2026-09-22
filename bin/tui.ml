(* Interactive terminal renderer over Nsdl.Sim's state model.

   Usage: tui.exe <file.nsdl> [file.nsdl ...]

   Pass one or more .nsdl files in any order -- typically an object
   definition, a scenario, an optional incident overlay, and an optional
   file of `at`/`between` schedule blocks (they can also all live in the
   same file). See Nsdl.Sim.load_files for exactly how these combine:
   every `object` across all files is registered before any instance is
   created (so file order doesn't matter for type binding), the first
   `scenario` found is loaded, every `incident` found is applied as an
   overlay, and every `at`/`between` schedule block found is registered
   on the virtual clock.

   Loop: render the world, read a command, apply it, repeat.

   Commands:
     inspect PATH             -- print the current value at PATH; also
                                  accepts PATH.state for an instance's
                                  current lifecycle state
     set PATH VALUE           -- configure PATH to VALUE
     power_cycle TARGET
     restart_service TARGET
     invoke TRIGGER TARGET    -- dispatch TARGET's object-defined handler
                                  matching trigger name TRIGGER and its
                                  current lifecycle state, if any
     advance DURATION         -- move the virtual clock forward (e.g. "30s",
                                  "2m30s") and fire any events now due
     live [SPEED]             -- start ticking the virtual clock in real
                                  time (SPEED virtual seconds per real
                                  second, default 1.0) until "stop"
     quit / exit

   By default the world only evolves in response to `advance` (or the
   field-mutating commands above) -- nothing happens on a wall-clock
   timer. This keeps everything deterministic and replayable, per the
   proposal's own "Time controls" section, rather than fighting for a
   real-time feel. `live` is an explicit opt-in escape hatch when you
   want the wall-clock feel instead: it polls stdin with a timeout
   (Unix.select) and calls the exact same `Sim.advance` in a loop, so
   it's built entirely on top of the deterministic clock, not a
   replacement for it -- every tick is still just `advance elapsed`.
   Object handlers/timers (the `on ... { }` / `after EXPR -> STATE`
   forms inside `object` definitions) are NOT executed by `advance` (or
   therefore by `live`) -- only top-level `at`/`between ... every`
   schedule blocks are, since those don't require instantiating an
   object type or dispatching handlers.

   Caveat: stdin is read line-buffered, so `live` only notices you've
   started typing once a full line (ending in Enter) is available --
   ticking pauses mid-keystroke rather than mid-tick, it doesn't drop
   ticks silently. *)

let clear_screen () = print_string "\x1b[2J\x1b[H"

(* ------------------------------------------------------------------ *)
(* Network diagram: any instance with an integer `ports` field (a      *)
(* naming convention, not anything enforced by a type system -- there  *)
(* isn't one yet) is drawn as a rectangle with that many numbered port *)
(* cells, with a connector line down to whichever device/medium sits   *)
(* on each occupied port. Connections not anchored to a rendered       *)
(* switch fall back to a flat "other connections" list.                *)
(* ------------------------------------------------------------------ *)

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

let cell_width port_count = max 3 (String.length (string_of_int port_count) + 2)

let place (buf : bytes) total_width start (text : string) =
  String.iteri
    (fun i c -> if start + i >= 0 && start + i < total_width then Bytes.set buf (start + i) c)
    text

let render_switch_diagram world name inst port_count =
  let cw = cell_width port_count in
  let total_width = 1 + (port_count * (cw + 1)) in
  let col_start i = 1 + (i * (cw + 1)) in
  (* remote path + medium sitting on each 1-based port number, if any *)
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
  let title = Printf.sprintf " %s : %s " name inst.Nsdl.Sim.inst_type in
  let left_pad = max 0 ((total_width - 2 - String.length title) / 2) in
  let right_pad = max 0 (total_width - 2 - String.length title - left_pad) in
  Printf.printf "\n+%s+\n" (String.make (total_width - 2) '-');
  Printf.printf "|%s%s%s|\n" (String.make left_pad ' ') title (String.make right_pad ' ');
  let border = "+" ^ String.concat "+" (List.init port_count (fun _ -> String.make cw '-')) ^ "+" in
  let numbers =
    let b = Bytes.make total_width ' ' in
    Bytes.set b 0 '|';
    for i = 0 to port_count - 1 do
      place b total_width (col_start i) (Printf.sprintf "%*d" cw (i + 1));
      Bytes.set b (col_start i + cw) '|'
    done;
    Bytes.to_string b
  in
  Printf.printf "%s\n%s\n%s\n" border numbers border;
  let occupied_center i = col_start i + (cw / 2) in
  (* Each connected device gets its own small box (name inside, medium
     annotation below) rather than a bare text label. A box is wider
     than a port column almost always, so strict per-column centering
     would make adjacent boxes overlap. Instead, each box is placed at
     its ideal centered position unless that would collide with the
     previous (already-placed) box, in which case it's pushed right
     just far enough to clear it -- ports stay in left-to-right order,
     at the cost of drifting from their exact column when crowded. *)
  let device_box remote_path medium =
    let dev, rest = Nsdl.Sim.split_path remote_path in
    let border = "+" ^ String.make (String.length dev) '-' ^ "+" in
    let annotation = Printf.sprintf "(%s%s)" (if rest = "" then "" else rest ^ " via ") medium in
    [ border; "|" ^ dev ^ "|"; border; annotation ]
  in
  let boxes =
    List.filter_map
      (fun i ->
        match remote_at.(i + 1) with
        | Some (remote_path, medium) -> Some (occupied_center i, device_box remote_path medium)
        | None -> None)
      (List.init port_count Fun.id)
  in
  if boxes <> [] then (
    let box_width lines = List.fold_left (fun w l -> max w (String.length l)) 0 lines in
    let center_line w l =
      let pad = (w - String.length l) / 2 in
      String.make pad ' ' ^ l ^ String.make (w - String.length l - pad) ' '
    in
    (* First pass: resolve horizontal collisions between boxes and find
       how wide the combined row actually needs to be -- pushed-right
       boxes can extend past the port grid's own width. Keep each box's
       actual final center (post-collision), not its ideal one, so the
       connector line drawn from it always lands on the box itself. *)
    let placements_rev, required_width =
      List.fold_left
        (fun (acc, cursor) (ideal_center, lines) ->
          let w = box_width lines in
          let start = max (ideal_center - (w / 2)) cursor in
          ((start, w, List.map (center_line w) lines) :: acc, start + w + 1))
        ([], 0) boxes
    in
    let placements = List.rev placements_rev in
    let width = max total_width required_width in
    let connector =
      let b = Bytes.make width ' ' in
      List.iter (fun (start, w, _) -> place b width (start + (w / 2)) "|") placements;
      Bytes.to_string b
    in
    Printf.printf "%s\n" connector;
    let n_rows = List.fold_left (fun n (_, _, lines) -> max n (List.length lines)) 0 placements in
    for row = 0 to n_rows - 1 do
      let b = Bytes.make width ' ' in
      List.iter
        (fun (start, _, lines) ->
          match List.nth_opt lines row with Some line -> place b width start line | None -> ())
        placements;
      Printf.printf "%s\n" (Bytes.to_string b)
    done)

let render_network_diagram world =
  let switches =
    Hashtbl.fold
      (fun name inst acc ->
        match switch_port_count inst with Some n -> (name, inst, n) :: acc | None -> acc)
      world.Nsdl.Sim.instances []
  in
  let switches = List.sort (fun (a, _, _) (b, _, _) -> compare a b) switches in
  List.iter (fun (name, inst, n) -> render_switch_diagram world name inst n) switches;
  let switch_names = List.map (fun (n, _, _) -> n) switches in
  let anchored_to_switch path =
    match parse_port_ref path with Some (dev, _) -> List.mem dev switch_names | None -> false
  in
  let leftover =
    List.filter
      (fun (a, b, _) -> not (anchored_to_switch a || anchored_to_switch b))
      world.Nsdl.Sim.connections
  in
  if leftover <> [] then (
    print_string "\nother connections:\n";
    List.iter (fun (a, b, m) -> Printf.printf "  %s -> %s via %s\n" a b m) (List.rev leftover))

let render world =
  clear_screen ();
  Printf.printf "NSDL network state -- t = %gs\n" world.Nsdl.Sim.clock;
  print_string (String.make 60 '=');
  print_newline ();
  let names = Hashtbl.fold (fun k _ acc -> k :: acc) world.Nsdl.Sim.instances [] in
  let names = List.sort compare names in
  List.iter
    (fun name ->
      let inst = Hashtbl.find world.Nsdl.Sim.instances name in
      Printf.printf "\n[%s : %s]\n" name inst.Nsdl.Sim.inst_type;
      let fields = Hashtbl.fold (fun k v acc -> (k, v) :: acc) inst.Nsdl.Sim.fields [] in
      let fields = List.sort (fun (a, _) (b, _) -> compare a b) fields in
      List.iter
        (fun (k, v) -> Printf.printf "  %-24s = %s\n" k (Nsdl.Sim.value_to_string v))
        fields)
    names;
  render_network_diagram world;
  if world.Nsdl.Sim.pending <> [] then (
    let pending = List.sort (fun a b -> compare a.Nsdl.Sim.due b.Nsdl.Sim.due) world.Nsdl.Sim.pending in
    print_string "\npending events:\n";
    List.iter
      (fun ev -> Printf.printf "  t=%gs  %s\n" ev.Nsdl.Sim.due ev.Nsdl.Sim.label)
      pending);
  let recent = List.filteri (fun i _ -> i < 5) world.Nsdl.Sim.log in
  if recent <> [] then (
    print_string "\nrecent actions:\n";
    List.iter (fun l -> Printf.printf "  %s\n" l) (List.rev recent));
  print_string
    "\ncommands: inspect PATH | set PATH VALUE | power_cycle TARGET | \
     restart_service TARGET | invoke TRIGGER TARGET | advance DURATION | live [SPEED] | quit\n> ";
  flush stdout

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

let print_observation (o : Nsdl.Sim.observation) =
  match o with
  | OValue v -> Printf.printf "=> %s\n" (Nsdl.Sim.value_to_string v)
  | OAck msg -> Printf.printf "=> %s\n" msg
  | OError msg -> Printf.printf "=> error: %s\n" msg

let handle_command world line =
  match String.split_on_char ' ' (String.trim line) with
  | [ "inspect"; path ] -> print_observation (Nsdl.Sim.perform world (Inspect path))
  | "set" :: path :: rest when rest <> [] ->
    let v = parse_value (String.concat " " rest) in
    print_observation (Nsdl.Sim.perform world (Configure (path, v)))
  | [ "power_cycle"; target ] -> print_observation (Nsdl.Sim.perform world (PowerCycle target))
  | [ "restart_service"; target ] ->
    print_observation (Nsdl.Sim.perform world (RestartService target))
  | [ "invoke"; trigger; target ] ->
    print_observation (Nsdl.Sim.perform world (Invoke (trigger, target)))
  | [ "advance"; dur ] -> (
    try
      let seconds = Nsdl.Lexer.parse_duration dur in
      Nsdl.Sim.advance world seconds;
      Printf.printf "=> advanced %s (t = %gs)\n" dur world.Nsdl.Sim.clock
    with Nsdl.Lexer.Lex_error msg -> Printf.printf "=> bad duration %S: %s\n" dur msg)
  | [ "" ] -> ()
  | _ -> print_string "=> unrecognized command\n"

(* Real seconds between idle re-renders while `live` is running. Small
   enough to feel responsive, large enough not to busy-loop. *)
let live_tick_interval = 0.5

let print_live_banner speed =
  Printf.printf "\n[live: ticking at %gx real time -- type a command, or \"stop\" to pause]\n> "
    speed;
  flush stdout

(* `loop` (normal, one-command-at-a-time mode) and `live_loop` (ticks
   the clock on a real-time timer between commands) call into each
   other -- `stop` inside live mode drops back to `loop`, and the
   `live` command from `loop` enters `live_loop`. *)
let rec loop world =
  render world;
  match In_channel.input_line stdin with
  | None -> ()
  | Some line when String.trim line = "quit" || String.trim line = "exit" -> ()
  | Some line -> (
    match String.split_on_char ' ' (String.trim line) with
    | [ "live" ] -> live_loop world 1.0
    | [ "live"; speed_str ] -> (
      match float_of_string_opt speed_str with
      | Some speed when speed > 0.0 -> live_loop world speed
      | _ ->
        print_string "=> bad speed (expected a positive number)\n\n[press enter to continue]";
        flush stdout;
        ignore (In_channel.input_line stdin);
        loop world)
    | _ ->
      handle_command world line;
      print_string "\n[press enter to continue]";
      flush stdout;
      ignore (In_channel.input_line stdin);
      loop world)

and live_loop world speed =
  render world;
  print_live_banner speed;
  let last = ref (Unix.gettimeofday ()) in
  let rec tick () =
    let readable, _, _ = Unix.select [ Unix.stdin ] [] [] live_tick_interval in
    if readable = [] then (
      let now = Unix.gettimeofday () in
      let elapsed = now -. !last in
      last := now;
      Nsdl.Sim.advance world (elapsed *. speed);
      render world;
      print_live_banner speed;
      tick ())
    else
      match In_channel.input_line stdin with
      | None -> ()
      | Some line -> (
        match String.trim line with
        | "stop" | "pause" -> loop world
        | "quit" | "exit" -> ()
        | trimmed ->
          if trimmed <> "" then handle_command world trimmed;
          last := Unix.gettimeofday ();
          render world;
          print_live_banner speed;
          tick ())
  in
  tick ()

let () =
  let paths = List.tl (Array.to_list Sys.argv) in
  if paths = [] then (
    Printf.eprintf "usage: tui.exe <file.nsdl> [file.nsdl ...]\n";
    exit 1);
  try loop (Nsdl.Sim.load_files paths) with
  | Nsdl.Lexer.Lex_error msg ->
    Printf.eprintf "lex error: %s\n" msg;
    exit 1
  | Nsdl.Parser.Error ->
    Printf.eprintf "parse error\n";
    exit 1
  | Failure msg ->
    Printf.eprintf "%s\n" msg;
    exit 1
