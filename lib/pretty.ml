(* Hand-written structural dump of the AST. Stands in for a derived
   printer since no ppx/sexp library is installed; also doubles as the
   "stable reference representation" the roadmap asks for while surface
   syntax is still expected to change. *)

open Ast

let buf_add_indent b n = Buffer.add_string b (String.make (n * 2) ' ')

let rec expr_to_string = function
  | EInt i -> string_of_int i
  | EFloat f -> string_of_float f
  | EString s -> "\"" ^ s ^ "\""
  | EDuration d -> Printf.sprintf "%gs" d
  | EIpAddr s -> s
  | EIdent s -> s
  | EField (e, f) -> expr_to_string e ^ "." ^ f
  | EIndex (e, i) -> Printf.sprintf "%s[%s]" (expr_to_string e) (expr_to_string i)
  | ECall (f, args) ->
    Printf.sprintf "%s(%s)" (expr_to_string f)
      (String.concat ", " (List.map arg_to_string args))
  | ERange (a, b) -> Printf.sprintf "%s .. %s" (expr_to_string a) (expr_to_string b)
  | EAnd (a, b) -> Printf.sprintf "%s && %s" (expr_to_string a) (expr_to_string b)
  | EOr (a, b) -> Printf.sprintf "%s || %s" (expr_to_string a) (expr_to_string b)
  | EDequeue e -> "dequeue " ^ expr_to_string e

and arg_to_string = function
  | APos e -> expr_to_string e
  | ANamed (n, e) -> n ^ " = " ^ expr_to_string e

let rec state_type_to_string = function
  | STName t -> t
  | STOption t -> state_type_to_string t ^ "?"
  | STUnion ts -> String.concat " | " (List.map state_type_to_string ts)

let rec stmt_to_buf b depth s =
  buf_add_indent b depth;
  match s with
  | SPort (n, t) -> Buffer.add_string b (Printf.sprintf "port %s : %s\n" n t)
  | SMemory (n, t, p) ->
    Buffer.add_string b
      (Printf.sprintf "memory %s : %s%s\n" n t
         (match p with Some p -> " " ^ p | None -> ""))
  | SLifecycleDecl (n, init) ->
    Buffer.add_string b (Printf.sprintf "lifecycle %s%s\n" n (if init then " initial" else ""))
  | SHandler h -> handler_to_buf b depth h
  | SLifecycleBlock (st, body) ->
    Buffer.add_string b (Printf.sprintf "in %s {\n" st);
    List.iter (stmt_to_buf b (depth + 1)) body;
    buf_add_indent b depth; Buffer.add_string b "}\n"
  | SInstance (n, t, body) ->
    Buffer.add_string b (Printf.sprintf "instance %s : %s {\n" n t);
    List.iter (stmt_to_buf b (depth + 1)) body;
    buf_add_indent b depth; Buffer.add_string b "}\n"
  | SWorkflow (n, body) ->
    Buffer.add_string b (Printf.sprintf "workflow %s {\n" n);
    List.iter (stmt_to_buf b (depth + 1)) body;
    buf_add_indent b depth; Buffer.add_string b "}\n"
  | SEnqueue (a, c) ->
    Buffer.add_string b (Printf.sprintf "enqueue %s, %s\n" (expr_to_string a) (expr_to_string c))
  | SSchedule (n, e) ->
    Buffer.add_string b (Printf.sprintf "schedule %s after %s\n" n (expr_to_string e))
  | SEmit (e, t) ->
    Buffer.add_string b (Printf.sprintf "emit %s through %s\n" (expr_to_string e) t)
  | STransition s -> Buffer.add_string b (Printf.sprintf "transition %s\n" s)
  | SClear s -> Buffer.add_string b (Printf.sprintf "clear %s\n" s)
  | SAssign (p, e) ->
    Buffer.add_string b (Printf.sprintf "%s = %s\n" (expr_to_string p) (expr_to_string e))
  | SAfterTransition (e, s) ->
    Buffer.add_string b (Printf.sprintf "after %s -> %s\n" (expr_to_string e) s)
  | SRetry (p, who) ->
    Buffer.add_string b (Printf.sprintf "retry %s by %s\n" (expr_to_string p) who)
  | SInject (kind, args, tgt) ->
    let args_str =
      if args = [] then "" else "(" ^ String.concat ", " (List.map arg_to_string args) ^ ")"
    in
    Buffer.add_string b
      (Printf.sprintf "inject %s%s on %s\n" kind args_str (expr_to_string tgt))
  | SConnect (a, c, m) ->
    Buffer.add_string b
      (Printf.sprintf "connect %s -> %s via %s\n" (expr_to_string a) (expr_to_string c) m)
  | SSubmit (a, c) ->
    Buffer.add_string b (Printf.sprintf "submit %s -> %s\n" (expr_to_string a) (expr_to_string c))
  | SReport (who, msg) -> Buffer.add_string b (Printf.sprintf "report %s: %S\n" who msg)
  | SAllow names -> Buffer.add_string b (Printf.sprintf "allow %s\n" (String.concat ", " names))
  | SRequireVerify e -> Buffer.add_string b (Printf.sprintf "require verify %s\n" (expr_to_string e))
  | SStateDecl fields ->
    Buffer.add_string b "state {\n";
    List.iter
      (fun (n, t) ->
        buf_add_indent b (depth + 1);
        Buffer.add_string b (Printf.sprintf "%s: %s\n" n (state_type_to_string t)))
      fields;
    buf_add_indent b depth;
    Buffer.add_string b "}\n"
  | SEmits n -> Buffer.add_string b (Printf.sprintf "emits %s\n" n)
  | SReceives ns -> Buffer.add_string b (Printf.sprintf "receives %s\n" (String.concat ", " ns))
  | SEndpoints (n, t) -> Buffer.add_string b (Printf.sprintf "endpoints: exactly<%d, %s>\n" n t)
  | SCapability n -> Buffer.add_string b (Printf.sprintf "capability %s\n" n)

and handler_to_buf b depth h =
  let params =
    if h.h_params = [] then ""
    else
      "(" ^ String.concat ", " (List.map (fun (n, t) -> n ^ ": " ^ t) h.h_params) ^ ")"
  in
  let at = match h.h_at with Some a -> " at " ^ a | None -> "" in
  let in_ = match h.h_in with Some i -> " in " ^ i | None -> "" in
  let whn = match h.h_when with Some e -> " when " ^ expr_to_string e | None -> "" in
  Buffer.add_string b (Printf.sprintf "on %s%s%s%s%s {\n" h.h_trigger params at in_ whn);
  List.iter (stmt_to_buf b (depth + 1)) h.h_body;
  buf_add_indent b depth; Buffer.add_string b "}\n"

let top_to_buf b = function
  | TObject (n, body) ->
    Buffer.add_string b (Printf.sprintf "object %s {\n" n);
    List.iter (stmt_to_buf b 1) body;
    Buffer.add_string b "}\n"
  | TScenario (n, body) ->
    Buffer.add_string b (Printf.sprintf "scenario %s {\n" n);
    List.iter (stmt_to_buf b 1) body;
    Buffer.add_string b "}\n"
  | TIncident (n, base, body) ->
    Buffer.add_string b (Printf.sprintf "incident %s on %s {\n" n base);
    List.iter (stmt_to_buf b 1) body;
    Buffer.add_string b "}\n"
  | TAt (d, body) ->
    Buffer.add_string b (Printf.sprintf "at %s {\n" (expr_to_string d));
    List.iter (stmt_to_buf b 1) body;
    Buffer.add_string b "}\n"
  | TBetween (a, c, e, body) ->
    Buffer.add_string b
      (Printf.sprintf "between %s .. %s every %s {\n" (expr_to_string a) (expr_to_string c)
         (expr_to_string e));
    List.iter (stmt_to_buf b 1) body;
    Buffer.add_string b "}\n"
  | TProfile (n, body) ->
    Buffer.add_string b (Printf.sprintf "profile %s {\n" n);
    List.iter (stmt_to_buf b 1) body;
    Buffer.add_string b "}\n"

let stmt_to_string (s : stmt) =
  let b = Buffer.create 64 in
  stmt_to_buf b 0 s;
  String.trim (Buffer.contents b)

let program_to_string (p : program) =
  let b = Buffer.create 256 in
  List.iter
    (fun t ->
      top_to_buf b t;
      Buffer.add_char b '\n')
    p;
  Buffer.contents b
