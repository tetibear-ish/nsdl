{
open Parser

exception Lex_error of string

(* "2m30s" / "18s" / "4m" -> seconds. Each run of digits is followed by
   exactly one unit letter (h, m, or s); the regex below guarantees at
   least one such pair is present. *)
let parse_duration s =
  let len = String.length s in
  let total = ref 0.0 in
  let i = ref 0 in
  while !i < len do
    let start = !i in
    while !i < len && s.[!i] >= '0' && s.[!i] <= '9' do incr i done;
    let num = float_of_string (String.sub s start (!i - start)) in
    let unit = s.[!i] in
    incr i;
    let mult = match unit with
      | 'h' -> 3600.0
      | 'm' -> 60.0
      | 's' -> 1.0
      | c -> raise (Lex_error (Printf.sprintf "bad duration unit %c in %s" c s))
    in
    total := !total +. num *. mult
  done;
  !total
}

let digit = ['0'-'9']
let ident_start = ['a'-'z' 'A'-'Z' '_']
let ident_char = ident_start | digit

rule token = parse
  | [' ' '\t' '\r' '\n']+          { token lexbuf }
  | "//" [^ '\n']*                 { token lexbuf }

  (* Longer, more specific literals must precede the generic INT/IDENT
     rules so ocamllex's longest-match doesn't need tie-breaking here. *)
  | digit+ '.' digit+ '.' digit+ '.' digit+ ('/' digit+)? as s
      { IPADDR s }
  | (digit+ ['h' 'm' 's'])+ as s
      { DURATION (parse_duration s) }
  | digit+ '.' digit+ as s         { FLOAT (float_of_string s) }
  | digit+ as s                    { INT (int_of_string s) }
  | '"' ([^ '"']* as s) '"'        { STRING s }

  | ".."  { DOTDOT }
  | "->"  { ARROW }
  | "&&"  { AMPAMP }
  | "||"  { PIPEPIPE }
  | '{'   { LBRACE }
  | '}'   { RBRACE }
  | '('   { LPAREN }
  | ')'   { RPAREN }
  | '['   { LBRACKET }
  | ']'   { RBRACKET }
  | '<'   { LT }
  | '>'   { GT }
  | '.'   { DOT }
  | ':'   { COLON }
  | ','   { COMMA }
  | '='   { EQUALS }

  (* Keywords: must precede the generic IDENT rule below so equal-length
     matches resolve to the keyword token, not IDENT. *)
  | "object"     { OBJECT }
  | "port"       { PORT }
  | "memory"     { MEMORY }
  | "lifecycle"  { LIFECYCLE }
  | "initial"    { INITIAL }
  | "on"         { ON }
  | "in"         { IN }
  | "at"         { AT }
  | "when"       { WHEN }
  | "after"      { AFTER }
  | "transition" { TRANSITION }
  | "clear"      { CLEAR }
  | "enqueue"    { ENQUEUE }
  | "dequeue"    { DEQUEUE }
  | "schedule"   { SCHEDULE }
  | "emit"       { EMIT }
  | "through"    { THROUGH }
  | "scenario"   { SCENARIO }
  | "instance"   { INSTANCE }
  | "connect"    { CONNECT }
  | "via"        { VIA }
  | "workflow"   { WORKFLOW }
  | "submit"     { SUBMIT }
  | "incident"   { INCIDENT }
  | "report"     { REPORT }
  | "set"        { SET }
  | "allow"      { ALLOW }
  | "require"    { REQUIRE }
  | "verify"     { VERIFY }
  | "retry"      { RETRY }
  | "by"         { BY }
  | "inject"     { INJECT }
  | "between"    { BETWEEN }
  | "every"      { EVERY }

  | ident_start ident_char* as s   { IDENT s }
  | eof                            { EOF }
  | _ as c { raise (Lex_error (Printf.sprintf "unexpected character %c" c)) }
