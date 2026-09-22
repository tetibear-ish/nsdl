%{
open Ast

(* Handler headers accumulate optional `at`/`in`/`when` modifiers in any
   order (the spec's examples use different subsets and orders), so we
   fold them into this scratch record before building the real Ast.handler. *)
type handler_mods = {
  hm_at : string option;
  hm_in : string option;
  hm_when : expr option;
}

let no_mods = { hm_at = None; hm_in = None; hm_when = None }
%}

%token <int> INT
%token <float> FLOAT
%token <float> DURATION
%token <string> STRING
%token <string> IPADDR
%token <string> IDENT

%token OBJECT PORT MEMORY LIFECYCLE INITIAL
%token ON IN AT WHEN AFTER TRANSITION CLEAR ENQUEUE DEQUEUE SCHEDULE EMIT THROUGH
%token SCENARIO INSTANCE CONNECT VIA WORKFLOW SUBMIT
%token INCIDENT REPORT SET ALLOW REQUIRE VERIFY RETRY BY INJECT
%token BETWEEN EVERY
%token STATE EMITS RECEIVES ENDPOINTS EXACTLY CAPABILITY PROFILE

%token LBRACE RBRACE LPAREN RPAREN LBRACKET RBRACKET
%token LT GT DOT DOTDOT COLON COMMA EQUALS ARROW AMPAMP PIPEPIPE PIPE QUESTION
%token EOF

%start <Ast.program> program

%%

program:
  | tops = list(top) EOF { tops }

top:
  | OBJECT name = IDENT b = block { TObject (name, b) }
  | SCENARIO name = IDENT b = block { TScenario (name, b) }
  | INCIDENT name = IDENT ON base = IDENT b = block { TIncident (name, base, b) }
  | AT d = expr b = block { TAt (d, b) }
  | BETWEEN a = postfix_expr DOTDOT c = postfix_expr EVERY e = expr b = block { TBetween (a, c, e, b) }
  | PROFILE name = IDENT b = block { TProfile (name, b) }

block:
  | LBRACE stmts = list(stmt) RBRACE { stmts }

ty:
  | t = IDENT { t }
  | t = IDENT LT u = ty GT { t ^ "<" ^ u ^ ">" }

persistence:
  | p = IDENT { p }

inject_args:
  | (* empty *) { [] }
  | LPAREN args = separated_list(COMMA, call_arg) RPAREN { args }

state_block:
  | LBRACE fields = list(state_field) RBRACE { fields }

state_field:
  | n = IDENT COLON t = state_type { (n, t) }

state_type:
  | atoms = separated_nonempty_list(PIPE, state_type_atom)
    { match atoms with [ t ] -> t | ts -> STUnion ts }

state_type_atom:
  | t = ty q = QUESTION?
    { let base = STName t in match q with Some _ -> STOption base | None -> base }

stmt:
  | PORT name = IDENT COLON t = ty { SPort (name, t) }
  | MEMORY name = IDENT COLON t = ty p = persistence? { SMemory (name, t, p) }
  | LIFECYCLE name = IDENT init = INITIAL? { SLifecycleDecl (name, init <> None) }
  | IN st = IDENT b = block { SLifecycleBlock (st, b) }
  | h = handler { SHandler h }
  | INSTANCE name = IDENT COLON t = ty b = block { SInstance (name, t, b) }
  | WORKFLOW name = IDENT b = block { SWorkflow (name, b) }
  | ENQUEUE a = expr COMMA b = expr { SEnqueue (a, b) }
  | SCHEDULE name = IDENT AFTER e = expr { SSchedule (name, e) }
  | EMIT e = expr THROUGH t = IDENT { SEmit (e, t) }
  | TRANSITION s = IDENT { STransition s }
  | CLEAR s = IDENT { SClear s }
  | AFTER e = expr ARROW s = IDENT { SAfterTransition (e, s) }
  | RETRY p = expr BY who = IDENT { SRetry (p, who) }
  | INJECT kind = IDENT args = inject_args ON tgt = expr { SInject (kind, args, tgt) }
  | STATE fields = state_block { SStateDecl fields }
  | EMITS n = IDENT { SEmits n }
  | RECEIVES ns = separated_nonempty_list(COMMA, IDENT) { SReceives ns }
  | ENDPOINTS COLON EXACTLY LT n = INT COMMA t = ty GT { SEndpoints (n, t) }
  | CAPABILITY n = IDENT { SCapability n }
  | CONNECT a = expr ARROW b = expr VIA m = IDENT { SConnect (a, b, m) }
  | SUBMIT a = expr ARROW b = expr { SSubmit (a, b) }
  | REPORT who = IDENT COLON msg = STRING { SReport (who, msg) }
  | SET p = expr EQUALS e = expr { SAssign (p, e) }
  | ALLOW names = separated_nonempty_list(COMMA, IDENT) { SAllow names }
  | REQUIRE VERIFY e = expr { SRequireVerify e }
  | p = expr EQUALS e = expr { SAssign (p, e) }

handler:
  | ON trig = IDENT params = handler_params mods = handler_mods b = block
    { { h_trigger = trig; h_params = params;
        h_at = mods.hm_at; h_in = mods.hm_in; h_when = mods.hm_when; h_body = b } }

handler_params:
  | (* empty *) { [] }
  | LPAREN ps = separated_list(COMMA, param) RPAREN { ps }

param:
  | n = IDENT COLON t = IDENT { (n, t) }

handler_mods:
  | (* empty *) { no_mods }
  | AT n = IDENT rest = handler_mods { { rest with hm_at = Some n } }
  | IN n = IDENT rest = handler_mods { { rest with hm_in = Some n } }
  | WHEN e = expr rest = handler_mods { { rest with hm_when = Some e } }

expr:
  | e = or_expr { e }

or_expr:
  | a = or_expr PIPEPIPE b = and_expr { EOr (a, b) }
  | e = and_expr { e }

and_expr:
  | a = and_expr AMPAMP b = range_expr { EAnd (a, b) }
  | e = range_expr { e }

range_expr:
  | a = postfix_expr DOTDOT b = postfix_expr { ERange (a, b) }
  | e = postfix_expr { e }

(* A handful of keywords ("port", "workflow", ...) double as ordinary
   names in the spec's own examples (e.g. "switch.port[2]",
   "workflow.patient_label" as a bare path root). Accept those specific
   tokens here too rather than making every keyword ambiguous with
   IDENT everywhere. *)
name:
  | s = IDENT { s }
  | PORT { "port" }
  | WORKFLOW { "workflow" }
  | STATE { "state" }

postfix_expr:
  | e = primary_expr { e }
  | e = postfix_expr DOT f = name { EField (e, f) }
  | e = postfix_expr LBRACKET i = expr RBRACKET { EIndex (e, i) }
  | e = postfix_expr LPAREN args = separated_list(COMMA, call_arg) RPAREN { ECall (e, args) }

call_arg:
  | n = IDENT EQUALS e = expr { ANamed (n, e) }
  | e = expr { APos e }

primary_expr:
  | i = INT { EInt i }
  | f = FLOAT { EFloat f }
  | d = DURATION { EDuration d }
  | s = STRING { EString s }
  | ip = IPADDR { EIpAddr ip }
  | n = name { EIdent n }
  | DEQUEUE e = postfix_expr { EDequeue e }
  | LPAREN e = expr RPAREN { e }
