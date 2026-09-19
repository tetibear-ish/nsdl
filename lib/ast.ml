(* Abstract syntax for the NSDL start-gate subset.

   Every brace-delimited body in the surface language (object, scenario,
   incident, workflow, handler, lifecycle block, schedule block) is
   represented as the same [stmt list]. The constructs are structurally
   different in the spec's prose but syntactically homogeneous in its own
   examples, so one recursive statement type covers all of them instead of
   a separate body grammar per construct. *)

type arg =
  | APos of expr
  | ANamed of string * expr

and expr =
  | EInt of int
  | EFloat of float
  | EString of string
  | EDuration of float (* canonicalized to seconds, e.g. "2m30s" -> 150.0 *)
  | EIpAddr of string (* kept as written, e.g. "192.168.20.1/24" *)
  | EIdent of string
  | EField of expr * string
  | EIndex of expr * expr
  | ECall of expr * arg list
  | ERange of expr * expr
  | EAnd of expr * expr
  | EOr of expr * expr
  | EDequeue of expr

and stmt =
  | SPort of string * string
  | SMemory of string * string * string option (* name, type, persistence *)
  | SLifecycleDecl of string * bool (* name, is_initial *)
  | SHandler of handler
  | SLifecycleBlock of string * stmt list (* in STATE { ... } *)
  | SInstance of string * string * stmt list (* name, type, fields *)
  | SWorkflow of string * stmt list
  | SEnqueue of expr * expr
  | SSchedule of string * expr (* schedule EVENT after EXPR *)
  | SEmit of expr * string (* emit EXPR through PORT *)
  | STransition of string
  | SClear of string
  | SAssign of expr * expr
  | SAfterTransition of expr * string (* after EXPR -> STATE *)
  | SRetry of expr * string (* retry PATH by ACTOR *)
  | SInject of string * expr * expr (* inject KIND AMOUNT on TARGET *)
  | SConnect of expr * expr * string (* connect A -> B via MEDIUM *)
  | SSubmit of expr * expr
  | SReport of string * string (* report ACTOR: "message" *)
  | SAllow of string list
  | SRequireVerify of expr

and handler = {
  h_trigger : string;
  h_params : (string * string) list;
  h_at : string option;
  h_in : string option;
  h_when : expr option;
  h_body : stmt list;
}

type top =
  | TObject of string * stmt list
  | TScenario of string * stmt list
  | TIncident of string * string * stmt list (* name, base scenario, body *)
  | TAt of expr * stmt list
  | TBetween of expr * expr * expr * stmt list (* start, end, every, body *)

type program = top list
