(* Abstract syntax for the NSDL start-gate subset.

   Every brace-delimited body in the surface language (object, scenario,
   incident, workflow, handler, lifecycle block, schedule block) is
   represented as the same [stmt list]. The constructs are structurally
   different in the spec's prose but syntactically homogeneous in its own
   examples, so one recursive statement type covers all of them instead of
   a separate body grammar per construct. *)

(* Types written inside a `state { field: TYPE }` block (v0.3). A union
   like `up | down` or `detached | attached | damaged` is [STUnion];
   the trailing `?` on `medium_id?`/`mbps?` is [STOption]. `half |
   full?` is parsed as `half | (full?)` -- the doc's own examples don't
   disambiguate whether `?` should bind to the last union arm or the
   whole union, so this is a documented choice, not a derived fact. *)
type state_type =
  | STName of string
  | STOption of state_type
  | STUnion of state_type list

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
  | SInject of string * arg list * expr (* inject KIND[(args)] on TARGET *)
  | SConnect of expr * expr * string (* connect A -> B via MEDIUM *)
  | SSubmit of expr * expr
  | SReport of string * string (* report ACTOR: "message" *)
  | SAllow of string list
  | SRequireVerify of expr
  | SStateDecl of (string * state_type) list (* state { field: TYPE ... } *)
  | SEmits of string (* emits EVENT *)
  | SReceives of string list (* receives EVENT, EVENT, ... *)
  | SEndpoints of int * string (* endpoints: exactly<N, TYPE> *)
  | SCapability of string (* capability NAME *)
  | SDhcpDiscover of string * expr * expr (* dhcp_discover SERVER ADDRESS LEASE -- self is the client *)
  (* Internal-only continuation-passing stmts for the port/message dispatch
     mechanism (see Sim.exec_stmt) -- no grammar rule ever produces either
     one; they only ever appear as the body of a Sim-scheduled event,
     exactly the same role [STransition] already plays as the body
     [SAfterTransition] reschedules. Payload values are carried as already
     -literal [expr]s (e.g. [EIpAddr "192.168.20.50"]), not [Sim.value]
     directly, so this module doesn't need to depend on Sim. *)
  | SMessageArrived of string * string * string * string * string * (string * expr) list
    (* target, trigger, receiving_port, sender, sender_port, payload
       fields -- the body a delivery fires with. Carries [target]
       explicitly (rather than relying on [self]) because deliveries
       scheduled via [send_via_path] always run with [self:None] -- the
       same convention [dhcp_discover]'s own hops already use, since a
       cross-instance delivery has no single instance context the way a
       self-timer ([SInvokeSelf], scheduled with [self:Some instance])
       does. Both ports are needed and are generally *different* named
       ports on two different devices (e.g. a client's "eth0" vs a
       gateway's "lan"): [receiving_port] is checked against the target's
       own `on TRIGGER at PORT` handlers, and [sender_port] is recorded
       (as `__reply_to_port`) so that if the target later replies, its
       reply is addressed to the *sender's own port*, not the target's. *)
  | SInvokeSelf of string (* trigger -- the body a `schedule`d self-timer fires with *)

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
  | TProfile of string * stmt list (* v0.3 fidelity profile *)
  | TWorld of string * stmt list (* Phase 6 world/embodiment binding: local_name = canonical_name *)

type program = top list
