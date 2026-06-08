(** Core workflow vocabulary and runtime types.

    These types are domain-neutral: they describe control flow (sequence,
    bounded loop, branch, gate) plus two leaf kinds (agent work and a terminal
    commit). No specific domain (the bounty pipeline, ZK, crypto, ...) is baked
    in; the bounty pipeline is merely one example workflow file. *)

type gate_id = string
(** Identifier of a gate whose verdict the engine evaluates via the backend. *)

(** A single workflow step. Illegal states are made hard to express:
    - there is no step constructor that can carry an approval token (the token
      is supplied at runtime to {!val:Engine.run}, never in a workflow file);
    - [Commit] is the only constructor that can file/submit anything. *)
type step =
  | Agent of { id : string; prompt : string; read_only : bool }
      (** Dispatch agent work via the backend; records (success, text). *)
  | Gate of { id : gate_id }  (** Engine evaluates the gate verdict. *)
  | Branch of { on : gate_id; then_ : step list; else_ : step list }
      (** Evaluate gate [on]; take [then_] on [Pass], [else_] on [Fail]. *)
  | Loop of { max_iters : int; until : gate_id; body : step list }
      (** Bounded loop: run [body] at most [max_iters] times, stopping early
          when [until] evaluates [Pass]. [max_iters] must be >= 1. *)
  | Commit of { id : string }
      (** The ONLY step that can file/submit. Requires a runtime token. *)

type workflow = { name : string; steps : step list }

type gate_verdict =
  | Pass
  | Fail

(** Terminal outcome of a run. *)
type outcome =
  | Committed of { id : string; token_digest : string }
      (** A [Commit] executed with a well-formed runtime token; [token_digest]
          is a hash of the token (the raw token is never stored). *)
  | Completed_no_commit  (** Workflow finished without reaching a commit. *)
  | Blocked of string  (** A floor invariant blocked progress (e.g. no token). *)
  | Aborted of string  (** Structural error encountered during execution. *)

(** A recorded effect for one executed step, in execution order. Replay
    re-interprets the workflow against these records without a backend. *)
type trace_entry =
  | Agent_ran of { id : string; success : bool; text : string }
  | Gate_evaluated of { id : gate_id; verdict : gate_verdict }
  | Committed_step of { id : string; token_digest : string }
  | Blocked_at of { id : string; reason : string }

type trace = trace_entry list
(** Trace entries in execution order (first executed step first). *)

val string_of_outcome : outcome -> string
val verdict_to_string : gate_verdict -> string
