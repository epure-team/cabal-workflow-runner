(** Core workflow vocabulary and runtime types.

    These types are domain-neutral: they describe control flow (sequence,
    governed loop, branch, gate) plus two leaf kinds (agent work and a terminal
    commit). No specific domain (the bounty pipeline, ZK, crypto, ...) is baked
    in; the bounty pipeline is merely one example workflow file. *)

(** A minimal required-field output spec for an agent's structured JSON. *)
module Schema : sig
  type ty =
    | String
    | Int
    | Number  (** int or float. *)
    | Bool
    | Enum of string list  (** a string drawn from a fixed set. *)
    | Any

  type t = (string * ty) list
  (** An ordered list of [(required_field, type)] constraints. *)

  val string_of_ty : ty -> string

  val validate : t -> Yojson.Safe.t -> (unit, string) result
  (** Fail-closed: each required field must be present and type-correct.
      Returns [Error field] naming the first offending field, else [Ok ()]. *)
end

(** A single workflow step. Illegal states are made hard to express:
    - there is no step constructor that can carry an approval token (the token
      is supplied at runtime to {!val:Engine.run}, never in a workflow file);
    - [Commit] is the only constructor that can file/submit anything;
    - gates / branches / loops carry {b pure} {!Expr.t} predicates evaluated over
      recorded agent outputs — there is no backend gate primitive. *)
type step =
  | Agent of {
      id : string;
      prompt : string;
      read_only : bool;
      output_schema : Schema.t option;
          (** If present, the agent's structured JSON is validated against it;
              a mismatch is fail-closed ([Aborted]). *)
    }
      (** Dispatch agent work; records [(success, structured_json)] and binds the
          output into the run context under ["outputs.<id>"]. *)
  | Gate of { id : string; when_ : Expr.t }
      (** Pure verdict: [Pass] iff [Expr.eval when_] over the run context. *)
  | Branch of { when_ : Expr.t; then_ : step list; else_ : step list }
      (** Evaluate [when_]; take [then_] when true, [else_] when false. *)
  | Loop of {
      body : step list;
      until : Expr.t option;  (** optional data-driven stop condition. *)
      governors : governor list;
          (** {b >= 1 required} (validator enforces). The termination guarantee:
              a loop may legitimately have NO [Max_iters] (unbounded but
              governed) — what is forbidden is an EMPTY governors list. *)
    }
  | Commit of { id : string }
      (** The ONLY step that can file/submit. Requires a runtime token. *)

(** A loop governor — each can independently fire to stop the loop. *)
and governor =
  | Max_iters of int  (** stop after [n] iterations ([n >= 1]). *)
  | Budget  (** stop once [backend.budget () <= 0]. *)
  | Fixpoint of { window : int; progress : Expr.t }
      (** stop after [window] consecutive iterations with [progress = false]
          ([window >= 1]). *)

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
  | Aborted of string  (** Structural / schema error encountered at runtime. *)

(** A recorded effect, in execution order. Replay re-feeds these without a
    backend. {b Everything a stop/branch decision reads is recorded} — each
    agent output, each budget reading, each Fixpoint progress verdict, each
    gate/branch/until verdict, and loop iteration counts — so an unbounded but
    governed loop still replays byte-identically (the bound is a function of
    recorded inputs). *)
type trace_entry =
  | Agent_ran of { id : string; success : bool; output : Yojson.Safe.t }
  | Gate_evaluated of { id : string; verdict : gate_verdict }
  | Branch_taken of { verdict : gate_verdict }
  | Loop_iter of { index : int }
  | Budget_read of { value : int }
  | Fixpoint_progress of { progress : bool }
  | Loop_stopped of { iterations : int; reason : string }
  | Committed_step of { id : string; token_digest : string }
  | Blocked_at of { id : string; reason : string }

type trace = trace_entry list
(** Trace entries in execution order (first executed step first). *)

val string_of_outcome : outcome -> string
val verdict_to_string : gate_verdict -> string
