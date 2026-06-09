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
type on_failure = Abort | Continue
(** What to do when an [Agent] step's run is UNSUCCESSFUL.

    - [Abort] (the default): fail closed — the run aborts ([Aborted]). The
      one-shot, safety-first behaviour.
    - [Continue]: a SOFT failure — record the failed [Agent_ran] and continue the
      walk. The agent's (failure) output is still bound under ["outputs.<id>"], so
      any predicate that reads it is fail-closed (a missing field evaluates
      false). Use this in a CONTINUOUS loop where one iteration's agent failure
      must not kill the whole run.

    [Continue] does NOT weaken the commit safety floor, because the validator
    REJECTS it in any workflow that contains a [Commit] (the [soft-fail-with-commit]
    error): soft-fail is permitted only in COMMIT-FREE workflows. This is enforced,
    not assumed — the commit-floor invariant tracks gate IDs, not whether a gate's
    predicate consumes the failed agent's output, so a trivially-true floor gate
    could otherwise let a commit fire despite a soft-failed agent. With no [Commit]
    reachable, [Continue] only changes whether a failed agent ABORTS the walk. *)

type step =
  | Agent of {
      id : string;
      prompt : string;
      read_only : bool;
      output_schema : Schema.t option;
          (** If present, the agent's structured JSON is validated against it;
              a mismatch is fail-closed ([Aborted]). *)
      on_failure : on_failure;
          (** Behaviour on an unsuccessful run; defaults to [Abort] when the
              workflow omits ["on_failure"]. *)
    }
      (** Dispatch agent work; records [(success, structured_json)] and binds the
          output into the run context under ["outputs.<id>"]. *)
  | Gate of { id : string; when_ : Expr.t }
      (** Pure verdict: [Pass] iff [Expr.eval when_] over the run context. A
          [Pass] records the verdict and continues; a [Fail] BLOCKS the run
          ([Blocked], naming the gate id) — a false floor gate can never reach a
          commit. *)
  | Branch of { when_ : Expr.t; then_ : step list; else_ : step list }
      (** Evaluate [when_]; take [then_] when true, [else_] when false. *)
  | Loop of {
      body : step list;
      until : Expr.t option;  (** optional data-driven stop condition. *)
      governors : governor list;
          (** {b >= 1 required} (validator enforces, by intent). These are
              {e early-stop} heuristics: a loop may legitimately have NO
              [Max_iters], and what the validator forbids is an EMPTY governors
              list. The termination {b guarantee} is the engine's unconditional
              iteration ceiling ([Engine.run ?max_loop_iters], default
              [10_000]) — every loop stops at the ceiling regardless of
              governors / [until] / budget / agent progress. *)
    }
  | Run of {
      id : string;
      cmd : string list;
          (** non-empty argv; executed WITHOUT a shell (no implicit [sh -c]). *)
      working_dir : string;
          (** REQUIRED, relative, no [..]; the effect scope + snapshot root. *)
      timeout_ms : int option;  (** optional bounded wall-clock cap. *)
      observe : string list option;
          (** relative paths to snapshot; default = the whole [working_dir]. *)
    }
      (** Run an observable shell command via an INJECTED effect
          ([Backend.run_command]); records the full {!run_result} into the trace
          and binds it into the run context under ["outputs.<id>"]. Executes only
          if the binary is in {!val:Engine.run}'s runtime [run_allowlist]
          (operator-supplied), else the step is [Blocked] (fail-closed). The
          [working_dir] bounds the cwd/snapshot but does NOT sandbox the command
          from absolute paths in its args; the allowlist is the trust control. *)
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
  | Blocked of string
      (** A floor invariant blocked progress: no/ill-formed runtime token, or a
          [Gate] whose predicate evaluated [false]. *)
  | Aborted of string  (** Structural / schema error encountered at runtime. *)

(** A single observed filesystem change produced by a {!Run} step. *)
type file_change_kind =
  | Created
  | Modified
  | Deleted

type file_change = {
  path : string;  (** relative to the run step's [working_dir]. *)
  change : file_change_kind;
  size : int;  (** post-change size in bytes (0 for [Deleted]). *)
  digest : string;
      (** post-change MD5 content digest ([""] for [Deleted]) — for
          change-detection / observability only, NOT a cryptographic integrity
          guarantee. *)
}

(** The structured result of executing a {!Run} step's command. Stdout/stderr
    are size-capped by the runner; [truncated] flags that a cap was hit. *)
type run_result = {
  exit : int;
  stdout : string;
  stderr : string;
  truncated : bool;
  files : file_change list;
}

val string_of_file_change_kind : file_change_kind -> string

val json_of_file_change : file_change -> Yojson.Safe.t
(** [{"path","change","size","digest"}]. *)

val json_of_run_result : run_result -> Yojson.Safe.t
(** [{"exit","stdout","stderr","truncated","files":[..]}] — the form bound into
    the run context under ["outputs.<id>"]. *)

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
  | Run_executed of { id : string; result : run_result }
      (** A {!Run} step's command executed once; the full result is recorded so
          {!Engine.replay} re-binds it WITHOUT re-executing (the command runs
          exactly once, on the live run, never on replay). *)
  | Committed_step of { id : string; token_digest : string }
  | Blocked_at of { id : string; reason : string }

type trace = trace_entry list
(** Trace entries in execution order (first executed step first). *)

val string_of_outcome : outcome -> string
val verdict_to_string : gate_verdict -> string
