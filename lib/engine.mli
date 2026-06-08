(** Deterministic interpreter for validated workflows, plus byte-identical
    replay from a recorded trace.

    Determinism rests on three pillars:
    - the interpreter is a deterministic structural walk;
    - the safety floor is an engine invariant ({!Validate}), not author-supplied;
    - every agent result and gate verdict is recorded in the {!Types.trace}, so
      {!val:replay} reproduces the same outcome and trace without a backend. *)

val run :
  backend:Backend.t ->
  token:string option ->
  Validate.Validated.t ->
  Types.outcome * Types.trace
(** [run ~backend ~token validated] interprets the workflow deterministically.

    - [Agent] -> [backend.run_agent], recorded as [Agent_ran].
    - [Gate] -> [backend.eval_gate], recorded as [Gate_evaluated].
    - [Branch] -> evaluate the named gate, take [then_] on [Pass] else [else_].
    - [Loop] -> run [body] up to [max_iters] times, stopping early when [until]
      evaluates [Pass]. The hard cap [max_iters] guarantees termination.
    - [Commit] -> requires a well-formed [token] (non-empty [Some]); absent or
      ill-formed token yields [Blocked] (recorded as [Blocked_at]). The token is
      never stored: only its digest is recorded.

    The token is exclusively a runtime parameter; no step can carry it. *)

val replay : trace:Types.trace -> Validate.Validated.t -> Types.outcome
(** [replay ~trace validated] re-interprets [validated] using the RECORDED agent
    results and gate verdicts in [trace] — no backend is consulted. It produces
    the same outcome as the original {!val:run} that produced [trace]. *)

val token_digest : string -> string
(** Hash of an approval token, as recorded in traces. The raw token is never
    persisted. *)
