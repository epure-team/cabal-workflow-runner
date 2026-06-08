(** Deterministic interpreter for validated workflows, plus byte-identical
    replay from a recorded trace.

    Determinism rests on three pillars:
    - the interpreter is a deterministic structural walk and all gate / branch /
      loop decisions are evaluated by the {b total} {!Expr} DSL over recorded
      agent outputs (always terminating, never raising);
    - the safety floor is an engine invariant ({!Validate}), not author-supplied;
    - every agent output, every budget reading, every Fixpoint progress verdict,
      every gate/branch/until verdict and every loop iteration count is recorded
      in the {!Types.trace}, so {!val:replay} reproduces the same outcome and
      trace without a backend. Because a governed loop's bound is purely a
      function of these recorded inputs, an unbounded-but-governed loop still
      replays byte-identically. *)

val run :
  backend:Backend.t ->
  token:string option ->
  Validate.Validated.t ->
  Types.outcome * Types.trace
(** [run ~backend ~token validated] interprets the workflow deterministically.

    - [Agent] -> [backend.run_agent] yields [(success, json)]; the JSON is bound
      into the run context under ["outputs.<id>"] and, if an [output_schema] is
      present, validated fail-closed (a mismatch yields [Aborted "schema
      mismatch: <field>"]).
    - [Gate]/[Branch] -> pure {!Expr.eval} over the run context.
    - [Loop] -> each iteration binds ["loop.iter"], runs [body], then stops if
      [until] holds OR any governor fires ([Max_iters], [Budget] via
      [backend.budget], or [Fixpoint]). The governors guarantee termination even
      with no [Max_iters].
    - [Commit] -> requires a well-formed [token]; absent/ill-formed yields
      [Blocked]. The token is never stored: only its digest is recorded.

    The token is exclusively a runtime parameter; no step can carry it. *)

val replay : trace:Types.trace -> Validate.Validated.t -> Types.outcome
(** [replay ~trace validated] re-interprets [validated] re-feeding the RECORDED
    agent outputs and budget readings in [trace] (no backend is consulted),
    re-evaluating the total DSL over the rebuilt context and asserting each
    recorded verdict. It produces the same outcome as the original {!val:run}. *)

val token_digest : string -> string
(** Hash of an approval token, as recorded in traces. The raw token is never
    persisted. *)
