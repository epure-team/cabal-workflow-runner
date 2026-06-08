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
  ?max_loop_iters:int ->
  backend:Backend.t ->
  token:string option ->
  Validate.Validated.t ->
  Types.outcome * Types.trace
(** [run ?max_loop_iters ~backend ~token validated] interprets the workflow
    deterministically.

    Every loop is hard-bounded by an unconditional engine iteration ceiling
    [max_loop_iters] (default [10_000]): a loop ALWAYS stops once it has executed
    that many iterations — recording [Loop_stopped { reason = "ceiling" }] —
    regardless of governors / [until] / budget / agent progress. So no loop can
    run unboundedly even if the backend's budget is a constant or the agent always
    reports progress. [Budget] / [Fixpoint] / [until] are {e early-stop} heuristics
    under the ceiling, and [Max_iters] sets an explicit lower bound. Because the
    ceiling is a constant, {!val:replay} reproduces the same trace.

    - [Agent] -> [backend.run_agent] yields [(success, json)]; the JSON is bound
      into the run context under ["outputs.<id>"] and, if an [output_schema] is
      present, validated fail-closed (a mismatch yields [Aborted "schema
      mismatch: <field>"]).
    - [Gate] -> pure {!Expr.eval}; a [Pass] continues, a [Fail] yields [Blocked]
      (naming the gate id) and ends the run. [Branch] -> pure {!Expr.eval} chooses
      the arm.
    - [Loop] -> bounded by the engine ceiling [max_loop_iters]; each iteration
      binds ["loop.iter"], runs [body], then stops if [until] holds OR any governor
      fires ([Max_iters], [Budget] via [backend.budget], or [Fixpoint]). The
      ceiling is the termination guarantee; governors are early-stop heuristics.
    - [Commit] -> requires a well-formed [token]; absent/ill-formed yields
      [Blocked]. The token is never stored: only its digest is recorded.

    The token is exclusively a runtime parameter; no step can carry it. *)

exception Replay_mismatch of string
(** Raised by {!val:replay} when the supplied [trace] does not match the
    workflow: an out-of-order/ill-typed entry, a re-evaluated verdict that
    diverges from the recorded one, a trace that is exhausted before the walk
    completes, or {b trailing entries left over after the walk completed} (a
    valid prefix followed by garbage does NOT replay successfully). *)

val replay :
  ?max_loop_iters:int -> trace:Types.trace -> Validate.Validated.t -> Types.outcome
(** [replay ?max_loop_iters ~trace validated] re-interprets [validated] re-feeding
    the RECORDED agent outputs and budget readings in [trace] (no backend is
    consulted), re-evaluating the total DSL over the rebuilt context and asserting
    each recorded verdict. It produces the same outcome as the original
    {!val:run}. Pass the same [max_loop_iters] used for the run (default
    [10_000]); the ceiling is a constant, so the recorded [Loop_stopped] is
    reproduced. Raises {!exception:Replay_mismatch} if [trace] does not match
    (including trailing extra entries after the walk completes). *)

val token_digest : string -> string
(** Hash of an approval token, as recorded in traces. The raw token is never
    persisted. *)
