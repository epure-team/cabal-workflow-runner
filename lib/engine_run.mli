(** [run] — the deterministic, backend-driven interpreter half of {!Engine}. *)

val run :
  ?max_loop_iters:int ->
  ?run_allowlist:string list ->
  ?initial_ctx:(string * Yojson.Safe.t) list ->
  ?attestation_signer:Attestation.signer ->
  ?attestation_artifact_root:string ->
  ?attestation_session_nonce:string ->
  ?agent_backend_id:string ->
  ?deadline:float ->
  ?now:(unit -> float) ->
  sw:Eio.Switch.t ->
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
      mismatch: <field>"]). An Agent with [input] must be read-only; its selected
      predecessor context is canonicalized into the prompt and engine-owned
      request/result receipts are bound under ["receipts.<id>"].
    - [Gate] -> pure {!Expr.eval}; a [Pass] continues, a [Fail] yields [Blocked]
      (naming the gate id) and ends the run. [Branch] -> pure {!Expr.eval} chooses
      the arm.
    - [Loop] -> bounded by the engine ceiling [max_loop_iters]; each iteration
      binds ["loop.iter"], runs [body], then stops if [until] holds OR any governor
      fires ([Max_iters], [Budget] via [backend.budget], [Deadline] via the
      wall clock, or [Fixpoint]). The ceiling is the termination guarantee;
      governors are early-stop heuristics.
    - [Run] -> execute an observable shell command via the INJECTED
      [backend.run_command] effect, recording the full {!Types.run_result} as a
      [Run_executed] trace entry and binding it into ["outputs.<id>"]. The
      optional [input] projection is restricted-canonicalized onto child stdin
      and digest-bound; optional [stdout_schema] validates object stdout and
      binds it as ["outputs.<id>.parsed"].
      command executes only if the basename of its head is in [run_allowlist] OR
      [run_allowlist] contains ["*"]; otherwise the step is [Blocked]
      (fail-closed). The command runs exactly ONCE (on the live run); replay
      re-feeds the recorded result and NEVER re-executes.
    - [Commit] -> requires a well-formed [token]; absent/ill-formed yields
      [Blocked]. An optional structured preflight then executes once through the
      runtime Run allowlist, immediately before Commit, and its canonical receipt
      is bound into the Commit trace entry. Any process/input/schema failure
      blocks. Replay validates the receipt without execution. The token is never
      stored: only its digest is recorded.

      When [preflight.lock_file] is present, the engine securely acquires that
      BSD advisory lock beneath the preflight working directory without waiting,
      injects its verified path/device/inode marker into canonical stdin, and
      holds the FD through receipt validation and Commit emission. Replay checks
      the recorded marker and post-command identity verdict but never locks.

    [run_allowlist] (default [[]]) is the OPERATOR-supplied, RUNTIME-only trust
    control for [Run] steps: an empty list means no [Run] step ever executes
    (fail-closed/off), ["*"] permits all binaries, and any other list permits a
    [Run] step iff [Filename.basename (List.hd cmd)] is a member. A workflow file
    can NEVER grant itself the allowlist — it is not a workflow field. The
    [working_dir] bounds the cwd / snapshot scope but does NOT sandbox the
    command from absolute paths in its args; full isolation is out of scope.

    [deadline] (absent by default) is likewise OPERATOR-supplied and RUNTIME-only:
    an absolute instant (Unix epoch seconds) after which a [Deadline] governor
    stops its loop. A workflow file cannot carry it. Each [Deadline] check
    records its verdict, and {!val:replay} re-feeds the RECORDED verdict rather
    than consulting the clock — which is why [replay] takes no [deadline]: a run
    recorded after its deadline passed must still reproduce. [now] (default
    [Unix.gettimeofday]) is the clock seam, so a caller can supply a
    deterministic clock; it is only ever read by a [Deadline] governor check.

    {b [deadline] does not bound a step's duration.} Governors are evaluated
    between iterations, so one long agent step can overrun it arbitrarily; it
    bounds how many further iterations start, not when the run ends.

    The token is exclusively a runtime parameter; no step can carry it. *)
