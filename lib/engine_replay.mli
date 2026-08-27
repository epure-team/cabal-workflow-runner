(** [replay] — re-interpreting a recorded {!Types.trace} without a backend, the counterpart to {!Engine_run.run}. *)

exception Replay_mismatch of string
(** Raised by {!val:replay} when the supplied [trace] does not match the
    workflow: an out-of-order/ill-typed entry, a re-evaluated verdict that
    diverges from the recorded one, a trace that is exhausted before the walk
    completes, or {b trailing entries left over after the walk completed} (a
    valid prefix followed by garbage does NOT replay successfully). *)

val replay :
  ?max_loop_iters:int ->
  ?initial_ctx:(string * Yojson.Safe.t) list ->
  ?attestation_verifier:Attestation.verifier ->
  ?attestation_session_nonce:string ->
  sw:Eio.Switch.t ->
  trace:Types.trace ->
  Validate.Validated.t ->
  Types.outcome
(** [replay ?max_loop_iters ~trace validated] re-interprets [validated] re-feeding
    the RECORDED agent outputs and budget readings in [trace] (no backend is
    consulted), re-evaluating the total DSL over the rebuilt context and asserting
    each recorded verdict. It produces the same outcome as the original
    {!val:run}. Pass the same [max_loop_iters] used for the run (default
    [10_000]); the ceiling is a constant, so the recorded [Loop_stopped] is
    reproduced. Raises {!exception:Replay_mismatch} if [trace] does not match
    (including trailing extra entries after the walk completes). *)
