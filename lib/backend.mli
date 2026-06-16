(** Abstraction over the effectful operations the engine needs: dispatching
    agent work (returning {b structured JSON}) and reading a remaining budget.
    A {!type:t} is a plain record of functions, so embedders can supply any
    implementation (a cabal-backed one in [bin], a deterministic stub for tests)
    without the engine depending on cabal.

    Note: there is {b no} gate primitive. Gates, branches and loop stop
    conditions are pure {!Expr.t} predicates evaluated by the engine over the
    recorded agent outputs. *)

type t = {
  run_agent :
    id:string ->
    prompt:string ->
    read_only:bool ->
    agent_type:string option ->
    model:string option ->
    bool * Yojson.Safe.t;
      (** Run agent work; returns [(success, structured_json)]. [agent_type]
          is an optional routing hint (e.g. ["code-reviewer"]) forwarded to the
          backend so it can select a specialised adapter. [model] is an optional
          per-step model override (e.g. ["claude-fable-5"]); when [None] the
          backend's default applies (e.g. its global [CWR_MODEL]). *)
  budget : unit -> int;
      (** Remaining budget. A [Budget] governor stops the loop once this is
          [<= 0]. Embedder-supplied; a decrementing stub lets tests force loop
          termination without any [Max_iters] cap. *)
  run_command :
    id:string ->
    argv:string list ->
    working_dir:string ->
    timeout_ms:int option ->
    observe:string list option ->
    Types.run_result;
      (** Execute a {!Types.Run} step's command and return its observed
          {!Types.run_result}. This is the INJECTED effect that keeps process
          execution out of the lib: the lib defines the types and calls this
          function; only [bin/] implements it via cabal/[Unix] + a before/after
          directory snapshot. The engine calls it at most ONCE per run step (on a
          live run, and only after the allowlist check passes); {!Engine.replay}
          NEVER calls it (it re-feeds the recorded result). *)
  run_shell_command : string -> int;
      (** Execute a shell command string via [sh -c] and return its exit code.
          Used by [Shell] and [Evidence] steps. The lib never spawns a process;
          only [bin/] provides a real implementation. {!Engine.replay} NEVER calls
          it (it re-feeds the recorded exit codes). *)
}

val stub :
  ?agent:(id:string ->
          prompt:string ->
          read_only:bool ->
          agent_type:string option ->
          model:string option ->
          bool * Yojson.Safe.t) ->
  ?budget:(unit -> int) ->
  ?run_command:
    (id:string ->
     argv:string list ->
     working_dir:string ->
     timeout_ms:int option ->
     observe:string list option ->
     Types.run_result) ->
  ?run_shell_command:(string -> int) ->
  unit ->
  t
(** A deterministic stub backend used by tests and as a default. By default all
    agents succeed returning [`Assoc []], [budget] returns [max_int]
    (effectively unbounded), and [run_command] is a no-op success (exit 0, empty
    output, no files). Override any of them for specific scenarios. *)
