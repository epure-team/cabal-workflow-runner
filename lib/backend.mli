(** Abstraction over the two effectful operations the engine needs: dispatching
    agent work and evaluating a gate verdict. A {!type:t} is a plain record of
    functions, so embedders can supply any implementation (a cabal-backed one in
    [bin], a deterministic stub for tests) without the engine depending on cabal. *)

type t = {
  run_agent : id:string -> prompt:string -> read_only:bool -> bool * string;
      (** Run agent work; returns [(success, agent_text)]. *)
  eval_gate : Types.gate_id -> Types.gate_verdict;
      (** Evaluate a gate's verdict. *)
}

val stub :
  ?gate:(Types.gate_id -> Types.gate_verdict) ->
  ?agent:(id:string -> prompt:string -> read_only:bool -> bool * string) ->
  unit ->
  t
(** A deterministic stub backend used by tests and as a default. By default all
    gates [Pass] and all agents succeed with their id echoed as text. Override
    [gate] or [agent] for specific test scenarios. *)
