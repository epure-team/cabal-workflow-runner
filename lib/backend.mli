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
    id:string -> prompt:string -> read_only:bool -> bool * Yojson.Safe.t;
      (** Run agent work; returns [(success, structured_json)]. *)
  budget : unit -> int;
      (** Remaining budget. A [Budget] governor stops the loop once this is
          [<= 0]. Embedder-supplied; a decrementing stub lets tests force loop
          termination without any [Max_iters] cap. *)
}

val stub :
  ?agent:(id:string -> prompt:string -> read_only:bool -> bool * Yojson.Safe.t) ->
  ?budget:(unit -> int) ->
  unit ->
  t
(** A deterministic stub backend used by tests and as a default. By default all
    agents succeed returning [`Assoc []] and [budget] returns [max_int]
    (effectively unbounded). Override [agent] or [budget] for specific
    scenarios. *)
