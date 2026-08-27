(** Execution-state and small stateless helpers shared by {!Engine}'s [run]
    and [replay] walkers: the [state] record threaded through the structural
    walk, ctx/output/receipt binding, approval-token digesting, and the
    run/commit-preflight input and output JSON helpers.

    This module is an internal implementation detail of {!Engine} — nothing
    here is part of the library's public interface (see [engine.mli]). *)

open Types

val token_digest : string -> string
(** Domain-separated SHA-256 of an approval token, encoded as
    [sha256:<64 lowercase hex>]. The raw token is never persisted. *)

val token_is_wellformed : string option -> bool

(** Execution state threaded through the walk. [rev_trace] accumulates in
    REVERSE order (most recent first) and is reversed at the end. [ctx] binds
    step ids to their recorded structured output (addressable as
    ["outputs.<id>..."]); the loop additionally binds ["loop"] to
    {"iter": <index>}. [terminal] is set when a Commit / Block / Abort ends
    the run. *)
type state = {
  rev_trace : trace_entry list;
  ctx : (string * Yojson.Safe.t) list;
  attest_counts : (string * int) list;
  terminal : outcome option;
}

val emit : state -> trace_entry -> state

val bind : state -> string -> Yojson.Safe.t -> state
(** Bind/overwrite a key in ctx (most recent write wins; assoc lookup finds
    it). *)

val ctx_for : state -> (string * Yojson.Safe.t) list
(** The expression context: agent outputs are nested under "outputs". We
    expose that to the DSL by keeping ctx keyed by "outputs" and "loop". The
    actual per-step output is merged into the single "outputs" object. *)

val finish : state -> outcome * trace

val bind_output : state -> string -> Yojson.Safe.t -> state
(** Merge an agent's output object under outputs.<id>, preserving prior
    outputs. *)

val bind_receipts : state -> string -> Yojson.Safe.t -> Yojson.Safe.t -> state

val sha256 : string -> string

val canonical_digest : Yojson.Safe.t -> (string * string, string) result

val bind_loop_iter : state -> int -> state

val run_input :
  state ->
  string list ->
  (string * string, string) result

val commit_lock_json : string -> Secure_fs.lock_identity -> Yojson.Safe.t

val commit_preflight_input :
  state ->
  string list ->
  Yojson.Safe.t option ->
  (string * string, string) result

val run_output_json :
  input_digest:string option ->
  parsed:Yojson.Safe.t option ->
  executable:executable_identity option ->
  run_result ->
  Yojson.Safe.t

val parse_run_stdout :
  Schema.t -> run_result -> (Yojson.Safe.t, string) result

val commit_preflight_receipt :
  id:string ->
  input_digest:string ->
  result:run_result ->
  parsed:Yojson.Safe.t ->
  lock_identity:Yojson.Safe.t option ->
  (Yojson.Safe.t * string, string) result

val default_max_loop_iters : int
(** Unconditional hard iteration ceiling for every loop. A loop ALWAYS stops
    once it has executed this many iterations, regardless of governors /
    until / budget / agent behaviour — it is the termination GUARANTEE
    (Budget/Fixpoint/until are early-stop heuristics under it). Default
    chosen generously; tests pass a small value. The ceiling is a constant,
    so replay reproduces byte-identically. *)

val run_permitted : run_allowlist:string list -> string list -> bool
(** A [Run] step executes only if the basename of its command's head is in
    the operator-supplied allowlist, OR the allowlist contains ["*"] (allow
    all). The default allowlist is [[]], so with no operator opt-in NO run
    step ever executes (fail-closed). The allowlist is a RUNTIME parameter,
    never read from the workflow file: a workflow cannot grant itself the
    right to run a command. *)

val path_bearing_head : string list -> bool
(** [cmd.(0)] must be a BARE command name resolved via PATH. A head
    containing a path separator ('/') — i.e. an absolute path ["/abs/x"], an
    explicit relative path ["./x"], or any ["a/b"] — is rejected: it bypasses
    the allowlist's [Filename.basename] match while executing an arbitrary
    binary. The bin runner execs bare names via PATH. *)
