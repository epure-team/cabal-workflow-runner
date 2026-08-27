(** Resolving a {!Types.Dynamic_parallel} step's [over] to its runtime branch
    keys, and instantiating one branch's copy of the step template (id
    suffixing, in-template selector/expr rewriting, and per-branch Attest
    output namespacing).

    This module is an internal implementation detail of {!Engine} — nothing
    here is part of the library's public interface (see [engine.mli]). *)

val resolve_dynamic_parallel_over :
  (string * Yojson.Safe.t) list -> string -> (string list, string) result
(** Resolve [over] (a flat ctx key, exactly like Foreach's) to the list of
    unique, non-empty runtime branch keys, or a clear fail-closed reason.
    Fails BEFORE any branch is dispatched: malformed/non-array/non-string
    element/duplicate/over-ceiling all reject here, never partway through
    forking. *)

val dynamic_parallel_template_ids : Types.step list -> string list
(** Every id a Dynamic_parallel template declares as its OWN producer
    (Agent/Run/Commit/Shell/Evidence/Attest/Dynamic_parallel/Gate/Spawn ids),
    collected ONCE per template. Used to tell "this selector/expr path refers
    to a sibling INSIDE this template" (must be rewritten per-branch) from
    "this selector refers to ctx bound before Dynamic_parallel ran" (must NOT
    be rewritten). *)

val dynamic_parallel_branch_steps :
  template_ids:string list ->
  key:string ->
  branch_idx:int ->
  Types.step list ->
  Types.step list
(** Instantiate one Dynamic_parallel branch's copy of the template: every
    step id gets suffixed with "#<key>" (so ctx/ledger keys can never
    collide across siblings — this is what makes a branch's Attest step_id
    genuinely branch-unique, with ZERO changes to attestation.ml), every
    in-template selector/expr path referencing a sibling producer id is
    rewritten to match, and every Attest output path is namespaced by
    branch index (so two siblings materializing the SAME output template —
    e.g. both using only "{occurrence}" — can never race on the same
    file). *)
