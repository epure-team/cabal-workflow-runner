(** Load-time, fail-closed validation. This is the safety floor expressed as an
    invariant over ANY workflow, enforced by the engine/validator rather than by
    the workflow author.

    A {!Validated.t} can only be produced by {!val:workflow}; {!val:Engine.run}
    requires a {!Validated.t}, so an unvalidated workflow cannot be executed. *)

module Validated : sig
  type t
  (** A workflow that has passed {!val:Validate.workflow}. Abstract, with no
      public constructor: the only way to obtain a value is via validation. *)

  val workflow : t -> Types.workflow
  (** Recover the underlying workflow (read-only). *)

  val floor_gates : t -> Types.gate_id list
  (** The floor gates this workflow was validated against. *)
end

val workflow :
  floor_gates:Types.gate_id list ->
  Types.workflow ->
  (Validated.t, string) result
(** [workflow ~floor_gates wf] validates [wf] fail-closed. Returns [Error
    reason] if any rule below is violated; otherwise [Ok validated].

    Rules (enforced for the whole workflow):
    - every [Loop] has [max_iters >= 1] (bounded);
    - every [Commit] is guaranteed-preceded by ALL [floor_gates] on EVERY path.
      A gate counts as guaranteed-before a commit only if it appears before the
      commit on every path: inside a [Branch] it counts only if present in BOTH
      [then_] and [else_]; gates inside a [Loop] body do NOT count (the loop may
      run zero iterations). Formally: [floor_gates] must be a subset of the
      gates guaranteed-evaluated before each commit. *)
