(** The canonical JSON Schema (draft 2020-12) of the workflow format.

    This is the machine-readable shape of EXACTLY what {!Workflow_json.of_json}
    parses. It is derived by hand from that parser and kept in lock-step with it
    by tests (the committed [schema/workflow.schema.json] artifact must byte-match
    {!to_string}, and the set of step [kind]s the schema enumerates must equal the
    set the parser accepts).

    A meta-agent that generates workflows can be pointed at this schema (via the
    [schema] CLI subcommand, or this library value) to constrain its generator to
    emit conformant workflows {e by construction} — the first of three layers:
    schema (shape at generation) → {!Lint} (semantics/safety pre-run) →
    {!Validate} (the run gate). yojson-only; no cabal dependency. *)

val schema : Yojson.Safe.t
(** The JSON Schema document (draft 2020-12), using [$defs] + [$ref] for the
    recursive [expr], [governor], [step] and [output_schema] parts. *)

val to_string : unit -> string
(** [schema] pretty-printed via [Yojson.Safe.pretty_to_string], newline-terminated.
    This is what the [schema] CLI subcommand prints and what is committed to
    [schema/workflow.schema.json]. *)
