(** On-disk persistence boundary for a recorded {!Types.trace} (the "ledger").

    A {!Types.trace} produced by {!Engine.run} can be serialised to NDJSON (one
    JSON object per line, each a faithful, tagged serialisation of a single
    {!Types.trace_entry}), written to disk, read back in a {b later process}, and
    fed to {!Engine.replay} for a {b byte-identical} re-interpretation. The
    serialisation round-trips ALL trace entry variants — including a full
    [Run_executed] result (exit / stdout / stderr / truncated / files[] with
    path / change / size / digest) — so persisted replay is exactly as faithful
    as the in-process replay it supersedes.

    This module is pure and depends on {b yojson only}: it is the on-disk
    encoding, not an effect. Reading/writing the file lives in [bin/].

    {b Integrity caveat.} A ledger is an unauthenticated, externally-editable
    file. {!Engine.replay} attests that the trace is {e internally consistent}
    with the workflow and the recorded agent/run outputs (every structural tamper
    fails closed); it does {b not} authenticate those outputs or the commit token.
    A forged ledger whose forged outputs make the gates pass can replay to
    [Committed]. For tamper-evidence, sign/MAC the ledger out of band. *)

val to_ndjson : Types.trace -> string
(** [to_ndjson trace] serialises [trace] to newline-delimited JSON: one JSON
    object per entry, in order, each terminated by ['\n'] (so the empty trace is
    the empty string and a single-entry trace is one line + newline). Each object
    is tagged by a ["kind"] field naming the variant and carries every field
    needed to reconstruct the entry, including the full {!Types.run_result} for a
    [Run_executed]. Total: never raises. *)

val of_ndjson : string -> (Types.trace, string) result
(** [of_ndjson s] is the parse-tolerant inverse of {!to_ndjson}: it parses each
    non-empty line as a JSON object and reconstructs the {!Types.trace}. It is
    {b fail-closed}: any malformed line — invalid JSON, an unknown/missing
    ["kind"], a missing or ill-typed field — yields [Error reason] and NEVER
    raises. Blank lines (e.g. a trailing newline) are ignored.

    Round-trip: [of_ndjson (to_ndjson t) = Ok t] for every [t : Types.trace]. *)

val entry_to_json : Types.trace_entry -> Yojson.Safe.t
(** Serialise a single {!Types.trace_entry} to a JSON object tagged by a
    ["kind"] field.  Used by [bin/] to write the [Ctx_snapshot] header as the
    first NDJSON line of an on-disk ledger. Never raises. *)

val entry_of_json : Yojson.Safe.t -> Types.trace_entry
(** Deserialise a single JSON object back to a {!Types.trace_entry}.
    Raises {!Decode_error} on any structural mismatch.  Used by [bin/] to
    parse the [Ctx_snapshot] header from the first line of an on-disk ledger.
    Callers must guard with [try ... with Decode_error _ | Yojson.Json_error _ -> ...]. *)

exception Decode_error of string
(** Raised by {!entry_of_json} on a structural mismatch. *)