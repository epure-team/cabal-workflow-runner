# Rich agent execution migration notes

The rich execution modules are additive. Existing `Backend.t`, `Backend.stub`,
`Engine.run`, workflow JSON, schema, and ledger callers require no migration.

Callers adopting the new API must observe these fail-closed boundaries:

- Pass an explicit `~status` to `Agent_execution.make_response`. It must agree
  with the final transport attempt, except that a transport-successful attempt
  carrying `schema_error` uses an overall `Failed _` status.
- Classify schema retry exhaustion only when telemetry contains exactly two
  attempts: the initial schema-rejected success and one fresh or resumed
  corrective attempt. The corrective attempt may be another schema rejection,
  a backend failure (including resume rejection), a timeout, or cancellation;
  preserve that final transport status on the response.
- Treat `Workflow_event.make_trace` as a lifecycle validator, not only an order
  check. Retained events may be an omitted prefix/subsequence, but visible
  lifecycle contradictions are rejected. `Workflow_event.trace` is agent-call
  telemetry, not the engine's `Types.trace` or a workflow ledger value. Tool
  identities and omission counters must be built with their validated smart
  constructors. `Exited code` rejects negative values but intentionally accepts
  every non-negative host `int`, without a Unix-only 255 ceiling.
- Emit `Usage_observed` as cumulative per-attempt snapshots, not deltas. Known
  dimensions must never decrease. Only the final retained snapshot is checked
  against the attempt aggregate. A positive unlocated omission count, a later
  sequence gap, or a later usage-truncation marker makes that snapshot unknown,
  so equality is conservatively skipped while retained monotonicity remains
  enforced.
- Measure attempt duration inside its retained `Attempt_started` to
  `Attempt_finished` envelope. Bridge/process-event overhead may make the
  envelope longer; an attempt may exceed it only by the exported one-sided
  millisecond tolerance.
- Keep JSON and public output within the constants exposed by
  `Canonical_json`, `Agent_execution`, and `Workflow_event`. Opaque constructors
  reject excessive depth, node count, canonical bytes, attempts, text, and
  serialized projection size. JSON byte accounting is iterative and includes
  string/key escaping and punctuation before any full serialization buffer is
  allocated; diagnostic paths are capped and never copy long/non-portable
  attacker-controlled object keys.
- Before using `Runtime.of_legacy_backend`, set `read_only` explicitly. Both
  `Some true` and `Some false` are forwarded unchanged. `None` is rejected
  before callback dispatch. JSON Schema, session resume, attachments, web
  access, and maximum turns are also rejected individually because the legacy
  interface cannot preserve them.
- Build `Runtime.capabilities` from exact supported media MIME types, the maximum
  web level, and an explicit restricted-domain support bit. Construction
  canonicalizes/rejects MIME claims and rejects restricted-domain support when
  web access is disabled. Legacy read-only/routing/model capability claims are
  false unless the caller opts in through the corresponding `attested_*` flag.

The event trace remains a bounded post-completion value. This migration does
not add a live event stream or wire the rich runtime into `Engine.run`.
