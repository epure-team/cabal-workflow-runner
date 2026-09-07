# Rich agent execution migration notes

The rich execution modules are additive. Existing `Backend.t`, `Backend.stub`,
`Engine.run`, workflow JSON, schema, and ledger callers require no migration.

Callers adopting the new API must observe these fail-closed boundaries:

- Pass an explicit `~status` to `Agent_execution.make_response`. It must agree
  with the final transport attempt, except that a transport-successful attempt
  carrying `schema_error` uses an overall `Failed _` status.
- Treat `Workflow_event.make_trace` as a lifecycle validator, not only an order
  check. Retained events may be an omitted prefix/subsequence, but visible
  lifecycle contradictions are rejected. `Workflow_event.trace` is agent-call
  telemetry, not the engine's `Types.trace` or a workflow ledger value. Tool
  identities and omission counters must be built with their validated smart
  constructors.
- Keep JSON and public output within the constants exposed by
  `Canonical_json`, `Agent_execution`, and `Workflow_event`. Opaque constructors
  reject excessive depth, node count, canonical bytes, attempts, text, and
  serialized trace size before a persistence projection can be produced.
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
