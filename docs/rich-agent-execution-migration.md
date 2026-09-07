# Rich agent execution migration notes

The rich execution modules are additive. Existing `Backend.t`, `Backend.stub`,
`Engine.run`, workflow JSON, schema, and ledger callers require no migration.

Callers adopting the new API must observe these fail-closed boundaries:

- Pass an explicit `~status` to `Agent_execution.make_response`. It must agree
  with the final transport attempt, except that a transport-successful attempt
  carrying `schema_error` uses an overall `Failed _` status.
- Treat `Workflow_event.make_trace` as a lifecycle validator, not only an order
  check. Retained events may be an omitted prefix/subsequence, but visible
  lifecycle contradictions are rejected.
- Keep JSON and public output within the constants exposed by
  `Canonical_json`, `Agent_execution`, and `Workflow_event`. Opaque constructors
  reject excessive depth, node count, canonical bytes, attempts, text, and
  serialized trace size before a persistence projection can be produced.
- Before using `Runtime.of_legacy_backend`, set `read_only` explicitly. Both
  `Some true` and `Some false` are forwarded unchanged. `None` is rejected
  before callback dispatch. JSON Schema, session resume, attachments, web
  access, and maximum turns are also rejected individually because the legacy
  interface cannot preserve them.

The event trace remains a bounded post-completion value. This migration does
not add a live event stream or wire the rich runtime into `Engine.run`.
