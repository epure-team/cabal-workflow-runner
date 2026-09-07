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
  dimensions must never decrease. Every known value remains a lower bound even
  if later snapshots omit that dimension, delivery is truncated, or a sequence
  gap follows. A known final attempt aggregate must meet every such bound; an
  unknown aggregate dimension is rejected once retained evidence establishes a
  bound. Exact equality applies only to dimensions present in the last retained
  observation with no positive unlocated omission count, later sequence gap, or
  later usage-truncation marker. A dimension absent from that observation stays
  lower-bound-only.
- Cross-check every retained `Retry_transition` against the complete response
  attempt list. The response must contain attempt N+1 and its fresh/resumed kind
  must match even when that attempt's `Attempt_started` event was omitted.
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

## Current Cabal `make_rich` outcome map

This table records the integration contract inspected in Cabal's current
`Backend_completer.make_rich`, `Runtime_dispatch.detailed_error`, and
`Backend_types.task_execution_error`. It is a bridge specification, not a Cabal
dependency in this library.

| Cabal `make_rich` outcome | Host-neutral CWR shape |
|---|---|
| Constructor `Error _` (the routing id is malformed) | `Dispatch_error Invalid_request` |
| Callback `Ok { execution; event_trace; _ }` with one or more completed attempts | `Ok response`, preserving every attempt, final status, cleanup state, and mapped trace |
| Callback `Ok` with zero completed attempts and final `Timeout` or `Cancelled` | Genuine bridge gap listed below; do not synthesize an attempt or claim that dispatch did not occur |
| Callback `Error { cause = Dispatch_failure failure; _ }` where `failure` proves a pre-invocation failure | `Dispatch_error (map_dispatch_cause failure)` |
| Callback `Error { cause = Dispatch_failure Backend_execution_failed; _ }` | Genuine bridge gap when no completed result exists: this cause does not reveal whether invocation began |
| Callback `Error { cause = Dispatch_failure_with_execution { failure; execution }; _ }` | `Post_execution_dispatch_failed` built with `map_dispatch_cause failure` and the non-empty mapped response, regardless of whether its final status is success, failure, timeout, cancellation, or schema rejection |
| Callback `Error { cause = Execution_failure (Native_backend_failure_with_schema { execution; _ }); _ }` | `Execution_failure Native_schema_rejection` with the mapped response |
| Callback `Error { cause = Execution_failure (Schema_retry_failed { execution; _ }); _ }` | `Execution_failure Schema_retry_failed` with both mapped attempts |

Use Cabal's sanitized `render_rich_completion_error` output only as the
in-process diagnostic. Safe CWR projection omits it. The fixed dispatch-cause
mapping is exhaustive for the current `Runtime_dispatch.error` algebra:

| Cabal dispatch cause | CWR `dispatch_failure_kind` |
|---|---|
| `Invalid_timeout` | `Invalid_request` |
| `Backend_not_registered` | `Backend_unavailable` |
| `Runtime_registration_untrusted` | `Capability_mismatch` |
| `Backend_quarantined _` | `Capability_mismatch` |
| `Preflight_failed _` | `Preflight_failed` |
| `Backend_version_unsupported` | `Capability_mismatch` |
| `Version_check_failed` | `Internal_dispatch_failure` |
| `Backend_unavailable` | `Backend_unavailable` |
| `Availability_check_failed` | `Internal_dispatch_failure` |
| `Prepared_already_consumed` | `Internal_dispatch_failure` |
| `Backend_execution_failed` | `Internal_dispatch_failure` once a response exists; without one, see the bridge gap below |
| `Schema_enforcement_failed _` | `Internal_dispatch_failure`; the current detailed `make_rich` path does not emit this compatibility-only projection and instead exposes structured schema errors as `Execution_failure` |

Straightforward field conversions are not bridge gaps: Cabal attempt kinds,
statuses, token counts, delivery modes, media references, web levels, cleanup
states, sessions, and normalized events all have conservative CWR projections.
Opaque retry reasons map to `Other_redacted`, process-exit text that cannot be
classified maps to `Unknown`, and process ids are discarded.

The genuine remaining conversion-policy gaps are:

1. Cabal can return a zero-completed-attempt `Timeout`/`Cancelled`, or a bare
   `Dispatch_failure Backend_execution_failed`, after an invocation may already
   have started but before any backend result was committed. CWR intentionally
   reserves `Dispatch_error` for proven no-invocation failures and requires every
   `response` to contain complete telemetry for an actual invocation. The Cabal
   values therefore lack enough evidence to select either shape without
   fabricating an attempt or making a false pre-dispatch claim.
2. Cabal reports USD cost as an optional float, while CWR requires integer
   micro-USD. The future bridge must adopt a checked finite range and rounding
   policy rather than silently truncating or overflowing.
