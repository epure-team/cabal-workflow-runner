# Rich agent execution migration notes

The rich execution modules are additive. Existing `Backend.t`, `Backend.stub`,
`Engine.run`, workflow JSON, schema, and ledger callers require no migration.

Callers adopting the new API must observe these fail-closed boundaries:

- Call `Cwr_cabal.bootstrap_hardened ()` once against an empty Cabal registry and
  retain its opaque handle. Pass `~bootstrap` to every `Cwr_cabal.create` and
  `register_custom_backend` call. The handle captures exact physical
  entry/backend identities and immutable binding metadata, supports concurrent
  `create`, and cannot be refreshed after registry clearing/replacement in the
  same process. Custom tokens are bound to that handle, ID, and exact entry.
  There is no first-available, raw-registration, YAML-adapter, or direct
  `Agentic_backend` fallback.
- Treat every `Cwr_cabal.create` result as fixed to the exact entry selected at
  construction. Its native-schema, session, media MIME, maximum-web, and
  read-only claims project that bound descriptor. Maximum-turn forwarding, hard
  deadlines, and model selection are bridge guarantees; restricted-domain web
  policy and routing are false. Request routing is a compatibility input only:
  omit it or pass the bound backend ID. Any other role/backend string fails
  before Cabal dispatch. This is also the live CLI rule for legacy `agent_type`.
- Keep `Backend_completer.make_rich_with_entry` as the sole call-time dispatch
  authority. Pass the selected snapshot as `~expected_entry`; do not precede an
  unguarded by-name call with a separate registry identity check. A replacement
  before Cabal capture must fail, while one after capture may execute only the
  captured original.
- Pass an explicit `~status` to `Agent_execution.make_response`. It must agree
  with the final transport attempt, except that a transport-successful attempt
  carrying `schema_error` uses an overall `Failed _` status.
- Classify schema retry exhaustion only when telemetry contains exactly two
  attempts: the initial schema-rejected success and one fresh or resumed
  corrective attempt. The corrective attempt may be another schema rejection,
  a backend failure (including resume rejection), a timeout, or cancellation;
  preserve that final transport status on the response.
- Use `Native_backend_failure_with_schema` when a native-schema backend fails.
  The name records only that a schema was in force; it does not claim the schema
  caused the backend failure.
- Keep a completed response independent from a later dispatch/cleanup failure's
  outer event trace. `Post_execution_dispatch_failed` stores both. Its outer
  trace terminates as failed while a nested completed response may remain
  successful; do not attach that outer trace to `make_response` or rewrite the
  successful attempt.
- Use `No_completed_attempt` when no complete backend result exists and dispatch
  may already have begun. Preserve its failed/timed-out/cancelled status,
  explicit invocation uncertainty, and optional normalized trace without
  inventing an attempt. Registry, capability, quarantine, and preflight errors
  known to occur before invocation remain `Dispatch_failure`.
- Use `Incomplete_execution` when at least one backend result was committed but
  the outer failed/timed-out/cancelled outcome is not a complete result for the
  latest invocation. Preserve committed attempts exactly and record at most one
  immediately following fresh/resumed continuation. A declared continuation
  owns the outer terminal at N+1. Mark it `Invocation_started` only when retained
  N+1 lifecycle or observation evidence proves entry. Use
  `Invocation_may_have_started` only when the terminal is N+1 and a positive
  truncation marker or sequence gap across the N→N+1 boundary makes
  start/activity evidence unknowable. With a retained retry transition, the
  omission must be after that transition (and any later retained N evidence).
  With an omitted transition, it must be after completed N's last retained
  lifecycle/observation event. A prefix/N omission that ends before a dense
  transition→terminal suffix does not qualify; neither does a bare unlocated
  omission count. A schema error is retry context, not proof that retry began.
  Dense cancellation on N before a retry transition therefore has no
  continuation. Never manufacture continuation result, duration, output, or
  session. Completed aggregate usage/cost and final session exclude it;
  retained bounded N+1 usage/cost observations are exposed only as separate
  lower bounds and remain present in the complete outer trace.
- Treat `Workflow_event.make_trace` as a lifecycle validator, not only an order
  check. Retained events may be an omitted prefix/subsequence, but visible
  lifecycle contradictions are rejected. `Workflow_event.trace` is agent-call
  telemetry, not the engine's `Types.trace` or a workflow ledger value. Tool
  identities and omission counters must be built with their validated smart
  constructors. `Exited code` rejects negative values but intentionally accepts
  every non-negative host `int`, without a Unix-only 255 ceiling.
- Preserve each event's sequence number, attempt, elapsed time, and payload.
  Cabal may deliver final session metadata, one non-empty bounded agent-text
  fallback when no earlier agent text was emitted, a positive text-truncation
  marker immediately following it in the source sequence when only a prefix was
  retained, and final usage after `Attempt_finished`; only that ordered
  same-attempt sequence is valid before the terminal. The retained prefix is a
  lower bound for the complete result text; the marker cannot stand alone or
  prove a continuation. Never rotate or reassociate payloads to make a trace
  fit. For a definitely pre-dispatch failure, normalize only `attempt` to zero;
  preserve the source sequence, timestamp, and safely mapped payload.
  Final public text/session/usage parser observations may occur after process
  exit while the attempt remains open; do not reject that Cabal ordering.
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
- Treat rich maximum-turn capability as acceptance and unchanged forwarding into
  Cabal's completion request, not as independent evidence that every backend CLI
  enforces the bound.
- Accept strict structured output only from a standard object/array in Cabal's
  report or normalized final text. If both sources are valid and differ, fail
  closed; never silently prefer one. Do not parse raw reports outside the bridge.

The event trace remains a bounded post-outcome value. This migration does not
add a live event stream or wire the rich runtime into `Engine.run`.

## Current guarded Cabal rich-completion outcome map

This table records the integration contract inspected in Cabal's current
`Backend_completer.make_rich_with_entry`, `Runtime_dispatch.detailed_error`, and
`Backend_types.task_execution_error`. It is a bridge specification, not a Cabal
dependency in this library. Every callback row preserves Cabal's complete
bounded normalized outer trace; private/raw fields outside CWR's safe contract
remain deliberately unrepresentable rather than being described as preserved.

Select the shape from committed evidence, not only from Cabal's synthetic
`final_result`: a value is an ordinary `response` only when its final result is
the last committed attempt result (allowing the documented schema-validation
projection). A nonempty committed prefix followed by an unrepresented outer
failure/timeout/cancellation is `Incomplete_execution`. A failure strictly after
a coherent completed execution, notably successful execution followed by sealed
input cleanup failure, is `Post_execution_dispatch_failed` and keeps the
completed response independent from the outer failed trace.

| Cabal guarded rich-completion outcome | Host-neutral CWR shape |
|---|---|
| Constructor `Error _` (the routing id is malformed) | `Dispatch_error Invalid_request` |
| Constructor `Ok rich_completer` | Preserve the callback; construction performs no dispatch |
| Callback `Ok { execution; event_trace; _ }`, non-empty attempts, final `Success` represented by the last completed attempt | `Ok response` with status `Success` and the mapped trace |
| Callback `Ok { execution; event_trace; _ }`, non-empty attempts, final `Failed message` represented by the last completed attempt | `Ok response` with the same failed status/attempt diagnostic in process and the mapped trace |
| Callback `Ok { execution; event_trace; _ }`, non-empty attempts, final `Timeout` represented by the last completed attempt | `Ok response` with status `Timed_out` and the mapped trace |
| Callback `Ok { execution; event_trace; _ }`, non-empty attempts, final `Cancelled` represented by the last completed attempt | `Ok response` with status `Cancelled` and the mapped trace |
| Callback `Ok { execution; event_trace; _ }`, non-empty attempts, synthetic final `Timeout` or `Cancelled` not represented by the last completed attempt | `Incomplete_execution` with the exact completed prefix, outer status/elapsed/cleanup, completed-only aggregates/session, full outer trace, and optional single continuation inferred as described below. This includes timeout/cancellation before retry transition (no continuation) and during a fresh/resumed retry (one continuation) |
| Callback `Ok { execution; event_trace; _ }`, zero attempts, final `Timeout` | `No_completed_attempt { status = Timed_out; invocation_may_have_started = true; event_trace = Some mapped_trace }` |
| Callback `Ok { execution; event_trace; _ }`, zero attempts, final `Cancelled` | `No_completed_attempt { status = Cancelled; invocation_may_have_started = true; event_trace = Some mapped_trace }` |
| Callback `Ok { execution; event_trace; _ }`, zero attempts, final `Failed _` | `No_completed_attempt { status = Failed _; invocation_may_have_started = true; event_trace = Some mapped_trace }` |
| Callback `Ok { execution; event_trace; _ }`, zero attempts, final `Success` | Invalid source contract; `Telemetry_mapping_failure` retains the exact mapped trace |
| Callback `Error { cause = Dispatch_failure failure; event_trace }` | Use the exhaustive cause table below; preserve `event_trace` on the selected error shape |
| Callback `Error { cause = Dispatch_failure_with_execution { failure; execution }; event_trace }` where `execution.final_result` is synthetic and not a committed attempt result | `Incomplete_execution` with outer `Failed _`, exact completed prefix, completed-only aggregates/session, cleanup state, complete outer trace, and at most one continuation. This is Cabal's retry-exception-after-progress path |
| Callback `Error { cause = Dispatch_failure_with_execution { failure; execution }; event_trace }` after a coherent completed execution, including successful execution plus attachment cleanup failure | `Post_execution_dispatch_failed` with `map_dispatch_cause failure`, the coherent non-empty response built from `execution` without attaching the outer trace, and `outer_event_trace = mapped_trace`. Preserve actual attempts/status/session/metrics/`Cleanup_failed`; the outer failed terminal remains separate |
| Callback `Error { cause = Execution_failure (Native_backend_failure_with_schema { execution; _ }); event_trace }` | `Execution_failure Native_backend_failure_with_schema` with the mapped non-empty response and trace; this makes no schema-causality claim |
| Callback `Error { cause = Execution_failure (Schema_retry_failed { execution; attempt_2_failure; _ }); event_trace }` | `Execution_failure Schema_retry_failed` with both mapped attempts, unchanged final status, and trace; `attempt_2_failure` maps as detailed below |
| Any callback whose event trace maps successfully but whose invalid/contradictory cost, session, attempt, total elapsed, result, or failure telemetry cannot satisfy the selected CWR constructor | `Telemetry_mapping_failure` with the exact already-valid mapped trace and no fabricated replacement telemetry. If the source event trace itself is invalid, no trace is claimed |

For `Incomplete_execution`, let N be the last committed attempt number. Retained
events may refer only to completed attempts 1..N and optionally N+1. A visible
retry transition or N+1 start fixes the continuation kind; it must be fresh or
resumed and consistent with the prior schema/retry context, but a schema error
alone does not prove N+1 exists. When a continuation is present, the outer
terminal must be attributed to N+1, never N. Explicit N+1 start, finish, process,
session, text, tool, usage, or other backend observation evidence selects
`Invocation_started`; terminal alone is insufficient. With no such evidence,
`Invocation_may_have_started` is valid only when the N+1 terminal is accompanied
by a positive N+1 truncation marker or retained sequence gap capable of hiding
transition/start/activity specifically at the N→N+1 boundary. A final-text
fallback truncation marker belonging to completed attempt N is unrelated. If
the retry transition is retained, scan only after it and any later retained N
evidence. If it is omitted, scan after completed N's last retained
lifecycle/observation event. A gap ending before that anchor, an earlier
truncation, or a global `omitted_count` with no boundary-local gap is unrelated
and must not upgrade a dense transition→terminal suffix to uncertainty. A dense
no-omission terminal on N selects `continuation = None`. A retained N+1 finish
must be failed, timed out, or cancelled exactly like the outer terminal. A
successful N+1 finish/terminal, N+2 evidence, skipped number, mismatched kind, unsupported
certainty claim, or completed-attempt usage/cost/session mismatch fails
conversion rather than fabricating telemetry. N+1 session observations remain
in the trace and N+1 metric observations remain separate lower bounds; neither
changes completed aggregates or final session.

The nested retry-failure algebra maps without rewriting attempt status:

| Cabal `attempt_2_failure` | CWR evidence |
|---|---|
| `Schema_validation_failure error` | Corrective attempt remains transport `Success`, carries `schema_error`, and the outer response is `Failed _` |
| `Transport_failure (Failed message)` | Corrective attempt and response remain `Failed message` |
| `Transport_failure Timeout` | Corrective attempt and response remain `Timed_out` |
| `Transport_failure Cancelled` | Corrective attempt and response remain `Cancelled` |
| `Transport_failure Success` | Rejected as incoherent rather than rewritten |
| `Resume_failure (Failed message)` | Corrective attempt remains `Resumed_attempt` with `Failed message` |
| `Resume_failure Timeout` | Corrective attempt remains `Resumed_attempt` with `Timed_out` |
| `Resume_failure Cancelled` | Corrective attempt remains `Resumed_attempt` with `Cancelled` |
| `Resume_failure Success` | Rejected as incoherent rather than rewritten |

The fixed host-neutral `Schema_retry_failed` category deliberately does not
expose Cabal's backend-specific distinction between a recognized resume rejection
and another failed resumed transport. It nevertheless preserves the resumed
attempt kind and actual result status; the table does not claim to retain the
discarded backend-specific label.

Use Cabal's sanitized `render_rich_completion_error` output only as the
in-process diagnostic. Safe CWR projection omits it. The dispatch-cause mapping
is exhaustive for the current `Runtime_dispatch.error` algebra:

| Cabal dispatch cause | Invocation knowledge | CWR error shape |
|---|---|---|
| `Invalid_timeout` | Definitely not invoked | `Dispatch_failure Invalid_request` |
| `Backend_not_registered` | Definitely not invoked | `Dispatch_failure Backend_unavailable` |
| `Runtime_registration_untrusted` | Definitely not invoked | `Dispatch_failure Capability_mismatch` |
| `Runtime_entry_invalid _` | Definitely not invoked | `Dispatch_failure Capability_mismatch` |
| `Expected_entry_mismatch` | Definitely not invoked | `Dispatch_failure Capability_mismatch` |
| `Backend_quarantined _` | Definitely not invoked | `Dispatch_failure Capability_mismatch` |
| `Preflight_failed _` | Definitely not invoked when carried by plain `Dispatch_failure`; post-execution cleanup uses `Dispatch_failure_with_execution` | `Dispatch_failure Preflight_failed`, or the post-execution shape in the table above |
| `Backend_version_unsupported` | Definitely not invoked | `Dispatch_failure Capability_mismatch` |
| `Version_check_failed` | Definitely not invoked | `Dispatch_failure Internal_dispatch_failure` |
| `Backend_unavailable` | Definitely not invoked | `Dispatch_failure Backend_unavailable` |
| `Availability_check_failed` | Definitely not invoked | `Dispatch_failure Internal_dispatch_failure` |
| `Prepared_already_consumed` | Definitely not invoked by this call | `Dispatch_failure Internal_dispatch_failure` |
| `Backend_execution_failed` in plain `Dispatch_failure` | May have started, no completed result | `No_completed_attempt { status = Failed _; invocation_may_have_started = true; event_trace = Some mapped_trace }` |
| `Backend_execution_failed` in `Dispatch_failure_with_execution` with a synthetic failed final result | Completed progress exists; a continuation may be invoked but uncommitted | `Incomplete_execution` with outer `Failed _`, exact completed prefix, optional continuation, and complete outer trace |
| `Schema_enforcement_failed _` | The current detailed guarded path does not emit this compatibility projection; structured cases are `Execution_failure` | If received defensively, `No_completed_attempt { status = Failed _; invocation_may_have_started = true; event_trace = Some mapped_trace }`; never claim pre-invocation |

Attempt kinds, result statuses, token counts, delivery modes, media counts, web
levels, cleanup states, sessions, and normalized events have direct conservative
projections. Opaque retry reasons map to `Other_redacted`; process-exit text that
cannot be classified maps to `Unknown`; process ids and raw process streams are
deliberately outside the safe host-neutral projection. These are explicit
redactions, not rewritten telemetry.

## Cabal release migration blocker

- [ ] A Cabal release must contain commit
  `95dff454331dc610ce2db9d44924f2be818a1c6b` before this bridge is merged or
  published as normally installable.
- [ ] Until then, keep CI, release, and developer setup pinned to that exact
  commit. Do not guess a future package version or claim release readiness.
- [ ] Once released, verify a clean unpinned package installation, then update
  dependency metadata and remove the temporary commit pins in one reviewed
  migration.

The installable bridge applies one deterministic conversion policy to Cabal's
optional `cost_usd : float`: finite non-negative values become
`ceil (cost_usd * 1_000_000)` integer micro-USD, and positive representational
overflow saturates at `Int64.max_int` consistently with CWR aggregate saturation.
Negative and non-finite values fail conversion rather than being dropped or
rewritten. The same rule applies to cost carried by normalized usage events.

### Pre-release telemetry-mapping failure addition

Exhaustive adopters of the unreleased `error_view` type must add a
`Telemetry_mapping_failure` arm. Its required `event_trace` is the exact safe
mapped Cabal trace and its in-process diagnostic is omitted from persistence.
The JSON projection uses error kind `"telemetry_mapping_failure"` under the
existing `cwr.agent-execution.error/v1` envelope.

### Pre-release incomplete-execution addition

Exhaustive adopters of the unreleased `error_view` type must add an
`Incomplete_execution` arm. Error-projection consumers must likewise accept the
new `"incomplete_execution"` kind, whose nested value is versioned
`cwr.agent-execution.incomplete/v1`. The existing error envelope remains
`cwr.agent-execution.error/v1` because none of these projections has shipped.

### Pre-release native error rename

The unreleased `Native_schema_rejection` name and
`"native_schema_rejection"` projection tag were replaced by
`Native_backend_failure_with_schema` and
`"native_backend_failure_with_schema"`. Exhaustive adopters of the pre-release
API must rename that match arm. No compatibility alias is kept because it would
continue to expose the incorrect schema-causality claim.
