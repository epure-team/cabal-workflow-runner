# Engine Receipts and Commit Preflight — Implementation Review

**Date:** 2026-07-11  
**Scope:** structured read-only Agent predecessor input, engine-owned Agent
request/result receipts, Attest/lint/replay integration, and optional
approval-time Commit preflight  
**Verdict:** **GO**

## Contract reviewed

1. A legacy Agent or Commit keeps its prior JSON encoding, runtime behavior, and
   ledger shape.
2. An Agent with `input` is read-only, consumes only guaranteed predecessor
   context, is excluded from Parallel, and cannot dispatch without a fresh
   session nonce and runtime backend capability identity.
3. CWR—not the model—constructs request and result receipts. The request binds
   workflow-declared selectors, canonical selected input, input digest,
   session-derived dispatch identity, step, backend capability, role, and the
   read-only bit. The result binds that request, success/outcome, exact output,
   and output digest.
4. Receipts are available at `receipts.<id>` for a later native Attest. Lint
   rejects mutable, missing, future/non-guaranteed, or Parallel receipt paths.
5. Replay reconstructs receipts from the workflow, current predecessor context,
   nonce, and recorded output. It rejects field, selector, digest, output,
   presence, and ordering divergence without dispatching an Agent.
6. Commit preflight is optional and not independently schedulable. CWR checks
   approval first, then applies the operator Run allowlist and path rules, sends
   canonical selected input on stdin, and requires exit zero plus untruncated
   canonical object stdout satisfying the declared schema.
7. A successful preflight emits an engine-owned receipt immediately followed by
   a Commit entry binding that exact receipt and its canonical digest. Replay
   reselects input, revalidates the recorded process result and schema, and
   verifies exact Commit binding without executing.

## Security review

| Threat | Enforced control |
|---|---|
| Model invents or edits its provenance | Receipt construction is after backend return and owned by the engine. |
| Mutable Agent receives trusted predecessor state | `structured-agent-not-read-only` is a validation error; Cabal's handwritten read-only backends remain the dispatch boundary. |
| Selector membership is reinterpreted | Request binds the declared `input_paths` and canonical selected object; replay reconstructs both. |
| Receipt from an unsafe or future producer is signed | Lint requires a guaranteed prior structured, read-only producer; Attest runtime selection remains fail-closed. |
| Branch trace loses receipt state | Structured Agents are rejected beneath Parallel. |
| Replay accepts a forged receipt field | Exact request/result reconstruction and digest checks reject divergence. Authenticated truth still requires the existing native Attest trust root. |
| Preflight runs before human approval | Token check precedes input selection, allowlist evaluation, and process dispatch; a regression test asserts zero calls. |
| Workflow self-authorizes executable | The existing operator-only runtime allowlist is reused; path-bearing command heads are blocked. |
| Truncated or malformed stdout is treated as approval | Nonzero, truncation, JSON/profile, and schema failures all block before Commit. |
| Preflight/Commit split permits an intervening mutation | Preflight is an internal Commit operation; successful trace entries are adjacent and the Commit binds the exact receipt digest. |
| Replay reruns the effect | Replay consumes the recorded process result only; an execution counter proves no second dispatch. |
| Legacy ledger becomes incompatible | New Agent/Commit fields are optional and omitted when absent; ledger decoding accepts the old shape. |

## Review corrections made

- Added `read_only=true` and the declared selector list to the Agent request
  receipt, then added tamper tests for both predecessor membership and result
  digest.
- Closed parser/schema drift by applying non-empty/unique selector validation to
  Agent input and expanding both OCaml and Python parity batteries.
- Added a replay path for a non-canonical Agent output, where a request receipt
  exists but a result receipt correctly cannot be constructed.
- Fixed the adjacent legacy Agent schema-failure replay arm to consume and check
  its recorded block rather than synthesize an unconsumed trace entry.
- Upgraded selected output/receipt paths without a guaranteed predecessor from
  advisory warnings to validation errors for Agent, Attest, and Commit preflight
  authority paths.
- Extended the “every trace variant” ledger round-trip fixture to cover
  structured Agent receipts, preflight execution, and receipt-bound Commit.

## Residual trust boundary

Replay proves internal consistency, not external authenticity. An unsigned
ledger remains attacker-editable. A security decision must select the relevant
`receipts.<id>` paths in a native Attest and verify that artifact against an
independently pinned workflow digest, session nonce, and public key. The Commit
preflight executable is also trusted for domain-specific predicates (for
example, returning nonzero when `ready` is false); CWR deliberately enforces the
generic process/schema/receipt contract rather than hard-coding application
semantics.

