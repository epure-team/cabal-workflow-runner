# Authenticated Engine-Output Export — Implementation Review

Date: 2026-07-11  
Status: implemented, independently verifiable, regression suite green  
Scope: uncommitted CWR working tree

## Outcome

CWR now has a native `attest` step that exports selected engine context as a
canonical Ed25519-signed JSON envelope. It is not a `Run` or `Shell` wrapper. The
private signer is held by the engine, is absent from `Backend.t`, and cannot be
forwarded to Agent, Run, or Shell adapters through the typed interface.

The envelope binds:

- schema and algorithm;
- independently pinnable public key and SHA-256 key ID;
- canonical SHA-256 workflow digest, name, and version;
- attest step ID;
- workflow-declared replay domain;
- operator-supplied run/session nonce;
- selected dotted paths and their exact JSON values.

## Trust-boundary decisions

Private seed ingress is `--attestation-key-fd N`, not an inline argument or private-key
pathname. CWR reads exactly 32 bytes, requires EOF, and closes the inherited descriptor
before constructing the cabal backend. Missing key, artifact root, or non-empty nonce
blocks the step.

Output paths are normalized relative paths. Absolute paths, traversal, `.` or empty
components, backslashes, symlink roots/parents/targets, and non-directory roots fail
closed. The writer creates an exclusive same-directory temporary file, flushes it,
atomically renames it, and flushes the parent directory.

Replay never signs or writes. It recomputes the workflow and selection bindings and
verifies the recorded envelope against an operator-pinned public identity and expected
nonce. A forged ledger can replace an envelope only by producing a signature under the
pinned key. The standalone `verify-attestation` command applies the same check directly
to a durable artifact, without a ledger or backend.

## Regression and adversarial coverage

- Ed25519 signing and verification;
- selected-output mutation;
- wrong workflow, step, replay domain, session nonce, and pinned key;
- malformed/missing signature and tampered ledger envelope;
- parser rejection of traversal, absolute, ambiguous, backslash, duplicate-selection,
  and blank-domain inputs;
- missing signer and symlinked output parent;
- exact-length FD read, overlong rejection, and descriptor closure;
- atomic disk output equals the trace envelope;
- authenticated replay and cross-session replay rejection;
- compiler refuses Attest instead of pretending the JS backend preserves authority;
- CLI durable verification rejects wrong context/workflow/step/session/key/artifact;
- all 110 pre-existing tests remain green (130 total, including 20 focused attestation tests).

Commands used for final verification are recorded in the handoff message; the canonical
end-to-end check is `scripts/attestation-selftest.sh`.

## Follow-up hardening

The initial pathname implementation was replaced with native descriptor-relative
`openat`/`renameat2` primitives. Every ancestor is traversed with `O_NOFOLLOW` and
`O_DIRECTORY`; parent inode identity is rechecked around publication; conflicting targets
are never overwritten. Sequential Loop/Foreach occurrences require an `{occurrence}`
template and publish immutable paths. Post-rename durability failure is an explicit `published-uncertain` abort.
Pins, artifacts, contexts, workflows, and ledgers use secure descriptor reads.

Recursive duplicate keys are rejected at parsing boundaries. Signed material uses the
documented restricted canonical profile and binds a deterministic occurrence. Attest IDs
and output paths are globally unique, Parallel ancestry is rejected, and CWR→JS refuses
Attest. Authenticated replay requires operator-pinned workflow digest plus a structurally
guaranteed required attestation. `scripts/verify-attestation.mjs` independently validates
canonical bytes and Ed25519 signatures in Node.

## Review closure table

| Review finding | Closure |
|---|---|
| Duplicate keys / first-vs-last wins | Recursive rejection at workflow, context, ledger, pin, selected-value, and envelope boundaries; raw duplicate parity cases added. |
| Floats and non-canonical numbers | Restricted signed profile rejects Float/Intlit while legacy non-attestation workflows retain 5.0 compatibility. |
| Symlink and concurrent path swaps | Native dirfd traversal, `O_NOFOLLOW|O_DIRECTORY`, inode rechecks, and `RENAME_NOREPLACE`. |
| Conflicting existing artifact | Only byte-identical idempotence is accepted; no replacement path exists. |
| Failure after rename | Explicit `published-uncertain` abort with injected regression. |
| Parallel embedded-trace bypass | Attest beneath Parallel is invalid. |
| Repeated Loop/Foreach substitution | Signed zero-based occurrence is substituted into required templates, producing immutable paths. |
| Duplicate Attest authority | Globally unique IDs and output paths across distinct nodes. |
| Lossy CWR→JS compilation | Compiler refuses every workflow containing Attest. |
| Forged/empty workflow replay | Pinned workflow digest plus statically guaranteed `--require-attestation`; empty and branch-around regressions. |
| Replay without structural requirement | Programmatic authenticated replay rejects a validated workflow with no required IDs. |
| Cross-language drift | Exact UTF-8 byte-order and JS-safe integer vectors run in OCaml and Node. |
| Artifact-embedded self-signing | Node verification requires independent identity/key ID, workflow digest, step/domain/session/occurrence/output, and selected-value pins; attacker-signed regression fails. |
| Relative secure reads/writes | Basename, `./file`, relative pin/ledger/workflow, and artifact root `.` regressions pass while descendant `.` remains forbidden. |
