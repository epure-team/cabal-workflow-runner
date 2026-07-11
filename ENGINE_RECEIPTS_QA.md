# Engine Receipts and Commit Preflight — QA

**Date:** 2026-07-11  
**Switch:** `/home/mathias/dev/cabal`  
**Verdict:** **GO**

## Deterministic gates

- `rtk opam exec --switch=/home/mathias/dev/cabal -- dune build @all` — **PASS**.
- `rtk opam exec --switch=/home/mathias/dev/cabal -- dune test` — **PASS**:
  123/123 core tests and 21/21 attestation tests.
- `rtk opam exec --switch=/home/mathias/dev/cabal -- python3 scripts/parity_check.py`
  — **PASS**, 0 divergences across 89 parser/schema/profile cases.
- `rtk opam exec --switch=/home/mathias/dev/cabal -- bash scripts/attestation-selftest.sh`
  — **PASS** (native export, pinned verification, replay, substitution and
  unsafe-value negatives).
- `rtk opam exec --switch=/home/mathias/dev/cabal -- bash scripts/read-only-selftest.sh`
  — **PASS** (exact safe argv, YAML-ID spoof resistance, unsafe/unknown
  zero-dispatch, unchanged target).
- `rtk git diff --check` — **PASS**.

## New behavioral coverage

- structured Agent parser/schema round-trip and non-empty/unique selectors;
- canonical predecessor input delivered to a read-only backend;
- request binding of backend capability, role, read-only bit, declared paths,
  selected input, input digest, nonce-derived dispatch identity, and step;
- result binding of request digest, success/outcome, exact output, and digest;
- Attest selection of prior request/result receipts;
- lint rejection of mutable, missing, and Parallel structured Agents;
- replay success plus request path, result digest, context, nonce, presence, and
  non-canonical-output negative cases;
- Commit preflight approval ordering and zero-dispatch without approval;
- exact argv, working directory, timeout, `observe=None`, and canonical stdin;
- operator allowlist and path-bearing command rejection;
- nonzero exit, truncation, malformed/schema-invalid output fail-closed paths;
- adjacent preflight/Commit trace, exact receipt/digest binding, replay
  no-execution, and binding-tamper rejection;
- legacy Agent and Commit traces, compiler refusal for unsupported structured
  features, and full ledger serialization round-trip.
- real BSD-flock Commit linearization, reserved inode marker binding, immediate
  busy-lock failure, symlink/hardlink/traversal controls, removal/replacement
  rejection, lock release after both outcomes, and replay-tamper rejection.
