# QA Verdict — authenticated engine attestation export

**Date:** 2026-07-11  
**Verdict:** **GO**  
**Scope:** native `attest` workflow step, Ed25519 export/verification, replay pinning, canonical JSON profile, secure artifact I/O, CLI integration, compiler/lint/schema compatibility

## Acceptance result

The feature is fit to integrate into `bounty-skills`. Engine-held outputs can be exported as an independently verifiable signed envelope without exposing the private seed to Agent or Run backends. Required workflow digest, public identity, session, attestation ID, occurrence, output path, replay domain, and selected values are bound and fail closed.

## Independent regression evidence

- `rtk opam exec --switch=/home/mathias/dev/cabal -- dune build @install @runtest --force` — **PASS**, 110 legacy + 20 focused tests.
- `rtk opam exec --switch=/home/mathias/dev/cabal -- python3 scripts/parity_check.py` — **PASS**, 0 divergences across 77 parser/schema/profile cases.
- `rtk opam exec --switch=/home/mathias/dev/cabal -- bash scripts/attestation-selftest.sh` — **PASS**, native export, pinned standalone verification, replay, relative-path compatibility, Node cross-runtime verification, attacker self-sign rejection, workflow-bypass rejection, and unsafe-value fail-closed controls.
- Strict C warnings, Bash/Node syntax, smoke/bounty lint, schema no-drift, and `rtk git diff --check` — **PASS**.

## Security controls verified

- Recursive duplicate-key rejection and a documented restricted signed-JSON profile.
- Bytewise UTF-8 key ordering and shared JavaScript-safe integer bounds across OCaml and Node.
- Exact 32-byte key-FD ingress closes before backend construction; private material never enters prompts, traces, ledgers, or errors.
- Operator-pinned workflow digest and required attestation IDs reject empty or branched-around forged workflows.
- `Attest` is rejected under `Parallel`; repeated sequential occurrences require `{occurrence}` and publish immutable no-replace paths.
- Descriptor-relative, no-follow reads/writes reject ancestor symlinks, target conflicts, target swaps, and parent swaps.
- Post-publication durability uncertainty is explicit; unsafe selected runtime values block normally rather than throwing.
- The lossy JavaScript compiler refuses attestation-bearing workflows.

## Residual operational requirement

The expected workflow digest, public identity, session nonce, private seed FD, and artifact root must come from operator-controlled deployment configuration. Runtime pins must never be derived from the mutable workflow or artifact being verified.
