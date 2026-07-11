# QA Verdict — CWR v0.17.1

**Date:** 2026-07-11  
**Verdict:** **GO**

## Verified gates

- Switched `dune build @install @runtest --force`: **115/115** core tests and **21/21** attestation tests.
- Schema/parser/profile parity: **0 divergences across 77 cases**.
- `scripts/attestation-selftest.sh`: **PASS**.
- `scripts/read-only-selftest.sh`: **PASS**, including global YAML ID spoof, exact safe argv, unsafe/unknown zero-dispatch, and unchanged target files.
- Cabal full and focused Claude/Codex/Story-517 suites: **PASS**.
- ShellCheck for the read-only selftest and `git diff --check`: **PASS**.

## Release boundary

Attestation-bearing workflows may sign Agent output only when the producing Agent is declared read-only. Runtime dispatch then requires an implementation whose concrete invocation enforces that restriction; a capability label or adapter ID alone is not authority.
