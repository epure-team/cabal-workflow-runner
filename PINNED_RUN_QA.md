# Pinned Run QA

**Date:** 2026-07-11  
**Status:** GO ✅

## Required gates

- `opam exec --switch=/home/mathias/dev/cabal -- dune runtest`
- `opam exec --switch=/home/mathias/dev/cabal -- dune build`
- `python3 scripts/parity_check.py`
- `scripts/pinned-run-selftest.sh`
- `shellcheck scripts/pinned-run-selftest.sh`
- `git diff --check`

## Behavioral coverage

- parser/schema accept a lowercase SHA-256 pin and reject malformed pins;
- pinned backend dispatch records path/digest in output and ledger;
- replay accepts the recorded identity and rejects digest tampering;
- a backend returning an identity different from the declared pin blocks;
- the CLI executes a verified snapshot, while the executing tool replaces its
  original pathname, and records the original resolved identity;
- symlink and hardlink executable candidates fail secure acquisition;
- a second run rejects the replaced bytes before dispatch;
- unpinned legacy Runs retain their existing trace shape and backend path.
