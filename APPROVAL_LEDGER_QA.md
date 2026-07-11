# Approval-Supplied Ledger Identity — QA

## Commands

```sh
opam exec --switch=/home/mathias/dev/cabal -- dune runtest
opam exec --switch=/home/mathias/dev/cabal -- dune build @all
```

## Covered cases

- approval evidence and terminal trace persist across `Committed`,
  `Completed_no_commit`, `Blocked`, and deterministic structured-Run `Aborted`
  outcomes;
- no approval + early block has no approval header;
- valid replay with the exact workflow/session/context;
- legacy header-free replay remains accepted;
- wrong workflow, session, context, ordering, or Commit token fails closed;
- raw approval token is absent from ledger bytes;
- ledger codec round-trips the new entry kind;
- a new or pre-existing ledger is held as one locked descriptor, has mode `0600`,
  and rejects symlink, hard-link, directory, and concurrently locked targets;
- prefix write/flush failures prevent the workflow from running;
- terminal write/flush/close failures and pathname replacement refuse success
  after effects, without printing a committed-success result;
- every negative shell check uses an explicit named failure path (no bare `!`
  command or non-final `&&` assertion), with mutation probes proving inverted
  equality and forbidden-content assertions are detected;
- restricted-canonical context failures in run and replay return controlled
  exit code `1` rather than an uncaught exception;
- a committed header-free legacy ledger with a 32-hex token digest still replays.

## Verdict

Implementation gate complete: 125 core tests, 21 attestation tests, the expanded
approval-ledger CLI regression, `dune build @all`, and the
attestation/pinned-run/read-only external self-tests all pass. Final shipment
remains subject to the independent security review. This repository does not
have an installed coverage instrumenter, so verification uses its complete
deterministic test matrix rather than a coverage percentage.
