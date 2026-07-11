# Approval-ledger independent security review

Date: 2026-07-11  
Reviewer: independent Codex review gate  
Scope: the uncommitted approval-ledger changes in `cabal-workflow-runner`  
Verdict: **GO after Correction Review 2026-07-11**

No implementation files were changed during this review. The only reviewer-authored
file is this Markdown report.

## Executive summary

The approval identity itself is substantially sound: the token digest is
domain-separated SHA-256, the raw token bytes are not serialized, a successful
replay binds the header to the actual validated workflow, attestation session,
initial context, and every Commit digest, and header-free legacy ledgers remain
replayable. The trace printer and ledger codec also handle the new variant.

The feature must not ship yet because requested audit persistence is fail-open.
A ledger initialization or append failure only prints a warning; the workflow can
still execute effects, Commit, and exit `0`. In addition, the prefix writer follows
symlinks, creates a world-readable-by-default file under a normal `022` umask, and
the append phase closes and reopens the pathname after arbitrary workflow execution.
That defeats the core purpose of retaining trustworthy run-start approval evidence.

## Findings

| ID | Severity | Status | Finding |
|---|---|---|---|
| CWR-APR-001 | Critical | Resolved | Requested ledger persistence was fail-open and pathname-racy |
| CWR-APR-002 | Medium | Resolved | Non-canonical context produced uncaught exceptions in run and replay |
| CWR-APR-003 | Medium | Resolved | The normative Commit specification still said approval tokens use MD5 |
| CWR-APR-004 | Low | Resolved | The regression suite omitted ledger I/O failure and filesystem-safety cases |

### CWR-APR-001: requested ledger persistence is fail-open and pathname-racy

Evidence:

- `write_ledger_prefix` uses `Out_channel.with_open_bin`, which follows symlinks
  and creates files using the process umask rather than an explicit private mode
  (`bin/main.ml:159-167`).
- `append_ledger_trace` closes over no durable descriptor and later reopens the
  pathname with `Open_append` (`bin/main.ml:169-175`). Arbitrary workflow execution
  occurs between those two operations (`bin/main.ml:227-233`).
- Prefix failure is converted to `ledger_started = false`, after which the engine
  still runs (`bin/main.ml:218-233`).
- Append failure only prints an error (`bin/main.ml:239-245`), and a committed or
  completed outcome still returns success (`bin/main.ml:246-249`).

Observed behavior:

1. Running a Commit workflow with `--ledger /tmp`, where `/tmp` is a directory,
   printed `could not initialize ledger: /tmp: Is a directory`, then reported
   `Committed(...)` and exited `0`.
2. A permitted Run step deleted the initialized ledger pathname and replaced it
   with a directory. The subsequent Commit succeeded, append printed `Is a
   directory`, and the CLI exited `0`.
3. Supplying a symlink as the ledger path truncated and replaced the symlink
   target with ledger contents.
4. A newly created ledger had mode `0644` under umask `022`.

Impact:

- An operator explicitly requesting an audit ledger can receive a success result
  even though no complete ledger exists.
- Effects and Commit can occur without the promised approval evidence or terminal
  trace being durably recorded.
- A local attacker, concurrent process, or the workflow's own allowed Run step can
  replace the path between prefix and append, redirecting or suppressing evidence.
- Symlink following can clobber an unintended file, and mode `0644` exposes the
  initial context and a deterministic token digest to other local users.

Required fix:

1. Open the requested ledger once, before engine execution, with explicit `0600`
   permissions and no symlink following. Define and document whether an existing
   regular file is replaced or rejected.
2. Keep the same file descriptor/inode open for prefix and trace writes. Do not
   reopen the path after workflow-controlled effects.
3. Flush the complete prefix before executing the engine. If creation, writing,
   or flushing the prefix fails, return non-zero without running the workflow.
4. If the terminal append/flush fails after effects have occurred, return non-zero
   with an explicit message that effects may have occurred and the audit ledger is
   incomplete. It cannot be represented as a successful committed run.
5. Add deterministic tests for initialization failure, append failure, path
   replacement, symlink rejection, private mode, and all four engine outcome
   classes.

### CWR-APR-002: non-canonical context produces uncaught exceptions

Evidence:

- `--ctx` is checked only with `Canonical_json.validate_no_duplicates`
  (`bin/main.ml:194-209`). This accepts JSON forms that the restricted canonical
  serializer rejects, including floats.
- `approval_run_context_digest` calls `Attestation.canonical_string` directly
  rather than returning or handling a validation result (`bin/main.ml:141-148`).
- The same function is called during replay validation (`bin/main.ml:264-270`).

Observed behavior:

```text
$ cwr run --approve tok --ledger ... --ctx '{"x":1.5}' workflow.json
exit 125
cabal-workflow-runner: internal error, uncaught exception:
Invalid_argument("$.initial_ctx.x: floats are not canonical; use an integer")
```

A syntactically valid headered ledger whose `ctx_snapshot` contains the same float
also caused `replay` to exit `125` with the same internal exception instead of a
clean corrupt-ledger error.

Required fix: validate context with the exact restricted-canonical contract before
constructing an approval header, make context-digest construction return a
`result`, and map invalid run input or ledger content to stable CLI exit codes and
messages. Add run and replay regression cases for floats and unsupported integer
representations.

### CWR-APR-003: normative specification contradicts the implementation

`SPEC.md:351-357` still states that the runtime approval token is hashed with
`Digest.MD5`. The implementation now correctly uses
`sha256("cwr.approval-token/v2\\0" || raw_token)` (`lib/engine.ml:3-5`), and the
new ledger section documents SHA-256 (`SPEC.md:493-504`). The two normative
statements conflict.

Required fix: update section 2.1 to the domain-separated SHA-256 construction and
clarify that a deterministic digest is not secrecy for a low-entropy or reused
token. Approval tokens should be high-entropy and scoped to one run; domain
separation prevents cross-protocol reuse but does not prevent offline guessing.

### CWR-APR-004: missing negative I/O regression coverage

The new shell test covers blocked and committed runs, approval absence, raw-byte
non-disclosure, workflow/session/context mismatches, header ordering, Commit digest
equality, and a header-free replay (`test/test_approval_ledger.sh:21-92`). It does
not cover any filesystem write failure or unsafe-path behavior even though the new
feature changes ledger write timing and introduces a two-phase write.

Required fix: add the CWR-APR-001 and CWR-APR-002 cases to the automated suite.
Also add an explicit committed legacy fixture containing the old 32-hex token
digest; that path passed this review's manual compatibility check but is not
protected by the new shell test.

## Checks that passed

- Domain-separated token digest known-answer test.
- Raw approval-token bytes were absent from generated ledgers.
- The approval header was written after `ctx_snapshot` and before engine events on
  ordinary writable paths, including early Blocked runs.
- Headered replay rejected mismatched workflow, session, initial context, header
  position, malformed token digest, and Commit/header digest disagreement.
- Duplicate JSON keys were rejected by the ledger decoder
  (`lib/ledger.ml:254-257`).
- Header-free legacy replay passed, including a manually tested committed ledger
  with a 32-hex legacy Commit digest.
- The new `Approval_supplied` trace variant is handled by the CLI printer
  (`bin/main.ml:68-71`) and ledger codec.
- Documentation correctly limits replay to internal consistency rather than token
  possession or ledger authentication (`SPEC.md:503-518`, `README.md:365-373`).

## Verification performed

All existing gates passed, which confirms the findings are uncovered cases rather
than ordinary suite failures:

```text
opam exec --switch=/home/mathias/dev/cabal -- dune build @all   PASS
opam exec --switch=/home/mathias/dev/cabal -- dune runtest     PASS
dune exec test/test_cwr.exe                                    125 tests PASS
dune exec test/test_attestation.exe                            21 tests PASS
bash test/test_approval_ledger.sh _build/default/bin/main.exe  PASS
git diff --check                                               PASS
```

## Initial ship gate

The initial verdict was **NO-GO**. It required CWR-APR-001 before any commit or
release and required CWR-APR-002 and CWR-APR-003 in the same approval-ledger
change. The correction review below supersedes that initial gate.

## Correction Review — final zero-open-finding gate

Date: 2026-07-11  
Verdict: **GO**  
Open findings: **0**

All four original fingerprints are closed in the current tree.

### Closure evidence

- **CWR-APR-001 resolved.** The ledger is opened once through the secure
  filesystem boundary, rejects symlinks, hard links, directories, and a busy
  existing file before truncation, forces mode `0600`, holds the same locked
  descriptor for prefix and terminal writes, and checks pathname identity after
  execution (`lib/secure_fs_stubs.c:241-323`, `bin/main.ml:162-180`). Prefix
  write/flush failure returns `1` before `Engine.run`; append write/flush/close
  failure or workflow pathname replacement returns `1`, suppresses a successful
  outcome, and explicitly warns that effects may have occurred
  (`bin/main.ml:230-264`). Independent adversarial runs confirmed all of these
  behaviors, including that a prefix-flush failure created no Run-step marker
  while an append-flush failure retained the effect but refused success.
- **CWR-APR-002 resolved.** Run validates `--ctx` with the restricted canonical
  contract before ledger construction (`bin/main.ml:199-228`), context digest
  construction returns a `result` (`bin/main.ml:141-154`), and replay maps an
  invalid `ctx_snapshot` to a controlled `corrupt ledger` exit `1`
  (`bin/main.ml:355-365`). Float-context run and replay cases pass.
- **CWR-APR-003 resolved.** The normative Commit section now specifies the
  domain-separated SHA-256 construction, high-entropy one-run token requirement,
  offline-guessing limitation, and legacy 32-hex compatibility
  (`SPEC.md:351-361`).
- **CWR-APR-004 resolved.** The CLI regression now covers all four outcomes,
  prefix/append write and flush failures, close failure, aliases, lock
  contention, pathname replacement, canonical context, and an explicit committed
  legacy fixture (`test/test_approval_ledger.sh:37-267`). Every negative check
  uses a named assertion failure path. Mutation probes at lines 26-35 prove that
  inverted equality and forbidden-content assertions fail instead of relying on
  Bash `set -e` behavior. The Aborted fixture uses a structured-output rejection
  and verifies both its `run_executed` receipt and terminal `blocked_at` event
  (`test/test_approval_ledger.sh:46-51`, `95-107`).

### Final verification

```text
opam exec --switch=/home/mathias/dev/cabal -- dune build @all   PASS
bash test/test_approval_ledger.sh _build/default/bin/main.exe  PASS
opam exec --switch=/home/mathias/dev/cabal -- dune runtest     PASS
test/test_cwr.exe                                              125 tests PASS
test/test_attestation.exe                                      21 tests PASS
git diff --check                                               PASS
```

The approval-ledger change has no remaining review finding and is cleared for
the next shipment gate.
