# Pinned Run implementation review

**Date:** 2026-07-11  
**Status:** GO

## Security boundary

`executable_digest` is runtime authority, not workflow discovery metadata. The
runner resolves only the command head through `PATH`, reads it with
`O_NOFOLLOW`, rejects non-regular and multiply-linked files, checks SHA-256 over
the acquired bytes, then executes a private mode-0500 snapshot. It never invokes
the mutable resolved pathname after verification.

The signed/ledger-visible Run result binds both the resolved absolute path and
digest. Replay requires the declared pin and recorded identity to agree and
does not execute. The JS compiler rejects pinned Runs because it cannot offer
the same native guarantee.

## Regression review

- Legacy Runs still call `Backend.run_command` and omit executable identity.
- The single-link restriction is isolated to
  `Secure_fs.read_unaliased_regular`; ordinary workflow, ledger, and artifact
  reads retain their previous semantics.
- Parser, generated schema, handwritten schema artifact, serializer, compiler,
  trace codec, replay, and backend wiring were changed together.
- Pin resolution is fail-closed: the first executable PATH match with a wrong
  digest is rejected rather than silently searching for a later same-name file.

## Residual assumptions

The process runs under the operating-system identity trusted to launch CWR.
The private snapshot prevents pathname replacement within the workflow threat
model; an attacker already able to control the CWR process or its user account
is outside that boundary. For scripts, this feature pins the script bytes, not
the shebang interpreter or other files the script loads; callers that need a
closed dependency set must pin or snapshot those dependencies separately.
