# Approval-Supplied Ledger Identity — Implementation Review

## Outcome

The runner now records runtime approval presence before executing the workflow,
including when a gate blocks before Commit. The ledger event is a header, not a
workflow-controlled trace value.

## Security properties

- `approval_supplied` is a singleton immediately after `ctx_snapshot`.
- It records `Engine.token_digest`, now domain-separated SHA-256
  (`cwr.approval-token/v2\\0`); raw approval bytes never enter the ledger.
- It binds the actual validated workflow digest, optional attestation session,
  and SHA-256 canonical run-context digest.
- Blocked and aborted executions retain the initialized ledger and engine trace.
- Replay validates placement, workflow/session/context binding, digest shape,
  and equality with every `committed_step.token_digest`.
- Ledgers without the new header remain replay-compatible.
- A requested ledger is opened once through a no-symlink directory walk, locked
  before truncation, restricted to an unaliased regular file, and forced to mode
  `0600`. Prefix and terminal trace use the same held descriptor and inode.
- The prefix is flushed before workflow execution. Prefix persistence failures
  prevent execution; terminal write, flush, pathname-identity, or close failures
  refuse a successful CLI result and warn that workflow effects may have occurred.
- Run and replay validate the initial context against the exact restricted
  canonical-JSON contract and report invalid values as controlled errors.

The ledger remains externally editable. Replay proves internal consistency and
does not claim raw-token reauthentication.

## Review verdict

Implementation is scoped to ledger persistence/replay. Workflow JSON, approval
ingress, and Commit authorization semantics are unchanged. Focused and full
tests cover early block, absent approval, legacy replay, reordered/tampered
headers, context/session/workflow mutation, Commit token mismatch, private mode,
symlink and hard-link rejection, a busy lock, pathname replacement, deterministic
write/flush/close failures, and legacy committed MD5-digest replay.
