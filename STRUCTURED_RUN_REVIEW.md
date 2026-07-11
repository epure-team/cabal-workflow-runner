# Independent Review — structured Run and read-only attestation inputs

**Date:** 2026-07-11  
**Final verdict:** **GO**

## Initial NO-GO findings

Independent adversarial review found four release blockers in the first v0.17 implementation:

1. `read_only:true` trusted backend IDs resolved to bundled/user YAML adapters whose commands enabled write-capable flags and ignored the task’s read-only flag.
2. Structured stdout was parsed and bound even when the runner marked output truncated.
3. Structured Runs beneath `Parallel` inherited the existing snapshot-based replay behavior, so embedded branch traces were not independently replay-verified.
4. An unknown explicit `agent_type` silently fell back to another backend.

## Closure

- Read-only requests bypass the mutable adapter registry and use only handwritten Claude Code or Codex builders. Exact argv tests require Claude disallowed tools and Codex `-s read-only`; project/global ID-spoof YAML is loaded and proven unable to dispatch.
- Unsafe, custom, or unknown explicit backends fail before dispatch. Default selection considers only the audited implementations.
- `stdout_schema` rejects truncated output before parsing, live and on replay.
- Validation/lint reject structured Runs anywhere under `Parallel`; legacy plain Runs remain compatible.
- Structured input is restricted-canonical JSON on stdin, with a SHA-256 digest and schema-validated parsed stdout recorded and replay-checked.

The reviewer reran every original exploit and found no residual blocker. A pre-existing documentation overstatement about generic Parallel sub-trace replay remains non-blocking because structured Runs are excluded from that path.
