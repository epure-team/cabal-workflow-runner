# Provider observation sidecar implementation

Status: COMPLETED; independent review and QA/ship gates pending.

Parent-approved scope: optional trusted-host durable observations, separate
from engine/replay/model output. Backend.t and Engine records are unchanged;
Cabal remains CLI-only. `README.md` documents flags, safety and unknown fields.
The additive Cabal parser helpers are required; upstream merge must wait for
the Cabal mirror contribution/dependency to be available.

Baseline build/full `dune runtest` passed. Test-first CLI flag acceptance failed
against baseline. Seven new tests and full `dune runtest` now pass, including
fake provider success/nonzero metadata and readonly argv, durable start, resume,
lock/write failure, identifier redaction, symlink/0600 and duplicate finals.
`git diff --check` passes. No configured format/lint gate was discovered.

One tiny authorized real read-only Claude haiku success captured input/output,
cache, session and elapsed; controlled failure tests never alter real provider
authentication/quota. Records omit prompts, raw output, environment and error
prose. Cost/provider/observed model/input semantics remain unknown. Elapsed is
explicitly wall-clock. Live partial events/sub-agent attribution are deferred.

Review focus: secure append's non-truncating branch, identity/lock/fsync and
sticky error handling; start precedes dispatch, final precedes return; repeated
occurrences resume distinct IDs; cancellation classification preserves exception
propagation; no replay/Cabal library dependency changes. Consumer ingress owns
dedup and campaign/finding/revision correlation.

Process deviations: approved parent research/contract reused; specialist slots
unavailable. Independent gates remain pending. The later approved provider
tool/budget policy is separate and is not included here. No merge.
