# Draft PR ship gate — claude-read-only-policy

CWR adds all-or-none trusted-host Claude controls for an exact Read/Glob/Grep tool subset and native per-invocation budget/turn ceilings. Mutable or non-Claude dispatch is refused; permission bypass is removed and strict empty MCP is selected.

Review and QA are GO in the task's recorded Full mode. Actual normalizer, invocation trace and convergence reports are retained. Build and forced full tests pass (202 cases across six suites; host policy 8/8); whitespace checks pass. Different-runtime attempts were degraded and discarded, with actual QA breaker checks. Full scope manifests and local hooks are absent and explicitly recorded; no task spec/KB or production request/child coverage is claimed.

Branch: `feat/provider-observation-sidecar`. Contribution repository: `epure-team/cabal-workflow-runner`, base `main`. GitHub base fetched and verified as ancestor; no existing PR for this branch was found. Original GitLab origin is preserved; push uses the explicit canonical GitHub URL.

The user authorized upstream draft PR creation. The requested stopping point is an open draft; merge and branch deletion are out of scope. Cabal's trusted same-repository PR must follow its automatic Épure mirror and may not merge independently. CWR adoption requires the Cabal helper/correction contribution.

Only owned source, tests and generic review/QA/ship artifacts are staged. Incidental Cabal package regeneration, private staged install and locally installed review tooling are excluded.
