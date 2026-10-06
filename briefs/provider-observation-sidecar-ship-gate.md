# Draft PR ship gate — provider-observation-sidecar

CWR adds opt-in durable start/final provider observations, rejects unterminated existing JSONL and withholds uncertified Codex usage. Backend/Engine replay contracts remain unchanged; Cabal stays outside the core library.

Review and QA are GO in the task's recorded Full mode. Actual normalizer, invocation trace and convergence reports are retained. Build and forced full tests pass (202 cases across six suites; sink 5/5, observation CLI 4/4, host policy 8/8); whitespace checks pass. Different-runtime attempts were degraded and discarded, with actual QA breaker checks. Full scope manifests and local hooks are absent and explicitly recorded; no task spec/KB or production request/child coverage is claimed.

Branch: `feat/provider-observation-sidecar`. Contribution repository: `epure-team/cabal-workflow-runner`, base `main`. GitHub base fetched and verified as ancestor; no existing PR for this branch was found. Original GitLab origin is preserved; push uses the explicit canonical GitHub URL.

The user authorized upstream draft PR creation. The requested stopping point is an open draft; merge and branch deletion are out of scope. Cabal's trusted same-repository PR must follow its automatic Épure mirror and may not merge independently. CWR adoption requires the Cabal helper/correction contribution.

Only owned source, tests and generic review/QA/ship artifacts are staged. Incidental Cabal package regeneration, private staged install and locally installed review tooling are excluded.
# Draft handoff result

Draft PR: https://github.com/epure-team/cabal-workflow-runner/pull/23, verified OPEN/draft against main with initial artifact head 9c29a78. Cabal dependency: https://github.com/epure-team/cabal/pull/38. CI queued/in progress, not claimed passing; no merge. Cabal remains CLI/runner-only, absent from core library dependencies.

Pre-push actual canonical Roster static review-convergence (max-rounds 5, strikes 2, timeout 120), QA convergence (max-rounds 5), and git diff --check all exited zero. Main 81dcdebbbcaba0c8d0845819b33b4bc98d24786d was fetched, checked as ancestor, and duplicate PR absence checked. Explicit GitHub-URL git push -u, gh pr create --draft, and gh pr view succeeded without altering GitLab origin. Metabolism skipped: no harness.json. Advisory cost snapshot skipped: ledger dates lack ISO time bounds.
