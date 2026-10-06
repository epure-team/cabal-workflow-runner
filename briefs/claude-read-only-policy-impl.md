# Claude read-only policy implementation

Status: COMPLETED; independent review/QA and upstream PR pending.

Scope/acceptance: `claude-read-only-policy-feature.md`. Implementation is entirely
in CWR's CLI backend layer. Read_only_policy validates host flags and narrows
the handwritten Claude command; Cabal result/task_spec and Backend/Engine
contracts are unchanged. Public CLI docs explain per-invocation semantics and
residual OS/managed-policy boundaries.

Baseline full suite passed. Test-first native bounded toolset test returned
124 (unknown flags) before implementation and passes now. Eight CLI tests pass:
exact safe argv, mutable/Codex/unsafe tools/partial policy/zero budget/NaN/zero
turn refusal with no fake provider dispatch. Full `dune runtest` and build pass.
`git diff --check` is the available whitespace gate; no formatter gate configured.

Installed Claude 2.1.291 help confirms tools, native safe/restricted mode,
strict MCP, budget and permission mode. Safe-mode preserves auth; bare explicitly
does not read OAuth, so it was not selected. No real provider call was performed
for this new policy by this agent; the consumer's approved bounded run remains
separate evidence. No target/findings, prompts, credentials or transcripts in
upstream artifacts.

Review focus: exact toolset versus permission allowlists, remove bypass,
strict empty MCP, all-or-none host validation, read-only Claude-only dispatch,
no fallback, host ceilings override old spec maximum turns, CLI limits not
campaign totals, admin-managed policy/OS boundaries honestly documented.

Process deviations: parent approved feature directly; no new intake interview.
Independent specialist/review pending availability. Canonical Roster review
bundle installed separately; consumer CWR ESM package breaks installed CommonJS
entrypoints, while canonical source scripts can operate on this target. Formal
pipeline GO is not asserted by this implementation.
