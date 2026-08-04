# `proof-carrying-change` — gating an agent-authored change on tooled verdicts

[`examples/proof-carrying-change.workflow.json`](../examples/proof-carrying-change.workflow.json)
is the first assembly of two layers of the epure stack: a cwr workflow that gates a code change
written by an agent on **verdicts from [arch-index](https://github.com/epure-team/arch-index)** —
change impact, architecture fitness functions — with a governed correction loop, an independent
reviewer, and a `Commit` that still needs a human-supplied runtime token. Nobody's opinion of the
diff, including the authoring agent's own, is a substitute for the tool's answer.

## The five floor gates

```sh
cabal-workflow-runner lint examples/proof-carrying-change.workflow.json \
  --floor g-computed --floor g-sound --floor g-no-new-findings \
  --floor g-rules-pass --floor g-independent
```

Declaring all five as `--floor` gates makes the validator prove, structurally, that **every**
path to `Commit "submit"` passes through all five — no future edit to the workflow file can route
around them. That is the whole mechanism this workflow exists to demonstrate: the workflow author
cannot forget a gate, because a forgotten gate is a validation error, not a silent gap.

| gate |`when` | fails when |
|---|---|---|
| `g-computed` | `outputs.impact.parsed.verdict == "pass"` AND `outputs.rules.parsed.computed == true` | `arch-impact`'s verdict is anything other than a clean pass — `"fail"` (a finding on a touched line) **or** `"refused"` (no decision analysis, exit 3) — or `arch-rules` never ran a real analysis. Gates on `verdict`, not `computed`: see "Why `g-computed` reads `verdict`, not `computed`" below — a round-1 review finding, not the original design |
| `g-sound` | `outputs.impact.parsed.contract_ok == true` | the index is not ⊤-marked, so the closed-cone claim behind the impact numbers is not trustworthy |
| `g-no-new-findings` | `outputs.impact.parsed.new_findings == 0` | belt-and-braces alongside `g-computed`'s `verdict` check — arch-impact's real code cannot actually produce `verdict:"pass"` with `new_findings > 0` (both derive from the same finding list), so on today's arch-impact this gate is structurally never the one that fires; it stays as a named, independently-computed check against a future regression in either field, at zero cost (see `test/test_pcc.ml`'s `T2b` for the synthetic scenario that proves it still catches a divergence) |
| `g-rules-pass` | `outputs.rules.parsed.failing == 0` | an architecture fitness rule fails — and `failing` already counts `VACUOUS` and `NOT_COMPUTED` verdicts as failing by arch-index's own default policy, so this workflow does not have to re-encode that judgment |
| `g-independent` | `outputs.reviewer.verdict == "approved"` | the independent reviewer agent — reading only the dossier, never the repo — did not approve |

### The exit-code / `verdict` tripartition, and how a `Run` step sees it

`arch-impact` has three outcomes: exit 0 (`verdict:"pass"`), exit 1 (`verdict:"fail"` — a finding
on a touched line), exit 3 (`verdict:"refused"` — `--fail-on-new-findings` requested but this
index carries no decision analysis, so "clean" would be a lie about data that was never
computed). `arch-rules` only ever produces `"pass"`/`"fail"` — it has no process-level refusal;
an un-⊤-marked or data-less index degrades *individual rules* to `UNKNOWN_NO_CONTRACT`/
`NOT_COMPUTED` instead (see arch-index's `docs/fitness-functions.md`). Exit 2 (malformed input or
infrastructure error) never reaches this workflow as a considered verdict at all — see cwr's own
tripartition below.

| arch-impact exit | `verdict` | this workflow's `g-computed` |
|---|---|---|
| 0 | `"pass"` | passes (assuming `g-rules-pass`/rules side is also clean) |
| 1 | `"fail"` | blocks |
| 3 | `"refused"` | blocks — same gate, same treatment as `"fail"`, never read as a pass |
| 2 | *(no JSON printed)* | the `Run` step itself aborts on an unparseable/empty stdout — see below, this never reaches a gate at all |

Every `when` follows the "UNKNOWN ≠ NO" discipline: each predicate requires the **positive
presence** of a computed value (`eq [...; {"lit":true}]` or `eq [...; {"lit":0}]`), never the
absence of a problem. A missing path evaluates `false` in cwr's total `Expr.eval` (a documented
property — a missing key never raises, it just makes the predicate false), so a bug that silently
drops a field fails the gate instead of silently passing it.

## Why `g-computed` reads `verdict`, not `computed`

A naive reading of `Run { stdout_schema }` might assume a nonzero exit code fails the step. It does
not: `lib/engine.ml`'s `Run` handling never inspects `result.exit` before binding the parsed
output — the full `run_result` (including `exit`) is bound to `outputs.<id>` unconditionally,
and pass/fail is entirely a downstream `Gate`'s job. (Contrast `Commit`'s `preflight`, which *does*
fail-close on a nonzero exit — a different, stricter contract for the one step that can file.)

This matters concretely for `arch-impact`: its `--fail-on-new-findings` sound-refusal path prints
its **full JSON object first** — with root **`computed:true`**, same as any clean run; only the
nested `findings.computed` goes `false` — and only *then* exits 3. So a refused run is not
rejected by cwr's `Run` machinery; it reaches the context exactly like a clean run does, and a
gate reading root `computed` alone would pass it straight through: `computed:true` is on **every**
path that prints an object at all, refusal included. `outputs.impact.parsed.verdict` is the field
that actually distinguishes `"pass"` from `"refused"`, so `g-computed` gates on it instead.

**This was a real bug in the first cut of this workflow**, caught in round-1 review: `g-computed`
originally read `outputs.impact.parsed.computed == true`, which a real refusal always satisfies —
the gate that was supposed to be the one thing standing between a refusal and `Commit` did not
actually stand there. The test that was meant to catch it (`T3`) did not, because its stub
encoded the wrong shape (`computed:false` at the root — a form the real tool cannot produce) and
so never exercised the actual bug. Fixed by reading `verdict` instead — which the `test/test_pcc.ml`
`T3` scenario now builds from arch-index's real refusal shape (`computed:true`,
`verdict:"refused"`, `findings.computed:false`) and confirms blocks at `g-computed`, and which
`test_each_floor_gate_is_load_bearing` confirms *structurally*: it mutates the workflow file to
remove each of the five floor gates in turn (including `g-computed`) and asserts
`Validate.workflow` fails, naming the missing gate, for every one of them — an automated,
in-suite version of the manual check done during review, not just a claim about what the
validator would do.

## The `pcc-*` convention

The workflow is domain-neutral — it does not know arch-index's CLI, only three fixed executable
names the target repo provides on `PATH` (and the operator allowlists at runtime via
`--allow-run`):

| binary | contract | arch-index's implementation |
|---|---|---|
| `pcc-index --db <path>` | prints `{"computed":bool,"functions":int,"contract_ok":bool}` | wraps arch-index's own indexing pipeline + `arch-query stats` |
| `pcc-baseline --db <path>` | prints `{"computed":bool,"findings":int}` | captures the reference counts from the freshly built index |
| `pcc-preflight` | prints `{"ok":bool,"tests":int}`, must exit 0 | `dune build && dune test` plus the selftest suite, mirroring `.github/workflows/ci.yml` |

`arch-impact` and `arch-rules` are invoked directly (`--format json`) rather than through a
`pcc-*` wrapper, because they already speak the machine-output contract natively as of
arch-index's `feat(json): machine-output contract for arch-impact and arch-rules` (WR-02
Workstream A) — `computed`, `contract_ok`, `verdict`, and int-only counts, no floats, exactly one
JSON object on stdout. See that repo's `docs/change-impact.md` and `docs/fitness-functions.md`
for the field-by-field contract.

## Why `--diff HEAD`, not a commit range

`arch-impact --diff` is passed straight to `git diff <range>` (a single allowlist check rejects
anything starting with `-` other than `--staged`/`--cached`; everything else must be a genuine git
range or ref). `git diff HEAD` — a single ref, not `A..B` — diffs the **working tree** against the
last commit, so it sees the author/fixer agent's changes even while they remain **uncommitted**.
The `author` and `fixer` steps are prompted to leave the change uncommitted for exactly this
reason: committing early would remove it from `--diff HEAD`'s view on the very next loop
iteration. (This resolves an open question from the WR-02 planning doc — the answer is in
arch-index's `lib/arch_tools/arch_diff.ml`, not assumed.)

## What `Commit` does — and does not — do

`Commit`, once its runtime token (and optional `preflight`) checks pass, emits a
`Committed_step` trace entry and sets the run's terminal outcome. That is the entire engine-level
effect. There is no git push, no PR creation, nothing beyond the trace and the outcome — SPEC.md
calls `Commit` "the only step that can file/submit," but the actual filing is either the
`preflight` command's own side effect (if you give it one) or entirely the calling host's
reaction to seeing `Committed` in the returned outcome. This workflow's `preflight` runs
`pcc-preflight` (the test suite) as a last check that the ledger's approval means something, but
it does not push. Wiring an actual "submit" action is a host/embedder decision, out of scope for
this workflow file, same as the live-agent dispatch phase (EP-05) is out of scope for WR-02.

## Testing without a backend or an LLM

`test/test_pcc.ml` runs against `Backend.stub` — no cabal, no LLM, no real subprocess —
exercising only the engine's interpretation of the workflow file against hand-written JSON that
matches arch-index's documented contract. Two workflow-structure tests, plus nine scenarios
(T1–T8 and T2b):

| # | scenario | expected outcome |
|---|---|---|
| — | the real workflow file lints/validates against the five floor gates | `Ok` |
| — | each floor gate is load-bearing | mutating the workflow to remove any one of the five fails `Validate.workflow`, naming the missing gate — the automated version of the round-1 review's manual check |
| T1 | nominal: tools clean, fixer converges on iteration 1, reviewer approves, token supplied | `Committed`; ledger round-trips; replay executes zero subprocesses |
| T2 | `new_findings > 0` on every iteration (`verdict:"fail"`), fixer never converges | the loop stops on its `Max_iters` governor, then `Blocked` at `g-computed` — `verdict != "pass"` catches it before `g-no-new-findings` gets a turn |
| T2b | a *synthetic* inconsistent stub: `verdict:"pass"` but `new_findings:1` (real arch-impact cannot produce this — both fields derive from the same finding list) | `g-computed` passes (verdict says pass), `g-no-new-findings` still blocks — proves the belt-and-braces gate catches what the verdict check, by construction, cannot |
| T3 | index carries no decision analysis — `arch-impact` refuses, real shape (`computed:true`, `verdict:"refused"`, `findings.computed:false`) | `Blocked` at `g-computed` specifically — not a crash, not confused with a clean run (this is the scenario that exposed the round-1 bug once its stub was corrected to the real shape) |
| T4 | index not ⊤-marked (`contract_ok:false`) | `Blocked` at `g-sound` |
| T5 | every tooled gate passes, reviewer rejects | `Blocked` at `g-independent` |
| T6 | everything approved, no runtime token supplied | `Blocked` at the `Commit` step itself |
| T7 | a stub prints a float where the schema declares `int` | `Aborted`, never a silent pass — `parse_run_stdout` rejects floats/`Intlit` unconditionally, on every structured `Run`/preflight stdout, with or without an `Attest` step in the workflow |
| T8 | replaying a `Blocked` run (T2's shape) from a serialised ledger | the same terminal outcome reproduced, zero subprocesses executed on replay |

Run them with `dune test`, or standalone with `dune exec test/test_pcc.exe`.

**A note on "byte-identical" replay.** The actual byte-identical guarantee is `Engine.replay`
succeeding at all — it raises `Replay_mismatch` on any structural divergence from the recorded
trace (an entry out of order, ill-typed, or left over after the walk completes). T1/T8's
`outcome_testable` assertions are a coarser, additional check that the terminal outcome *string*
also matches; they're worth having, but don't call them "the proof" — the absence of an exception
already is one.

**A caveat on `Run` stdout size.** `stdout_schema`-validated stdout is capped at 64 KiB
(`lib/engine.ml`'s runner truncation); a very large diff could in principle push `arch-impact`'s
JSON (whose lists are unbounded — see arch-index's `docs/change-impact.md`) past that cap. The
failure mode is explicit, not silent: `parse_run_stdout` errors `"stdout was truncated"` and the
`Run` step `Aborted`s, same as any other structured-stdout rejection — it does not read as a
pass. Worth knowing about before pointing this workflow at a repo with unusually large diffs.

## What this is not

- **Not a score.** Every gate reads a count or a boolean — findings, failing rules, a contract
  flag — never a percentage or an arbitrary numeric threshold.
- **Not an LLM-judged gate.** The five floor gates are pure predicates over tool output. The
  `reviewer` agent is *in addition* to them, not instead of them — it can only add a sixth
  blocking condition (`g-independent`), never waive the first four.
- **Not a way around a sound refusal.** `arch-impact`'s exit 3 means "this index cannot answer
  soundly." This workflow turns that into `Blocked` at `g-computed`, same as a genuine failure —
  there is no path that reads a refusal as a pass. If the target repo has no decision analysis,
  running `decision-lint`/the equivalent indexer against it first is the fix; changing the gate is
  not.
- **Not the live-agent phase.** Every scenario above stubs `Backend.t`. Wiring real cabal-backed
  agents and a real allowlisted `PATH` is the meta-agent host's job (EP-05), not this workflow
  file's.
