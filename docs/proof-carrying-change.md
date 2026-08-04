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
| `g-computed` | `outputs.impact.parsed.computed == true` AND `outputs.rules.parsed.computed == true` | either tool never produced a real analysis (a `Run`'s result is bound to the context **regardless of its exit code** — see "Why `computed` is not belt-and-braces" below — so this is the primary defense, not a redundant one) |
| `g-sound` | `outputs.impact.parsed.contract_ok == true` | the index is not ⊤-marked, so the closed-cone claim behind the impact numbers is not trustworthy |
| `g-no-new-findings` | `outputs.impact.parsed.new_findings == 0` | the diff touches a line that already carries a dead-logic/dead-block finding |
| `g-rules-pass` | `outputs.rules.parsed.failing == 0` | an architecture fitness rule fails — and `failing` already counts `VACUOUS` and `NOT_COMPUTED` verdicts as failing by arch-index's own default policy, so this workflow does not have to re-encode that judgment |
| `g-independent` | `outputs.reviewer.verdict == "approved"` | the independent reviewer agent — reading only the dossier, never the repo — did not approve |

Every `when` follows the "UNKNOWN ≠ NO" discipline: each predicate requires the **positive
presence** of a computed value (`eq [...; {"lit":true}]` or `eq [...; {"lit":0}]`), never the
absence of a problem. A missing path evaluates `false` in cwr's total `Expr.eval` (a documented
property — a missing key never raises, it just makes the predicate false), so a bug that silently
drops a field fails the gate instead of silently passing it.

## Why `computed` is not belt-and-braces

A naive reading of `Run { stdout_schema }` might assume a nonzero exit code fails the step. It does
not: `lib/engine.ml`'s `Run` handling never inspects `result.exit` before binding the parsed
output — the full `run_result` (including `exit`) is bound to `outputs.<id>` unconditionally,
and pass/fail is entirely a downstream `Gate`'s job. (Contrast `Commit`'s `preflight`, which *does*
fail-close on a nonzero exit — a different, stricter contract for the one step that can file.)

This matters concretely for `arch-impact`: its `--fail-on-new-findings` sound-refusal path prints
its full JSON object (with `computed:false`) **and then** exits 3 — the print happens before the
refusal check. So a refused run is *not* rejected by cwr's Run machinery; it reaches the context
exactly like a clean run does, and `g-computed` is the only thing standing between a refusal and
`g-independent`/`Commit`. Dropping `g-computed` would not just be redundant-but-safe — it would be
a real hole (verified: `test/test_pcc.ml`'s "T3" scenario, and a mutation check that removes the
gate from the workflow file and confirms every scenario then fails at the validator, before the
engine ever runs).

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

`test/test_pcc.ml` runs eight scenarios (T1–T8) entirely against `Backend.stub` — no cabal, no
LLM, no real subprocess — exercising only the engine's interpretation of the workflow file against
hand-written JSON that matches arch-index's documented contract:

| # | scenario | expected outcome |
|---|---|---|
| T1 | nominal: tools clean, fixer converges on iteration 1, reviewer approves, token supplied | `Committed`; ledger round-trips; replay executes zero subprocesses |
| T2 | `new_findings > 0` on every iteration, fixer never converges | the loop stops on its `Max_iters` governor, then `Blocked` at `g-no-new-findings` by name |
| T3 | index carries no decision analysis (`arch-impact` refuses, `computed:false`) | `Blocked` at `g-computed` specifically — not a crash, not confused with the exit-3 refusal itself |
| T4 | index not ⊤-marked (`contract_ok:false`) | `Blocked` at `g-sound` |
| T5 | every tooled gate passes, reviewer rejects | `Blocked` at `g-independent` |
| T6 | everything approved, no runtime token supplied | `Blocked` at the `Commit` step itself |
| T7 | a stub prints a float where the schema declares `int` | `Aborted`, never a silent pass (see the code comment in `test_pcc.ml` for the exact rejection path — it is stricter than SPEC.md's attestation-only float caveat suggests: `parse_run_stdout` rejects floats/`Intlit` unconditionally, on every structured `Run`/preflight stdout, whether or not the workflow has an `Attest` step) |
| T8 | replaying a `Blocked` run (T2) from a serialised ledger | byte-identical outcome, zero subprocesses executed on replay |

Run them with `dune test`, or standalone with `dune exec test/test_pcc.exe`.

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
