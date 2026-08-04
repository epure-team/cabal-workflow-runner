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
| `g-computed` | `outputs.impact-final.parsed.verdict == "pass"` AND `outputs.rules-final.parsed.computed == true` | `arch-impact`'s verdict is anything other than a clean pass — `"fail"` (a finding on a touched line) **or** `"refused"` (no decision analysis, exit 3) — or `arch-rules` never ran a real analysis. Gates on `verdict`, not `computed` (round-1 review), and on the **post-loop** re-verification pass, not the loop's own last iteration (round-2 review) — see the two sections below. The `rules-final.parsed.computed` conjunct is, honestly, the same shape as `g-no-new-findings`: arch-rules always emits root `computed:true` when it prints at all (round-1 finding), so on today's arch-rules this conjunct is constant-true — it looks load-bearing and isn't (round-2 review, F8). It stays: fail-closed, zero cost, and the only thing standing between a `rules-final` schema failure and a gate silently evaluating over a stale/absent binding, same reasoning as arch-impact's own root `computed` field |
| `g-sound` | `outputs.impact-final.parsed.contract_ok == true` | the index is not ⊤-marked, so the closed-cone claim behind the impact numbers is not trustworthy |
| `g-no-new-findings` | `outputs.impact-final.parsed.new_findings == 0` | belt-and-braces alongside `g-computed`'s `verdict` check — arch-impact's real code cannot actually produce `verdict:"pass"` with `new_findings > 0` (both derive from the same finding list), so on today's arch-impact this gate is structurally never the one that fires; it stays as a named, independently-computed check against a future regression in either field, at zero cost (see `test/test_pcc.ml`'s `T2b` for the synthetic scenario that proves it still catches a divergence) |
| `g-rules-pass` | `outputs.rules-final.parsed.failing == 0` | an architecture fitness rule fails. `arch-rules` is invoked with `--on-unknown fail --on-possible fail --on-vacuous fail --on-not-computed fail` pinned explicitly (round-2 review, F3) — arch-index's own defaults (`--on-unknown warn` in particular) are argued for *human* PRs, where a wall of UNKNOWN findings teaches people to delete the rule; gating an *adversarial* agent needs the opposite default, because an UNKNOWN verdict here can mean the agent introduced dynamic dispatch specifically to escape the rule's reach. Pinning also closes an inter-repo drift hole: without it, this gate's real strictness would live in arch-index's runtime defaults, unversioned from cwr's side |
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
path that prints an object at all, refusal included. `outputs.impact-final.parsed.verdict` is the
field that actually distinguishes `"pass"` from `"refused"`, so `g-computed` gates on it instead.

**This was a real bug in the first cut of this workflow**, caught in round-1 review: `g-computed`
originally read `outputs.impact.parsed.computed == true`, which a real refusal always satisfies —
the gate that was supposed to be the one thing standing between a refusal and `Commit` did not
actually stand there. The test that was meant to catch it (`T3`) did not, because its stub
encoded the wrong shape (`computed:false` at the root — a form the real tool cannot produce) and
so never exercised the actual bug. Fixed by reading `verdict` instead — which `T3` now builds from
arch-index's real refusal shape (`computed:true`, `verdict:"refused"`, `findings.computed:false`)
and confirms blocks at `g-computed`, and which `test_each_floor_gate_is_load_bearing` confirms
*structurally*: it mutates the workflow file to remove each of the five floor gates in turn
(including `g-computed`) and asserts `Validate.workflow` fails, naming the missing gate, for every
one of them — an automated, in-suite version of the manual check done during review, not just a
claim about what the validator would do.

## Why the gates read a POST-LOOP re-verification pass, not the loop's own last iteration

**A second, more serious bug**, caught in round-2 review, static, in a version of this workflow
that had already fixed the one above. The correction loop's body ran, in order,
`reindex → impact → rules → dossier → fixer` — `fixer` (write access) **last**. The loop exits on
`fixer`'s own `done` self-report. The floor gates, in that version, read `outputs.impact.parsed`/
`outputs.rules.parsed` — the bindings from the **last executed iteration** of the in-loop steps.

Walk through what that certifies. On the iteration where `fixer` finally reports `done:true`, the
verdicts the gates go on to check were computed by `impact`/`rules`, which ran **before** that
iteration's `fixer` step — against whatever tree state existed **before** `fixer`'s last edit.
`fixer` then makes that last edit and declares itself done. The loop exits. The gates check
verdicts that describe a tree state that no longer exists — the one before the very edit the
fixer made to convince itself it was finished. Neither `reviewer` (structured `input`, reads only
the dossier the gates already trust, never the repo) nor `preflight`/`pcc-preflight` (`dune test`
— the exact thing "code that only looks correct" is built to slip past) can recover the gap. The
committed tree and the verified tree could differ, and nothing in the workflow would know.

For a workflow whose entire thesis is *the proof carries the change*, that gap is the whole
argument, tautologically failing. Fixed by adding a **post-loop verification pass** —
`reindex-final → impact-final → rules-final`, run exactly once after the loop exits, regardless of
which governor or `until` stopped it — and pointing every floor gate (and `reviewer`'s `input`) at
its outputs instead of the in-loop steps'. The in-loop `reindex`/`impact`/`rules` still exist and
still matter: they are what `fixer` sees (via the dossier, next section) to decide what to fix
next. They are simply no longer what anything *trusts*. The loop proposes; the post-loop pass
disposes.

`test/test_pcc.ml`'s `T-late-regression` is the regression test for exactly this bug: `impact`
(in-loop) reports clean, `fixer` declares `done:true` on iteration 1 anyway (as a real fixer would
after making one more edit without re-invoking arch-impact itself — see below), and
`impact-final` (post-loop) reports a finding. Verified red-then-green in the session that fixed
this: pointing the gates back at `outputs.impact.parsed` (the pre-fix shape) makes `T-late-
regression` report `Committed` — the exact false positive — and restoring the fix makes it
`Blocked` at `g-computed` again. `T-converge-iter-3` is the companion positive case: a fixer that
genuinely needs three tries, verified in-loop verdicts vary by iteration via a real per-call stub
sequence (not a single static value), converging to `Committed` only once both the fixer's own
belief *and* the independent post-loop pass agree.

## How `author`/`fixer` actually receive context — and why the obvious approach doesn't work

**A third round-2 finding.** `author` and `fixer` are prompted to read run-context values
(`ctx.task`, `outputs.impact`/`outputs.rules`) that, as originally written, they never actually
received. cwr's *only* channel from the run context into an agent's prompt is the structured
`input` field (`lib/engine.ml`: without it, `effective_prompt` is just the literal `prompt` string
plus two optional file reads — nothing from `outputs`/`ctx` is ever appended). And `input` is
gated: `lib/lint.ml`'s `structured-input-requires-read-only` check rejects any `Agent` step that
declares `input` without `read_only: true`. `author` and `fixer` need write access — that is their
entire job — so they can **never** validly declare `input`. Under any real backend, they were
prompted against information that could not reach them; only the stub backend's own hand-written
per-id table masked this, because a test stub does not go through `effective_prompt`/`input` at
all — it answers by agent id directly.

The fix uses the other channel `Agent` exposes: `protocol`/`brief` are **file paths**, read fresh
by the engine on every invocation (`In_channel.with_open_text`, not cached, not backend-mediated,
not restricted to read-only agents). A `Run` step **can** write a file — that's an ordinary,
observable, allowlisted side effect, unlike trying to smuggle context through the prompt string.
So:

- `author`'s `brief` is `.pcc/task.md`, a file the **operator** writes before invoking
  `cwr run` — replacing the previous (broken) claim that the task arrives via `--ctx`. `--ctx`
  populates the top-level run context, which `author` (no `input`) still cannot see; the file is
  the only channel that actually reaches it.
- `fixer`'s `brief` is `.pcc/dossier.md`, written **fresh every loop iteration** by a new `dossier`
  Run step (`pcc-dossier`, see the convention table below) placed right before `fixer` in the loop
  body. `dossier` re-runs `arch-impact`/`arch-rules` in `--format md` (the same commands `impact`/
  `rules` just ran in `--format json`, against the same just-reindexed tree, so the two never
  disagree) and writes the readable result to a fixed path. `fixer`'s `brief` reads that path
  every iteration, so it always sees the CURRENT iteration's dossier, not a stale one.

This is deliberately not "give the agent tool access to re-run arch-impact itself." A real
cabal-backed coding agent likely *could* do that — but an ad hoc re-run outside a `Run` step is
unobserved: not in the trace, not replay-checked, not gated by the allowlist. The dossier file is
weaker than cwr's structured `input` mechanism (no request/result receipt, no input digest, no
attestation binding — `brief` was never designed to carry adversarial-strength provenance), but it
is still produced by a real, recorded, allowlisted `Run` step, which a free-floating tool call
inside the agent's own session is not. See the threat model below for what this channel does and
does not protect against.

## The `pcc-*` convention

The workflow is domain-neutral — it does not know arch-index's CLI, only four fixed executable
names the target repo provides on `PATH` (and the operator allowlists at runtime via
`--allow-run`):

| binary | contract | arch-index's implementation |
|---|---|---|
| `pcc-index --db <path>` | prints `{"computed":bool,"functions":int,"contract_ok":bool}` | wraps arch-index's own indexing pipeline + `arch-query stats` |
| `pcc-dossier --db <path> --repo <dir> --diff <range> --rules <path> --out <path>` | writes a human-readable Markdown dossier to `--out`; exit 0 regardless of the underlying verdicts (it informs, it does not gate — `impact-final`/`rules-final` are what gate) | runs `arch-impact ... --format md` and `arch-rules ... --format md` against the same DB/args and concatenates them |
| `pcc-preflight` | prints `{"ok":bool,"tests":int}`; **the exit code is the only thing cwr's `Commit` actually reads** — `ok`/`tests` are for a human reading the ledger, not evaluated as a boolean gate (round-2 review, F7; `parse_run_stdout` only checks presence/type against the schema, and `Commit`'s preflight handling blocks on `result.exit <> 0`, never on the *value* of `ok`) | `dune build && dune test` plus the selftest suite, mirroring `.github/workflows/ci.yml`; **must** exit nonzero on any test failure for this contract to mean anything |

`arch-impact` and `arch-rules` are invoked directly (`--format json`/`--format md`) rather than
through a `pcc-*` wrapper, because they already speak the machine-output contract natively as of
arch-index's `feat(json): machine-output contract for arch-impact and arch-rules` (WR-02
Workstream A) — `computed`, `contract_ok`, `verdict`, and int-only counts, no floats, exactly one
JSON object on stdout. See that repo's `docs/change-impact.md` and `docs/fitness-functions.md`
for the field-by-field contract. (An earlier draft also had a `pcc-baseline` step capturing
reference finding counts for a delta comparison — removed, round-2 review F9: `arch-impact`'s own
`new_findings` is already scoped to lines the diff touches, so there was never a comparison left
for a separate baseline to feed; the step was dead code, consumed by nothing.)

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

## Threat model for `read_only: false` agents

`author` and `fixer` have write access to the working tree — that is unavoidable; they are the
steps that make the change. Round-2 review (F4) asked what stops one of them, adversarially or by
accident, from defeating the gates rather than satisfying them. Three buckets, honestly:

**Mitigable, not yet mitigated.** `Run` supports `executable_digest` (`sha256:<hex>`, pins the
exact resolved binary bytes before dispatch) on all five commands this workflow invokes
(`pcc-index`, `arch-impact`, `arch-rules`, `pcc-dossier`, `pcc-preflight`) — none of them use it
today. Without it, `fixer` editing the `pcc-*` scripts or the `arch-impact`/`arch-rules` binaries
themselves (if they're in-repo or on a writable `PATH`) is invisible to the workflow: the next
`Run` step would faithfully execute the *tampered* tool and report its (fabricated) verdict as
gospel. Adopting `executable_digest` needs the real binaries pinned first — out of scope until
Jalon 3 (real `pcc-*` implementations) exists to pin against; noted here so it is not forgotten
once it does.

**The host/operator's responsibility, not this workflow file's.** `fixer` can edit
`arch-rules.txt` — the exact file `rules`/`rules-final` evaluate — and loosen or delete the rule
that would have failed it. It can `git commit` mid-loop, which empties `--diff HEAD` on the next
iteration and blinds `impact`/`impact-final` to everything committed so far (the prompt asks it
not to; nothing enforces that it doesn't). It can edit the `pcc-*` scripts or test files the
`pcc-preflight` invocation runs. None of these are structural gaps in the *engine* — `Run`'s
`observe`/file-diff mechanism would show every one of these edits in the trace, if something
downstream looked for them. Nothing in this workflow does yet. A host embedding this workflow
should, at minimum: treat `arch-rules.txt` as a file whose diff triggers separate scrutiny (a
rule change should never ride through silently inside a "just a bug fix" commit); and verify, out
of band, that the tree is in the state the workflow expects before trusting a `Committed` outcome
(e.g. that `git log` since the run started contains no commits from `fixer` itself).

**Structural, and out of scope for a workflow file to fix.** Coverage: `arch-impact`/`arch-rules`
only see what was indexed. A change entirely inside files the index does not cover (CI config,
build scripts, docs, a tool the index's producer doesn't parse) reaches every gate with zero
findings **by construction**, not because it was judged clean. This is arch-index's own honest-
negative discipline working as designed — "not in the index" is reported as such, never silently
folded into "no impact" — but it means this workflow's guarantee is exactly as wide as the index's
coverage, not wider. Widening coverage is arch-index's job, not this workflow's.

None of this is a reason to distrust the five floor gates for what they *do* check — sound
reachability, fitness rules, no new findings on touched lines. It is a reason not to read
`Committed` as "provably safe in every dimension." It proves what it checks, not more.

## Testing without a backend or an LLM

`test/test_pcc.ml` runs against `Backend.stub` — no cabal, no LLM, no real subprocess —
exercising only the engine's interpretation of the workflow file against hand-written JSON that
matches arch-index's documented contract. `author`/`fixer`'s `brief` file reads are a genuine
engine-level file read (not backend-mediated — see above), so the test binary creates
`.pcc/task.md`/`.pcc/dossier.md` on disk once at startup; every other effect is stubbed. Two
workflow-structure tests, plus thirteen scenarios:

| # | scenario | expected outcome |
|---|---|---|
| — | the real workflow file lints/validates against the five floor gates | `Ok` |
| — | each floor gate is load-bearing | mutating the workflow to remove any one of the five fails `Validate.workflow`, naming the missing gate |
| T1 | nominal: tools clean, fixer converges on iteration 1, reviewer approves, token supplied | `Committed`; ledger round-trips; replay executes zero subprocesses |
| T2 | `new_findings > 0` on every iteration (`verdict:"fail"`), fixer never converges | the loop stops on its `Max_iters` governor, then `Blocked` at `g-computed` — `verdict != "pass"` catches it before `g-no-new-findings` gets a turn |
| T2b | a *synthetic* inconsistent stub: `verdict:"pass"` but `new_findings:1` on the post-loop pass (real arch-impact cannot produce this) | `g-computed` passes (verdict says pass), `g-no-new-findings` still blocks — proves the belt-and-braces gate catches what the verdict check, by construction, cannot |
| T3 | index carries no decision analysis — `arch-impact` refuses, real shape, both in-loop and post-loop (it is a structural DB property, unaffected by the fixer's edits) | `Blocked` at `g-computed` specifically — not a crash, not confused with a clean run |
| T4 | index not ⊤-marked (`contract_ok:false`), both in-loop and post-loop | `Blocked` at `g-sound` |
| T5 | every tooled gate passes, reviewer rejects | `Blocked` at `g-independent` |
| T6 | everything approved, no runtime token supplied | `Blocked` at the `Commit` step itself |
| T7 | a stub prints a float where the schema declares `int` | `Aborted`, never a silent pass — `parse_run_stdout` rejects floats/`Intlit` unconditionally, on every structured `Run`/preflight stdout, with or without an `Attest` step in the workflow |
| T8 | replaying a `Blocked` run (T2's shape) from a serialised ledger | the same terminal outcome reproduced, zero subprocesses executed on replay |
| T-rules-fail | a real fitness-rule violation (`rules-final.failing > 0`), everything else clean | `Blocked` at `g-rules-pass` specifically — the one floor gate no earlier scenario had ever actually exercised |
| T-converge-iter-3 | fixer genuinely needs three tries: in-loop `impact` verdict varies by call (`fail`, `fail`, `pass`, via a real per-iteration stub sequence, not a static value); post-loop pass agrees clean | `Committed` after 3 real iterations — proves multi-iteration convergence, not just T1's single-iteration case |
| T-late-regression | **the regression test for the post-loop-pass fix above**: in-loop `impact` clean, fixer declares done anyway on iteration 1, post-loop `impact-final` dirty | `Blocked` at `g-computed`, via the post-loop pass — confirmed red-then-green: pointing the gates back at the in-loop outputs makes this scenario report `Committed` |
| T-preflight-fails | `pcc-preflight` exits nonzero (the real gate — see F7 above) | `Blocked` at the commit step |

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
- **Not a sandbox.** See the threat model above: `author`/`fixer` have real write access, and
  nothing in this workflow file stops them from editing the rules file they're graded against or
  committing mid-loop. The floor gates prove what they check; they do not constrain what a
  write-access agent can touch.
- **Not the live-agent phase.** Every scenario above stubs `Backend.t`. Wiring real cabal-backed
  agents and a real allowlisted `PATH` is the meta-agent host's job (EP-05), not this workflow
  file's.
