# Changelog

## v0.8

A **corrective release** addressing five findings from an external review. Two engine
safety fixes (fail-closed on a failed agent; replay rejects trailing trace garbage), two
analysis fixes (`Exists` over a present object; branch-dependent dangling-ref detection),
and one CLI honesty fix (a real consumable budget). CLI `--version` → `0.8.0`.

### Fixed

- **F1 (High) — a failed agent step now FAILS CLOSED.** Previously `Engine.run`'s `Agent`
  branch fell through `| _ -> st` on `success = false`, binding the backend's `{"error":…}`
  output and CONTINUING; a later always-true gate + token could then `Commit` despite the
  failure. A `success = false` agent run now **aborts** the walk (`Aborted`, emitting
  `Blocked_at`), mirroring the schema-mismatch arm. The symmetric change is made in
  `Engine.replay` (it consumes the recorded `Blocked_at` and reproduces the `Aborted`), so
  run and replay agree on the trace shape `[Agent_ran{success=false}; Blocked_at; …Aborted]`.
- **F2 (Medium) — `Exists` treats a present object/array as present.** `value_of_json`
  flattens `` `Assoc _ `` to `Null` (fine for comparisons), so `{"exists":"outputs.a.obj"}`
  was false even when `obj` was a present object. `Expr` now resolves `Exists` over the
  RAW JSON (new `resolve_raw`): a present object/array/scalar ⇒ true; an explicit JSON
  `null` or a missing path ⇒ false. Comparison operators keep value-based `resolve`
  (semantics unchanged); `Expr.eval` stays total.
- **F3 (Medium) — lint catches branch-dependent dangling output refs.** `Lint`'s `Branch`
  case previously UNIONED both arms' produced outputs, so a reference AFTER the branch to an
  output produced in only ONE arm was not flagged. The guaranteed-available produced set
  after a `Branch` is now the **INTERSECTION** of the two arms' sets (an output is guaranteed
  only if BOTH arms produce it), mirroring the floor's branch=intersection discipline.
  Field-level merge: an `id` survives only if in both arms; its fields are the field-set
  intersection when both declare a schema, else `None`. Intra-arm behaviour is unchanged. The
  examples remain lint-clean (0 diagnostics).
- **F4 (Medium) — replay rejects trailing extra trace entries.** `Engine.replay` consumed
  from the pending trace but never asserted it was empty when the walk finished, so a trace =
  valid-prefix ++ garbage replayed "successfully". It now raises
  `Replay_mismatch "trailing trace entries after workflow completed"` if any entries remain.
  (`Replay_mismatch` is now exposed in `engine.mli`.)
- **F5 (Low/Med) — `CWR_BUDGET` is a real consumable cap.** `bin/backend_cabal.ml`'s
  `budget ()` returned the env value on EVERY call, so `CWR_BUDGET=10` read `10` forever and
  the `Budget` governor never fired. `budget` is now a per-`make` (per-run, shared across all
  loops = a total run budget) mutable counter initialised from `CWR_BUDGET` (default
  1_000_000) that **decrements and returns** the remaining. Documented off-by-one: with
  `CWR_BUDGET=N` the counter starts at N, readings are N−1…0, and the `Budget` governor stops
  on the Nth check, so the run performs **at most N** budget-governed loop iterations total.
  Determinism is unaffected (every `Budget_read` is recorded; replay re-feeds recorded
  values and never calls `budget`).

### Tests

- **F1**: a workflow whose first agent stub returns `success = false` (then an always-true
  gate + commit + token) ⇒ `Engine.run` yields `Aborted` (NOT `Committed`), no `Committed_step`
  in the trace, and `Engine.replay ~trace` reproduces the same `Aborted`.
- **F2**: `Exists ["outputs";"a";"obj"]` ⇒ true over a present object, false over an explicit
  `null`, false over a missing path, true over a present scalar and a present array.
- **F3**: a gate after a branch referencing `outputs.x.v` with `x` produced only in the `then`
  arm ⇒ `dangling-output-ref` Warning; a reference to an output produced in BOTH arms ⇒ no
  warning.
- **F4**: a real recorded trace with one extra dummy entry appended ⇒ `Replay_mismatch`; the
  unmodified trace still replays fine.
- **F5**: the engine-level decrementing-budget test pins the exact iteration count (the CLI
  constant→decrement change is verified by reading).

### Docs

- `SPEC.md` / `README.md`: the `Agent` step / engine §3 note the fail-closed-on-failed-agent
  behaviour; the `CWR_BUDGET` description states the honest consumable-budget semantics and the
  off-by-one. `Lint`'s branch comment now documents the intersection discipline.

### Preserved invariants

- Loop ceiling (termination guarantee); gate-Fail-blocks; runtime token digest-only; floor
  reachability; determinism / byte-identical replay; `lib/` depends on **yojson only**;
  `Expr.eval` totality; lint-clean ⇒ validate; schema↔parser parity / no-drift
  (`scripts/parity_check.py` still 0 divergences).

## v0.7.1

Final parity fix from a confirming external audit (94/95 cases agreed; this closes the last one).

- **Integer-valued floats.** JSON Schema's `"type":"integer"` matches any number with zero
  fractional part, so `{"n": 5.0}` is schema-valid — but the parser's `req_bounded_int` accepted
  only `` `Int ``, rejecting `` `Float ``. The schema cannot cleanly forbid `5.0`, so the parser now
  accepts an integer-valued float in `[1, max_int]` (and rejects a fractional one), restoring the
  "parser accepts iff structurally schema-valid" iff. Safe direction either way (the bug was
  over-rejection, never under-acceptance).
- **`scripts/parity_check.py`** — a reproducible, real-validator-driven parity check (jsonschema
  Draft 2020-12 vs the parser over a 30-case battery; exits non-zero on any divergence). Addresses
  the prior weakness that the in-suite behavioral test's expected verdicts were hand-authored.
- Added integer-valued-float cases to `test_schema_parser_behavioral_parity`.

## v0.7

A **corrective release** addressing a third re-audit finding: the published JSON
Schema and the parser **still disagreed on 7 of 54 enumerated inputs**, verified
against the Draft 2020-12 validator. The divergences were value-bound mismatches
and an expr `_`-escape mismatch. This release closes them definitively and states
the governing principle that makes the agreement testable.

**Governing principle (now stated in SPEC and made true):** `Workflow_json.of_string`
accepts a workflow **iff** that workflow is structurally valid per
`schema/workflow.schema.json`. Every *structural* constraint the schema expresses is
enforced by the parser, and vice-versa. Semantic *floor* checks (gate reachability,
ungoverned-loop, etc.) remain `Lint`/`Validate`'s job and are deliberately not encoded
in the schema.

### Fixed

- **`max_iters.n` and `fixpoint.window` bounds now agree on both sides.** The parser
  enforces `1 ≤ v ≤ max_int` **at parse** (new `req_bounded_int`): a value `< 1` is
  rejected with a clear message (`field "n" must be >= 1 (got 0)`); a literal `> max_int`
  is yielded by yojson as `` `Intlit `` and rejected. Previously the parser **accepted**
  `n = -5` / `n = 0` that the schema rejected. The schema's `bounded_int` `maximum` was
  raised from `1073741823` (2³⁰−1) to `4611686018427387903` (OCaml `max_int` on 64-bit),
  so a large-but-valid literal like `1073741824` is now **schema-valid AND parser-accepted**
  (previously schema-rejected); a literal beyond `max_int` remains invalid on both sides.
- **Empty `governors` is now a parse-level shape error.** The schema declares the loop
  `governors` array `minItems:1`; the parser now requires a non-empty list (new
  `req_nonempty_list`), so it no longer **accepts** an empty `governors` array the schema
  rejects. (The richer `ungoverned-loop` *semantic* diagnostic still lives in
  `Lint`/`Validate`.)
- **Expr operator objects are now *strictly* closed on the schema side too.** Expr objects
  are single-operator: the parser requires exactly one key and rejects any extra key,
  including a leading-underscore one (`{"lit":true,"_x":1}` → rejected). The schema's expr
  branches previously carried the `^_` `patternProperty` (inherited from the shared
  closed-object builder) and therefore **accepted** `{"lit":true,"_x":1}`. Expr branches now
  use a distinct strictly-closed builder (`additionalProperties:false`, **no** `^_`), so the
  schema rejects it too. Workflow / step / governor objects keep the `^_` escape hatch (the
  parser allows `_` there).
- The committed `schema/workflow.schema.json` is regenerated from the lib value (the
  no-drift test enforces byte-equality; the `schema` CLI output byte-equals the file).

### Tests

- **New behavioral parity test** (`test_schema_parser_behavioral_parity`) enumerates a
  battery of candidate workflow JSON strings (mirroring the audit's 54-case methodology) —
  unknown vs `_`-prefixed keys per object kind, `max_iters.n` / `fixpoint.window` ∈
  {−5, 0, 1, 2, 1073741824, `>max_int`}, empty vs single `governors`, expr objects with
  0/1/2 keys and an `_`-key, and type confusions — and asserts, per case, that the **parser**
  accepts iff the case is structurally schema-valid (the expected verdict is hard-coded to
  mirror the schema). This drives the parser over the battery, which the prior
  structural-only parity test never did; it fails on pre-fix code (catching the 7
  divergences) and passes after. The structural parity test was adapted: expr branches are
  now asserted strictly closed (no `^_`).

### Docs

- `SPEC.md` / `README.md` state the governing principle, correct the over-claim that expr
  objects took `_` metadata, document the `max_int` bound and the `minItems:1` governors
  rule, and note `output_schema` is the one intentionally-open map.

## v0.6

A **corrective release** addressing a re-audit finding: the published JSON
Schema and the parser **disagreed on unknown keys**. v0.5 made the schema reject
extra keys on `step`/`governor`/`agent` objects (`additionalProperties:false`),
but the **parser still silently accepted them** — so the schema rejected
workflows the runner happily executed, voiding the "schema == what runs"
contract.

### Fixed

- **The parser now rejects unknown keys (closed objects), with a `_`-prefixed
  metadata escape hatch.** `Workflow_json` now rejects, fail-closed, any key that
  is neither a known key for the object nor prefixed with an underscore `_`, on
  the **top-level workflow object**, each **step** (`agent`/`gate`/`branch`/
  `loop`/`commit`), and each **governor** (`max_iters`/`budget`/`fixpoint`). The
  error names the key and the object (e.g. `unknown key "junk" in agent step`).
  Keys beginning with `_` (e.g. `_doc`, `_note`) are ignored metadata and
  allowed — the documented escape hatch the examples use. This mirrors the
  discipline `expr_of_json` already enforced ("expression object must have
  exactly one operator key"). `output_schema` is unchanged: it is intentionally
  an open field→type map.
- **The schema regained agreement with the parser.** Every closed object
  (workflow, the 5 steps, the 3 governors, the 13 expr branches) now carries
  `patternProperties: {"^_": {}}` alongside `additionalProperties:false`, so the
  schema accepts exactly the `_`-prefixed metadata the parser accepts and rejects
  exactly the unknown keys the parser rejects. The committed
  `schema/workflow.schema.json` is regenerated (the no-drift test enforces
  equality; the `schema` CLI output byte-equals the committed file).

### Docs

- **Schema↔parser conformance wording corrected** in `SPEC.md` / `README.md` to
  the now-accurate statement: the parser and the published schema agree — every
  workflow / step / governor / expr object is closed (unknown keys rejected by
  BOTH) except keys prefixed with `_` (ignored metadata); integers are bounded; a
  schema-valid workflow parses, and a parser-accepted workflow is schema-valid.
  The prior sentence that conflated the expr case with step/governor and claimed
  the schema "does not accept what the parser rejects" while the parser was still
  lenient has been replaced. `Workflow_json.mli` documents the closed-object rule
  and the `_` escape hatch.

### Other

- CLI `--version` → `0.6.0`.
- **New tests (behavioral parity guards the v0.5 suite lacked):**
  - *Parser strictness, per object type*: a table asserting the parser
    `of_string ⇒ Error` on an unknown non-underscore key for EACH of the
    top-level workflow object, `agent`/`gate`/`branch`/`loop`/`commit` steps, and
    `max_iters`/`budget`/`fixpoint` governors.
  - *Underscore metadata accepted*: a workflow with a top-level `_doc` and a step
    `_note` (and a governor `_why`) parses OK.
  - *Schema/parser parity (structural cross-check, no JSON-Schema-validator
    dependency)*: each closed object def in the schema carries both
    `additionalProperties:false` and a `^_` patternProperty, AND the set of
    declared `properties` names for each step/governor/workflow object EQUALS the
    parser's hard-coded known-key set.
  - The examples (`bounty.workflow.json`, `smoke.workflow.json` — the latter has
    a top-level `_doc`) still parse, validate, and lint clean. All v0.5 tests
    remain green (39 tests total).

### Preserved invariants

- Runtime token on every `Commit` (hashed for the trace, never stored raw).
- Floor gates guaranteed-evaluated and must PASS on every path to a commit; a
  false gate blocks.
- Engine loop ceiling (termination guarantee); determinism / byte-identical
  replay.
- `Expr.eval` totality; lint-clean ⇒ validate.
- `lib/` depends on **yojson only** — cabal stays confined to `bin/`.

## v0.5

A **corrective release** addressing findings from an external audit. Four fixes:
two correctness/safety blockers in the engine, one schema soundness gap, and a
round of doc honesty.

### Fixed

- **Loop termination is now an engine guarantee, not a backend/agent promise
  (BLOCKER).** Previously a `Budget`-only loop could run forever (the shipped
  cabal backend returns `budget ()` as a *constant*, so `Budget` never fired), and
  a `Fixpoint`-only loop never terminated if the agent always reported progress.
  `Engine.run` / `Engine.replay` now take `?max_loop_iters` (default `10_000`) and
  every loop **always** stops once it has executed that many iterations —
  recording `Loop_stopped { reason = "ceiling" }` — regardless of governors,
  `until`, budget, or agent behaviour. `Budget` / `Fixpoint` / `until` are now
  documented as *early-stop heuristics* under the ceiling, and `Max_iters` as an
  explicit *lower* bound. The validator still requires ≥1 governor (intent), but
  the termination guarantee is the ceiling. The ceiling is a constant, so replay
  reproduces it byte-identically.
- **A failing `Gate` now BLOCKS the run.** A `Gate` whose predicate evaluates
  **false** terminates the walk as `Blocked` (naming the gate id) instead of
  recording `Fail` and continuing. The floor now means "floor gates must *pass* on
  every path to a commit." A `Gate` that evaluates true records `Pass` and
  continues, unchanged.
- **`examples/bounty.workflow.json` restructured.** It previously gated
  `g-independent` on `outputs.final-review.verdict`, produced *later* inside the
  branch — which both warned (`dangling-output-ref`) and, under the new gate
  semantics, would have blocked. The independent review (`final-review`) and the
  `draft-poc` output schema now run *before* the floor gates, so every gate
  references only outputs produced before it. The example is now **lint-clean
  (zero diagnostics, no warnings)** and reaches `commit` when the gate predicates
  hold.
- **JSON Schema no longer accepts what the parser rejects (major).** Every expr
  operator object and every step/governor object is now **closed**
  (`additionalProperties:false`) — matching the parser's exactly-one-operator/`kind`
  requirement, so `{"path":"x","junk":2}` is schema-invalid. The integer governor
  fields (`max_iters.n`, `fixpoint.window`) carry `minimum:1` and
  `maximum:1073741823` (2³⁰−1, inside OCaml's safe `int` range), so a literal too
  large for the parser to read as an `int` is schema-invalid too. The committed
  `schema/workflow.schema.json` is regenerated (the no-drift test enforces
  equality); the schema test now asserts the closed objects and the integer
  bounds structurally (still no JSON-Schema validator dependency).

### Docs

- **Loop wording reframed** across `SPEC.md` / `README.md` / the `Engine` and
  `Types` interfaces: every loop is hard-bounded by the engine ceiling; governors
  and `until` are early-stop heuristics; no loop can run unboundedly regardless of
  the backend's budget or the agent's progress reports.
- **Floor wording updated:** floor gates must *pass* (not merely be present) on
  every path to a commit; a false gate blocks.
- **Schema↔parser conformance wording** made accurate (it now rejects the two
  divergences above).
- **`Lint` performance claim made honest:** no longer "instant / free"; now "pure,
  offline, no agent/IO; linear for realistic workflows, worst-case superlinear on
  pathologically deep branch nesting."
- **`extract_json` (`bin/backend_cabal.ml`) docstring** notes it is best-effort,
  not balanced-bracket-aware, and **fails closed** on ambiguous input
  (valid-JSON-then-prose-with-braces may be rejected — the safe direction). The
  disclosed cross-branch `dangling-output-ref` limitation note is kept.

### Other

- CLI `--version` → `0.5.0`.
- New tests: a `Budget`-only loop with a *constant* budget stops at the ceiling
  (`~max_loop_iters:5`, reason `"ceiling"`); a `Fixpoint`-only loop whose agent
  always reports `progressed:true` also stops at the ceiling; a false floor gate
  yields `Blocked`; the bounty example lints with zero diagnostics; the smoke
  example still reaches `Committed`; the schema's expr/step/governor objects are
  closed and its integer fields are bounded, and the parser rejects the junk-key /
  out-of-range-integer divergences. The decrements-to-zero `Budget` early-stop and
  `Fixpoint` early-stop tests are retained.

### Preserved invariants

- Abstract `Validate.Validated.t`; `Engine.run`/`replay` require it.
- `Commit` needs a runtime token (hashed for the trace, never stored raw).
- Floor gates guaranteed on every path to a commit.
- `lib/` depends on **yojson only** — cabal stays confined to `bin/`.

## v0.4

A **machine-readable JSON Schema of the workflow format**, so a meta-agent can
constrain its generator to produce conformant workflows *by construction* — the
first of three layers: **schema** (shape at generation) → **lint**
(semantics/safety pre-run) → **validate** (the run gate). Library-first.

### Added

- **`Workflow_schema` (`lib/workflow_schema.ml(i)`)** — a pure, yojson-only module
  exposing the canonical **JSON Schema (draft 2020-12)** of a workflow as a library
  value (`schema : Yojson.Safe.t`, `to_string : unit -> string`). It uses
  `$defs` + `$ref` for the recursive `expr`, `governor`, `step` and `output_schema`
  parts, and is **derived from `Workflow_json`** (the actual parser) so it describes
  exactly what the parser accepts (every step `kind`, the single-operator expr
  encoding, the governor kinds, the `output_schema` type tags). Unknown object keys
  (the `_doc` convention) are tolerated, matching the parser's leniency.
- **`schema/workflow.schema.json`** — the committed, pretty-printed artifact a
  generator/embedder is pointed at. Generated from the library value (like cabal's
  `generate_opam_files`); a **no-drift** test asserts it byte-matches
  `Workflow_schema.to_string ()`.
- **CLI `schema`** — `cabal-workflow-runner schema` prints `Workflow_schema.to_string
  ()` to stdout (exit 0). A thin wrapper.
- **Tests:** schema is valid JSON with `$schema` + `$defs(expr, governor, step)`;
  **no drift** (committed file == lib value, byte-for-byte); **parser ↔ schema kinds
  agree** (the schema's enumerated step `kind`s equal `{agent, gate, branch, loop,
  commit}` and the parser rejects an unknown kind). No JSON-Schema validator
  dependency was added.

### Docs

- A "Constrain your generator with the schema" note in the meta-agent sections of
  `README.md` and `SPEC.md` (the three layers: schema → lint → validate).
- **Fixed a v0.2.1 doc gap:** `README.md` now documents `CWR_BACKEND`, `CWR_MODEL`
  and `CWR_BUDGET` in a "Live run against a backend" subsection, referencing
  `examples/smoke.workflow.json` (verified live to `Committed`).
- `SPEC.md` §5: "workflow JSON Schema" moved from planned → done; remaining
  follow-ups (on-disk ledger + `replay` CLI, YAML/MD front-end, `Spawn`/subworkflow
  step) kept.

### Preserved invariants

- `lib/` depends on **yojson only** — cabal stays confined to `bin/`.
- Abstract `Validate.Validated.t`; `Engine.run`/`replay` require it.
- `Commit` needs a runtime token; floor gates guaranteed on every path to a commit.

## v0.3

A **`Lint` library** designed to be embedded by a meta-agent that builds its own
workflows dynamically. Library-first: `lib/lint.mli` is the deliverable; the CLI
`lint` subcommand is a thin convenience.

### Added

- **`Lint` (`lib/lint.ml(i)`)** — a pure, offline, yojson-only linter.
  - `check ?floor_gates wf` runs ALL floor + semantic checks over a typed
    workflow and collects every diagnostic in one pass (never first-error-only).
  - `check_json ?floor_gates raw` is the meta-agent entry point: it lints a
    generator's RAW string output. **Parse-tolerant** — malformed JSON becomes a
    single `invalid-json` error and a shape error becomes `invalid-shape`; it
    **never raises**.
  - `diagnostic`s carry a STABLE machine `code`, a human/agent `message`, and a
    JSON-path `loc` (e.g. `steps[3].body[0]`). `diagnostic_to_json` / `to_json`
    serialise them (`{"diagnostics":[..]}`) for feeding back into a generator
    prompt. `has_errors` distinguishes floor failures from advisory warnings.
  - **Error codes** (floor + parse/shape, make a workflow unrunnable):
    `invalid-json`, `invalid-shape`, `ungoverned-loop`, `unbounded-max-iters`,
    `bad-fixpoint-window`, `commit-missing-floor-gate`.
  - **Warning codes** (legal + runnable, but a generator likely erred):
    `dangling-output-ref`, `missing-output-schema`, `no-commit`,
    `unreachable-after-commit`.
- **CLI `lint <file> [--floor <g>]... [--json]`** — reads the file, calls
  `Lint.check_json`, prints a human table by default or `Lint.to_json` with
  `--json`. Exits non-zero **iff** there is an error-severity diagnostic
  (warnings alone exit 0).
- **Docs:** a "Meta-agent: building workflows dynamically" section in `SPEC.md`
  and `README.md` (the generate → lint → fix embedding loop, the stable
  diagnostic-code table, and the lint-clean ⇒ validate contract).

### Changed

- **`Validate.workflow` is now defined in terms of `Lint.check`.** It computes
  diagnostics and returns `Ok validated` iff `not (has_errors ds)`, else `Error`
  rendered from the error diagnostics. The gate and the linter therefore share
  **ONE source of truth** and cannot drift: a workflow with no error-severity
  diagnostics is **guaranteed** to validate. The signature
  (`?floor_gates -> Types.workflow -> (Validated.t, string) result`) and all
  existing behaviour are unchanged.

### Preserved invariants

- Abstract `Validate.Validated.t`; `Engine.run`/`replay` require it.
- `Commit` needs a runtime token (hashed for the trace, never stored raw).
- Floor gates guaranteed on every path to a commit.
- `lib/` depends on **yojson only** — cabal stays confined to `bin/`.

## v0.2.1

First **live end-to-end run** against a real agent (validated on Claude Haiku).

### Fixed / Added

- **The cabal backend now populates the registry** via `Adapter_loader.register_all`
  before any lookup. Previously the registry was empty at runtime, so `run` always
  fell through to "no cabal backend available" and fail-closed — the live dispatch
  path was never actually functional.
- **Backend + model selection** via `CWR_BACKEND` (backend id, e.g. `claude-code`)
  and `CWR_MODEL` (e.g. `haiku`), so a run can target a small/cheap/fast model.
- **Robust structured-output extraction.** Small models often wrap JSON in prose or
  a fenced code block; `structured_output` now tolerates that (direct parse → strip
  fence → extract first `{`/`[` … last `}`/`]`) instead of failing closed on "JSON
  plus noise". A live smoke run reproduced the brittleness (the first call failed
  strict parse) and this fix resolves it.
- **`examples/smoke.workflow.json`** — a dumb end-to-end workflow exercising the
  whole tool (structured output + schema, a gate expression, a governed loop with
  `fixpoint`, a branch on structured output, a token-gated commit). Verified live:
  `Committed` with a hashed runtime token.

## v0.2

Structured, data-driven workflows — without losing determinism or the safety floor.

### Added

- **Structured agent output + run context.** `Agent` carries an optional
  `output_schema` (a minimal required-field spec over `String | Int | Number | Bool |
  Enum | Any`). `backend.run_agent` now returns `bool * Yojson.Safe.t`. Each agent's
  output is bound into a **run context** under `outputs.<id>`; the current loop
  iteration index is exposed at `loop.iter`. On a successful run the output is
  validated against `output_schema` **fail-closed** — a mismatch yields
  `Aborted "schema mismatch: <field>"` (no silent pass, no retry).
- **Total predicate DSL (`Expr`).** `Path`, `Lit`, `Eq/Ne/Lt/Le/Gt/Ge`, `In`,
  `And/Or/Not`, `Exists`. `Expr.eval ~ctx : t -> bool` is **total**: a missing path, a
  type mismatch, or an incomparable comparison yields `false` — it never raises and
  never diverges. No recursion/iteration constructs. JSON encoding in `Workflow_json`.
- **Governed (possibly-unbounded) loops.** `Loop { body; until; governors }` replaces
  the fixed-cap loop. Governors: `Max_iters n` (`n ≥ 1`), `Budget` (stop once
  `backend.budget () <= 0`), `Fixpoint { window; progress }` (stop after `window`
  consecutive non-progress iterations). A loop may have **no `Max_iters`** — unbounded
  but governed; what is forbidden is an **empty** `governors` list.

### Changed

- **Gates / branches / loops are now PURE DSL.** `Gate { id; when_ }`,
  `Branch { when_; then_; else_ }`, `Loop` carry `Expr.t` predicates evaluated by the
  engine over the run context. **`eval_gate` is removed from the backend entirely** —
  including the old always-`Pass` CLI placeholder.
- **Backend** is now `{ run_agent : ... -> bool * Yojson.Safe.t; budget : unit -> int }`.
- **Validator (fail-closed):** `governors = []` ⇒ `Error "loop is ungoverned"`;
  `Max_iters n` with `n < 1` ⇒ Error; `Fixpoint window < 1` ⇒ Error. The floor-gate
  guarantee (every commit preceded by all floor gates on every path; branch =
  intersection; loop-body gates do not count) and the runtime token are unchanged.
- **Determinism / replay.** The trace records, in order, everything a stop/branch
  decision reads — each agent's structured JSON, each `Budget` reading, each `Fixpoint`
  progress verdict, each `until`/gate/branch verdict, and loop iteration counts. A
  governed loop's bound is purely a function of these recorded inputs, so
  `Engine.replay` reproduces the outcome **and** trace byte-identically even for an
  unbounded loop.
- **CLI (`bin/backend_cabal.ml`):** `run_agent` forces structured output (cabal
  `Structured_report`, parsing `raw_json`, fall back to JSON in `agent_text`; fail
  closed otherwise); `budget` defaults to 1,000,000 or reads `CWR_BUDGET`.

### Preserved invariants

- Abstract `Validate.Validated.t`; `Engine.run`/`replay` require it.
- `Commit` needs a runtime token (hashed for the trace, never stored raw).
- Floor gates guaranteed on every path to a commit.
- `lib/` depends on **yojson only** — cabal stays confined to `bin/`.

## v0.1

Initial release: deterministic engine, fail-closed validator, bounded loops,
context-free gates evaluated by the backend, runtime-token commits, JSON loader.
