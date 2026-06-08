# Changelog

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
