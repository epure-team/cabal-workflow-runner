# cabal-workflow-runner — Specification

A **deterministic workflow engine on [cabal](https://github.com/epure-team/cabal)**.
It interprets a declarative workflow — steps, **governed loops**, branches, gates, and a
terminal commit — executing structural/control-flow steps **deterministically in
OCaml** and dispatching **agent steps via cabal**. Agents return **structured JSON**;
gate / branch / loop decisions are a **total predicate DSL** over a **run context** of
those recorded outputs. Loops may be **unbounded** but must declare ≥1 **governor**
(the termination guarantee). See [`CHANGELOG.md`](CHANGELOG.md) for the v0.2 changes.

The thesis: you get **data-driven workflows without losing determinism**, because

1. the interpreter is a deterministic structural walk;
2. the **safety floor is an engine/validator invariant** over *any* workflow, not a
   property the workflow author must remember to encode; and
3. the workflow definition plus the agent results / gate verdicts are recorded, so a
   run **replays** byte-identically.

It is **domain-neutral**. No specific domain (the bounty pipeline, ZK, crypto, ...) is
baked in. `examples/bounty.workflow.json` is *one* illustrative workflow.

## 1. Step vocabulary

Modeled on a small hooks/step DSL (sequence, branch, bounded loop, gate, agent,
commit). A `workflow` is a name plus a `step list`:

| Step | Meaning | Determinism |
|------|---------|-------------|
| `Agent { id; prompt; read_only; output_schema }` | Dispatch agent work; records `(success, structured_json)` and binds it into the run context under `outputs.<id>`. If `output_schema` is present the JSON is validated **fail-closed**. | Effect isolated to the backend; structured result recorded. |
| `Gate { id; when_ }` | **Pure** verdict: `Pass` iff `Expr.eval when_` over the run context (no backend). | Verdict recorded. |
| `Branch { when_; then_; else_ }` | Evaluate `when_`; take `then_` when true, `else_` when false. | Pure control flow over the recorded verdict. |
| `Loop { body; until; governors }` | Run `body`; bind its outputs; stop when `until` holds **or** any governor fires. | **Governed** — possibly unbounded but ≥1 governor guarantees termination; the bound is a function of recorded inputs. |
| `Commit { id }` | The **only** step that can file/submit. | Requires a runtime token (below). |

Illegal states are made hard to express: there is **no** step constructor that can
carry an approval token, and `Commit` is the only constructor that can file/submit.

### 1.1 Structured agent output + run context

`backend.run_agent` returns `bool * Yojson.Safe.t` (success + structured JSON). The
engine maintains a **run context** `ctx`: each agent's output is bound under
`outputs.<id>`, and inside a loop the current 0-based iteration index is exposed at
`loop.iter`. The DSL addresses into it by dotted path (`outputs.assess.severity`).

An `output_schema` is a minimal required-field spec — a list of `(field, ty)` with
`ty ∈ { String | Int | Number | Bool | Enum of string list | Any }`. After a
successful agent run the JSON is validated against it: each required field must be
present and type-correct. A mismatch is **fail-closed** → `Aborted "schema mismatch:
<field>"` (no silent pass, no retry in this MVP).

### 1.2 Total predicate DSL

`Expr.t` is a small predicate language: `Path`, `Lit`, the comparisons
`Eq/Ne/Lt/Le/Gt/Ge`, `In` (membership in a literal/list), `And/Or/Not`, and `Exists`
(a path resolves to a present, non-null value). `Expr.eval ~ctx : t -> bool` is
**TOTAL**: a missing path, a type mismatch, or comparing incomparable types yields a
defined result (the predicate ⇒ `false`) — it **never raises** and **never diverges**.
There are no recursion / iteration constructs, so a single evaluation is bounded by the
expression's size. This is the property that lets a governed loop's stop decision
always *terminate* even when the *run* may be open-ended.

JSON encoding (parsed by `Workflow_json`): `{"path":"outputs.a.sev"}`, `{"lit":<json>}`,
`{"eq":[e1,e2]}` (and `ne`/`lt`/`le`/`gt`/`ge`/`in`), `{"and":[..]}`, `{"or":[..]}`,
`{"not":e}`, `{"exists":"outputs.a.sev"}`.

### 1.3 Governed (possibly-unbounded) loops

A `Loop` carries an optional data-driven stop condition `until : Expr.t option` and a
**non-empty** list of `governors`:

- `Max_iters n` — stop after `n` iterations (`n ≥ 1`);
- `Budget` — stop once `backend.budget () <= 0`;
- `Fixpoint { window; progress }` — stop after `window` consecutive iterations where
  `progress` evaluated `false` (`window ≥ 1`).

Per iteration the engine binds `loop.iter`, runs `body` (binding its agent outputs),
then stops if `until` holds **or** any governor fires. A loop may legitimately have
**no `Max_iters`** (e.g. only `Budget` or `Fixpoint`) — that is the feature:
**unbounded but governed**. What is forbidden is an **empty** `governors` list.

## 2. The safety floor — enforced by the engine/validator, NOT the workflow

This is the crux. The *mechanism* is hardcoded; the *specific gate ids* are a
parameter supplied by the embedder.

### 2.1 Commit requires a runtime human-approval token

The engine, executing a `Commit`, requires a well-formed token passed **at runtime**
to `Engine.run ~token`. The token is **never** a field of any step and never present
in a workflow file. No token (or an empty/blank one) ⇒ `Blocked`. The token is hashed
(`Digest.MD5` → hex) for the trace; the raw token is never stored. There is
structurally no way for a workflow file to express "commit without a token."

### 2.2 `Validate.workflow ~floor_gates wf` is fail-closed

It returns `Error reason` (rejecting the workflow before any execution) when:

- a `Loop` has an **empty** `governors` list (`Error "loop is ungoverned"`), or a
  `Max_iters n` with `n < 1`, or a `Fixpoint` with `window < 1`. A loop may be
  unbounded (no `Max_iters`) as long as it declares some governor; or
- a `Commit` is reachable on a path where the `floor_gates` are **not** all
  guaranteed-evaluated before it.

A gate counts toward the floor purely by its **id** being *evaluated* on every path;
its `when_` verdict is irrelevant to the floor (the floor is "the gate was reached", the
runtime token is the second lock).

**Conservative static analysis.** Walking the steps, we thread the set of gates
*guaranteed* to have been evaluated on **every** path reaching the current position:

- `Gate g` adds `g` to the guaranteed set.
- `Branch` contributes only gates guaranteed in **both** `then_` and `else_` (set
  intersection) — at runtime only one branch is taken.
- gates inside a `Loop` body do **not** count (the loop may run zero iterations); the
  body is still recursively checked for its own violations.
- at each `Commit`, require `floor_gates ⊆ guaranteed-set-here`, else `Error`.

`floor_gates` is supplied by the embedder (e.g. a bounty embedder would pass its
G1–G4 / observed / independence gate ids). The MECHANISM — commit needs the floor
gates on every path **and** the runtime token — is hardcoded.

### 2.3 An unvalidated workflow cannot be executed

`Validate.workflow` is the only producer of the abstract `Validate.Validated.t`, and
`Engine.run` / `Engine.replay` take a `Validated.t`. The type system therefore makes
"run an unvalidated workflow" unrepresentable.

## 3. Engine: deterministic interpreter + replay

`backend` is a record of functions — note there is **no** `eval_gate`; gates are pure
DSL:

```ocaml
type t = {
  run_agent : id:string -> prompt:string -> read_only:bool -> bool * Yojson.Safe.t;
  budget    : unit -> int;   (* a Budget governor stops the loop once this is <= 0 *)
}
```

The library ships a deterministic **stub** backend (`Backend.stub`) used by tests.
The CLI builds a **cabal-backed** backend (`bin/backend_cabal.ml`): `run_agent` forces
**structured output** via `Cabal.Registry.first_available` +
`Cabal.Agentic_backend.run_task_with_ctxt` over a spec from
`Cabal.Backend_types.make_task_spec` (with `expected_outputs` including
`Structured_report`), parsing the structured report's `raw_json` (falling back to
parsing `agent_text` as JSON); if no parseable JSON is produced it **fails closed**
(`success = false`). `budget` returns a large constant (1_000_000) by default, or reads
the `CWR_BUDGET` env var if set. cabal usage is confined to this boundary: the
`cabal_workflow_runner` **library depends on yojson only**; only `bin/` links cabal.

`Engine.run ~backend ~token validated : outcome * trace` performs a deterministic walk:
`Agent` → `run_agent`, bind `outputs.<id>`, validate `output_schema` (fail-closed);
`Gate`/`Branch` → pure `Expr.eval` over the run context; `Loop` → governed iteration
(see §1.3), stopping on `until` or any fired governor; `Commit` → require token
(`Blocked` if absent/ill-formed) else `Committed`.

**Determinism / replay.** The trace records, *in order, everything a stop/branch
decision reads*: each agent's structured JSON output, each `Budget` reading, each
`Fixpoint` progress verdict, each `until`/gate/branch verdict, and loop iteration
counts. `Engine.replay ~trace validated : outcome` re-feeds the recorded agent outputs
and recorded budget readings (no backend calls), re-evaluates the total DSL over the
rebuilt context (asserting each recorded verdict), and produces the **same** outcome and
trace. Because the loop's bound is purely a function of these recorded inputs, **an
unbounded-but-governed loop still replays byte-identically.**

## 4. Embedding

An embedder supplies two things:

1. **`floor_gates`** — the gate ids that every commit must be guaranteed-preceded by.
2. **a `Backend.t`** — how to run an agent (returning structured JSON) and a `budget`.

```ocaml
match Validate.workflow ~floor_gates:["g-observed"; "g-independent"] wf with
| Error reason -> reject reason
| Ok validated ->
    let outcome, trace = Engine.run ~backend ~token validated in
    ...
```

## 4a. Meta-agent: building workflows dynamically

A meta-agent that *generates* its own workflows needs to know, **before** running
anything, whether a candidate workflow is well-formed and safe — and, when it is not,
**why**, in a form it can act on. The `Lint` library is that feedback channel. It is
**pure, offline, and instant** (no agent, no backend, no I/O), so it is free to call in
a tight generate → lint → fix loop.

### The embedding pattern

```ocaml
open Cabal_workflow_runner

let floor_gates = [ "g-observed"; "g-independent" ]

let rec gen attempts =
  let raw = run_generator_agent prompt in            (* the LLM emits a workflow JSON *)
  match Lint.check_json ~floor_gates raw with
  | [] ->                                             (* lint-clean: guaranteed to validate *)
      (match Workflow_json.of_string raw with
       | Ok wf -> Validate.workflow ~floor_gates wf   (* returns Ok by the contract below *)
       | Error _ -> assert false)                     (* unreachable: check_json already parsed it *)
  | ds when Lint.has_errors ds && attempts > 0 ->
      (* feed the machine-readable diagnostics back into the next prompt *)
      let feedback = Yojson.Safe.to_string (Lint.to_json ds) in
      gen ~prompt:(prompt ^ "\nFix these diagnostics:\n" ^ feedback) (attempts - 1)
  | ds ->
      (* only warnings (or out of attempts): accept and validate *)
      ...
```

`Lint.check_json` is **parse-tolerant and never raises**: malformed JSON yields a single
`invalid-json` error (loc `"$"`), a shape error yields `invalid-shape`, and only a
well-formed workflow reaches the semantic checks. So a generator's *raw* string output
can be linted directly, with no exception handling around the call.

### The contract: lint-clean ⇒ validate

`Validate.workflow` is **defined in terms of `Lint.check`**: it computes the diagnostics
and returns `Ok validated` iff `not (Lint.has_errors ds)`. The gate and the linter
therefore share **one source of truth** and cannot drift. The guarantee the meta-agent
relies on:

> **A workflow with no error-severity diagnostics is guaranteed to `Validate.workflow`.**

Warnings never fail the floor — they are legal, runnable shapes that a generator likely
got wrong (a dangling output reference, a missing output schema, no commit at all,
unreachable steps after a commit).

### Stable diagnostic codes

The `code` is a stable identifier an embedder can branch on (the `message` is for
humans/agents; the `loc` is a JSON path to the offending node, e.g. `steps[3].body[0]`).

| Code | Severity | Meaning |
|------|----------|---------|
| `invalid-json` | error | `check_json` received non-JSON (loc `"$"`). |
| `invalid-shape` | error | parsed JSON is not a valid workflow (unknown `kind`, missing/ill-typed field). |
| `ungoverned-loop` | error | a `Loop` with an empty `governors` list. |
| `unbounded-max-iters` | error | a `Max_iters n` with `n < 1`. |
| `bad-fixpoint-window` | error | a `Fixpoint` with `window < 1`. |
| `commit-missing-floor-gate` | error | a `Commit` reachable without all floor gates guaranteed on every path (one per offending commit, naming the missing gate(s)). |
| `dangling-output-ref` | warning | an expression references `outputs.<id>.<field>` where no prior agent step produced `<id>` on the path, or its `output_schema` declares no such `<field>`. |
| `missing-output-schema` | warning | a referenced agent step declares no `output_schema` (its output can't be validated). |
| `no-commit` | warning | the workflow has no `Commit` step at all. |
| `unreachable-after-commit` | warning | a step follows a `Commit` at the same level (a commit ends the run). |

The error codes are **exactly** the floor + parse/shape failures `Validate.workflow`
enforces — which is what makes the lint-clean ⇒ validate contract hold by construction.

## 5. Input format and planned follow-ups

MVP input is **JSON** (`Workflow_json`, fail-closed on malformed input). Each step is
an object with a `kind` discriminator (`agent` / `gate` / `branch` / `loop` /
`commit`). See `Workflow_json` docs for the schema.

Expressions and governors are encoded as described in §1.2–§1.3; see the
`Workflow_json` module docs and [`examples/bounty.workflow.json`](examples/bounty.workflow.json)
for a worked example (structured outputs, an expression-gated commit, a governed loop).

Planned follow-ups (not in the MVP):

- **YAML** workflow files (cabal already depends on a YAML library).
- **Markdown front-matter** workflows (a prose body plus a structured header).
- Agent-output-driven retries on schema mismatch (currently fail-closed `Aborted`).
