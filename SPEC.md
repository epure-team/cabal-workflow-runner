# cabal-workflow-runner — Specification

A **deterministic workflow engine on [cabal](https://github.com/epure-team/cabal)**.
It interprets a declarative workflow — steps, bounded loops, branches, gates, and a
terminal commit — executing structural/control-flow steps **deterministically in
OCaml** and dispatching **agent steps via cabal**.

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
| `Agent { id; prompt; read_only }` | Dispatch agent work via the backend; records `(success, text)`. | Effect isolated to the backend; result recorded. |
| `Gate { id }` | Engine evaluates the gate's verdict via the backend. | Verdict recorded. |
| `Branch { on; then_; else_ }` | Evaluate gate `on`; take `then_` on `Pass`, `else_` on `Fail`. | Pure control flow over the recorded verdict. |
| `Loop { max_iters; until; body }` | Run `body` at most `max_iters` times; stop early when `until` evaluates `Pass`. | **Bounded** — hard cap `max_iters`, can never spin. |
| `Commit { id }` | The **only** step that can file/submit. | Requires a runtime token (below). |

Illegal states are made hard to express: there is **no** step constructor that can
carry an approval token, and `Commit` is the only constructor that can file/submit.

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

- a `Loop` has `max_iters < 1` (loops must be bounded); or
- a `Commit` is reachable on a path where the `floor_gates` are **not** all
  guaranteed-evaluated before it.

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

`backend` is a record of functions:

```ocaml
type t = {
  run_agent : id:string -> prompt:string -> read_only:bool -> bool * string;
  eval_gate : gate_id -> gate_verdict;
}
```

The library ships a deterministic **stub** backend (`Backend.stub`) used by tests.
The CLI builds a **cabal-backed** backend (`bin/backend_cabal.ml`): `run_agent` uses
`Cabal.Registry.first_available` + `Cabal.Agentic_backend.run_task_with_ctxt` over a
spec from `Cabal.Backend_types.make_task_spec`; if no backend is available it fails
closed (`success = false`). cabal usage is confined to this boundary: the
`cabal_workflow_runner` **library depends on yojson only**; only `bin/` links cabal.

`Engine.run ~backend ~token validated : outcome * trace` performs a deterministic
walk: `Agent` → `run_agent` + record; `Gate` → `eval_gate` + record; `Branch` →
evaluate the named gate, take a branch; `Loop` → run `body` up to `max_iters`,
stopping early when `until` evaluates `Pass` (hard cap = `max_iters`); `Commit` →
require token (`Blocked` if absent/ill-formed) else `Committed`.

The `trace` records each executed step's id and result/verdict in order.
`Engine.replay ~trace validated : outcome` re-interprets the workflow using the
**recorded** agent results and gate verdicts (no backend calls) and produces the
**same** outcome the original run produced.

## 4. Embedding

An embedder supplies two things:

1. **`floor_gates`** — the gate ids that every commit must be guaranteed-preceded by.
2. **a `Backend.t`** — how to run an agent and how to evaluate a gate verdict.

```ocaml
match Validate.workflow ~floor_gates:["g-observed"; "g-independent"] wf with
| Error reason -> reject reason
| Ok validated ->
    let outcome, trace = Engine.run ~backend ~token validated in
    ...
```

## 5. Input format and planned follow-ups

MVP input is **JSON** (`Workflow_json`, fail-closed on malformed input). Each step is
an object with a `kind` discriminator (`agent` / `gate` / `branch` / `loop` /
`commit`). See `Workflow_json` docs for the schema.

Planned follow-ups (not in the MVP):

- **YAML** workflow files (cabal already depends on a YAML library).
- **Markdown front-matter** workflows (a prose body plus a structured header).
- Richer gate evaluators wired to observed evidence / CI signals.
