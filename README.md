# cabal-workflow-runner

A **deterministic workflow engine on [cabal](https://github.com/epure-team/cabal)**
(the Caml Agent Backend Abstraction Library). It interprets a declarative workflow —
sequence, **governed loop**, branch, gate, and a terminal commit — running the
control-flow **deterministically in OCaml** and dispatching **agent steps via cabal**.

Agents return **structured JSON** bound into a **run context**; gate / branch / loop
decisions are a **total predicate DSL** over that context (always terminating, never
raising). Every loop is **hard-bounded by an engine iteration ceiling** (default
`10_000`) — the termination guarantee; `Max_iters` / `Budget` / `Fixpoint` / `until` are
early-stop heuristics under it (a loop must still declare ≥1 governor, by intent). A
floor `Gate` that evaluates **false blocks** the run.

You get data-driven workflows **without** losing determinism, because the interpreter
is deterministic, the **safety floor is enforced by the engine/validator as an
invariant over any workflow** (not by the workflow author), and runs **replay** from a
recorded trace — the governor's inputs (and the constant ceiling) are recorded, so even
a loop that hits the ceiling replays byte-identically. See [`SPEC.md`](SPEC.md) and
[`CHANGELOG.md`](CHANGELOG.md).

The project is **domain-neutral**. [`examples/bounty.workflow.json`](examples/bounty.workflow.json)
is just one illustration — the bounty pipeline expressed as a single workflow file.

- **Library** `cabal_workflow_runner` (`lib/`): types, fail-closed validator,
  deterministic engine + replay, backend abstraction, JSON loader. Depends on
  **yojson only** — *not* on cabal.
- **Executable** `cabal-workflow-runner` (`bin/`): a small CLI; this is the only place
  that links cabal.

## Build & test

Built and tested in the cabal opam switch (cabal, eio, cmdliner, alcotest, yojson):

```sh
eval $(opam env --switch=/path/to/cabal --set-switch)
dune build
dune test
```

## CLI

```sh
# Lint a workflow (parse-tolerant: malformed JSON / bad shape become diagnostics).
# Human table by default; --json prints {"diagnostics":[..]}. Exits non-zero ONLY
# on an error-severity diagnostic (warnings alone exit 0).
cabal-workflow-runner lint examples/bounty.workflow.json \
  --floor g-validated --floor g-observed --floor g-independent
cabal-workflow-runner lint examples/smoke.workflow.json --floor g-observed --json

# Fail-closed validation against an embedder-supplied safety floor:
cabal-workflow-runner validate examples/bounty.workflow.json \
  --floor g-validated --floor g-observed --floor g-independent

# Run it. --approve is the runtime human-approval token required by any Commit
# (hashed for the trace, never stored raw). Omit it and a Commit is Blocked.
cabal-workflow-runner run examples/bounty.workflow.json \
  --floor g-validated --floor g-observed --floor g-independent \
  --approve "$APPROVAL_TOKEN"
```

```sh
# Print the canonical JSON Schema (draft 2020-12) of the workflow format. Point a
# workflow generator at this so it emits conformant workflows by construction.
cabal-workflow-runner schema > workflow.schema.json
```

`validate` rejects (exit 1) any workflow with an **ungoverned** loop (empty `governors`,
or a `Max_iters`/`Fixpoint` with an out-of-range bound) or a commit that is not
guaranteed-gated by the floor gates on every path. `run` dispatches agent steps to the
first available cabal backend (forcing structured output, failing closed if none or if
the agent returns no parseable JSON) and prints the outcome plus the recorded trace.
`schema` is a thin wrapper printing `Workflow_schema.to_string ()` (the committed copy
lives at [`schema/workflow.schema.json`](schema/workflow.schema.json)).

### Live run against a backend

A `run` dispatches each agent step through cabal. Two environment variables target a
specific (typically small/cheap/fast) model:

- **`CWR_BACKEND`** — the cabal backend id to use (e.g. `claude-code`). Unset ⇒ the
  first available backend in the registry.
- **`CWR_MODEL`** — the model to pin (e.g. `haiku`). Unset ⇒ the backend's default.
- **`CWR_BUDGET`** — caps the `Budget` governor (default 1,000,000).

```sh
CWR_BACKEND=claude-code CWR_MODEL=haiku \
  cabal-workflow-runner run examples/smoke.workflow.json \
    --floor g-observed --approve "$APPROVAL_TOKEN"
```

[`examples/smoke.workflow.json`](examples/smoke.workflow.json) is a dumb end-to-end
workflow (structured output + schema, a gate, a governed loop, a branch, a token-gated
commit) whose agents just echo fixed JSON so it runs cheaply; it was verified **live**
all the way to `Committed`.

## Embedding the library

Supply a backend and the floor gates; everything else is enforced for you:

```ocaml
open Cabal_workflow_runner

let backend : Backend.t =
  { run_agent = (fun ~id:_ ~prompt:_ ~read_only:_ -> (true, `Assoc [ ("severity", `String "high") ]));
    budget = (fun () -> 1_000_000) }

let () =
  match Workflow_json.of_file "wf.json" with
  | Error e -> prerr_endline e
  | Ok wf ->
    match Validate.workflow ~floor_gates:["g-observed"; "g-independent"] wf with
    | Error reason -> Printf.eprintf "rejected: %s\n" reason
    | Ok validated ->
      let outcome, trace = Engine.run ~backend ~token:(Some "approve") validated in
      assert (Engine.replay ~trace validated = outcome)  (* deterministic replay *)
```

## Meta-agent: building workflows dynamically

Three layers guard a generated workflow, each tighter than the last:

1. **Schema (shape at generation).** `Workflow_schema` (`lib/workflow_schema.ml(i)`,
   **pure, yojson-only**) publishes the canonical **JSON Schema (draft 2020-12)** of the
   workflow format — derived from `Workflow_json` (the actual parser), so it describes
   *exactly* what the parser accepts. **Governing principle:** `Workflow_json.of_string`
   accepts a workflow **iff** that workflow is structurally valid per the schema; every
   structural constraint is enforced by both, and a behavioral parity test drives the
   parser over a battery of inputs to prove it. Workflow / step / governor objects are
   **closed** with a `_`-metadata escape hatch (unknown keys rejected by both, but `_doc`
   / `_note` accepted by both). **Expr operator objects are *strictly* closed — a single
   operator key, no `_` metadata** — so `{"lit":true,"_x":1}` is rejected by both. The
   bounded integers `max_iters.n` / `fixpoint.window` are `1 ≤ v ≤ max_int` (so
   `1073741824` is valid; a literal beyond `max_int` is rejected by both); a loop's
   `governors` array must be non-empty. `output_schema` is the one intentionally-open
   field→type map. Prompt your generator with
   `cabal-workflow-runner schema` (or the committed
   [`schema/workflow.schema.json`](schema/workflow.schema.json), or
   `Workflow_schema.to_string ()`) so it emits **conformant workflows by construction**
   — the right `kind`s, the expr/governor encodings, required fields.
2. **Lint (semantics + safety, pre-run).** `Lint.check_json` then catches what shape
   alone cannot: ungoverned loops, commits missing a floor gate, dangling output refs.
3. **Validate (the run gate).** `Validate.workflow` is the fail-closed gate the engine
   requires before any execution.

The `Lint` library (`lib/lint.ml(i)`, **pure, offline, yojson-only**) is the feedback
channel for a meta-agent that *generates* its own workflows. With no agent, backend or
I/O it is linear for realistic workflows (worst-case superlinear only on pathologically
deep branch nesting, since the analyses re-walk both arms of every `Branch`), so it is
cheap to call in a tight generate → lint → fix loop:

```ocaml
open Cabal_workflow_runner

let rec gen ~prompt attempts =
  let raw = run_generator_agent prompt in              (* LLM emits a workflow JSON string *)
  match Lint.check_json ~floor_gates raw with          (* parse-tolerant; never raises *)
  | [] ->                                               (* lint-clean ⇒ guaranteed to validate *)
      Result.bind (Workflow_json.of_string raw) (Validate.workflow ~floor_gates)
  | ds when Lint.has_errors ds && attempts > 0 ->
      let feedback = Yojson.Safe.to_string (Lint.to_json ds) in   (* machine-readable *)
      gen ~prompt:(prompt ^ "\nFix:\n" ^ feedback) (attempts - 1) (* feed back into next prompt *)
  | ds -> (* warnings only / out of attempts: accept *) accept raw ds
```

Two properties make this work:

- **`check_json` is parse-tolerant and never raises** — malformed JSON → one
  `invalid-json` error, a bad shape → `invalid-shape`. You lint the generator's *raw*
  output with no exception handling.
- **lint-clean ⇒ validate.** `Validate.workflow` is *defined in terms of `Lint.check`*,
  so a workflow with **no error-severity diagnostics is guaranteed to validate** — the
  gate and the linter share one source of truth and cannot drift.

Diagnostics carry a **stable machine `code`** (e.g. `ungoverned-loop`,
`commit-missing-floor-gate`, `dangling-output-ref`), an agent/human `message`, and a
JSON-path `loc` (e.g. `steps[3].body[0]`). Errors are exactly the floor + parse/shape
failures; warnings (`dangling-output-ref`, `missing-output-schema`, `no-commit`,
`unreachable-after-commit`) are legal-but-likely-mistaken shapes that never fail the
floor. See [`SPEC.md`](SPEC.md) §4a for the full code table and the contract.

## License

MIT. Copyright (c) 2026 Epure Team.
