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
| `Agent { id; prompt; read_only; output_schema; on_failure }` | Dispatch agent work; records `(success, structured_json)` and binds it into the run context under `outputs.<id>`. A **`success = false` run fails closed** → `Aborted` **by default** (`on_failure = "abort"`); with **`on_failure = "continue"`** the failure is **soft** — the failed `Agent_ran` is recorded and the walk continues (for continuous loops where one iteration's failure must not kill the run). On success, if `output_schema` is present the JSON is validated **fail-closed**. | Effect isolated to the backend; structured result recorded. |
| `Gate { id; when_ }` | **Pure** verdict: `Pass` iff `Expr.eval when_` over the run context (no backend). A `Pass` records the verdict and continues; a **`Fail` BLOCKS** the run (`Blocked`, naming the gate id). | Verdict recorded; a false gate is a terminal block. |
| `Branch { when_; then_; else_ }` | Evaluate `when_`; take `then_` when true, `else_` when false. | Pure control flow over the recorded verdict. |
| `Loop { body; until; governors }` | Run `body`; bind its outputs; stop when `until` holds, any governor fires, **or** the engine iteration ceiling is reached. | **Hard-bounded** — every loop stops at an unconditional engine ceiling (default `10_000`); `until`/`Budget`/`Fixpoint` are early-stop heuristics under it. |
| `Run { id; cmd; working_dir; timeout_ms; observe }` | Execute an observable shell command (no shell) via the **injected** `Backend.run_command` effect, recording the `run_result` and binding it under `outputs.<id>`. Executes **only** if the binary is in the operator's runtime **allowlist** (`[]` = fail-closed/off, `"*"` = all), else `Blocked`. | Command runs **exactly once** on the live run (recorded as `Run_executed`); **replay re-feeds the recorded result and never re-executes** (side-effect-free, byte-identical). |
| `Commit { id }` | The **only** step that can file/submit. | Requires a runtime token (below). |

Illegal states are made hard to express: there is **no** step constructor that can
carry an approval token, and `Commit` is the only constructor that can file/submit.

`on_failure = "continue"` does **not** weaken the commit safety floor, because the
validator **rejects** it in any workflow that contains a `Commit` (the
`soft-fail-with-commit` error). Soft-fail is permitted only in **commit-free** workflows
(e.g. a continuous coverage loop that never files). This is *enforced*, not assumed: the
commit-floor invariant tracks gate **IDs**, not whether a gate's predicate consumes the
failed agent's output, so a trivially-true floor gate could otherwise let a commit fire
past a soft-failed agent. With no `Commit` reachable, `"continue"` changes only whether a
failed agent *aborts the walk*; the default remains fail-closed `"abort"`.

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

### 1.3 Hard-bounded loops (ceiling) + governors

Every loop is **hard-bounded by an engine iteration ceiling** (`Engine.run
?max_loop_iters`, default `10_000`): a loop ALWAYS stops once it has executed `ceiling`
iterations — recording `Loop_stopped { reason = "ceiling" }` — **regardless** of
governors, `until`, budget, or the agent's progress reports. This is the termination
**guarantee**: no loop can run unboundedly even if the backend's `budget ()` is a
constant (as the shipped cabal backend's is) or the agent always reports progress. The
ceiling is a constant, so replay reproduces it byte-identically.

Under that ceiling, a `Loop` carries an optional data-driven stop condition `until :
Expr.t option` and a **non-empty** list of `governors` that act as **early-stop
heuristics**:

- `Max_iters n` — an explicit **lower** bound: stop after `n` iterations (`n ≥ 1`);
- `Budget` — stop once `backend.budget () <= 0`;
- `Fixpoint { window; progress }` — stop after `window` consecutive iterations where
  `progress` evaluated `false` (`window ≥ 1`).

Per iteration the engine first checks the ceiling, then binds `loop.iter`, runs `body`
(binding its agent outputs), then stops if `until` holds **or** any governor fires. A
loop may legitimately have **no `Max_iters`** (e.g. only `Budget` or `Fixpoint`): the
ceiling still bounds it. What is forbidden by the validator is an **empty** `governors`
list (intent), but the termination guarantee itself is the ceiling, not the governors.

### 1.4 The `run` step — observable, operator-allowlisted shell commands

A `Run` step executes an **observable shell command** (added in v0.9):

```json
{ "kind": "run", "id": "mk", "cmd": ["mkdir", "-p", "out"],
  "working_dir": "scratch", "timeout_ms": 30000, "observe": ["out"] }
```

- **`cmd`** — a non-empty argv array, executed **without a shell** (there is no implicit
  `sh -c`; a workflow that wants a shell must spell `["sh","-c","…"]`).
- **`working_dir`** — **required**, **relative**, **rejected at parse if absolute or
  containing a `..` component** (mirroring the existing path-escape discipline). It is the
  command's cwd, the effect scope, and the snapshot root.
- **`timeout_ms`** — optional bounded wall-clock cap (same bounds as the integer governor
  fields: `1 ≤ v ≤ max_int`).
- **`observe`** — optional relative paths to snapshot; default = the whole `working_dir`.

**Keeping cabal out of the library.** The engine must *run* a command without depending
on a process library from `lib/`, so the run is an **injected effect**: `Backend.t` gains
`run_command : id:string -> argv:string list -> working_dir:string -> timeout_ms:int
option -> observe:string list option -> run_result`, where

```ocaml
type file_change_kind = Created | Modified | Deleted
type file_change = { path : string; change : file_change_kind; size : int; digest : string }
type run_result   = { exit : int; stdout : string; stderr : string;
                      truncated : bool; files : file_change list }
```

The library defines these types and *calls* the injected function; only `bin/`
(`bin/runner.ml`, wired in `bin/backend_cabal.ml`) implements it via cabal's
`Backend_process.run_process` (no shell) plus a before/after directory snapshot
(path → digest + size) diffed into a `file_change list`. `stdout`/`stderr` are
**size-capped** (64 KiB) with the `truncated` flag. The library's stub backend supplies a
deterministic `run_command` for tests. This mirrors how `run_agent` keeps cabal out of
`lib/`: **`lib/` depends on yojson only.**

`digest` is an **MD5 content digest** (OCaml stdlib `Digest`) used for
**change-detection / observability** in the file diff — it is **NOT** a cryptographic
integrity guarantee.

The result is bound into the run context under `outputs.<id>` as JSON
(`{ "exit", "stdout", "stderr", "truncated", "files":[{ "path","change","size","digest" }] }`),
so the DSL can read `outputs.mk.exit`, `exists(outputs.mk.files)`, etc.

**The allowlist is the trust control (operator-supplied at RUNTIME, never from the
workflow file).** `Engine.run` gains `?run_allowlist:string list` (default `[]`). `cmd[0]`
**must be a bare command name resolved via `PATH`**: a `cmd[0]` containing a path
separator (`/`) — i.e. an absolute or relative path like `/usr/bin/mkdir`, `./x`, or
`a/b` — is **`Blocked`** before the allowlist is consulted (`run command must be a bare
name resolved via PATH, not a path: <cmd0>`), closing the bypass where a path-bearing
`cmd[0]` would pass the basename match yet exec an arbitrary binary. Otherwise the step
executes only if `Filename.basename (List.hd cmd)` (the bare name) is in the allowlist
**or** the allowlist contains `"*"`; if not it is **`Blocked "run command \"<bin>\" not
permitted (allowlist)"`** — fail-closed. With the default `[]`, **no `run` step ever
executes**. The CLI flag `--allow-run BIN` (repeatable; `--allow-run '*'` allows all)
populates it; it is **operator-only and runtime-only** — a workflow file cannot grant
itself the right to run a command.

**The run effect never crashes the engine.** The `bin` runner wraps the whole effect in
`try … with`: a spawn failure (command not found ⇒ exit `127`), an output-capture
buffer-limit overflow (⇒ exit `125`, `truncated=true`), a timeout (⇒ exit `124`), or any
other error is turned into a **well-formed recorded `run_result`** (non-zero `exit`, a
short `stderr` message, the diff computed so far), never an uncaught exception. This
preserves "exactly one recorded result, replayable" even for an attacker-authored
engine-kill attempt (e.g. `yes` flooding stdout). The contract is that the injected
`run_command` **must not raise**; the `bin` runner honors it.

**Determinism / replay.** A live run executes the command **exactly once**, captures the
full `run_result`, **records it in the trace** as `Run_executed { id; result }`, and binds
it into the context. **Replay consumes the recorded `Run_executed`, re-binds it, and NEVER
re-executes** (it does not call `run_command`, and does not consult the allowlist, since
nothing executes) — so a `run` of `rm` happens once and replay is byte-identical and
side-effect-free, exactly mirroring the `Agent_ran` replay arm. The allowlist-`Blocked`
case is recorded/replayed as `Blocked_at` + terminal `Blocked`, like the gate/commit
`Fail` arms.

**Honest scope — this is NOT a sandbox.** `working_dir` bounds the cwd and the snapshot
scope but **does NOT** prevent the command from touching **absolute paths** named in its
args. The **allowlist is the trust control** (operator opt-in; `[]` = off/fail-closed,
`"*"` = all); full isolation (container/chroot) is **out of scope** for this MVP. Two
caveats follow from this:

- **Do NOT allowlist a wrapper binary.** Allowlisting `sh`, `bash`, `env`, `xargs`,
  `find`, `python3`, `perl`, `node`, … effectively allows **arbitrary commands** (the
  wrapper runs whatever it is told). Keep the allowlist to **leaf tools** (e.g. `mkdir`,
  `touch`), never an interpreter/dispatcher.
- **A symlinked `working_dir` is followed.** If `working_dir` resolves through a symlink,
  the command runs with cwd = the **resolved target** (and the snapshot is taken there).
  This reinforces that the allowlist — not the path — is the trust boundary.

`Lint` emits an informational `run-step-executes-commands` warning for every `run` step,
and a louder `run-step-destructive-command` warning for known-destructive binaries
(`rm`, `rmdir`, `mv`, `dd`, `mkfs`, `shred`, `truncate`, the `:(){` fork-bomb idiom — a
small, documented, **best-effort** set, **not** a security control). Both are warnings, so
they never make `has_errors` true and never block validation.

### 1.5 The `parallel` step — concurrent branches

A `Parallel` step runs multiple branches **concurrently** via Eio fibers and joins when all
branches complete (or when one fails):

```json
{ "kind": "parallel", "branches": [
    [ { "kind": "agent", "id": "r1", "prompt": "…" } ],
    [ { "kind": "agent", "id": "r2", "prompt": "…" } ]
] }
```

**Branch semantics.**
- Each element of `branches` is a `step list` executed as an independent sub-walk sharing the
  same entry context (`ctx`) at the point the `Parallel` step is reached.
- Agent outputs from each branch are recorded in that branch's sub-trace and merged into the
  host context under `outputs.<id>` after all branches complete. Deep-merge: if two branches
  write to distinct ids under `outputs`, both ids survive.

**Cancel-all on first failure.** If any branch reaches an `Aborted` or `Blocked` outcome, all
sibling fibers are cancelled via `Eio.Switch.fail`. The `cancelled-by-sibling` outcome is
assigned to any branch that did not complete before cancellation. The overall parallel outcome
is the worst of all branch outcomes (`Aborted > Blocked > Committed > Completed_no_commit`).
A `Parallel_completed` entry is always emitted.

**Safety floor — commits inside parallel branches are rejected.** The lint/validate check
`commit-in-parallel` (error severity) rejects any `Commit` reachable inside a parallel branch.
Commits are terminal steps: a commit inside a parallel branch would prevent the switch from
joining all branches cleanly. **Commits must appear outside parallel steps.**

**Floor-gate intersection.** A gate guaranteed in **all** branches counts as guaranteed after
the parallel step (consistent with `Branch`'s intersection discipline). A gate guaranteed in
only some branches does NOT satisfy the floor for a subsequent commit.

**Output-collision detection.** Two branches with the same agent id cause a `parallel-output-collision`
error diagnostic: their outputs would overwrite each other nondeterministically.

**Replay determinism.** The live run records each branch's sub-trace inside
`Parallel_branch_completed { branch_idx; trace; outcome; branch_outputs }`. Replay re-feeds
each branch's sub-trace without re-executing fibers, producing the same outcome byte-identically.
The allowlist is not consulted on replay (nothing re-executes).

**Trace entries emitted:**
- `Parallel_started` — emitted before any branch fiber starts.
- `Parallel_branch_completed { branch_idx; trace; outcome; branch_outputs }` — one per branch,
  in index order (0, 1, …), emitted after all branches join.
- `Parallel_completed { outcome }` — emitted last with the worst overall outcome.

### 1.6 The `foreach` step — iterating over a context array

A `Foreach` step iterates a body step list over every element of a JSON array in the run context:

```json
{ "kind": "foreach", "over": "items", "steps": [
    { "kind": "agent", "id": "process", "prompt": "Process item: {{item}}" }
] }
```

**`over` key resolution.** `over` is a **bare top-level ctx key** (not a dotted path). The
engine looks up `ctx["items"]` directly. Typical use: pre-populate the context via
`Engine.run ~initial_ctx:[("items", `List [...])]`. Inside a workflow, a preceding agent or
`Run` step cannot write a bare top-level key directly — the engine only binds `"outputs"`,
`"loop"`, and `"item"` at the top level. The `--ctx` CLI option allows operator-supplied
initial context.

**`item` binding.** On each iteration, `ctx["item"]` is bound to the current array element.
The `item` binding is **scoped to the foreach**: after the foreach completes, the prior value
of `ctx["item"]` (if any) is restored, or `"item"` is removed if it was not present before the
foreach. This prevents leaking the last element to subsequent steps and allows nested foreach
loops without clobbering the outer `item`.

**Empty array.** An empty array (`[]`) produces zero iterations; no body step is executed and
`Foreach_completed { iterations = 0 }` is emitted.

**Missing or non-array key.** If `ctx[over]` is absent or not a `List`, the step is `Blocked`
with a descriptive message naming the key.

**Replay.** The live run records `Foreach_iter_started`, body sub-trace, `Foreach_iter_completed`
for each iteration, and `Foreach_completed`. Replay re-feeds the recorded elements and verifies
element identity. The `initial_ctx` used for the original run must be supplied to `Engine.replay`
via `?initial_ctx` for the array to be available.

**Trace entries emitted (per successful foreach):**
- `Foreach_iter_started { index; element }` — one per element, before the body executes.
- `Foreach_iter_completed { index; outcome }` — one per element, after the body completes.
- `Foreach_completed { iterations }` — once at the end, with the total number of iterations.

### 1.7 The `version` field on workflow root

The workflow root object accepts an optional `"version"` string field:

```json
{ "name": "my-workflow", "version": "1.0", "steps": [ … ] }
```

`version` is informational only — the engine does not interpret it. It is surfaced in the
compiled-to-Claude-Workflow header comment (`// Compiled from CWR v1.0`) and propagated
through the ledger/replay path unchanged.

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

The validator is **structural**: it requires each floor gate's **id** to be
*guaranteed-evaluated* on every path to a commit. The gates must additionally **PASS**:
at runtime a `Gate` whose `when_` evaluates **false** terminates the walk as `Blocked`
(naming the gate id), so a false floor gate can never reach a commit. The floor therefore
means "floor gates must *pass* on every path to a commit"; the runtime token is the
second lock.

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
  run_agent   : id:string -> prompt:string -> read_only:bool -> bool * Yojson.Safe.t;
  budget      : unit -> int;   (* a Budget governor stops the loop once this is <= 0 *)
  run_command : id:string -> argv:string list -> working_dir:string ->
                timeout_ms:int option -> observe:string list option -> run_result;
    (* the Run-step effect; bin/ implements it (process + dir snapshot), lib stays yojson-only *)
}
```

The library ships a deterministic **stub** backend (`Backend.stub`) used by tests.
The CLI builds a **cabal-backed** backend (`bin/backend_cabal.ml`): `run_agent` forces
**structured output** via `Cabal.Registry.first_available` +
`Cabal.Agentic_backend.run_task_with_ctxt` over a spec from
`Cabal.Backend_types.make_task_spec` (with `expected_outputs` including
`Structured_report`), parsing the structured report's `raw_json` (falling back to
parsing `agent_text` as JSON); if no parseable JSON is produced it **fails closed**
(`success = false`). `budget` is a genuine **consumable** counter created per run
(`make`), initialised to 1_000_000 by default or from the `CWR_BUDGET` env var: each
`Budget`-governor check **decrements and returns** the remaining, so with `CWR_BUDGET=N`
the run performs **at most N** budget-governed loop iterations total (shared across all
loops in that run) before the `Budget` governor stops the loop. (Determinism is
unaffected — every `Budget_read` is recorded and replay re-feeds the recorded values.)
cabal usage is confined to this boundary: the
`cabal_workflow_runner` **library depends on yojson only**; only `bin/` links cabal.

`Engine.run ?max_loop_iters ~backend ~token validated : outcome * trace` performs a
deterministic walk: `Agent` → `run_agent`, bind `outputs.<id>`; a `success = false` run
**aborts** the walk (fail-closed), and on success the `output_schema` is validated
(fail-closed); `Gate` → pure `Expr.eval`; a `Pass` continues, a **`Fail` blocks** the
run (`Blocked`, naming the gate); `Branch` → pure `Expr.eval` chooses the arm; `Loop` →
iterate under the engine **ceiling** (`?max_loop_iters`, default `10_000`; see §1.3),
also stopping early on `until` or any fired governor; `Commit` → require token
(`Blocked` if absent/ill-formed) else `Committed`.

**Determinism / replay.** The trace records, *in order, everything a stop/branch
decision reads*: each agent's structured JSON output, each `Budget` reading, each
`Fixpoint` progress verdict, each `until`/gate/branch verdict, and loop iteration
counts. `Engine.replay ~trace validated : outcome` re-feeds the recorded agent outputs
and recorded budget readings (no backend calls), re-evaluates the total DSL over the
rebuilt context (asserting each recorded verdict), and produces the **same** outcome and
trace. Because the loop's bound is purely a function of these recorded inputs, **an
unbounded-but-governed loop still replays byte-identically.**

**On-disk ledger (persisted replay).** Replay is no longer in-memory-only. The
pure, yojson-only `Ledger` module is the persistence boundary: `Ledger.to_ndjson
: Types.trace -> string` serialises a trace to newline-delimited JSON (one tagged
object per `trace_entry`, including the full `Run_executed` result —
exit/stdout/stderr/truncated and each file's path/change/size/digest), and
`Ledger.of_ndjson : string -> (Types.trace, string) result` is its **fail-closed**
inverse (any malformed line ⇒ `Error`, never raises), with round-trip
`of_ndjson (to_ndjson t) = Ok t` for every trace. The CLI writes the ledger with
`run --ledger <path>` and re-runs it in a **later process** with
`replay <wf> --ledger <path> [--floor …]`, which validates the workflow, decodes
the ledger, and feeds the trace to the same `Engine.replay` (no backend; nothing
is dispatched/executed). A workflow/ledger mismatch fails closed as
`Replay_mismatch`. The ledger is **runtime output, not workflow input** — the
schema is unchanged and there is no parity impact. Consumers such as
`tools/bounty-pipeline` thereby gain persisted, deterministic replay.

**What replay attests — and what it does NOT (read before trusting a ledger).**
Replay re-evaluates the pure DSL over the rebuilt context and asserts each recorded
verdict matches the recompute, so it guarantees the trace is **internally consistent
with the workflow and the recorded agent/run outputs** — every *structural* tamper
(corrupt/truncated/reordered/trailing line, a cross-workflow trace, or a verdict
inconsistent with its inputs) fails closed. It does **NOT authenticate** those
recorded outputs or the commit token: a ledger is an **unauthenticated,
attacker-editable file**, so a *forged* ledger (one whose forged agent/run outputs
make the gates genuinely pass, plus a forged `committed_step`) **can replay to
`Committed`**. Replay proves "this trace is a valid execution of this workflow given
these outputs," not "these outputs are real." For tamper-evidence, sign or MAC the
ledger out of band. (Same spirit as the `file_change.digest` caveat: not a
cryptographic integrity guarantee.)

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
**pure and offline** (no agent, no backend, no I/O): linear for realistic workflows,
worst-case superlinear on pathologically deep branch nesting (the floor/ref analyses
re-walk both arms of every `Branch`). For any sane generated workflow it is effectively
instant and cheap to call in a tight generate → lint → fix loop.

### Constrain your generator with the schema

Three layers guard a generated workflow, each tighter than the last:

1. **Schema — shape at generation.** `Workflow_schema` (`lib/workflow_schema.ml(i)`,
   pure, yojson-only) exposes the canonical **JSON Schema (draft 2020-12)** of the
   workflow format as a library value (`schema : Yojson.Safe.t`, `to_string : unit ->
   string`). It is **derived from `Workflow_json`** — the actual parser — and a test
   asserts the committed [`schema/workflow.schema.json`](schema/workflow.schema.json)
   byte-matches `Workflow_schema.to_string ()` and that the step `kind`s it enumerates
   are exactly the ones the parser accepts, so it cannot drift from what runs.

   **Governing principle.** `Workflow_json.of_string` accepts a workflow **iff** that
   workflow is **structurally valid per `schema/workflow.schema.json`**. Every *structural*
   constraint the schema expresses is enforced by the parser, and vice-versa. (Semantic
   *floor* checks — gate reachability, commit-must-be-gated, ungoverned-loop, dangling
   refs — remain `Lint`/`Validate`'s job and are deliberately **not** encoded in the
   schema.) A behavioral parity test drives the parser over a battery of candidate JSON
   strings and asserts, per case, that the parser's accept/reject verdict matches the
   schema's structural verdict.

   Concretely:
   - **Workflow / step / governor objects are closed with a metadata escape hatch:** the
     schema marks each `additionalProperties:false` **plus** `patternProperties: {"^_": {}}`,
     mirroring the parser, which rejects any key that is neither a known key for that object
     nor `_`-prefixed. So `{"kind":"agent",…,"junk":1}` is rejected by both, while `_doc` /
     `_note` are accepted by both.
   - **Expr operator objects are *strictly* closed — they take NO `_` metadata.** An expr is a
     single-operator object: the parser requires exactly one key and rejects any extra key,
     *including* a leading-underscore one. The schema models each expr branch with
     `additionalProperties:false` and **no** `^_` pattern. So `{"lit":true,"_x":1}` is rejected
     by both; an empty `{}` (zero keys) and a two-key object are likewise rejected by both.
   - **Bounded integers** (`max_iters.n`, `fixpoint.window`) carry `minimum:1` and
     `maximum: 4611686018427387903` (OCaml `max_int` on 64-bit). A value `< 1` is rejected at
     parse and by the schema; a large-but-valid literal such as `1073741824` is accepted by
     both; a literal `> max_int` is yielded by yojson as `` `Intlit `` (which the parser
     rejects) and exceeds the schema `maximum`, so it is invalid on both sides. Because JSON
     Schema's `"type":"integer"` matches any number with zero fractional part, an
     integer-valued float (`5.0`) is schema-valid; the parser therefore accepts an
     integer-valued float in range and rejects a fractional one (`5.5`), preserving the iff.
   - The schema↔parser iff is checked two ways: the in-suite behavioral test
     (`test_schema_parser_behavioral_parity`) and `scripts/parity_check.py`, which drives the
     committed schema with a real JSON-Schema validator (jsonschema, Draft 2020-12) against the
     parser over a battery and exits non-zero on any divergence (dev/CI; not part of `dune test`).
   - **A loop's `governors` array is `minItems:1`** in the schema and a non-empty list at
     parse, so an empty `governors` array is a parse-level *shape* error (the richer
     `ungoverned-loop` *semantic* diagnostic still lives in `Lint`/`Validate`).
   - `output_schema` is intentionally the **one open** field→type map (`additionalProperties`
     is a type tag), so arbitrary field names are allowed there.

   Prompt a
   generator with `cabal-workflow-runner schema` (or `Workflow_schema.to_string ()`) so
   it emits **conformant workflows by construction** — correct `kind`s, the expr /
   governor encodings, required fields.
2. **Lint — semantics + safety, pre-run.** `Lint.check_json` then catches what shape
   alone cannot (ungoverned loops, commits missing a floor gate, dangling output refs).
3. **Validate — the run gate.** `Validate.workflow` is the fail-closed gate `Engine.run`
   requires (defined in terms of `Lint.check`; see the contract below).

So: **schema (shape at generation) → lint (semantics/safety pre-run) → validate (gate at
run).**

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
| `commit-in-parallel` | error | a `Commit` is reachable inside a `parallel` branch (commits must appear outside parallel steps). |
| `parallel-output-collision` | error | two parallel branches declare the same agent `id` (their outputs would collide). |
| `soft-fail-with-commit` | error | an agent has `on_failure="continue"` in a workflow that contains a `Commit` (soft-fail is permitted only in commit-free workflows). |
| `run-step-executes-commands` | warning | a `run` step executes a shell command (informational; it runs only if the operator's runtime allowlist permits its binary). |
| `run-step-destructive-command` | warning | a `run` step invokes a known-destructive binary (`rm`/`dd`/… — best-effort advisory, **not** a security control; the runtime allowlist is the trust boundary). |

The error codes are **exactly** the floor + parse/shape failures `Validate.workflow`
enforces — which is what makes the lint-clean ⇒ validate contract hold by construction.

## 5. Input format and planned follow-ups

MVP input is **JSON** (`Workflow_json`, fail-closed on malformed input). Each step is
an object with a `kind` discriminator (`agent` / `gate` / `branch` / `loop` / `run` /
`commit`). See `Workflow_json` docs for the schema.

Expressions and governors are encoded as described in §1.2–§1.3; see the
`Workflow_json` module docs and [`examples/bounty.workflow.json`](examples/bounty.workflow.json)
for a worked example (structured outputs, an expression-gated commit, a governed loop).

A **machine-readable JSON Schema (draft 2020-12)** of this format is published as a
library value (`Workflow_schema`) and a committed artifact
([`schema/workflow.schema.json`](schema/workflow.schema.json)), printable via
`cabal-workflow-runner schema`. It is derived from `Workflow_json` and kept in lock-step
by tests — see §4a, "Constrain your generator with the schema."

Done since the MVP:

- **Workflow JSON Schema** (`Workflow_schema` + `schema/workflow.schema.json` + the
  `schema` CLI) — shipped (v0.4).

- **On-disk ledger** plus a `replay` CLI subcommand — shipped (v0.10): `run
  --ledger <file>` persists a run's trace as NDJSON and `replay <wf> --ledger
  <file>` re-runs it byte-identically in a later process.

Planned follow-ups (not yet shipped):

- **YAML** / **Markdown front-matter** workflow front-end (a prose body plus a
  structured header; cabal already depends on a YAML library).
- A `Spawn` / subworkflow step.
- Agent-output-driven retries on schema mismatch (currently fail-closed `Aborted`).
