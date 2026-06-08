# cabal-workflow-runner

A **deterministic workflow engine on [cabal](https://github.com/epure-team/cabal)**
(the Caml Agent Backend Abstraction Library). It interprets a declarative workflow —
sequence, bounded loop, branch, gate, and a terminal commit — running the
control-flow **deterministically in OCaml** and dispatching **agent steps via cabal**.

You get data-driven workflows **without** losing determinism, because the interpreter
is deterministic, the **safety floor is enforced by the engine/validator as an
invariant over any workflow** (not by the workflow author), and runs **replay** from a
recorded trace. See [`SPEC.md`](SPEC.md) for the full design.

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
# Fail-closed validation against an embedder-supplied safety floor:
cabal-workflow-runner validate examples/bounty.workflow.json \
  --floor g-validated --floor g-observed --floor g-independent

# Run it. --approve is the runtime human-approval token required by any Commit
# (hashed for the trace, never stored raw). Omit it and a Commit is Blocked.
cabal-workflow-runner run examples/bounty.workflow.json \
  --floor g-validated --floor g-observed --floor g-independent \
  --approve "$APPROVAL_TOKEN"
```

`validate` rejects (exit 1) any workflow with an unbounded loop or a commit that is
not guaranteed-gated by the floor gates on every path. `run` dispatches agent steps to
the first available cabal backend (failing closed if none) and prints the outcome plus
the recorded trace.

## Embedding the library

Supply a backend and the floor gates; everything else is enforced for you:

```ocaml
open Cabal_workflow_runner

let backend : Backend.t =
  { run_agent = (fun ~id ~prompt:_ ~read_only:_ -> (true, id));
    eval_gate = (fun _ -> Types.Pass) }

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

## License

MIT. Copyright (c) 2026 Epure Team.
