# Contributing

Thanks for your interest. This is an early / experimental (v0.x) project; please
keep changes small and well-tested.

## Build & test

Everything is built and tested in an opam switch that has the public
[cabal](https://github.com/epure-team/cabal) library and the dependencies declared in
`dune-project`. The library currently uses `yojson`, `eio`, `unix`, `base64`,
`digestif`, and `mirage-crypto-ec`; the bridge/executable/test toolchain also uses
`cabal`, `eio_posix`, `eio_main`, `cmdliner`, and `alcotest`. Pin cabal and install deps:

```sh
opam pin add -n cabal https://github.com/epure-team/cabal.git#eccda75cede474c8682db41ab5c99d639a655441
opam install . --deps-only --with-test

dune build
dune test          # the full suite must stay green
```

The test binary also runs standalone from the repo root:

```sh
dune exec test/test_cwr.exe
```

(It resolves `examples/` and `schema/` fixtures via `DUNE_SOURCEROOT`, so it works
both under the `dune test` sandbox and standalone.)

## Schema ↔ parser parity contract

The published JSON Schema (`schema/workflow.schema.json`) and the parser
(`Workflow_json`) must agree: `Workflow_json.of_string` accepts a workflow **iff**
that workflow is structurally valid per the schema. After **any** change to the
schema or the parser, run the real-validator-driven parity check (it exits non-zero
on any divergence):

```sh
pip install jsonschema          # dev-only dependency
dune build                      # produces _build/default/bin/main.exe
python3 scripts/parity_check.py # expect: 0 divergence(s)
```

The in-suite no-drift test additionally enforces that the committed
`schema/workflow.schema.json` byte-matches `Workflow_schema.to_string ()`. If you
change the schema, regenerate the artifact (`cabal-workflow-runner schema >
schema/workflow.schema.json`) so the no-drift test stays green.

## Layering rule: `lib/` stays host-neutral

The library `cabal_workflow_runner` (`lib/`) has the dependencies listed above but
must never depend on Cabal or a host application's workflow/orchestration layers.
Cabal is linked only by the separate installable
`cabal_workflow_runner.cabal_bridge` library and the executable. Keep `lib/`
backend bridges injected behind library-owned contracts such as `Backend.t` or
the additive rich `Runtime.t`.

The rich execution DTOs are not part of workflow input: do not wire them into
`Engine.run`, workflow JSON/schema, or workflow ledgers without a separately reviewed
compatibility change. Normalized event traces are post-completion values in this batch;
do not document the API as live streaming. Keep their opaque-constructor resource bounds,
event/response cross-validation, and pre-dispatch legacy-adapter rejections covered when
extending the contract.

## Safety floor must not regress

These invariants are the whole point of the engine; a change must preserve all of
them:

- **Runtime-token commit** — every `Commit` requires a runtime human-approval token,
  hashed for the trace and never stored raw.
- **Gate-fail-blocks** — a floor `Gate` that evaluates false blocks the run.
- **Loop ceiling** — every loop is hard-bounded by the engine iteration ceiling (the
  termination guarantee); governors / `until` are early-stop heuristics under it.
- **Floor gates on every path** — a commit must be guaranteed-gated by the floor
  gates on every path (branch = intersection; a gate inside a loop body does not
  count).

Determinism / byte-identical replay and `Expr.eval` totality must also be preserved.

## AI assistance

This project was built and audited with AI assistance; the per-commit
`Co-Authored-By` trailers disclose where.
