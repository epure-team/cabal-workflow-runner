# Contributing

Thanks for your interest. This is an early / experimental (v0.x) project; please
keep changes small and well-tested.

## Build & test

Everything is built and tested in an opam switch that has the public
[cabal](https://github.com/epure-team/cabal) library plus `eio`, `cmdliner`,
`alcotest`, and `yojson`. Pin cabal and install deps:

```sh
opam pin add -n cabal https://github.com/epure-team/cabal.git
opam install . --deps-only --with-test

dune build
dune test          # 45 tests; must stay green
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

## Layering rule: `lib/` stays yojson-only

The library `cabal_workflow_runner` (`lib/`) depends on **yojson only** — never on
cabal. cabal is linked **only** in the executable (`bin/`). Do not add a cabal (or
eio) dependency to anything under `lib/`.

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
