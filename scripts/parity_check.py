#!/usr/bin/env python3
"""Schema <-> parser parity check (real-validator driven).

The in-suite OCaml test (`test_schema_parser_behavioral_parity`) drives the
parser over a battery but encodes the *expected* verdict by hand. This script
closes that blind spot: it drives the committed `schema/workflow.schema.json`
with a REAL JSON Schema validator (jsonschema, Draft 2020-12) and the actual
parser (via the `lint` CLI), and asserts, for every case,

    schema-structurally-valid  <=>  parser-parses

which is the governing principle in SPEC.md. Any divergence exits non-zero.

Dev/CI only (NOT part of `dune test`): requires python3 + `jsonschema`.
Usage (after `dune build`):
    eval $(opam env --switch=<switch> --set-switch)
    python3 scripts/parity_check.py
"""
import json, os, subprocess, sys, tempfile

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
SCHEMA = os.path.join(ROOT, "schema", "workflow.schema.json")
BIN = os.path.join(ROOT, "_build", "default", "bin", "main.exe")

try:
    from jsonschema import Draft202012Validator
except ImportError:
    sys.exit("parity_check: needs the python 'jsonschema' package (dev-only).")

if not os.path.exists(BIN):
    sys.exit(f"parity_check: {BIN} not found — run `dune build` first.")

V = Draft202012Validator(json.load(open(SCHEMA)))


def parser_parses(wf):
    """True iff the parser accepts wf structurally (a parse failure surfaces as
    a lint diagnostic with code invalid-json / invalid-shape; semantic floor
    diagnostics do NOT count as a parse failure)."""
    with tempfile.NamedTemporaryFile("w", suffix=".json", delete=False) as f:
        json.dump(wf, f)
        p = f.name
    try:
        out = subprocess.run(
            [BIN, "lint", p, "--floor", "g", "--json"],
            capture_output=True, text=True
        ).stdout
        ds = json.loads(out).get("diagnostics", [])
        return not any(d.get("code") in ("invalid-json", "invalid-shape") for d in ds)
    finally:
        os.unlink(p)


def A(i): return {"kind": "agent", "id": i, "prompt": "x", "read_only": True}
def G(): return {"kind": "gate", "id": "g", "when": {"lit": True}}
def loop(govs): return {"kind": "loop", "governors": govs, "body": [A("b")]}
def wf(steps, **extra): return {"name": "w", "steps": steps, **extra}
def mi(n): return loop([{"kind": "max_iters", "n": n}])
def fpw(w): return loop([{"kind": "fixpoint", "window": w, "progress": {"lit": True}}])

CASES = [
    ("baseline valid", wf([A("a")])),
    ("agent +junk", wf([{**A("a"), "junk": 9}])),
    ("agent +_note", wf([{**A("a"), "_note": "ok"}])),
    ("workflow +_doc", wf([A("a")], _doc="hi")),
    ("gate +junk", wf([{**G(), "junk": 1}])),
    ("commit +junk", wf([G(), {"kind": "commit", "id": "c", "junk": 1}])),
    ("branch +junk", wf([{"kind": "branch", "when": {"lit": True},
                          "then": [A("t")], "else": [A("e")], "junk": 1}])),
    ("unknown kind", wf([{"kind": "frobnicate"}])),
    ("governor +junk", wf([loop([{"kind": "max_iters", "n": 3, "junk": 1}])])),
    ("governor unknown kind", wf([loop([{"kind": "throttle"}])])),
    ("empty governors", wf([loop([])])),
    # int bounds
    ("max_iters n=-5", wf([mi(-5)])),
    ("max_iters n=0", wf([mi(0)])),
    ("max_iters n=1", wf([mi(1)])),
    ("max_iters n=1073741824", wf([mi(1073741824)])),
    ("max_iters n=max_int", wf([mi(4611686018427387903)])),
    ("max_iters n=>max_int", wf([mi(99999999999999999999)])),
    ("max_iters n=5.0 float", wf([mi(5.0)])),
    ("max_iters n=5.5 float", wf([mi(5.5)])),
    ("max_iters n=0.0 float", wf([mi(0.0)])),
    ("max_iters n string", wf([loop([{"kind": "max_iters", "n": "3"}])])),
    ("fixpoint window=0", wf([fpw(0)])),
    ("fixpoint window=2", wf([fpw(2)])),
    ("fixpoint window=2.0 float", wf([fpw(2.0)])),
    ("fixpoint window=2.5 float", wf([fpw(2.5)])),
    # expr operator objects
    ("expr {} 0-key", wf([{"kind": "gate", "id": "g", "when": {}}])),
    ("expr 1-key", wf([{"kind": "gate", "id": "g", "when": {"lit": True}}])),
    ("expr 2-key", wf([{"kind": "gate", "id": "g",
                        "when": {"eq": [{"lit": 1}, {"lit": 1}], "lit": True}}])),
    ("expr +_x underscore", wf([{"kind": "gate", "id": "g",
                                 "when": {"lit": True, "_x": 1}}])),
    # nested defect (deep)
    ("loop-in-branch empty governors (deep)",
     wf([{"kind": "branch", "when": {"lit": True},
          "then": [loop([])], "else": [A("e")]}])),
]

div = 0
for name, w in CASES:
    s = V.is_valid(w)
    p = parser_parses(w)
    ok = (s == p)
    div += not ok
    print(f"{name:34s} schema={str(s):5s} parser={str(p):5s} "
          f"{'AGREE' if ok else '*** DIVERGE ***'}")
print(f"\nparity_check: {div} divergence(s) across {len(CASES)} cases")
sys.exit(1 if div else 0)
