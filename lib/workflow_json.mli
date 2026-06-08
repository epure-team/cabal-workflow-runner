(** Parse a {!Types.workflow} from JSON (yojson), fail-closed on malformed input.

    JSON schema (MVP). Gates / branches / loop stop conditions carry a {!Expr.t}
    expression; loops carry a list of governors.

    Expression encoding:
    - [{"path": "outputs.assess.severity"}] — dotted path into the run context;
    - [{"lit": <json>}] — a literal value;
    - [{"eq": [e1, e2]}] (and [ne]/[lt]/[le]/[gt]/[ge]/[in]);
    - [{"and": [..]}], [{"or": [..]}], [{"not": e}];
    - [{"exists": "outputs.x.y"}].

    Governor encoding:
    - [{"kind": "max_iters", "n": 3}];
    - [{"kind": "budget"}];
    - [{"kind": "fixpoint", "window": 2, "progress": <expr>}].

    {[
      { "name": "demo",
        "steps": [
          { "kind": "agent", "id": "assess", "prompt": "...", "read_only": true,
            "output_schema": { "severity": { "enum": ["low","high","critical"] } } },
          { "kind": "gate",   "id": "g1",
            "when": { "exists": "outputs.assess.severity" } },
          { "kind": "branch",
            "when": { "in": [ {"path":"outputs.assess.severity"},
                              {"lit": ["high","critical"]} ] },
            "then": [ ... ], "else": [ ... ] },
          { "kind": "loop",
            "until": { "eq": [ {"path":"outputs.fix.done"}, {"lit": true} ] },
            "governors": [ { "kind": "budget" },
                           { "kind": "fixpoint", "window": 2,
                             "progress": {"path":"outputs.fix.progressed"} } ],
            "body": [ ... ] },
          { "kind": "commit", "id": "submit" }
        ] }
    ]} *)

val of_json : Yojson.Safe.t -> (Types.workflow, string) result
(** Parse a workflow from an already-parsed JSON value. *)

val of_string : string -> (Types.workflow, string) result
(** Parse a workflow from a JSON string; fail-closed on malformed JSON. *)

val of_file : string -> (Types.workflow, string) result
(** Read and parse a workflow from a file path; fail-closed. *)

val to_json : Types.workflow -> Yojson.Safe.t
(** Serialise a workflow back to JSON (round-trips with {!val:of_json}). *)
