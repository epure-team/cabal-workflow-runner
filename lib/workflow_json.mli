(** Parse a {!Types.workflow} from JSON (yojson), fail-closed on malformed input.

    JSON schema (MVP). YAML and Markdown front-matter are planned follow-ups.

    {[
      { "name": "demo",
        "steps": [
          { "kind": "agent",  "id": "draft", "prompt": "do x", "read_only": false },
          { "kind": "gate",   "id": "g1" },
          { "kind": "branch", "on": "g1",
            "then": [ ... ], "else": [ ... ] },
          { "kind": "loop",   "max_iters": 3, "until": "g2", "body": [ ... ] },
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
