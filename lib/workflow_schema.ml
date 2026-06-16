(* The JSON Schema (draft 2020-12) of the workflow format, as a yojson value.

   Hand-derived from [Workflow_json] (the actual parser) so it accepts EXACTLY
   what the parser accepts:

   - Every workflow / step / governor object is CLOSED with a metadata escape
     hatch: the parser rejects any key that is neither a known key for that
     object nor a leading-underscore metadata key (the documented [_doc] /
     [_note] convention). We model that with [additionalProperties:false] PLUS
     [patternProperties: {"^_": {}}] on each such object.
   - Expr operator objects are STRICTLY closed: the parser requires EXACTLY one
     operator key and rejects any other key — including a leading-underscore one.
     So expr objects do NOT take [_] metadata; we model them with
     [additionalProperties:false] and NO [^_] patternProperty.
   - ([output_schema] is intentionally an open field->type map and is the one
     exception to closedness.)
   - The parser dispatches steps on a [kind] discriminator and rejects any other
     kind; governors likewise. We model both as [oneOf] keyed on [kind].
   - The expr encoding is the single-operator-object form produced/consumed by
     [Workflow_json.expr_op] / [expr_to_json]: exactly one operator key.
   - [output_schema] maps field name -> a type tag ("string"|"int"|"number"
     |"bool"|"any") or an [{ "enum": [string, ...] }] object, matching
     [Workflow_json.ty_of_json] / [ty_to_json]. *)

let s str : Yojson.Safe.t = `String str
let obj fields : Yojson.Safe.t = `Assoc fields
let arr items : Yojson.Safe.t = `List items
let ref_ name : Yojson.Safe.t = obj [ ("$ref", s ("#/$defs/" ^ name)) ]

(* { "type": "string" } etc. *)
let typ t = obj [ ("type", s t) ]

(* A bounded integer: [minimum:1] and [maximum: max_int] (OCaml [max_int] on a
   64-bit platform, 2^62-1). This matches the parser exactly: the parser accepts
   any [`Int] in [1, max_int] (see [Workflow_json.req_bounded_int]) and rejects
   both [< 1] and any literal [> max_int] (yojson yields the latter as [`Intlit],
   which the parser rejects). A large-but-valid literal like [1073741824] is thus
   schema-valid AND parser-accepted; a literal beyond [max_int] is invalid on
   both sides. *)
let bounded_int : Yojson.Safe.t =
  obj
    [
      ("type", s "integer");
      ("minimum", `Int 1);
      ("maximum", `Int max_int);
    ]

(* An object schema that requires [required] keys, gives [props] for the named
   ones, and tolerates any extra key (the parser ignores unknown fields). *)
let object_with ~required ~props : Yojson.Safe.t =
  obj
    [
      ("type", s "object");
      ("required", arr (List.map s required));
      ("properties", obj props);
      ("additionalProperties", `Bool true);
    ]

(* A CLOSED object schema: the parser rejects any key that is neither a known
   key for the object nor a leading-underscore metadata key (the documented
   [_doc]/[_note] escape hatch). We model that with [additionalProperties:false]
   (rejects unknown keys) PLUS a [patternProperties] allowing [^_] keys (the
   metadata escape hatch the parser permits), so the schema and the parser agree
   exactly. Used for the workflow / step / governor / expr objects. *)
let closed_object_with ~required ~props : Yojson.Safe.t =
  obj
    [
      ("type", s "object");
      ("required", arr (List.map s required));
      ("properties", obj props);
      ("patternProperties", obj [ ("^_", obj []) ]);
      ("additionalProperties", `Bool false);
    ]

(* A STRICTLY closed object schema with NO [^_] metadata escape hatch: it is
   exactly the declared keys and nothing else. Used for expr operator objects,
   which the parser requires to be a single-operator object — it rejects ANY
   extra key, including a leading-underscore one (see [Workflow_json.expr_of_json],
   whose [`Assoc [ (key, v) ]] pattern matches exactly one key). So expr objects
   do NOT take [_doc]/[_note] metadata, and the schema must agree. *)
let strictly_closed_object_with ~required ~props : Yojson.Safe.t =
  obj
    [
      ("type", s "object");
      ("required", arr (List.map s required));
      ("properties", obj props);
      ("additionalProperties", `Bool false);
    ]

(* ---- expr ($defs/expr) -------------------------------------------------- *)

(* Helpers describing the operand shapes the parser enforces:
   - "path"/"exists" -> a (dotted) string;
   - "lit" -> any JSON value;
   - eq/ne/lt/le/gt/ge/in -> a 2-element list of exprs;
   - and/or -> a list of exprs;
   - not -> a single expr. *)
let expr_def : Yojson.Safe.t =
  let single key value_schema =
    strictly_closed_object_with ~required:[ key ] ~props:[ (key, value_schema) ]
  in
  let pair key =
    single key
      (obj
         [
           ("type", s "array");
           ("items", ref_ "expr");
           ("minItems", `Int 2);
           ("maxItems", `Int 2);
         ])
  in
  let nary key =
    single key (obj [ ("type", s "array"); ("items", ref_ "expr") ])
  in
  obj
    [
      ( "description",
        s
          "A total-predicate-DSL expression: a single-operator object. See \
           Workflow_json's expr encoding." );
      ( "oneOf",
        arr
          [
            single "path" (typ "string");
            single "exists" (typ "string");
            single "lit" (obj [] (* any JSON value *));
            pair "eq";
            pair "ne";
            pair "lt";
            pair "le";
            pair "gt";
            pair "ge";
            pair "in";
            nary "and";
            nary "or";
            single "not" (ref_ "expr");
          ] );
    ]

(* ---- governor ($defs/governor) ------------------------------------------ *)

let governor_def : Yojson.Safe.t =
  let kind_const k = obj [ ("const", s k) ] in
  obj
    [
      ("description", s "A loop governor (termination guarantee).");
      ( "oneOf",
        arr
          [
            closed_object_with ~required:[ "kind"; "n" ]
              ~props:[ ("kind", kind_const "max_iters"); ("n", bounded_int) ];
            closed_object_with ~required:[ "kind" ]
              ~props:[ ("kind", kind_const "budget") ];
            closed_object_with
              ~required:[ "kind"; "window"; "progress" ]
              ~props:
                [
                  ("kind", kind_const "fixpoint");
                  ("window", bounded_int);
                  ("progress", ref_ "expr");
                ];
          ] );
    ]

(* ---- output_schema ($defs/output_schema) -------------------------------- *)

let output_schema_def : Yojson.Safe.t =
  let type_tag =
    obj
      [
        ( "oneOf",
          arr
            [
              obj
                [
                  ("type", s "string");
                  ("enum", arr (List.map s [ "string"; "int"; "number"; "bool"; "any" ]));
                ];
              object_with ~required:[ "enum" ]
                ~props:
                  [
                    ( "enum",
                      obj
                        [
                          ("type", s "array");
                          ("items", typ "string");
                        ] );
                  ];
            ] );
      ]
  in
  obj
    [
      ( "description",
        s
          "Maps each required field name to a type tag \
           (\"string\"|\"int\"|\"number\"|\"bool\"|\"any\") or an \
           {\"enum\":[string,...]} object." );
      ("type", s "object");
      ("additionalProperties", type_tag);
    ]

(* ---- step ($defs/step) -------------------------------------------------- *)

let step_def : Yojson.Safe.t =
  let kind_const k = obj [ ("const", s k) ] in
  let step_list = obj [ ("type", s "array"); ("items", ref_ "step") ] in
  let agent =
    closed_object_with
      ~required:[ "kind"; "id"; "prompt" ]
      ~props:
        [
          ("kind", kind_const "agent");
          ("id", typ "string");
          ("prompt", typ "string");
          ("read_only", typ "boolean");
          ("output_schema", ref_ "output_schema");
          ("on_failure", obj [ ("enum", arr [ s "abort"; s "continue" ]) ]);
          ("protocol", typ "string");
          ("brief", typ "string");
          ("agent_type", typ "string");
          ("model", typ "string");
        ]
  in
  let gate =
    closed_object_with
      ~required:[ "kind"; "id"; "when" ]
      ~props:
        [
          ("kind", kind_const "gate");
          ("id", typ "string");
          ("when", ref_ "expr");
        ]
  in
  let branch =
    closed_object_with
      ~required:[ "kind"; "when"; "then"; "else" ]
      ~props:
        [
          ("kind", kind_const "branch");
          ("when", ref_ "expr");
          ("then", step_list);
          ("else", step_list);
        ]
  in
  let loop =
    closed_object_with
      ~required:[ "kind"; "governors"; "body" ]
      ~props:
        [
          ("kind", kind_const "loop");
          ("until", ref_ "expr");
          ( "governors",
            obj
              [
                ("type", s "array");
                ("items", ref_ "governor");
                ("minItems", `Int 1);
              ] );
          ("body", step_list);
        ]
  in
  let run =
    closed_object_with
      ~required:[ "kind"; "id"; "cmd"; "working_dir" ]
      ~props:
        [
          ("kind", kind_const "run");
          ("id", typ "string");
          ( "cmd",
            obj
              [
                ("type", s "array");
                ("items", typ "string");
                ("minItems", `Int 1);
              ] );
          (* working_dir: a RELATIVE path with no ".." component (mirrors the
             parser's [req_relative_path]). The pattern rejects an absolute path
             (leading "/") and any ".." path segment, while accepting names that
             merely CONTAIN ".." (e.g. "a..b"). *)
          ( "working_dir",
            obj
              [
                ("type", s "string");
                ("pattern", s "^(?!/)(?!(.*/)?\\.\\.(/|$)).+$");
              ] );
          ("timeout_ms", bounded_int);
          ("observe", obj [ ("type", s "array"); ("items", typ "string") ]);
        ]
  in
  let commit =
    closed_object_with ~required:[ "kind"; "id" ]
      ~props:[ ("kind", kind_const "commit"); ("id", typ "string") ]
  in
  let parallel =
    closed_object_with
      ~required:[ "kind"; "branches" ]
      ~props:
        [
          ("kind", kind_const "parallel");
          ( "branches",
            obj
              [
                ("type", s "array");
                ("minItems", `Int 2);
                ("items", step_list);
              ] );
        ]
  in
  let foreach =
    closed_object_with
      ~required:[ "kind"; "over"; "steps" ]
      ~props:
        [
          ("kind", kind_const "foreach");
          ("over", typ "string");
          ("steps", step_list);
        ]
  in
  let shell =
    closed_object_with
      ~required:[ "kind"; "id"; "commands" ]
      ~props:
        [
          ("kind", kind_const "shell");
          ("id", typ "string");
          ( "commands",
            obj
              [
                ("type", s "array");
                ("items", typ "string");
                ("minItems", `Int 1);
              ] );
          ("on_failure", obj [ ("enum", arr [ s "abort"; s "continue" ]) ]);
        ]
  in
  let evidence =
    closed_object_with
      ~required:[ "kind"; "id"; "build"; "check"; "zero_admits"; "tier"; "output" ]
      ~props:
        [
          ("kind", kind_const "evidence");
          ("id", typ "string");
          ("build", typ "string");
          ("check", typ "string");
          ("zero_admits", typ "string");
          ("tier", typ "string");
          ("output", typ "string");
        ]
  in
  obj
    [
      ( "description",
        s "A workflow step, discriminated by the \"kind\" key." );
      ( "oneOf",
        arr [ agent; gate; branch; loop; run; commit; parallel; foreach; shell; evidence ] );
    ]

(* ---- top level ---------------------------------------------------------- *)

let schema : Yojson.Safe.t =
  obj
    [
      ("$schema", s "https://json-schema.org/draft/2020-12/schema");
      ( "$id",
        s "https://epure-team.github.io/cabal-workflow-runner/workflow.schema.json"
      );
      ("title", s "cabal-workflow-runner workflow");
      ( "description",
        s
          "A declarative workflow for cabal-workflow-runner: a name plus a list \
           of steps. This schema describes exactly the JSON that Workflow_json \
           parses. Every workflow / step / governor / expr object is closed: \
           unknown keys are rejected, except keys prefixed with _ (ignored \
           metadata, e.g. the _doc convention). Schema constrains shape at \
           generation; Lint catches semantics/safety pre-run; Validate is the \
           run gate." );
      ("type", s "object");
      ("required", arr [ s "name"; s "steps" ]);
      ( "properties",
        obj
          [
            ("name", typ "string");
            ("steps", obj [ ("type", s "array"); ("items", ref_ "step") ]);
            ("version", typ "string");
          ] );
      ("patternProperties", obj [ ("^_", obj []) ]);
      ("additionalProperties", `Bool false);
      ( "$defs",
        obj
          [
            ("step", step_def);
            ("expr", expr_def);
            ("governor", governor_def);
            ("output_schema", output_schema_def);
          ] );
    ]

let to_string () = Yojson.Safe.pretty_to_string schema ^ "\n"
