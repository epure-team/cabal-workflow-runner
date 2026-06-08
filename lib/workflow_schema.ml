(* The JSON Schema (draft 2020-12) of the workflow format, as a yojson value.

   Hand-derived from [Workflow_json] (the actual parser) so it accepts EXACTLY
   what the parser accepts:

   - The parser looks fields up with [List.assoc_opt] and ignores anything it
     does not name, so unknown keys (including the [_doc] / leading-underscore
     convention used in examples) are tolerated everywhere. We model that with
     [additionalProperties: true] on every object.
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

(* ---- expr ($defs/expr) -------------------------------------------------- *)

(* Helpers describing the operand shapes the parser enforces:
   - "path"/"exists" -> a (dotted) string;
   - "lit" -> any JSON value;
   - eq/ne/lt/le/gt/ge/in -> a 2-element list of exprs;
   - and/or -> a list of exprs;
   - not -> a single expr. *)
let expr_def : Yojson.Safe.t =
  let single key value_schema =
    object_with ~required:[ key ] ~props:[ (key, value_schema) ]
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
            object_with ~required:[ "kind"; "n" ]
              ~props:[ ("kind", kind_const "max_iters"); ("n", typ "integer") ];
            object_with ~required:[ "kind" ]
              ~props:[ ("kind", kind_const "budget") ];
            object_with
              ~required:[ "kind"; "window"; "progress" ]
              ~props:
                [
                  ("kind", kind_const "fixpoint");
                  ("window", typ "integer");
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
    object_with
      ~required:[ "kind"; "id"; "prompt" ]
      ~props:
        [
          ("kind", kind_const "agent");
          ("id", typ "string");
          ("prompt", typ "string");
          ("read_only", typ "boolean");
          ("output_schema", ref_ "output_schema");
        ]
  in
  let gate =
    object_with
      ~required:[ "kind"; "id"; "when" ]
      ~props:
        [
          ("kind", kind_const "gate");
          ("id", typ "string");
          ("when", ref_ "expr");
        ]
  in
  let branch =
    object_with
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
    object_with
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
  let commit =
    object_with ~required:[ "kind"; "id" ]
      ~props:[ ("kind", kind_const "commit"); ("id", typ "string") ]
  in
  obj
    [
      ( "description",
        s "A workflow step, discriminated by the \"kind\" key." );
      ("oneOf", arr [ agent; gate; branch; loop; commit ]);
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
           parses. Unknown object keys (e.g. the _doc convention) are tolerated. \
           Schema constrains shape at generation; Lint catches semantics/safety \
           pre-run; Validate is the run gate." );
      ("type", s "object");
      ("required", arr [ s "name"; s "steps" ]);
      ( "properties",
        obj
          [
            ("name", typ "string");
            ("steps", obj [ ("type", s "array"); ("items", ref_ "step") ]);
          ] );
      ("additionalProperties", `Bool true);
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
