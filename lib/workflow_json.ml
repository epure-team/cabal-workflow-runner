open Types

(* A tiny fail-closed parser. We use exceptions internally and convert to a
   result at the boundary, so any structural defect yields [Error reason]. *)

exception Parse_error of string

let err fmt = Printf.ksprintf (fun s -> raise (Parse_error s)) fmt

let member_opt key = function
  | `Assoc fields -> List.assoc_opt key fields
  | _ -> None

let req_string key json =
  match member_opt key json with
  | Some (`String s) -> s
  | Some _ -> err "field %S must be a string" key
  | None -> err "missing required field %S" key

let opt_bool key default json =
  match member_opt key json with
  | Some (`Bool b) -> b
  | Some _ -> err "field %S must be a boolean" key
  | None -> default

(* A bounded integer field, used for [max_iters.n] and [fixpoint.window]. The
   schema declares [1 <= v <= max_int]; we enforce the SAME bounds at parse so
   the parser accepts a workflow iff it is structurally schema-valid:
   - a literal [> max_int] (e.g. 100000000000000000000) is yielded by yojson as
     [`Intlit] (not [`Int]); we reject it explicitly with a clear message.
   - a value [< 1] is rejected here (the schema's [minimum:1]). *)
let req_bounded_int key json =
  match member_opt key json with
  | Some (`Int n) ->
      if n < 1 then err "field %S must be >= 1 (got %d)" key n else n
  | Some (`Float f) ->
      (* JSON Schema's ["type":"integer"] matches any number with zero
         fractional part, so [5.0] is schema-valid. Accept an integer-valued
         float in range (parity with the schema); reject a fractional one. *)
      if not (Float.is_integer f) then
        err "field %S must be an integer (got %g)" key f
      else if f < 1.0 || f > Float.of_int max_int then
        err "field %S is out of range (1 <= n <= %d)" key max_int
      else int_of_float f
  | Some (`Intlit _) ->
      err "field %S is out of the supported integer range (max %d)" key max_int
  | Some _ -> err "field %S must be an integer" key
  | None -> err "missing required field %S" key

let req_list key json =
  match member_opt key json with
  | Some (`List l) -> l
  | Some _ -> err "field %S must be a list" key
  | None -> err "missing required field %S" key

(* A required list that the schema declares with [minItems:1] (e.g. a loop's
   [governors]): an empty array is a parse-level shape error, matching the
   schema. ([Lint]/[Validate] keep their richer [ungoverned-loop] diagnostic;
   this just ensures the parser does not ACCEPT what the schema rejects.) *)
let req_nonempty_list key json =
  match req_list key json with
  | [] -> err "field %S must be a non-empty list" key
  | l -> l

(* Closed-object discipline (mirrors [expr_of_json], which already rejects an
   object with more than one operator key). After reading the known keys of an
   object, reject any present key that is neither a known key nor a leading-
   underscore metadata key (the documented [_doc]/[_note] escape hatch). [what]
   names the object in the error message. [output_schema] is intentionally
   exempt: it is an open field->type map. *)
let reject_unknown_keys ~what ~known json =
  match json with
  | `Assoc fields ->
      List.iter
        (fun (k, _) ->
          if
            (not (List.mem k known))
            && not (String.length k > 0 && k.[0] = '_')
          then err "unknown key %S in %s" k what)
        fields
  | _ -> ()

(* ---- Schema ------------------------------------------------------------- *)

let ty_of_json = function
  | `String "string" -> Schema.String
  | `String "int" -> Schema.Int
  | `String "number" -> Schema.Number
  | `String "bool" -> Schema.Bool
  | `String "any" -> Schema.Any
  | `Assoc _ as j -> (
      match member_opt "enum" j with
      | Some (`List opts) ->
          Schema.Enum
            (List.map
               (function `String s -> s | _ -> err "enum members must be strings")
               opts)
      | _ -> err "object type must be {\"enum\": [..]}")
  | _ -> err "unknown schema type (want string|int|number|bool|any|{enum:[..]})"

(* output_schema JSON: {"field": <ty>, ...} where <ty> is "string"|... or
   {"enum":[...]}. *)
let schema_of_json json : Schema.t =
  match json with
  | `Assoc fields -> List.map (fun (k, v) -> (k, ty_of_json v)) fields
  | _ -> err "output_schema must be an object"

let opt_schema key json : Schema.t option =
  match member_opt key json with None -> None | Some j -> Some (schema_of_json j)

(* ---- Expr --------------------------------------------------------------- *)

(* A path string "outputs.a.severity" splits on '.'. *)
let split_path s = String.split_on_char '.' s

let value_of_lit (j : Yojson.Safe.t) : Expr.value = Expr.value_of_json j

let rec expr_of_json (j : Yojson.Safe.t) : Expr.t =
  match j with
  | `Assoc [ (key, v) ] -> expr_op key v
  | `Assoc _ -> err "expression object must have exactly one operator key"
  | _ -> err "expression must be a JSON object"

and expr_op key v : Expr.t =
  let two name =
    match v with
    | `List [ a; b ] -> (expr_of_json a, expr_of_json b)
    | _ -> err "%S takes a 2-element list" name
  in
  let many name =
    match v with
    | `List l -> List.map expr_of_json l
    | _ -> err "%S takes a list" name
  in
  match key with
  | "path" -> (
      match v with
      | `String s -> Expr.Path (split_path s)
      | _ -> err "\"path\" takes a dotted string")
  | "lit" -> Expr.Lit (value_of_lit v)
  | "exists" -> (
      match v with
      | `String s -> Expr.Exists (split_path s)
      | _ -> err "\"exists\" takes a dotted string")
  | "eq" -> let a, b = two "eq" in Expr.Eq (a, b)
  | "ne" -> let a, b = two "ne" in Expr.Ne (a, b)
  | "lt" -> let a, b = two "lt" in Expr.Lt (a, b)
  | "le" -> let a, b = two "le" in Expr.Le (a, b)
  | "gt" -> let a, b = two "gt" in Expr.Gt (a, b)
  | "ge" -> let a, b = two "ge" in Expr.Ge (a, b)
  | "in" -> let a, b = two "in" in Expr.In (a, b)
  | "and" -> Expr.And (many "and")
  | "or" -> Expr.Or (many "or")
  | "not" -> Expr.Not (expr_of_json v)
  | other -> err "unknown expression operator %S" other

let req_expr key json : Expr.t =
  match member_opt key json with
  | Some j -> expr_of_json j
  | None -> err "missing required field %S" key

let opt_expr key json : Expr.t option =
  match member_opt key json with None -> None | Some j -> Some (expr_of_json j)

(* ---- governors ---------------------------------------------------------- *)

let governor_of_json (j : Yojson.Safe.t) : governor =
  let kind = req_string "kind" j in
  match kind with
  | "max_iters" ->
      let g = Max_iters (req_bounded_int "n" j) in
      reject_unknown_keys ~what:"max_iters governor" ~known:[ "kind"; "n" ] j;
      g
  | "budget" ->
      reject_unknown_keys ~what:"budget governor" ~known:[ "kind" ] j;
      Budget
  | "fixpoint" ->
      let g =
        Fixpoint
          { window = req_bounded_int "window" j; progress = req_expr "progress" j }
      in
      reject_unknown_keys ~what:"fixpoint governor"
        ~known:[ "kind"; "window"; "progress" ] j;
      g
  | other -> err "unknown governor kind %S" other

(* ---- steps -------------------------------------------------------------- *)

let rec step_of_json json =
  let kind = req_string "kind" json in
  match kind with
  | "agent" ->
      let s =
        Agent
          {
            id = req_string "id" json;
            prompt = req_string "prompt" json;
            read_only = opt_bool "read_only" false json;
            output_schema = opt_schema "output_schema" json;
          }
      in
      reject_unknown_keys ~what:"agent step"
        ~known:[ "kind"; "id"; "prompt"; "read_only"; "output_schema" ] json;
      s
  | "gate" ->
      let s = Gate { id = req_string "id" json; when_ = req_expr "when" json } in
      reject_unknown_keys ~what:"gate step" ~known:[ "kind"; "id"; "when" ] json;
      s
  | "branch" ->
      let s =
        Branch
          {
            when_ = req_expr "when" json;
            then_ = List.map step_of_json (req_list "then" json);
            else_ = List.map step_of_json (req_list "else" json);
          }
      in
      reject_unknown_keys ~what:"branch step"
        ~known:[ "kind"; "when"; "then"; "else" ] json;
      s
  | "loop" ->
      let s =
        Loop
          {
            body = List.map step_of_json (req_list "body" json);
            until = opt_expr "until" json;
            governors =
              List.map governor_of_json (req_nonempty_list "governors" json);
          }
      in
      reject_unknown_keys ~what:"loop step"
        ~known:[ "kind"; "body"; "until"; "governors" ] json;
      s
  | "commit" ->
      let s = Commit { id = req_string "id" json } in
      reject_unknown_keys ~what:"commit step" ~known:[ "kind"; "id" ] json;
      s
  | other -> err "unknown step kind %S" other

let of_json json =
  try
    let name = req_string "name" json in
    let steps = List.map step_of_json (req_list "steps" json) in
    reject_unknown_keys ~what:"workflow" ~known:[ "name"; "steps" ] json;
    Ok { name; steps }
  with Parse_error msg -> Error msg

let of_string s =
  match Yojson.Safe.from_string s with
  | json -> of_json json
  | exception Yojson.Json_error msg -> Error ("malformed JSON: " ^ msg)

let of_file path =
  match Yojson.Safe.from_file path with
  | json -> of_json json
  | exception Yojson.Json_error msg -> Error ("malformed JSON: " ^ msg)
  | exception Sys_error msg -> Error ("cannot read file: " ^ msg)

(* ---- serialisation ------------------------------------------------------ *)

let ty_to_json = function
  | Schema.String -> `String "string"
  | Schema.Int -> `String "int"
  | Schema.Number -> `String "number"
  | Schema.Bool -> `String "bool"
  | Schema.Any -> `String "any"
  | Schema.Enum opts -> `Assoc [ ("enum", `List (List.map (fun s -> `String s) opts)) ]

let schema_to_json (s : Schema.t) : Yojson.Safe.t =
  `Assoc (List.map (fun (k, ty) -> (k, ty_to_json ty)) s)

let path_to_string p = String.concat "." p

let rec expr_to_json (e : Expr.t) : Yojson.Safe.t =
  let two k a b = `Assoc [ (k, `List [ expr_to_json a; expr_to_json b ]) ] in
  let many k es = `Assoc [ (k, `List (List.map expr_to_json es)) ] in
  match e with
  | Expr.Path p -> `Assoc [ ("path", `String (path_to_string p)) ]
  | Expr.Lit v -> `Assoc [ ("lit", Expr.json_of_value v) ]
  | Expr.Exists p -> `Assoc [ ("exists", `String (path_to_string p)) ]
  | Expr.Eq (a, b) -> two "eq" a b
  | Expr.Ne (a, b) -> two "ne" a b
  | Expr.Lt (a, b) -> two "lt" a b
  | Expr.Le (a, b) -> two "le" a b
  | Expr.Gt (a, b) -> two "gt" a b
  | Expr.Ge (a, b) -> two "ge" a b
  | Expr.In (a, b) -> two "in" a b
  | Expr.And es -> many "and" es
  | Expr.Or es -> many "or" es
  | Expr.Not e -> `Assoc [ ("not", expr_to_json e) ]

let governor_to_json = function
  | Max_iters n -> `Assoc [ ("kind", `String "max_iters"); ("n", `Int n) ]
  | Budget -> `Assoc [ ("kind", `String "budget") ]
  | Fixpoint { window; progress } ->
      `Assoc
        [
          ("kind", `String "fixpoint");
          ("window", `Int window);
          ("progress", expr_to_json progress);
        ]

let rec step_to_json = function
  | Agent { id; prompt; read_only; output_schema } ->
      `Assoc
        ([
           ("kind", `String "agent");
           ("id", `String id);
           ("prompt", `String prompt);
           ("read_only", `Bool read_only);
         ]
        @
        match output_schema with
        | None -> []
        | Some s -> [ ("output_schema", schema_to_json s) ])
  | Gate { id; when_ } ->
      `Assoc
        [ ("kind", `String "gate"); ("id", `String id); ("when", expr_to_json when_) ]
  | Branch { when_; then_; else_ } ->
      `Assoc
        [
          ("kind", `String "branch");
          ("when", expr_to_json when_);
          ("then", `List (List.map step_to_json then_));
          ("else", `List (List.map step_to_json else_));
        ]
  | Loop { body; until; governors } ->
      `Assoc
        ([ ("kind", `String "loop") ]
        @ (match until with None -> [] | Some e -> [ ("until", expr_to_json e) ])
        @ [
            ("governors", `List (List.map governor_to_json governors));
            ("body", `List (List.map step_to_json body));
          ])
  | Commit { id } -> `Assoc [ ("kind", `String "commit"); ("id", `String id) ]

let to_json { name; steps } =
  `Assoc
    [ ("name", `String name); ("steps", `List (List.map step_to_json steps)) ]
