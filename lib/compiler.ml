(* One-way CWR → Claude Workflow JS compiler.

   Compiles a CWR workflow to a Claude Workflow JavaScript snippet.
   This is a pure emitter: it produces a JS text approximation of the
   CWR workflow, with inline comments noting anything that cannot be
   faithfully represented in Claude Workflow JS.

   Direction: CWR → JS only. No JS parser. *)

open Types

(* Indentation helpers *)
let indent_str n = String.make (n * 2) ' '

(* Collect compilation notes for the stderr summary. Each note is a
   (kind, description) pair. *)
type note = { kind : string; description : string }

(* The compilation context threaded through the recursive emitter. *)
type ctx = {
  buf : Buffer.t;
  notes : note list ref;
  indent : int;
  loop_counter : int ref;
}

let emit ctx s =
  Buffer.add_string ctx.buf (indent_str ctx.indent);
  Buffer.add_string ctx.buf s;
  Buffer.add_char ctx.buf '\n'

let emit_comment ctx s =
  Buffer.add_string ctx.buf (indent_str ctx.indent);
  Buffer.add_string ctx.buf "// ";
  Buffer.add_string ctx.buf s;
  Buffer.add_char ctx.buf '\n'

let add_note ctx ~kind ~description =
  ctx.notes := { kind; description } :: !(ctx.notes)

let indent ctx = { ctx with indent = ctx.indent + 1 }

let cmd_to_string cmd =
  String.concat " " (List.map (fun s ->
    if String.contains s ' ' then Printf.sprintf "%S" s else s) cmd)

(* ------------------------------------------------------------------ *)
(* JS string escaping                                                   *)
(* ------------------------------------------------------------------ *)

let js_escape_string s =
  let buf = Buffer.create (String.length s + 4) in
  String.iter (fun c ->
    match c with
    | '"'    -> Buffer.add_string buf "\\\""
    | '\\'   -> Buffer.add_string buf "\\\\"
    | '\n'   -> Buffer.add_string buf "\\n"
    | '\r'   -> Buffer.add_string buf "\\r"
    | '\t'   -> Buffer.add_string buf "\\t"
    | '\b'   -> Buffer.add_string buf "\\b"
    | '\x0C' -> Buffer.add_string buf "\\f"
    | c when Char.code c < 0x20 ->
        Buffer.add_string buf (Printf.sprintf "\\u%04X" (Char.code c))
    | c      -> Buffer.add_char buf c) s;
  Buffer.contents buf

let js_escape_single_quoted s =
  let buf = Buffer.create (String.length s + 4) in
  String.iter (fun c ->
    match c with
    | '\'' -> Buffer.add_string buf "\\'"
    | '\\' -> Buffer.add_string buf "\\\\"
    | '\n' -> Buffer.add_string buf "\\n"
    | '\r' -> Buffer.add_string buf "\\r"
    | c    -> Buffer.add_char buf c) s;
  Buffer.contents buf

(* ------------------------------------------------------------------ *)
(* Expr.t → JS expression translator                                    *)
(* ------------------------------------------------------------------ *)

(* Sanitize an arbitrary string to a valid JS identifier:
   replace any character outside [a-zA-Z0-9_] with '_', then prefix '_' if
   the first character is a digit. *)
let js_ident s =
  if s = "" then "_"
  else
    let sanitize c =
      if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
         (c >= '0' && c <= '9') || c = '_' then c else '_'
    in
    let s = String.map sanitize s in
    if s.[0] >= '0' && s.[0] <= '9' then "_" ^ s else s

let js_path (path : string list) : string =
  match path with
  | [] -> "false"
  | "outputs" :: id :: rest ->
      js_ident id ^ String.concat "" (List.map (fun s -> "." ^ js_ident s) rest)
  | k :: rest ->
      "args." ^ js_ident k ^ String.concat "" (List.map (fun s -> "." ^ js_ident s) rest)

let rec expr_to_js (e : Expr.t) : string =
  match e with
  | Lit (Bool b) -> if b then "true" else "false"
  | Lit (Int n) -> string_of_int n
  | Lit (Float f) -> Printf.sprintf "%g" f
  | Lit Null -> "null"
  | Lit (String s) -> "\"" ^ js_escape_string s ^ "\""
  | Lit (List vs) ->
      "[" ^ String.concat ", " (List.map (fun v -> expr_to_js (Lit v)) vs) ^ "]"
  | Path p -> js_path p
  | Exists p ->
      let ref_js = js_path p in
      Printf.sprintf "(%s !== null && %s !== undefined)" ref_js ref_js
  | Not e -> Printf.sprintf "!(%s)" (expr_to_js e)
  | And [] -> "true"
  | And es -> "(" ^ String.concat " && " (List.map expr_to_js es) ^ ")"
  | Or [] -> "false"
  | Or es -> "(" ^ String.concat " || " (List.map expr_to_js es) ^ ")"
  | Eq (a, b) -> Printf.sprintf "(%s === %s)" (expr_to_js a) (expr_to_js b)
  | Ne (a, b) -> Printf.sprintf "(%s !== %s)" (expr_to_js a) (expr_to_js b)
  | Lt (a, b) -> Printf.sprintf "(%s < %s)"   (expr_to_js a) (expr_to_js b)
  | Le (a, b) -> Printf.sprintf "(%s <= %s)"  (expr_to_js a) (expr_to_js b)
  | Gt (a, b) -> Printf.sprintf "(%s > %s)"   (expr_to_js a) (expr_to_js b)
  | Ge (a, b) -> Printf.sprintf "(%s >= %s)"  (expr_to_js a) (expr_to_js b)
  | In (a, b) -> Printf.sprintf "(%s).includes(%s)" (expr_to_js b) (expr_to_js a)

(* ------------------------------------------------------------------ *)
(* Schema.t → JSON Schema inline object                                 *)
(* ------------------------------------------------------------------ *)

let schema_ty_to_js (ty : Schema.ty) : string =
  match ty with
  | Schema.String -> {|{type: "string"}|}
  | Schema.Int    -> {|{type: "integer"}|}
  | Schema.Number -> {|{type: "number"}|}
  | Schema.Bool   -> {|{type: "boolean"}|}
  | Schema.Any    -> "{}"
  | Schema.Enum opts ->
      let quoted = List.map (fun s -> "\"" ^ js_escape_string s ^ "\"") opts in
      Printf.sprintf {|{type: "string", enum: [%s]}|} (String.concat ", " quoted)

let schema_to_js (schema : Schema.t) : string =
  let props = String.concat ", " (List.map (fun (k, ty) ->
    Printf.sprintf "\"%s\": %s" (js_escape_string k) (schema_ty_to_js ty)) schema) in
  let required = String.concat ", " (List.map (fun (k, _) ->
    "\"" ^ js_escape_string k ^ "\"") schema) in
  Printf.sprintf {|{type: "object", properties: {%s}, required: [%s]}|} props required

(* ------------------------------------------------------------------ *)
(* Step compiler                                                        *)
(* ------------------------------------------------------------------ *)

let rec compile_steps ctx steps =
  List.iter (compile_step ctx) steps

and compile_step ctx step =
  match step with
  | Types.Agent { id; prompt; read_only; output_schema; on_failure } ->
      if read_only then emit_comment ctx "[read-only]";
      let var = js_ident id in
      let schema_opt = match output_schema with
        | None -> ""
        | Some s -> Printf.sprintf ", schema: %s" (schema_to_js s)
      in
      let agent_call =
        Printf.sprintf "await agent(\"%s\", {label: \"%s\"%s});"
          (js_escape_string prompt) (js_escape_string id) schema_opt
      in
      (match on_failure with
      | Types.Abort ->
          emit ctx (Printf.sprintf "const %s = %s" var agent_call)
      | Types.Continue ->
          emit ctx (Printf.sprintf "let %s;" var);
          emit ctx (Printf.sprintf
            "try { %s = %s } catch (e) { %s = null; /* soft fail */ }"
            var agent_call var))
  | Types.Gate { id; when_ } ->
      emit ctx (Printf.sprintf
        "if (!(%s)) { throw new Error(\"gate %s failed\"); }"
        (expr_to_js when_) (js_escape_string id))
  | Types.Commit { id } ->
      emit_comment ctx "[CWR commit — token approval mechanism not preserved]";
      emit ctx (Printf.sprintf
        "await agent(\"request human approval\", {label: \"commit_%s\"});"
        (js_escape_string id));
      add_note ctx ~kind:"commit"
        ~description:(Printf.sprintf
          "commit %S token approval not preserved in JS output" id)
  | Types.Branch { when_; then_; else_ } ->
      emit ctx (Printf.sprintf "if (%s) {" (expr_to_js when_));
      compile_steps (indent ctx) then_;
      emit ctx "} else {";
      compile_steps (indent ctx) else_;
      emit ctx "}"
  | Types.Loop { body; until; governors } ->
      let k = !(ctx.loop_counter) in
      incr ctx.loop_counter;
      List.iter (fun gov -> match gov with
        | Types.Max_iters _ ->
            emit ctx (Printf.sprintf "let _maxiters_%d = 0;" k)
        | Types.Fixpoint _ ->
            emit ctx (Printf.sprintf "let _fixcount_%d = 0;" k)
        | Types.Budget -> ()) governors;
      emit ctx "while (true) {";
      let bctx = indent ctx in
      compile_steps bctx body;
      (match until with
      | None -> ()
      | Some e ->
          emit bctx (Printf.sprintf "if (%s) break;" (expr_to_js e)));
      List.iter (fun gov -> match gov with
        | Types.Max_iters n ->
            emit bctx (Printf.sprintf "if (++_maxiters_%d >= %d) break;" k n)
        | Types.Budget ->
            emit bctx "if (budget.remaining() <= 0) break;"
        | Types.Fixpoint { window; progress } ->
            emit bctx (Printf.sprintf
              "if (!(%s)) { if (++_fixcount_%d >= %d) break; } else { _fixcount_%d = 0; }"
              (expr_to_js progress) k window k)) governors;
      emit ctx "}";
      if governors = [] then
        add_note ctx ~kind:"loop"
          ~description:"no JS-level termination: engine ceiling not emitted"
  | Types.Run { id; cmd; working_dir; timeout_ms = _; observe = _ } ->
      let cmd_str = cmd_to_string cmd in
      emit_comment ctx (Printf.sprintf
        "[CWR run: cmd=%S working_dir=%S — replay safety and allowlist not preserved]"
        cmd_str working_dir);
      emit ctx (Printf.sprintf
        "await agent(\"%s\", {label: \"%s\"});"
        (js_escape_string (Printf.sprintf "run: %s" cmd_str)) (js_escape_string id));
      add_note ctx ~kind:"run"
        ~description:(Printf.sprintf
          "run cmd=%S in working_dir=%S; allowlist and replay safety not preserved"
          cmd_str working_dir)
  | Types.Parallel { branches } ->
      let n = List.length branches in
      emit ctx (Printf.sprintf "await parallel([  // %d branches" n);
      List.iteri (fun i branch ->
        let sep = if i < n - 1 then "," else "" in
        emit ctx "  async () => {";
        compile_steps { ctx with indent = ctx.indent + 2 } branch;
        emit ctx (Printf.sprintf "  }%s" sep)
      ) branches;
      emit ctx "]);"
  | Types.Foreach { over; steps = body } ->
      emit_comment ctx (Printf.sprintf
        "[CWR foreach: over=%S — static ctx reference]" over);
      emit ctx (Printf.sprintf "await pipeline(%s, async (item) => {" over);
      compile_steps (indent ctx) body;
      emit ctx "});";
      add_note ctx ~kind:"foreach"
        ~description:(Printf.sprintf
          "foreach over ctx key %S compiled to pipeline(); static ctx reference" over)

(* Compile a validated CWR workflow to Claude Workflow JS text.
   Returns (js_text, notes) where notes is a list of compilation notes
   for display on stderr. *)
let compile_workflow (wf : Types.workflow) : string * note list =
  let buf = Buffer.create 512 in
  let notes = ref [] in
  let version_str = match wf.version with
    | Some v -> Printf.sprintf "v%s" v
    | None -> "(unversioned)"
  in
  let name_safe =
    String.concat "\\n" (String.split_on_char '\n'
      (String.concat "\\r" (String.split_on_char '\r' wf.name)))
  in
  Buffer.add_string buf (Printf.sprintf "// Compiled from CWR %s\n" version_str);
  Buffer.add_string buf (Printf.sprintf "// Workflow: %s\n\n" name_safe);
  Buffer.add_string buf
    (Printf.sprintf "export const meta = { name: '%s', description: '' };\n\n"
       (js_escape_single_quoted wf.name));
  let ctx = { buf; notes; indent = 0; loop_counter = ref 0 } in
  compile_steps ctx wf.steps;
  (Buffer.contents buf, List.rev !notes)
