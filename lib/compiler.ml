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
  read_file : string -> string option;
      (** Read a file by path; returns [None] if the file can't be opened. Used
          to inline [protocol]/[brief] file contents into agent() calls. The
          default (set by [compile_workflow]) reads from the filesystem. *)
}

let emit ctx s =
  Buffer.add_string ctx.buf (indent_str ctx.indent);
  Buffer.add_string ctx.buf s;
  Buffer.add_char ctx.buf '\n'

(* Make a string safe to embed in a single-line JS [//] comment: replace every
   character that terminates a line comment with a visible literal placeholder,
   so the comment can never be split and have its tail become executable code.
   A JS LineTerminator is LF, CR, and the Unicode separators U+2028 / U+2029
   (UTF-8 [E2 80 A8] / [E2 80 A9]). Escapes are NOT interpreted inside a comment,
   so we emit the placeholder text (e.g. backslash-n), not a real escape. *)
let js_comment_safe s =
  let buf = Buffer.create (String.length s + 4) in
  let n = String.length s in
  let i = ref 0 in
  while !i < n do
    let c = s.[!i] in
    if c = '\n' then (Buffer.add_string buf "\\n"; incr i)
    else if c = '\r' then (Buffer.add_string buf "\\r"; incr i)
    else if !i + 2 < n && c = '\xE2' && s.[!i + 1] = '\x80'
            && (s.[!i + 2] = '\xA8' || s.[!i + 2] = '\xA9') then begin
      Buffer.add_string buf (if s.[!i + 2] = '\xA8' then "\\u2028" else "\\u2029");
      i := !i + 3
    end
    else (Buffer.add_char buf c; incr i)
  done;
  Buffer.contents buf

(* All comment emission routes through here, so [js_comment_safe] is applied
   centrally: no parser-accepted workflow string interpolated into a comment can
   split it and leak its tail as executable JS (the silent-invalid-output class). *)
let emit_comment ctx s =
  Buffer.add_string ctx.buf (indent_str ctx.indent);
  Buffer.add_string ctx.buf "// ";
  Buffer.add_string ctx.buf (js_comment_safe s);
  Buffer.add_char ctx.buf '\n'

let add_note ctx ~kind ~description =
  ctx.notes := { kind; description } :: !(ctx.notes)

let indent ctx = { ctx with indent = ctx.indent + 1 }

let read_file_default path =
  try Some (In_channel.with_open_text path In_channel.input_all)
  with Sys_error _ -> None

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
  | Types.Agent { id; prompt; read_only; output_schema; on_failure; protocol; brief; agent_type } ->
      if read_only then emit_comment ctx "[read-only]";
      let var = js_ident id in
      let schema_opt = match output_schema with
        | None -> ""
        | Some s -> Printf.sprintf ", schema: %s" (schema_to_js s)
      in
      let agent_type_opt = match agent_type with
        | None -> ""
        | Some at -> Printf.sprintf ", agentType: \"%s\"" (js_escape_string at)
      in
      let inline_file label path_opt =
        match path_opt with
        | None -> None
        | Some p ->
            match ctx.read_file p with
            | Some content -> Some content
            | None ->
                add_note ctx ~kind:"agent"
                  ~description:(Printf.sprintf
                    "agent %S: %s file %S could not be read at compile time; \
                     content omitted from JS output" id label p);
                None
      in
      let effective_prompt =
        let parts = List.filter_map (fun x -> x)
          [ inline_file "protocol" protocol;
            inline_file "brief" brief;
            Some prompt ]
        in
        String.concat "\n\n" parts
      in
      let agent_call =
        Printf.sprintf "await agent(\"%s\", {label: \"%s\"%s%s});"
          (js_escape_string effective_prompt) (js_escape_string id)
          schema_opt agent_type_opt
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
      (* [over] is a ctx key referencing a prior step's output, which is bound as
         [const <js_ident id>]; sanitize it the same way so the emitted JS is both
         valid and consistent with the variable naming, never raw user text. *)
      emit ctx (Printf.sprintf "await pipeline(%s, async (item) => {" (js_ident over));
      compile_steps (indent ctx) body;
      emit ctx "});";
      add_note ctx ~kind:"foreach"
        ~description:(Printf.sprintf
          "foreach over ctx key %S compiled to pipeline(); static ctx reference" over)
  | Types.Shell { id; commands; on_failure = _ } ->
      emit_comment ctx (Printf.sprintf
        "[CWR shell: id=%S (%d command(s)) — not representable in Claude Workflow JS]" id
        (List.length commands));
      List.iter (fun cmd ->
        emit_comment ctx (Printf.sprintf "  %s" cmd)) commands;
      add_note ctx ~kind:"shell"
        ~description:(Printf.sprintf
          "shell step %S (%d command(s)) omitted from JS output; \
           not representable in Claude Workflow JS" id (List.length commands))
  | Types.Evidence { id; tier; build; check; zero_admits; output } ->
      emit_comment ctx (Printf.sprintf
        "[CWR evidence: id=%S tier=%s — formal verification, not representable in Claude Workflow JS]"
        id tier);
      emit_comment ctx (Printf.sprintf "  build: %s" build);
      emit_comment ctx (Printf.sprintf "  check: %s" check);
      emit_comment ctx (Printf.sprintf "  zero_admits: %s in %s" zero_admits output);
      add_note ctx ~kind:"evidence"
        ~description:(Printf.sprintf
          "evidence step %S (tier %s) omitted from JS output; \
           formal verification not representable in Claude Workflow JS" id tier)

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
  let name_safe = js_comment_safe wf.name in
  Buffer.add_string buf
    (Printf.sprintf "// Compiled from CWR %s\n" (js_comment_safe version_str));
  Buffer.add_string buf (Printf.sprintf "// Workflow: %s\n\n" name_safe);
  Buffer.add_string buf
    (Printf.sprintf "export const meta = { name: '%s', description: '' };\n\n"
       (js_escape_single_quoted wf.name));
  let ctx = { buf; notes; indent = 0; loop_counter = ref 0; read_file = read_file_default } in
  compile_steps ctx wf.steps;
  (Buffer.contents buf, List.rev !notes)
