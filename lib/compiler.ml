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

let rec compile_steps ctx steps =
  List.iter (compile_step ctx) steps

and compile_step ctx step =
  match step with
  | Types.Agent { id; prompt; read_only = _; output_schema = _; on_failure = _ } ->
      emit ctx (Printf.sprintf "const %s = await agent(%S, {label: %S});" id prompt id)
  | Types.Gate { id; when_ = _ } ->
      emit_comment ctx (Printf.sprintf "[CWR gate: %s — no direct Claude Workflow equivalent]" id);
      emit ctx (Printf.sprintf "await agent(%S, {label: %S});" (Printf.sprintf "evaluate gate %s" id) id);
      add_note ctx ~kind:"gate"
        ~description:(Printf.sprintf "gate %S has no direct Claude Workflow equivalent; approximated as agent call" id)
  | Types.Commit { id } ->
      emit_comment ctx "[CWR commit — token approval mechanism not preserved]";
      emit ctx (Printf.sprintf "await agent(\"request human approval\", {label: \"commit_%s\"});" id);
      add_note ctx ~kind:"commit"
        ~description:(Printf.sprintf "commit %S token approval not preserved in JS output" id)
  | Types.Branch { when_ = _; then_; else_ } ->
      (* Approximate: use a synthetic agent call for the condition *)
      emit ctx "if (await agent(\"evaluate branch condition\", {label: \"branch\"})) {";
      compile_steps (indent ctx) then_;
      emit ctx "} else {";
      compile_steps (indent ctx) else_;
      emit ctx "}";
      add_note ctx ~kind:"branch"
        ~description:"branch condition is a CWR expr; approximated as agent call in JS"
  | Types.Loop { body; until = _; governors } ->
      let gov_note = match governors with
        | [] -> "no governors"
        | govs -> Printf.sprintf "%d governor(s)" (List.length govs)
      in
      emit_comment ctx (Printf.sprintf "[CWR loop: %s — loop termination not preserved]" gov_note);
      emit ctx "while (true) {";
      compile_steps (indent ctx) body;
      (indent ctx) |> (fun ictx -> emit_comment ictx "TODO: add governor check break here");
      emit ctx "}";
      add_note ctx ~kind:"loop"
        ~description:(Printf.sprintf "loop with %s; governor termination not represented in JS" gov_note)
  | Types.Run { id; cmd; working_dir; timeout_ms = _; observe = _ } ->
      let cmd_str = cmd_to_string cmd in
      emit_comment ctx (Printf.sprintf "[CWR run: cmd=%S working_dir=%S — replay safety and allowlist not preserved]" cmd_str working_dir);
      emit ctx (Printf.sprintf "await agent(%S, {label: %S});" (Printf.sprintf "run: %s" cmd_str) id);
      add_note ctx ~kind:"run"
        ~description:(Printf.sprintf "run cmd=%S in working_dir=%S; allowlist and replay safety not preserved" cmd_str working_dir)
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
      emit_comment ctx (Printf.sprintf "[CWR foreach: over=%S — static ctx reference]" over);
      emit ctx (Printf.sprintf "await pipeline(%s, async (item) => {" over);
      compile_steps (indent ctx) body;
      emit ctx "});";
      add_note ctx ~kind:"foreach"
        ~description:(Printf.sprintf "foreach over ctx key %S compiled to pipeline(); static ctx reference" over)

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
  Buffer.add_string buf (Printf.sprintf "// Compiled from CWR %s\n" version_str);
  Buffer.add_string buf (Printf.sprintf "// Workflow: %s\n\n" wf.name);
  let ctx = { buf; notes; indent = 0 } in
  compile_steps ctx wf.steps;
  (Buffer.contents buf, List.rev !notes)
