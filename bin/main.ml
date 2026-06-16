open Cabal_workflow_runner

let load_and_validate ~floor_gates file =
  match Workflow_json.of_file file with
  | Error e -> Error (Printf.sprintf "parse error: %s" e)
  | Ok wf -> (
      match Validate.workflow ~floor_gates wf with
      | Error e -> Error (Printf.sprintf "validation rejected workflow: %s" e)
      | Ok v -> Ok v)

let print_trace trace =
  List.iter
    (fun entry ->
      match entry with
      | Types.Agent_ran { id; success; output } ->
          Printf.printf "  agent    %-16s success=%b output=%s\n" id success
            (Yojson.Safe.to_string output)
      | Types.Gate_evaluated { id; verdict } ->
          Printf.printf "  gate     %-16s %s\n" id
            (Types.verdict_to_string verdict)
      | Types.Branch_taken { verdict } ->
          Printf.printf "  branch   -> %s\n" (Types.verdict_to_string verdict)
      | Types.Loop_iter { index } -> Printf.printf "  loop     iter=%d\n" index
      | Types.Budget_read { value } ->
          Printf.printf "  budget   value=%d\n" value
      | Types.Fixpoint_progress { progress } ->
          Printf.printf "  fixpoint progress=%b\n" progress
      | Types.Loop_stopped { iterations; reason } ->
          Printf.printf "  loop     stopped after %d iter(s) (%s)\n" iterations
            reason
      | Types.Run_executed { id; result } ->
          Printf.printf
            "  run      %-16s exit=%d truncated=%b files=%d\n" id
            result.Types.exit result.Types.truncated
            (List.length result.Types.files)
      | Types.Committed_step { id; token_digest } ->
          Printf.printf "  commit   %-16s token_digest=%s\n" id token_digest
      | Types.Blocked_at { id; reason } ->
          Printf.printf "  block    %-16s %s\n" id reason
      | Types.Parallel_started ->
          Printf.printf "  parallel started\n"
      | Types.Parallel_branch_completed { branch_idx; outcome; trace = _; branch_outputs = _ } ->
          Printf.printf "  parallel branch[%d] %s\n" branch_idx
            (Types.string_of_outcome outcome)
      | Types.Parallel_completed { outcome } ->
          Printf.printf "  parallel completed %s\n"
            (Types.string_of_outcome outcome)
      | Types.Foreach_iter_started { index; element } ->
          Printf.printf "  foreach  iter=%d element=%s\n" index
            (Yojson.Safe.to_string element)
      | Types.Foreach_iter_completed { index; outcome } ->
          Printf.printf "  foreach  iter=%d %s\n" index
            (Types.string_of_outcome outcome)
      | Types.Foreach_completed { iterations } ->
          Printf.printf "  foreach  completed %d iter(s)\n" iterations
      | Types.Shell_executed { id; results } ->
          Printf.printf "  shell    %-16s %d command(s)\n" id (List.length results)
      | Types.Evidence_evaluated { id; tier; passed } ->
          Printf.printf "  evidence %-16s tier=%s passed=%b\n" id tier passed
      | Types.Ctx_snapshot _ ->
          (* ledger-layer header, never appears in an engine trace *) ())
    trace

(* ---- validate subcommand ---- *)

let cmd_validate file floor_gates =
  match load_and_validate ~floor_gates file with
  | Error e ->
      Printf.eprintf "INVALID: %s\n" e;
      1
  | Ok _ ->
      Printf.printf "VALID: %s passes the safety floor (floor_gates=[%s])\n"
        file (String.concat "; " floor_gates);
      0

(* ---- lint subcommand ---- *)

let severity_str = function
  | Lint.Error -> "error"
  | Lint.Warning -> "warning"

let print_lint_table (ds : Lint.diagnostic list) =
  if ds = [] then print_endline "no diagnostics"
  else
    List.iter
      (fun (d : Lint.diagnostic) ->
        Printf.printf "  %-7s %-26s %-22s %s\n" (severity_str d.severity) d.code
          d.loc d.message)
      ds

let cmd_lint file floor_gates json =
  let raw =
    try Ok (In_channel.with_open_bin file In_channel.input_all)
    with Sys_error msg -> Error msg
  in
  match raw with
  | Error msg ->
      Printf.eprintf "cannot read file: %s\n" msg;
      1
  | Ok raw ->
      let ds = Lint.check_json ~floor_gates raw in
      if json then print_endline (Yojson.Safe.to_string (Lint.to_json ds))
      else print_lint_table ds;
      if Lint.has_errors ds then 1 else 0

(* ---- schema subcommand ---- *)

let cmd_schema () =
  print_string (Workflow_schema.to_string ());
  0

(* ---- run subcommand ---- *)

let cmd_run file floor_gates approve allow_run ledger ctx_json =
  match load_and_validate ~floor_gates file with
  | Error e ->
      Printf.eprintf "%s\n" e;
      1
  | Ok validated ->
      Eio_main.run (fun env ->
          Eio.Switch.run (fun sw ->
              let cwd = Sys.getcwd () in
              let backend = Backend_cabal.make ~sw ~env ~working_dir:cwd in
              let initial_ctx = match ctx_json with
                | None -> []
                | Some raw ->
                    (match Yojson.Safe.from_string raw with
                     | `Assoc fields -> fields
                     | _ ->
                         Printf.eprintf "--ctx must be a JSON object\n";
                         exit 1
                     | exception Yojson.Json_error msg ->
                         Printf.eprintf "--ctx parse error: %s\n" msg;
                         exit 1)
              in
              let outcome, trace =
                Engine.run ~sw ~run_allowlist:allow_run ~backend ~token:approve
                  ~initial_ctx validated
              in
              Printf.printf "outcome: %s\ntrace:\n"
                (Types.string_of_outcome outcome);
              print_trace trace;
              (* Persist the run's trace as an on-disk ledger so it can be
                 replayed byte-identically in a later process. Written after a
                 successful walk only (a Blocked/Aborted run exits 2 below). *)
              (match (ledger, outcome) with
              | Some path, (Types.Committed _ | Types.Completed_no_commit) -> (
                  (* Fail gracefully on an unwritable path (matches the read
                     path), rather than an uncaught Sys_error after the run's
                     effects already happened. *)
                  try
                    Out_channel.with_open_bin path (fun oc ->
                        let header = Ledger.entry_to_json
                          (Types.Ctx_snapshot { ctx = initial_ctx }) in
                        Out_channel.output_string oc
                          (Yojson.Safe.to_string header ^ "\n");
                        Out_channel.output_string oc (Ledger.to_ndjson trace));
                    Printf.printf "ledger written: %s\n" path
                  with Sys_error msg ->
                    Printf.eprintf "could not write ledger: %s\n" msg)
              | _ -> ());
              match outcome with
              | Types.Committed _ | Types.Completed_no_commit -> ()
              | Types.Blocked _ | Types.Aborted _ -> exit 2));
      0

(* ---- replay subcommand ---- *)

let cmd_replay file floor_gates ledger =
  match load_and_validate ~floor_gates file with
  | Error e ->
      Printf.eprintf "%s\n" e;
      1
  | Ok validated -> (
      let raw =
        try Ok (In_channel.with_open_bin ledger In_channel.input_all)
        with Sys_error msg -> Error msg
      in
      match raw with
      | Error msg ->
          Printf.eprintf "cannot read ledger: %s\n" msg;
          1
      | Ok contents -> (
          (* Split into lines; the FIRST non-empty line may be a Ctx_snapshot
             header (written by cmd_run since v0.11). Strip it and recover
             initial_ctx. Legacy ledgers without the header fall through with
             empty initial_ctx and all lines fed to of_ndjson. *)
          let lines = String.split_on_char '\n' contents
                      |> List.filter (fun s -> String.trim s <> "") in
          let initial_ctx, trace_lines =
            match lines with
            | first :: rest -> (
                match Ledger.entry_of_json (Yojson.Safe.from_string first) with
                | Types.Ctx_snapshot { ctx } -> (ctx, rest)
                | _ -> ([], lines)
                | exception _ -> ([], lines))
            | [] -> ([], [])
          in
          let trace_str = String.concat "\n" trace_lines in
          match Ledger.of_ndjson trace_str with
          | Error e ->
              Printf.eprintf "corrupt ledger: %s\n" e;
              1
          | Ok trace ->
              (* Re-feed the recorded trace; NO backend is consulted and no
                 command is ever dispatched/executed (same as in-memory replay).
                 A workflow/ledger mismatch surfaces as Replay_mismatch. *)
              let result = ref 0 in
              Eio_main.run (fun _env ->
                Eio.Switch.run (fun sw ->
                  match Engine.replay ~sw ~trace ~initial_ctx validated with
                  | outcome ->
                      Printf.printf "replayed outcome: %s\ntrace:\n"
                        (Types.string_of_outcome outcome);
                      print_trace trace
                  | exception Engine.Replay_mismatch reason ->
                      Printf.eprintf "replay mismatch: %s\n" reason;
                      result := 2));
              !result))

(* ---- to-claude-workflow subcommand ---- *)

let cmd_to_claude_workflow file =
  match Workflow_json.of_file file with
  | Error e ->
      Printf.eprintf "parse error: %s\n" e;
      1
  | Ok wf ->
      match Compiler.compile_workflow wf with
      | exception Compiler.Compile_error msg ->
          Printf.eprintf "compile error: %s\n" msg;
          1
      | js, notes ->
          print_string js;
          if notes <> [] then begin
            Printf.eprintf "\nCompilation notes (%d):\n" (List.length notes);
            List.iter (fun (n : Compiler.note) ->
              Printf.eprintf "  [%s] %s\n" n.kind n.description
            ) notes
          end;
          0

(* ---- cmdliner wiring ---- *)

open Cmdliner

let file_arg =
  Arg.(
    required
    & pos 0 (some string) None
    & info [] ~docv:"WORKFLOW.json" ~doc:"Workflow definition (JSON).")

let floor_arg =
  Arg.(
    value
    & opt_all string []
    & info [ "floor" ]
        ~docv:"GATE_ID"
        ~doc:
          "A floor gate id that every commit must be guaranteed-preceded by on \
           every path. Repeatable. Supplied by the embedder.")

let approve_arg =
  Arg.(
    value
    & opt (some string) None
    & info [ "approve" ] ~docv:"TOKEN"
        ~doc:
          "Runtime human-approval token required to execute a Commit. Hashed \
           for the trace; never stored raw. Absent => commit is Blocked.")

let json_arg =
  Arg.(
    value & flag
    & info [ "json" ]
        ~doc:"Print diagnostics as JSON ({\"diagnostics\":[..]}) instead of a table.")

let lint_cmd =
  let doc =
    "Lint a workflow file (parse-tolerant). Prints all diagnostics; exits \
     non-zero iff there is an error-severity diagnostic (warnings alone exit 0)."
  in
  Cmd.v
    (Cmd.info "lint" ~doc)
    Term.(const cmd_lint $ file_arg $ floor_arg $ json_arg)

let validate_cmd =
  let doc = "Validate a workflow against the safety floor (fail-closed)." in
  Cmd.v
    (Cmd.info "validate" ~doc)
    Term.(const cmd_validate $ file_arg $ floor_arg)

let allow_run_arg =
  Arg.(
    value
    & opt_all string []
    & info [ "allow-run" ] ~docv:"BIN"
        ~doc:
          "Permit a run step whose command's basename is BIN to execute. \
           Repeatable; use '*' to allow all. OPERATOR-only and RUNTIME-only: a \
           workflow file cannot grant it. With no --allow-run flag, the \
           allowlist is empty and NO run step ever executes (fail-closed). The \
           working_dir bounds the cwd/snapshot but does NOT sandbox the command \
           from absolute paths in its args.")

let ledger_arg =
  Arg.(
    value
    & opt (some string) None
    & info [ "ledger" ] ~docv:"PATH"
        ~doc:
          "After a successful run, write the recorded trace as an on-disk \
           ledger (NDJSON) to PATH. The ledger can later be replayed \
           byte-identically with the 'replay' subcommand, in a separate \
           process. The ledger is runtime output, never workflow input.")

let ctx_arg =
  Arg.(
    value
    & opt (some string) None
    & info [ "ctx" ] ~docv:"JSON"
        ~doc:
          "Pre-populate the run context with a top-level JSON object.            Each top-level key becomes a bare ctx key accessible to            foreach.over and expressions. Absent => empty context.")

let run_cmd =
  let doc = "Run a workflow deterministically, dispatching agents via cabal." in
  Cmd.v (Cmd.info "run" ~doc)
    Term.(
      const cmd_run $ file_arg $ floor_arg $ approve_arg $ allow_run_arg
      $ ledger_arg $ ctx_arg)

let replay_ledger_arg =
  Arg.(
    required
    & opt (some string) None
    & info [ "ledger" ] ~docv:"PATH"
        ~doc:
          "Path to an on-disk ledger (NDJSON) previously written by 'run \
           --ledger'. Required.")

let replay_cmd =
  let doc =
    "Replay a workflow from an on-disk ledger byte-identically (in a later \
     process). Loads + validates the workflow (same --floor gates), reads the \
     ledger, and re-feeds the recorded trace to the engine. NO agent or command \
     is ever dispatched/executed — recorded results are re-fed. Exits 0 on a \
     faithful replay; non-zero on a corrupt ledger, a validation error, or a \
     Replay_mismatch (incl. a workflow/ledger mismatch)."
  in
  Cmd.v (Cmd.info "replay" ~doc)
    Term.(const cmd_replay $ file_arg $ floor_arg $ replay_ledger_arg)

let schema_cmd =
  let doc =
    "Print the canonical JSON Schema (draft 2020-12) of the workflow format to \
     stdout. Point a workflow generator at this to emit conformant workflows by \
     construction."
  in
  Cmd.v (Cmd.info "schema" ~doc) Term.(const cmd_schema $ const ())

let to_claude_workflow_cmd =
  let doc =
    "Compile a CWR workflow JSON file to Claude Workflow JavaScript. One-way \
     compiler only (CWR → JS). Outputs the compiled JS to stdout and prints \
     compilation notes (steps with no direct JS equivalent) to stderr."
  in
  Cmd.v
    (Cmd.info "to-claude-workflow" ~doc)
    Term.(const cmd_to_claude_workflow $ file_arg)

let () =
  let doc = "Deterministic workflow engine on cabal." in
  let info = Cmd.info "cabal-workflow-runner" ~version:"0.14.0" ~doc in
  let group =
    Cmd.group info
      [ lint_cmd; validate_cmd; run_cmd; replay_cmd; schema_cmd;
        to_claude_workflow_cmd ]
  in
  exit (Cmd.eval' group)
