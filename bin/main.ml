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
      | Types.Committed_step { id; token_digest } ->
          Printf.printf "  commit   %-16s token_digest=%s\n" id token_digest
      | Types.Blocked_at { id; reason } ->
          Printf.printf "  block    %-16s %s\n" id reason)
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

let cmd_run file floor_gates approve =
  match load_and_validate ~floor_gates file with
  | Error e ->
      Printf.eprintf "%s\n" e;
      1
  | Ok validated ->
      Eio_main.run (fun env ->
          Eio.Switch.run (fun sw ->
              let cwd = Sys.getcwd () in
              let backend = Backend_cabal.make ~sw ~env ~working_dir:cwd in
              let outcome, trace =
                Engine.run ~backend ~token:approve validated
              in
              Printf.printf "outcome: %s\ntrace:\n"
                (Types.string_of_outcome outcome);
              print_trace trace;
              match outcome with
              | Types.Committed _ | Types.Completed_no_commit -> ()
              | Types.Blocked _ | Types.Aborted _ -> exit 2));
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

let run_cmd =
  let doc = "Run a workflow deterministically, dispatching agents via cabal." in
  Cmd.v (Cmd.info "run" ~doc)
    Term.(const cmd_run $ file_arg $ floor_arg $ approve_arg)

let schema_cmd =
  let doc =
    "Print the canonical JSON Schema (draft 2020-12) of the workflow format to \
     stdout. Point a workflow generator at this to emit conformant workflows by \
     construction."
  in
  Cmd.v (Cmd.info "schema" ~doc) Term.(const cmd_schema $ const ())

let () =
  let doc = "Deterministic workflow engine on cabal." in
  let info = Cmd.info "cabal-workflow-runner" ~version:"0.6.0" ~doc in
  let group = Cmd.group info [ lint_cmd; validate_cmd; run_cmd; schema_cmd ] in
  exit (Cmd.eval' group)
