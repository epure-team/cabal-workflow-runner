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
      | Types.Agent_ran { id; success; text } ->
          Printf.printf "  agent  %-16s success=%b text=%S\n" id success text
      | Types.Gate_evaluated { id; verdict } ->
          Printf.printf "  gate   %-16s %s\n" id (Types.verdict_to_string verdict)
      | Types.Committed_step { id; token_digest } ->
          Printf.printf "  commit %-16s token_digest=%s\n" id token_digest
      | Types.Blocked_at { id; reason } ->
          Printf.printf "  block  %-16s %s\n" id reason)
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

let validate_cmd =
  let doc = "Validate a workflow against the safety floor (fail-closed)." in
  Cmd.v
    (Cmd.info "validate" ~doc)
    Term.(const cmd_validate $ file_arg $ floor_arg)

let run_cmd =
  let doc = "Run a workflow deterministically, dispatching agents via cabal." in
  Cmd.v (Cmd.info "run" ~doc)
    Term.(const cmd_run $ file_arg $ floor_arg $ approve_arg)

let () =
  let doc = "Deterministic workflow engine on cabal." in
  let info = Cmd.info "cabal-workflow-runner" ~version:"0.1.0" ~doc in
  let group = Cmd.group info [ validate_cmd; run_cmd ] in
  exit (Cmd.eval' group)
