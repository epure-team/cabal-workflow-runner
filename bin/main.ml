open Cabal_workflow_runner

let load_and_validate ?(required_attestations = []) ~floor_gates file =
  match Workflow_json.of_file file with
  | Error e -> Error (Printf.sprintf "parse error: %s" e)
  | Ok wf -> (
      match Validate.workflow ~required_attestations ~floor_gates wf with
      | Error e -> Error (Printf.sprintf "validation rejected workflow: %s" e)
      | Ok v -> Ok v)

let print_trace trace =
  List.iter
    (fun entry ->
      match entry with
      | Types.Agent_ran { id; success; output; _ } ->
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
      | Types.Deadline_read { expired } ->
          Printf.printf "  deadline expired=%b\n" expired
      | Types.Loop_stopped { iterations; reason } ->
          Printf.printf "  loop     stopped after %d iter(s) (%s)\n" iterations
            reason
      | Types.Run_executed { id; result; _ } ->
          Printf.printf
            "  run      %-16s exit=%d truncated=%b files=%d\n" id
            result.Types.exit result.Types.truncated
            (List.length result.Types.files)
      | Types.Commit_preflight_executed { id; result; receipt; _ } ->
          Printf.printf
            "  preflight %-16s exit=%d truncated=%b receipt=%b\n" id
            result.Types.exit result.Types.truncated (Option.is_some receipt)
      | Types.Committed_step { id; token_digest; _ } ->
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
      | Types.Spawn_started { id } -> Printf.printf "  spawn    started %s\n" id
      | Types.Spawn_child_completed { spawn_id; child_id; outcome; ctx = _ } ->
          Printf.printf "  spawn    child %s/%s %s\n" spawn_id child_id
            (Types.string_of_outcome outcome)
      | Types.Spawn_completed { id; children; outcome } ->
          Printf.printf "  spawn    completed %s (%d) %s\n" id children
            (Types.string_of_outcome outcome)
      | Types.Dynamic_parallel_started { id; branches } ->
          Printf.printf "  dynpar   started %s (%d branch(es))\n" id branches
      | Types.Dynamic_parallel_branch_completed { id; branch_idx; key; outcome; trace = _; branch_outputs = _ } ->
          Printf.printf "  dynpar   branch[%d/%s] %s %s\n" branch_idx key id
            (Types.string_of_outcome outcome)
      | Types.Dynamic_parallel_completed { id; branches; outcome; failed = _ } ->
          Printf.printf "  dynpar   completed %s (%d) %s\n" id branches
            (Types.string_of_outcome outcome)
      | Types.Shell_executed { id; results } ->
          Printf.printf "  shell    %-16s %d command(s)\n" id (List.length results)
      | Types.Evidence_evaluated { id; tier; passed } ->
          Printf.printf "  evidence %-16s tier=%s passed=%b\n" id tier passed
      | Types.Attestation_exported { id; envelope } ->
          Printf.printf "  attest   %-16s key=%s\n" id
            (match envelope with `Assoc f -> (match List.assoc_opt "key_id" f with
             Some (`String s) -> s | _ -> "<unknown>") | _ -> "<invalid>")
      | Types.Ctx_snapshot _ ->
          (* ledger-layer header, never appears in an engine trace *) ()
      | Types.Approval_supplied _ ->
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
    Secure_fs.read_regular file
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

external file_descr_of_int : int -> Unix.file_descr = "%identity"

let signer_from_fd = function
  | None -> Ok None
  | Some n when n >= 0 -> Result.map Option.some
      (Attestation.signer_of_fd (file_descr_of_int n))
  | Some _ -> Error "--attestation-key-fd must be non-negative"

let check_expected_workflow_digest validated expected required =
  let actual = Attestation.workflow_digest (Validate.Validated.workflow validated) in
  match expected with
  | Some digest when digest = actual -> Ok ()
  | Some digest -> Error (Printf.sprintf
      "workflow digest mismatch: expected %s, actual %s" digest actual)
  | None when required <> [] ->
      Error "--expected-workflow-digest is required with --require-attestation"
  | None -> Ok ()

let approval_run_context_digest ~workflow_digest ~session_nonce ~initial_ctx =
  let binding = `Assoc [
    ("workflow_digest", `String workflow_digest);
    ("session_nonce", match session_nonce with None -> `Null | Some value -> `String value);
    ("initial_ctx", `Assoc initial_ctx);
  ] in
  Result.map (fun canonical -> "sha256:" ^ Digestif.SHA256.(to_hex
    (digest_string canonical))) (Canonical_json.to_string binding)

let approval_header ~workflow_digest ~session_nonce ~initial_ctx token =
  Result.map (fun run_context_digest -> Types.Approval_supplied {
    token_digest = Engine.token_digest token; workflow_digest; session_nonce;
    run_context_digest;
  }) (approval_run_context_digest ~workflow_digest ~session_nonce ~initial_ctx)

let ledger_prefix ~initial_ctx approval =
  let header = Ledger.entry_to_json (Types.Ctx_snapshot { ctx = initial_ctx }) in
  Yojson.Safe.to_string header ^ "\n" ^
  match approval with None -> "" | Some entry ->
    Yojson.Safe.to_string (Ledger.entry_to_json entry) ^ "\n"

let prepare_ledger path prefix =
  Result.bind (Secure_fs.ledger_open path) (fun handle ->
    match Result.bind (Secure_fs.ledger_write handle ~phase:"prefix" prefix)
      (fun () -> Secure_fs.ledger_flush handle ~phase:"prefix") with
    | Ok () -> Ok handle
    | Error message ->
        ignore (Secure_fs.ledger_close handle);
        Error message)

let finish_ledger handle trace =
  let result = Result.bind
    (Secure_fs.ledger_write handle ~phase:"append" (Ledger.to_ndjson trace))
    (fun () -> Result.bind (Secure_fs.ledger_flush handle ~phase:"append")
      (fun () -> Result.bind (Secure_fs.ledger_identity_matches handle)
        (function true -> Ok () | false -> Error "ledger path identity changed during workflow"))) in
  let close_result = Secure_fs.ledger_close handle in
  match result, close_result with
  | Ok (), Ok () -> Ok ()
  | Error message, _ | Ok (), Error message -> Error message

let cmd_run file floor_gates approve allow_run ledger ctx_json attestation_key_fd
    attestation_root attestation_session required_attestations expected_digest deadline =
  match load_and_validate ~required_attestations ~floor_gates file with
  | Error e ->
      Printf.eprintf "%s\n" e;
      1
  | Ok validated ->
      (match check_expected_workflow_digest validated expected_digest required_attestations with
      | Error e -> Printf.eprintf "%s\n" e; 1
      | Ok () ->
      (match signer_from_fd attestation_key_fd with
      | Error e -> Printf.eprintf "attestation key error: %s\n" e; 1
      | Ok attestation_signer ->
      Eio_main.run (fun env ->
          Eio.Switch.run (fun sw ->
              let cwd = Sys.getcwd () in
              let backend = Backend_cabal.make ~sw ~env ~working_dir:cwd in
              let initial_ctx = match ctx_json with
                | None -> []
                | Some raw ->
                    (match Yojson.Safe.from_string raw with
                     | `Assoc fields as json ->
                         (match Attestation.validate_canonical_json json with
                         | Ok () -> fields
                         | Error e ->
                             Printf.eprintf "--ctx is non-canonical: %s\n" e;
                             exit 1)
                     | _ ->
                         Printf.eprintf "--ctx must be a JSON object\n";
                         exit 1
                     | exception Yojson.Json_error msg ->
                         Printf.eprintf "--ctx parse error: %s\n" msg;
                         exit 1)
              in
              let actual_workflow_digest =
                Attestation.workflow_digest (Validate.Validated.workflow validated)
              in
              let approval_result = match approve with
                | None -> Ok None
                | Some token -> Result.map Option.some
                    (approval_header ~workflow_digest:actual_workflow_digest
                      ~session_nonce:attestation_session ~initial_ctx token)
              in
              match approval_result with
              | Error message ->
                  Printf.eprintf "--ctx is non-canonical: %s\n" message;
                  1
              | Ok supplied_approval ->
                  let ledger_result = match ledger with
                    | None -> Ok None
                    | Some path -> Result.map Option.some
                        (prepare_ledger path (ledger_prefix ~initial_ctx supplied_approval))
                  in
                  (match ledger_result with
                  | Error message ->
                      Printf.eprintf "could not initialize audit ledger: %s\n" message;
                      1
                  | Ok ledger_handle ->
                      let outcome, trace =
                        Engine.run ~sw ~run_allowlist:allow_run ~backend ~token:approve
                          ~initial_ctx ?attestation_signer
                          ?attestation_artifact_root:attestation_root
                          ?attestation_session_nonce:attestation_session
                          ?deadline
                          ~agent_backend_id:"cabal-read-only-v1" validated
                      in
                      let ledger_finish = match ledger_handle with
                        | None -> Ok ()
                        | Some handle -> finish_ledger handle trace
                      in
                      (match ledger_finish with
                      | Error message ->
                          Printf.eprintf
                            "audit ledger incomplete after workflow effects; refusing success: %s\n"
                            message;
                          1
                      | Ok () ->
                          Printf.printf "outcome: %s\ntrace:\n"
                            (Types.string_of_outcome outcome);
                          print_trace trace;
                          Option.iter (fun path -> Printf.printf "ledger written: %s\n" path) ledger;
                          match outcome with
                          | Types.Committed _ | Types.Completed_no_commit -> 0
                          | Types.Blocked _ | Types.Aborted _ -> 2
                          | Types.Cancelled _ -> 2))));
      ))

(* ---- replay subcommand ---- *)

let load_public_identity path =
  Result.bind (Secure_fs.read_regular path) (fun raw ->
    try Attestation.verifier_of_identity (Yojson.Safe.from_string raw)
    with Yojson.Json_error e -> Error ("invalid public-key JSON: " ^ e))

let valid_token_digest value =
  String.length value = 71 && String.starts_with ~prefix:"sha256:" value &&
  String.for_all (function
    | '0' .. '9' | 'a' .. 'f' -> true
    | _ -> false) (String.sub value 7 64)

let validate_approval_header ~workflow_digest ~session_nonce ~initial_ctx
    approval trace =
  match approval with
  | None -> Ok () (* legacy ledgers remain replayable *)
  | Some (Types.Approval_supplied supplied) ->
      Result.bind (approval_run_context_digest
        ~workflow_digest ~session_nonce ~initial_ctx) (fun expected_context ->
        if supplied.workflow_digest <> workflow_digest then
          Error "approval header workflow digest mismatch"
        else if supplied.session_nonce <> session_nonce then
          Error "approval header session nonce mismatch"
        else if supplied.run_context_digest <> expected_context then
          Error "approval header run context digest mismatch"
        else if not (valid_token_digest supplied.token_digest) then
          Error "approval header token digest is malformed"
        else
          let committed_digests = List.filter_map (function
            | Types.Committed_step { token_digest; _ } -> Some token_digest
            | _ -> None) trace in
          if List.for_all (( = ) supplied.token_digest) committed_digests then Ok ()
          else Error "approval header token digest disagrees with committed step")
  | Some _ -> Error "invalid approval header"

let cmd_replay file floor_gates ledger attestation_public_key attestation_session
    required_attestations expected_digest =
  match load_and_validate ~required_attestations ~floor_gates file with
  | Error e ->
      Printf.eprintf "%s\n" e;
      1
  | Ok validated -> (
      match check_expected_workflow_digest validated expected_digest required_attestations with
      | Error e -> Printf.eprintf "%s\n" e; 1
      | Ok () ->
      let attestation_verifier = match attestation_public_key with None -> Ok None
        | Some p -> Result.map Option.some (load_public_identity p) in
      match attestation_verifier with
      | Error e -> Printf.eprintf "attestation public-key error: %s\n" e; 1
      | Ok attestation_verifier ->
      let raw =
        Secure_fs.read_regular ledger
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
          let initial_ctx, after_ctx =
            match lines with
            | first :: rest -> (
                match Ledger.entry_of_json (Yojson.Safe.from_string first) with
                | Types.Ctx_snapshot { ctx } -> (ctx, rest)
                | _ -> ([], lines)
                | exception _ -> ([], lines))
            | [] -> ([], [])
          in
          let approval, trace_lines =
            match after_ctx with
            | first :: rest -> (
                match Ledger.entry_of_json (Yojson.Safe.from_string first) with
                | Types.Approval_supplied _ as header -> (Some header, rest)
                | _ -> (None, after_ctx)
                | exception _ -> (None, after_ctx))
            | [] -> (None, [])
          in
          let trace_str = String.concat "\n" trace_lines in
          match Ledger.of_ndjson trace_str with
          | Error e ->
              Printf.eprintf "corrupt ledger: %s\n" e;
              1
          | Ok trace ->
              let actual_workflow_digest =
                Attestation.workflow_digest (Validate.Validated.workflow validated)
              in
              (match Result.bind (Attestation.validate_canonical_json (`Assoc initial_ctx))
                (fun () -> validate_approval_header ~workflow_digest:actual_workflow_digest
                  ~session_nonce:attestation_session ~initial_ctx approval trace) with
              | Error message ->
                  Printf.eprintf "corrupt ledger: %s\n" message;
                  exit 1
              | Ok () -> ());
              (* Re-feed the recorded trace; NO backend is consulted and no
                 command is ever dispatched/executed (same as in-memory replay).
                 A workflow/ledger mismatch surfaces as Replay_mismatch. *)
              let result = ref 0 in
              Eio_main.run (fun _env ->
                Eio.Switch.run (fun sw ->
                  match Engine.replay ~sw ~trace ~initial_ctx ?attestation_verifier
                    ?attestation_session_nonce:attestation_session validated with
                  | outcome ->
                      Printf.printf "replayed outcome: %s\ntrace:\n"
                        (Types.string_of_outcome outcome);
                      print_trace trace
                  | exception Engine.Replay_mismatch reason ->
                      Printf.eprintf "replay mismatch: %s\n" reason;
                      result := 2));
              !result))

let cmd_attestation_public_key key_fd =
  match signer_from_fd (Some key_fd) with
  | Error e -> Printf.eprintf "attestation key error: %s\n" e; 1
  | Ok None -> assert false
  | Ok (Some signer) ->
      print_endline (Attestation.canonical_string
        (Attestation.public_identity signer)); 0

let cmd_workflow_digest file =
  match Workflow_json.of_file file with
  | Error e -> Printf.eprintf "parse error: %s\n" e; 1
  | Ok wf -> print_endline (Attestation.workflow_digest wf); 0

let parse_context ~ctx_json ~ctx_file =
  match ctx_json, ctx_file with
  | Some _, Some _ -> Error "use only one of --ctx and --ctx-file"
  | None, None -> Ok []
  | Some raw, None | None, Some raw ->
      let parsed = match ctx_file with
        | Some path -> Result.bind (Secure_fs.read_regular path) (fun raw ->
            try Ok (Yojson.Safe.from_string raw) with Yojson.Json_error e -> Error e)
        | None -> (try Ok (Yojson.Safe.from_string raw) with Yojson.Json_error e -> Error e) in
      Result.bind parsed (function
        | `Assoc f as json ->
            Result.map (fun () -> f)
              (Canonical_json.validate_no_duplicates json)
        | _ -> Error "context must be a JSON object")

let rec attest_steps steps = List.concat_map (function
  | Types.Attest { id; select; replay_domain; output } ->
      [id, select, replay_domain, output]
  | Types.Branch { then_; else_; _ } -> attest_steps then_ @ attest_steps else_
  | Types.Loop { body; _ } -> attest_steps body
  | Types.Parallel { branches } -> List.concat_map attest_steps branches
  | Types.Foreach { steps; _ } -> attest_steps steps
  | Types.Spawn { children; _ } -> List.concat_map (fun (child : Types.spawn_child) -> attest_steps child.steps) children
  | Types.Dynamic_parallel { steps; _ } -> attest_steps steps
  | Types.Agent _ | Types.Gate _ | Types.Run _ | Types.Commit _ | Types.Shell _
  | Types.Evidence _ -> []) steps

let load_regular_json label path =
  Result.bind (Secure_fs.read_regular path) (fun raw ->
    try
      let json = Yojson.Safe.from_string raw in
      Result.map (fun () -> json) (Canonical_json.validate json)
    with Yojson.Json_error e -> Error (label ^ " is invalid JSON: " ^ e))

let cmd_verify_attestation file floor_gates artifact step_id public_key_path
    session_nonce ctx_json ctx_file expected_digest occurrence =
  match load_and_validate ~floor_gates file with
  | Error e -> Printf.eprintf "%s\n" e; 1
  | Ok validated ->
      let wf = Validate.Validated.workflow validated in
      if Attestation.workflow_digest wf <> expected_digest then
        (Printf.eprintf "workflow digest mismatch\n"; 1)
      else
      match List.filter (fun (id, _, _, _) -> id = step_id) (attest_steps wf.steps) with
      | [] -> Printf.eprintf "attest step %S not found\n" step_id; 1
      | _ :: _ :: _ -> Printf.eprintf "attest step %S is not unique\n" step_id; 1
      | [(_, select, replay_domain, output)] ->
          let result = Result.bind (load_public_identity public_key_path) (fun verifier ->
            Result.bind (parse_context ~ctx_json ~ctx_file) (fun ctx ->
              Result.bind (Attestation.select_context ctx select) (fun selected ->
                Result.bind (load_regular_json "attestation artifact" artifact) (fun envelope ->
                  Attestation.verify ~verifier ~workflow:wf ~step_id ~occurrence
                    ~output_path:(Attestation.materialize_output_path
                      ~template:output
                      ~occurrence)
                    ~replay_domain ~session_nonce ~selected envelope)))) in
          (match result with Ok () -> Printf.printf "VALID ATTESTATION: %s (%s)\n" artifact step_id; 0
           | Error e -> Printf.eprintf "INVALID ATTESTATION: %s\n" e; 1)

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

let attestation_key_fd_arg = Arg.(value & opt (some int) None &
  info ["attestation-key-fd"] ~docv:"FD" ~doc:"Read an exact 32-byte Ed25519 seed from inherited FD and close it before backend construction.")
let required_attestation_key_fd_arg = Arg.(required & opt (some int) None & info ["attestation-key-fd"] ~docv:"FD")
let attestation_root_arg = Arg.(value & opt (some string) None & info ["attestation-root"] ~docv:"DIR")
let attestation_session_arg = Arg.(value & opt (some string) None & info ["attestation-session"] ~docv:"NONCE")
let attestation_public_key_arg = Arg.(value & opt (some string) None & info ["attestation-public-key"] ~docv:"PATH")
let required_attestation_public_key_arg = Arg.(required & opt (some string) None & info ["attestation-public-key"] ~docv:"PATH")
let attestation_artifact_arg = Arg.(required & opt (some string) None & info ["attestation"] ~docv:"PATH")
let attestation_step_arg = Arg.(required & opt (some string) None & info ["step"] ~docv:"STEP_ID")
let required_attestation_session_arg = Arg.(required & opt (some string) None & info ["attestation-session"] ~docv:"NONCE")
let ctx_file_arg = Arg.(value & opt (some string) None & info ["ctx-file"] ~docv:"PATH")
let require_attestation_arg = Arg.(value & opt_all string [] &
  info ["require-attestation"] ~docv:"STEP_ID")
let expected_workflow_digest_arg = Arg.(value & opt (some string) None &
  info ["expected-workflow-digest"] ~docv:"SHA256")
let required_workflow_digest_arg = Arg.(required & opt (some string) None &
  info ["expected-workflow-digest"] ~docv:"SHA256")
let occurrence_arg = Arg.(value & opt int 0 & info ["occurrence"] ~docv:"N")
let deadline_arg = Arg.(value & opt (some float) None &
  info ["deadline"] ~docv:"EPOCH_SECONDS"
    ~doc:"Operator-supplied wall-clock deadline (Unix epoch seconds, e.g. via date -d '22:00' +%s) \
          for the workflow's Deadline governor. Runtime-only: never read from the workflow file. \
          Absent => a workflow declaring Deadline never stops via that governor.")

let run_cmd =
  let doc = "Run a workflow deterministically, dispatching agents via cabal." in
  Cmd.v (Cmd.info "run" ~doc)
    Term.(
      const cmd_run $ file_arg $ floor_arg $ approve_arg $ allow_run_arg
      $ ledger_arg $ ctx_arg $ attestation_key_fd_arg $ attestation_root_arg
      $ attestation_session_arg $ require_attestation_arg
      $ expected_workflow_digest_arg $ deadline_arg)

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
    Term.(const cmd_replay $ file_arg $ floor_arg $ replay_ledger_arg
      $ attestation_public_key_arg $ attestation_session_arg
      $ require_attestation_arg $ expected_workflow_digest_arg)

let attestation_public_key_cmd =
  Cmd.v (Cmd.info "attestation-public-key" ~doc:"Derive a pinnable public identity from an inherited seed FD.")
    Term.(const cmd_attestation_public_key $ required_attestation_key_fd_arg)

let workflow_digest_cmd =
  Cmd.v (Cmd.info "workflow-digest" ~doc:"Print the canonical pinned workflow digest.")
    Term.(const cmd_workflow_digest $ file_arg)

let verify_attestation_cmd =
  Cmd.v (Cmd.info "verify-attestation" ~doc:"Verify a durable native attestation without a backend or ledger.")
    Term.(const cmd_verify_attestation $ file_arg $ floor_arg $ attestation_artifact_arg
      $ attestation_step_arg $ required_attestation_public_key_arg
      $ required_attestation_session_arg $ ctx_arg $ ctx_file_arg
      $ required_workflow_digest_arg $ occurrence_arg)

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
  let info = Cmd.info "cabal-workflow-runner" ~version:"0.19.0" ~doc in
  let group =
    Cmd.group info
      [ lint_cmd; validate_cmd; run_cmd; replay_cmd; schema_cmd;
        to_claude_workflow_cmd; attestation_public_key_cmd;
        verify_attestation_cmd; workflow_digest_cmd ]
  in
  exit (Cmd.eval' group)
