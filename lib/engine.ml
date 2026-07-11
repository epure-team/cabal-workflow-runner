open Types

let token_digest tok = Digest.to_hex (Digest.string tok)

let token_is_wellformed = function
  | None -> false
  | Some t -> String.length (String.trim t) > 0

(* Execution state threaded through the walk. [rev_trace] accumulates in REVERSE
   order (most recent first) and is reversed at the end. [ctx] binds step ids to
   their recorded structured output (addressable as ["outputs.<id>..."]); the
   loop additionally binds ["loop"] to {"iter": <index>}. [terminal] is set when
   a Commit / Block / Abort ends the run. *)
type state = {
  rev_trace : trace_entry list;
  ctx : (string * Yojson.Safe.t) list;
  attest_counts : (string * int) list;
  terminal : outcome option;
}

let emit st entry = { st with rev_trace = entry :: st.rev_trace }

(* Bind/overwrite a key in ctx (most recent write wins; assoc lookup finds it). *)
let bind st key json =
  { st with ctx = (key, json) :: List.remove_assoc key st.ctx }

(* The expression context: agent outputs are nested under "outputs". We expose
   that to the DSL by keeping ctx keyed by "outputs" and "loop". The actual
   per-step output is merged into the single "outputs" object. *)
let ctx_for st = st.ctx

let finish st =
  let trace = List.rev st.rev_trace in
  let outcome =
    match st.terminal with Some o -> o | None -> Completed_no_commit
  in
  (outcome, trace)

(* Merge an agent's output object under outputs.<id>, preserving prior outputs. *)
let bind_output st id output =
  let prior =
    match List.assoc_opt "outputs" st.ctx with
    | Some (`Assoc fields) -> fields
    | _ -> []
  in
  let merged = `Assoc ((id, output) :: List.remove_assoc id prior) in
  bind st "outputs" merged

let bind_receipts st id request result =
  let prior =
    match List.assoc_opt "receipts" st.ctx with
    | Some (`Assoc fields) -> fields
    | _ -> []
  in
  let receipt = `Assoc [ ("request", request); ("result", result) ] in
  bind st "receipts"
    (`Assoc ((id, receipt) :: List.remove_assoc id prior))

let sha256 canonical =
  "sha256:" ^ Digestif.SHA256.(to_hex (digest_string canonical))

let canonical_digest json =
  Result.map (fun canonical -> (canonical, sha256 canonical))
    (Canonical_json.to_string json)

let bind_loop_iter st index = bind st "loop" (`Assoc [ ("iter", `Int index) ])

let run_input st paths =
  Result.bind (Attestation.select_context st.ctx paths) (fun selected ->
      Result.map
        (fun canonical ->
          let digest =
            "sha256:"
            ^ Digestif.SHA256.(to_hex (digest_string canonical))
          in
          (canonical, digest))
        (Canonical_json.to_string (`Assoc selected)))

let commit_lock_json path (identity : Secure_fs.lock_identity) =
  `Assoc [ ("path", `String path); ("held", `Bool true);
           ("device", `String identity.device);
           ("inode", `String identity.inode) ]

let reserved_commit_lock_path path =
  path = "cwr.commit_lock"
  || (String.length path > String.length "cwr.commit_lock"
      && String.sub path 0 (String.length "cwr.commit_lock" + 1)
         = "cwr.commit_lock.")

let commit_preflight_input st paths lock_identity =
  if List.exists reserved_commit_lock_path paths then
    Error "reserved engine path cwr.commit_lock cannot be selected"
  else Result.bind (Attestation.select_context st.ctx paths) (fun selected ->
      let selected = match lock_identity with
        | None -> selected
        | Some marker -> ("cwr.commit_lock", marker) :: selected in
      Result.map (fun canonical -> (canonical, sha256 canonical))
        (Canonical_json.to_string (`Assoc selected)))

let executable_json (identity : executable_identity) =
  `Assoc [ ("path", `String identity.path);
           ("digest", `String identity.digest) ]

let run_output_json ~input_digest ~parsed ~executable result =
  match json_of_run_result result with
  | `Assoc fields ->
      `Assoc
        (fields
        @ (match input_digest with
          | None -> []
          | Some digest -> [ ("input_digest", `String digest) ])
        @ (match executable with None -> []
          | Some identity -> [ ("executable", executable_json identity) ])
        @ match parsed with None -> [] | Some json -> [ ("parsed", json) ])
  | json -> json

let parse_run_stdout schema (result : run_result) =
  if result.truncated then Error "stdout was truncated"
  else match Yojson.Safe.from_string result.stdout with
  | exception Yojson.Json_error msg -> Error ("stdout is not JSON: " ^ msg)
  | (`Assoc _ as parsed) ->
      Result.bind (Canonical_json.to_string parsed) (fun canonical ->
          let normalized = Yojson.Safe.from_string canonical in
          Result.map
            (fun () -> normalized)
            (Result.map_error
               (fun field -> "stdout schema mismatch: " ^ field)
               (Schema.validate schema normalized)))
  | _ -> Error "stdout must be a JSON object"

let commit_preflight_receipt ~id ~input_digest ~result ~parsed ~lock_identity =
  Result.bind (canonical_digest parsed) (fun (_, output_digest) ->
      let receipt =
        `Assoc
          ([ ("step_id", `String id);
            ("input_digest", `String input_digest);
            ("process_exit", `Int result.exit);
            ("output_digest", `String output_digest);
            ("parsed", parsed) ]
          @ match lock_identity with None -> []
            | Some marker -> [ ("commit_lock", marker) ])
      in
      Result.map (fun (_, digest) -> (receipt, digest))
        (canonical_digest receipt))

(* ------------------------------------------------------------------ *)
(* run: deterministic interpreter driven by a backend.                 *)
(* ------------------------------------------------------------------ *)

(* Unconditional hard iteration ceiling for every loop. A loop ALWAYS stops once
   it has executed this many iterations, regardless of governors / until / budget
   / agent behaviour — it is the termination GUARANTEE (Budget/Fixpoint/until are
   early-stop heuristics under it). Default chosen generously; tests pass a small
   value. The ceiling is a constant, so replay reproduces byte-identically. *)
let default_max_loop_iters = 10_000

(* A [Run] step executes only if the basename of its command's head is in the
   operator-supplied allowlist, OR the allowlist contains ["*"] (allow all). The
   default allowlist is [[]], so with no operator opt-in NO run step ever
   executes (fail-closed). The allowlist is a RUNTIME parameter, never read from
   the workflow file: a workflow cannot grant itself the right to run a command. *)
let run_permitted ~run_allowlist cmd =
  match cmd with
  | [] -> false (* validator rejects this; defensive. *)
  | head :: _ ->
      List.mem "*" run_allowlist
      || List.mem (Filename.basename head) run_allowlist

(* [cmd.(0)] must be a BARE command name resolved via PATH. A head containing a
   path separator ('/') — i.e. an absolute path ["/abs/x"], an explicit relative
   path ["./x"], or any ["a/b"] — is rejected: it bypasses the allowlist's
   [Filename.basename] match while executing an arbitrary binary. The bin runner
   execs bare names via PATH. *)
let path_bearing_head = function
  | head :: _ -> String.contains head '/'
  | [] -> false

let run ?(max_loop_iters = default_max_loop_iters) ?(run_allowlist = [])
    ?(initial_ctx = []) ?attestation_signer ?attestation_artifact_root
    ?attestation_session_nonce ?agent_backend_id ~sw:(_sw : Eio.Switch.t)
    ~backend ~token validated =
  let wf = Validate.Validated.workflow validated in
  let agent ~id ~prompt ~read_only ~agent_type ~model =
    backend.Backend.run_agent ~id ~prompt ~read_only ~agent_type ~model
  in
  let eval st e = Expr.eval ~ctx:(ctx_for st) e in
  let rec go st steps =
    match (st.terminal, steps) with
    | Some _, _ | _, [] -> st
    | None, step :: rest ->
        let st = go_step st step in
        go st rest
  and go_step st step =
    match step with
    | Agent { id; prompt; read_only; output_schema; on_failure; protocol; brief; agent_type; model; input } ->
        let read_opt label = function
          | None -> `Ok ""
          | Some p ->
              (try `Ok (In_channel.with_open_text p In_channel.input_all)
               with Sys_error msg ->
                 `Err (Printf.sprintf "agent %S: cannot read %s %S: %s" id label p msg))
        in
        (match (read_opt "protocol" protocol, read_opt "brief" brief) with
        | `Err reason, _ | _, `Err reason ->
            let st = { st with terminal = Some (Aborted reason) } in
            emit st (Blocked_at { id; reason })
        | `Ok pc, `Ok bc ->
        let effective_prompt =
          let parts = List.filter (fun s -> s <> "") [ pc; bc; prompt ] in
          String.concat "\n\n" parts
        in
        let structured = match input with
          | None -> Ok (effective_prompt, None)
          | Some paths ->
              (match agent_backend_id, attestation_session_nonce with
              | None, _ -> Error "structured Agent requires runtime agent_backend_id"
              | _, None -> Error "structured Agent requires a fresh runtime session nonce"
              | Some backend_id, Some nonce ->
                  Result.bind (run_input st paths) (fun (canonical, input_digest) ->
                    let dispatch_id = sha256
                      (String.concat "\000"
                         [ nonce; id; string_of_int (List.length st.rev_trace) ]) in
                    let request = `Assoc
                      [ ("schema_version", `String "cwr.agent-request.v1");
                        ("dispatch_id", `String dispatch_id);
                        ("step_id", `String id);
                        ("backend_id", `String backend_id);
                        ("role", match agent_type with None -> `Null | Some r -> `String r);
                        ("read_only", `Bool read_only);
                        ("input_paths", `List
                          (List.map (fun path -> `String path) paths));
                        ("input_digest", `String input_digest);
                        ("input", Yojson.Safe.from_string canonical) ] in
                    Result.map
                      (fun (_, request_digest) ->
                        (effective_prompt ^ "\n\nCWR_STRUCTURED_INPUT_JSON\n" ^ canonical,
                         Some (request, request_digest)))
                      (canonical_digest request)))
        in
        (match structured with
        | Error reason ->
            let st = { st with terminal = Some (Aborted reason) } in
            emit st (Blocked_at { id; reason })
        | Ok (effective_prompt, request_binding) ->
        let success, output = agent ~id ~prompt:effective_prompt ~read_only ~agent_type ~model in
        let receipt_result = match request_binding with
          | None -> Ok None
          | Some (request, request_digest) ->
              Result.bind (canonical_digest output) (fun (_, output_digest) ->
                let result = `Assoc
                  [ ("schema_version", `String "cwr.agent-result.v1");
                    ("dispatch_id", List.assoc "dispatch_id" (match request with `Assoc f -> f | _ -> assert false));
                    ("step_id", `String id);
                    ("request_digest", `String request_digest);
                    ("success", `Bool success);
                    ("outcome", `String (if success then "success" else "failure"));
                    ("output_digest", `String output_digest);
                    ("output", output) ] in
                Ok (Some (request, result)))
        in
        (match receipt_result with
        | Error reason ->
            let reason = "structured Agent returned non-canonical output: " ^ reason in
            let st = emit st (Agent_ran { id; success; output;
              request_receipt = Option.map fst request_binding;
              result_receipt = None }) in
            let st = { st with terminal = Some (Aborted reason) } in
            emit st (Blocked_at { id; reason })
        | Ok receipts ->
        let st = emit st (Agent_ran { id; success; output;
          request_receipt = Option.map fst receipts;
          result_receipt = Option.map snd receipts }) in
        let st = bind_output st id output in
        let st = match receipts with None -> st
          | Some (request, result) -> bind_receipts st id request result in
        (* An UNSUCCESSFUL agent run is fail-closed by default ([Abort]): it aborts
           the walk (mirroring the schema-mismatch arm). Continuing past a failed
           agent binds the backend's error output, but a commit is still barred —
           the validator guarantees every commit is floor-gated, and those gates
           read the (now-missing) output and Block. With [on_failure = Continue]
           the failure is SOFT: the failed [Agent_ran] is recorded and its output
           bound, and the walk CONTINUES — for a continuous loop where one
           iteration's agent failure must not kill the run. The schema check below
           is fail-closed and only on a successful run. *)
        if not success then begin
          match on_failure with
          | Types.Continue -> st
          | Types.Abort ->
              let reason =
                Printf.sprintf
                  "agent step %S did not produce a successful structured output"
                  id
              in
              let st = { st with terminal = Some (Aborted reason) } in
              emit st (Blocked_at { id; reason })
        end
        else
          match output_schema with
          | Some schema -> (
              match Schema.validate schema output with
              | Ok () -> st
              | Error field ->
                  let reason = Printf.sprintf "schema mismatch: %s" field in
                  {
                    st with
                    terminal = Some (Aborted reason);
                  }
                  |> fun st -> emit st (Blocked_at { id; reason }))
          | None -> st)))
    | Gate { id; when_ } -> (
        let verdict = if eval st when_ then Pass else Fail in
        let st = emit st (Gate_evaluated { id; verdict }) in
        match verdict with
        | Pass -> st
        | Fail ->
            (* A floor gate evaluating false must BLOCK the walk: a false gate
               cannot reach a commit. *)
            let reason = Printf.sprintf "gate %S evaluated false" id in
            let st = emit st (Blocked_at { id; reason }) in
            { st with terminal = Some (Blocked reason) })
    | Branch { when_; then_; else_ } ->
        let verdict = if eval st when_ then Pass else Fail in
        let st = emit st (Branch_taken { verdict }) in
        let chosen = match verdict with Pass -> then_ | Fail -> else_ in
        go st chosen
    | Loop { body; until; governors } -> run_loop st body until governors
    | Run { id; cmd; working_dir; timeout_ms; observe; input; stdout_schema;
            executable_digest } ->
        (* Fail-closed allowlist gate. The allowlist is operator-supplied at
           runtime; if the binary is not permitted, the step is Blocked WITHOUT
           executing — mirroring the gate/commit Fail arms (emit Blocked_at,
           terminal Blocked). Nothing is recorded as executed, so replay never
           sees a Run_executed for it.

           First, cmd[0] MUST be a BARE command name resolved via PATH: any path
           separator ('/') in it — i.e. an absolute or relative path — is
           rejected (closes the allowlist bypass where a path-bearing cmd[0]
           passes the basename match but execs an attacker-chosen binary). *)
        if path_bearing_head cmd then begin
          let head = match cmd with hd :: _ -> hd | [] -> "<empty>" in
          let reason =
            Printf.sprintf
              "run command must be a bare name resolved via PATH, not a path: %s"
              head
          in
          let st = emit st (Blocked_at { id; reason }) in
          { st with terminal = Some (Blocked reason) }
        end
        else if not (run_permitted ~run_allowlist cmd) then begin
          let bin =
            match cmd with hd :: _ -> Filename.basename hd | [] -> "<empty>"
          in
          let reason =
            Printf.sprintf "run command %S not permitted (allowlist)" bin
          in
          let st = emit st (Blocked_at { id; reason }) in
          { st with terminal = Some (Blocked reason) }
        end
        else begin
          let input_material =
            match input with
            | None -> Ok (None, None)
            | Some paths ->
                Result.map
                  (fun (canonical, digest) -> (Some canonical, Some digest))
                  (run_input st paths)
          in
          match input_material with
          | Error detail ->
              let reason = Printf.sprintf "run input selection failed: %s" detail in
              let st = emit st (Blocked_at { id; reason }) in
              { st with terminal = Some (Aborted reason) }
          | Ok (stdin_content, input_digest) ->
              (* Execute the injected effect exactly ONCE, record the full result,
                 and bind it into ctx. Replay re-feeds this without re-executing. *)
              let execution = match executable_digest with
                | None -> Ok (backend.Backend.run_command ~id ~argv:cmd
                    ~working_dir ~timeout_ms ~observe ~stdin_content, None)
                | Some expected ->
                    Result.bind (backend.Backend.run_pinned_command ~id ~argv:cmd
                      ~working_dir ~timeout_ms ~observe ~stdin_content
                      ~expected_digest:expected) (fun (result, identity) ->
                        if identity.digest <> expected then
                          Error "pinned executable identity digest mismatch"
                        else Ok (result, Some identity)) in
              match execution with
              | Error detail ->
                  let reason = Printf.sprintf
                    "run %S pinned executable rejected: %s" id detail in
                  let st = emit st (Blocked_at { id; reason }) in
                  { st with terminal = Some (Blocked reason) }
              | Ok (result, executable) ->
              let parsed_result =
                match stdout_schema with
                | None -> Ok None
                | Some schema ->
                    Result.map Option.some
                      (parse_run_stdout schema result)
              in
              (match parsed_result with
              | Ok parsed ->
                  let st =
                    emit st (Run_executed { id; input_digest; parsed; result;
                      executable })
                  in
                  bind_output st id
                    (run_output_json ~input_digest ~parsed ~executable result)
              | Error detail ->
                  let st =
                    emit st
                      (Run_executed
                         { id; input_digest; parsed = None; result; executable })
                  in
                  let reason =
                    Printf.sprintf "run %S structured stdout rejected: %s" id
                      detail
                  in
                  let st = emit st (Blocked_at { id; reason }) in
                  { st with terminal = Some (Aborted reason) })
        end
    | Commit { id; preflight } ->
        if not (token_is_wellformed token) then begin
          let reason =
            Printf.sprintf "Commit %S requires a runtime approval token" id
          in
          let st = emit st (Blocked_at { id; reason }) in
          { st with terminal = Some (Blocked reason) }
        end else
          let commit st preflight_receipt preflight_receipt_digest =
            let digest = token_digest (Option.get token) in
            let st = emit st (Committed_step { id; token_digest = digest;
              preflight_receipt; preflight_receipt_digest }) in
            { st with terminal = Some (Committed { id; token_digest = digest }) }
          in
          (match preflight with
          | None -> commit st None None
          | Some { cmd; working_dir; timeout_ms; input; stdout_schema;
                   lock_file } ->
              let block st reason =
                let st = emit st (Blocked_at { id; reason }) in
                { st with terminal = Some (Blocked reason) }
              in
              if path_bearing_head cmd then
                block st (Printf.sprintf
                  "Commit %S preflight command must be a bare name resolved via PATH"
                  id)
              else if not (run_permitted ~run_allowlist cmd) then
                block st (Printf.sprintf
                  "Commit %S preflight command %S not permitted (allowlist)"
                  id (match cmd with hd :: _ -> Filename.basename hd
                     | [] -> "<empty>"))
              else
                let execute lock_marker lock_identity =
                (match commit_preflight_input st input lock_marker with
                | Error detail -> block st (Printf.sprintf
                    "Commit %S preflight input selection failed: %s" id detail)
                | Ok (canonical, input_digest) ->
                    let result = backend.Backend.run_command ~id ~argv:cmd
                      ~working_dir ~timeout_ms ~observe:None
                      ~stdin_content:(Some canonical) in
                    let parsed =
                      if result.exit <> 0 then
                        Error (Printf.sprintf "process exited %d" result.exit)
                      else parse_run_stdout stdout_schema result
                    in
                    (match parsed with
                    | Error detail ->
                        let st = emit st (Commit_preflight_executed {
                          id; input_digest; parsed = None; result;
                          receipt = None; lock_file;
                          lock_identity = lock_marker;
                          lock_identity_valid = None }) in
                        block st (Printf.sprintf
                          "Commit %S preflight rejected: %s" id detail)
                    | Ok parsed ->
                        let identity_still_matches = match lock_file,
                            lock_identity with
                          | None, None -> Ok true
                          | Some path, Some identity ->
                              Secure_fs.lock_identity_matches
                                ~root:working_dir ~relative:path identity
                          | _ -> Ok false in
                        (match identity_still_matches with
                        | Error detail ->
                            let st = emit st (Commit_preflight_executed {
                              id; input_digest; parsed = Some parsed; result;
                              receipt = None; lock_file;
                              lock_identity = lock_marker;
                              lock_identity_valid = Some false }) in
                            block st (Printf.sprintf
                              "Commit %S preflight lock identity check failed: %s"
                              id detail)
                        | Ok false ->
                            let st = emit st (Commit_preflight_executed {
                              id; input_digest; parsed = Some parsed; result;
                              receipt = None; lock_file;
                              lock_identity = lock_marker;
                              lock_identity_valid = Some false }) in
                            block st (Printf.sprintf
                              "Commit %S preflight lock inode changed" id)
                        | Ok true ->
                        (match commit_preflight_receipt ~id ~input_digest
                            ~result ~parsed ~lock_identity:lock_marker with
                        | Error detail -> block st (Printf.sprintf
                            "Commit %S preflight receipt rejected: %s" id detail)
                        | Ok (receipt, receipt_digest) ->
                            let st = emit st (Commit_preflight_executed {
                              id; input_digest; parsed = Some parsed; result;
                              receipt = Some receipt; lock_file;
                              lock_identity = lock_marker;
                              lock_identity_valid = Option.map (fun _ -> true)
                                lock_marker }) in
                            commit st (Some receipt) (Some receipt_digest)))))
                in
                match lock_file with
                | None -> execute None None
                | Some path ->
                    (match Secure_fs.with_exclusive_lock ~root:working_dir
                        ~relative:path (fun identity ->
                          let marker = commit_lock_json path identity in
                          execute (Some marker) (Some identity)) with
                    | Ok st -> st
                    | Error detail -> block st (Printf.sprintf
                        "Commit %S preflight lock acquisition failed: %s"
                        id detail)))
    | Foreach { over; steps = body } -> (
        match List.assoc_opt over st.ctx with
        | Some (`List items) ->
            let prior_item = List.assoc_opt "item" st.ctx in
            let (st, n) = List.fold_left (fun (st, i) element ->
              if st.terminal <> None then (st, i + 1)
              else
                let st = emit st (Foreach_iter_started { index = i; element }) in
                (* bind the current element as ctx["item"] *)
                let st = bind st "item" element in
                let st = go st body in
                let outcome = match st.terminal with
                  | Some o -> o
                  | None -> Completed_no_commit
                in
                let st = emit st (Foreach_iter_completed { index = i; outcome }) in
                (st, i + 1)
            ) (st, 0) items in
            (* Restore item binding that existed before this foreach (or remove it) *)
            let st = match prior_item with
              | Some v -> bind st "item" v
              | None -> { st with ctx = List.remove_assoc "item" st.ctx }
            in
            emit st (Foreach_completed { iterations = n })
        | Some other ->
            let msg = Printf.sprintf "foreach.over=%S is not a JSON array (got %s)"
              over (Yojson.Safe.to_string other) in
            { st with terminal = Some (Blocked msg) }
        | None ->
            let msg = Printf.sprintf "foreach.over=%S not found in ctx" over in
            { st with terminal = Some (Blocked msg) })
    | Parallel { branches } ->
        let n = List.length branches in
        (* None = branch was cancelled before completing *)
        let results : (Types.outcome * Types.trace * (string * Yojson.Safe.t) list) option array =
          Array.make n None in
        let st = emit st Parallel_started in
        (* Run all branches concurrently via Eio fibers. Cancel-all on first abort. *)
        (try
          Eio.Switch.run (fun branch_sw ->
            List.iteri (fun i branch_steps ->
              Eio.Fiber.fork ~sw:branch_sw (fun () ->
                let branch_st = { st with rev_trace = [] } in
                let branch_st =
                  (try go branch_st branch_steps
                   with
                   | Eio.Cancel.Cancelled _ as e -> raise e
                   | exn ->
                       { branch_st with
                         terminal = Some (Aborted (Printexc.to_string exn)) })
                in
                let outcome = match branch_st.terminal with
                  | Some o -> o
                  | None -> Completed_no_commit
                in
                let trace = List.rev branch_st.rev_trace in
                results.(i) <- Some (outcome, trace, branch_st.ctx);
                (* Signal sibling cancellation on branch failure *)
                (match outcome with
                 | Aborted _ | Blocked _ -> Eio.Switch.fail branch_sw Exit
                 | _ -> ()))
            ) branches
          )
        with Exit -> ());  (* expected cancellation signal *)
        (* Fill any slots that were cancelled before completing *)
        Array.iteri (fun i slot ->
          if slot = None then
            results.(i) <- Some (Aborted "cancelled-by-sibling", [], [])
        ) results;
        (* Emit one Parallel_branch_completed per branch in index order.
           Store branch_outputs so replay can reconstruct ctx without re-walking. *)
        let st = Array.fold_left (fun (st, i) result_opt ->
          let (outcome, trace, branch_ctx) = Option.get result_opt in
          let branch_outputs =
            match List.assoc_opt "outputs" branch_ctx with
            | Some (`Assoc fields) -> fields
            | _ -> []
          in
          let st = emit st (Parallel_branch_completed { branch_idx = i; trace; outcome; branch_outputs }) in
          (st, i + 1)
        ) (st, 0) results |> fst in
        (* Deep-merge outputs from all branches into host ctx.
           Each branch may have written to "outputs"; we merge them all in. *)
        let st = Array.fold_left (fun st result_opt ->
          let (_outcome, _trace, branch_ctx) = Option.get result_opt in
          match List.assoc_opt "outputs" branch_ctx with
          | None -> st
          | Some (`Assoc branch_outputs) ->
              let prior =
                match List.assoc_opt "outputs" st.ctx with
                | Some (`Assoc fields) -> fields
                | _ -> []
              in
              let merged = List.fold_left (fun acc (k, v) ->
                (k, v) :: List.remove_assoc k acc
              ) prior branch_outputs in
              bind st "outputs" (`Assoc merged)
          | Some _ -> st  (* malformed; skip *)
        ) st results in
        (* Compute worst outcome across all branches *)
        let worst = Array.fold_left (fun acc result_opt ->
          let (outcome, _, _) = Option.get result_opt in
          match (acc, outcome) with
          | Aborted r, _ -> Aborted r
          | _, Aborted r -> Aborted r
          | Blocked r, _ -> Blocked r
          | _, Blocked r -> Blocked r
          | Committed _ as c, _ -> c
          | _, (Committed _ as c) -> c
          | Completed_no_commit, Completed_no_commit -> Completed_no_commit
        ) Completed_no_commit results in
        let st = match worst with
          | Aborted _ | Blocked _ ->
              { st with terminal = Some worst }
          | _ -> st
        in
        emit st (Parallel_completed { outcome = worst })
    | Shell { id; commands; on_failure } ->
        let results = ref [] in
        let failed_cmd = ref None in
        let rec run_cmds = function
          | [] -> ()
          | cmd :: rest ->
              let exit_code = backend.Backend.run_shell_command cmd in
              results := (cmd, exit_code) :: !results;
              if exit_code <> 0 then failed_cmd := Some (cmd, exit_code)
              else run_cmds rest
        in
        run_cmds commands;
        let results_list = List.rev !results in
        let st = emit st (Shell_executed { id; results = results_list }) in
        (match !failed_cmd with
        | None -> st
        | Some (cmd, code) ->
            match on_failure with
            | Types.Continue -> st
            | Types.Abort ->
                let reason =
                  Printf.sprintf "shell step %S: command %S exited with code %d"
                    id cmd code
                in
                let st = { st with terminal = Some (Aborted reason) } in
                emit st (Blocked_at { id; reason }))
    | Evidence { id; build; check; zero_admits; tier; output } ->
        let run_cmd cmd = backend.Backend.run_shell_command cmd in
        let file_contains content pattern =
          let nc = String.length content and np = String.length pattern in
          let rec at i = i + np <= nc && (String.sub content i np = pattern || at (i + 1)) in
          np > 0 && at 0
        in
        let build_exit = run_cmd build in
        let check_exit = run_cmd check in
        let output_content =
          try In_channel.with_open_text output In_channel.input_all
          with Sys_error _ -> ""
        in
        let has_admits = file_contains output_content zero_admits in
        let passed = build_exit = 0 && check_exit = 0 && not has_admits in
        let st = emit st (Evidence_evaluated { id; tier; passed }) in
        if not passed then begin
          let reason =
            if build_exit <> 0 then
              Printf.sprintf "evidence %S: build exited with code %d" id build_exit
            else if check_exit <> 0 then
              Printf.sprintf "evidence %S: check exited with code %d" id check_exit
            else
              Printf.sprintf "evidence %S: zero_admits pattern %S found in %S" id zero_admits output
          in
          let st = { st with terminal = Some (Aborted reason) } in
          emit st (Blocked_at { id; reason })
        end else st
    | Attest { id; select; replay_domain; output } ->
        let occurrence = Option.value (List.assoc_opt id st.attest_counts) ~default:0 in
        let st = { st with attest_counts =
          (id, occurrence + 1) :: List.remove_assoc id st.attest_counts } in
        let result =
          match (attestation_signer, attestation_artifact_root,
                 attestation_session_nonce) with
          | Some signer, Some artifact_root, Some session_nonce
            when String.trim session_nonce <> "" -> (
              match Attestation.select_context st.ctx select with
              | Error e -> Error (`Failed e)
              | Ok selected ->
                  let output_path = Attestation.materialize_output_path
                    ~template:output ~occurrence in
                  (match Attestation.create ~signer ~workflow:wf
                           ~step_id:id ~occurrence ~output_path ~replay_domain
                           ~session_nonce ~selected with
                  | Error e -> Error (`Failed
                      ("attestation canonical-profile rejected selection: " ^ e))
                  | Ok envelope ->
                      (match Attestation.write_atomic ~artifact_root
                               ~relative_path:output_path envelope with
                      | Ok () -> Ok envelope
                      | Error (Attestation.Failed e) ->
                          Error (`Failed ("attestation export failed: " ^ e))
                      | Error (Attestation.Published_uncertain e) ->
                          Error (`Uncertain e))))
          | _ -> Error (`Failed "missing signer, artifact root, or non-empty session nonce")
        in
        (match result with
        | Ok envelope -> emit st (Attestation_exported { id; envelope })
        | Error (`Failed detail) ->
            let reason = Printf.sprintf "attest step %S blocked: %s" id detail in
            emit { st with terminal = Some (Blocked reason) }
              (Blocked_at { id; reason })
        | Error (`Uncertain detail) ->
            let reason = Printf.sprintf
              "published-uncertain at attest step %S: %s" id detail in
            emit { st with terminal = Some (Aborted reason) }
              (Blocked_at { id; reason }))
  (* Governed loop. Per iteration: bind loop.iter, run body, then stop if
     [until] holds OR any governor fires. The bound is a pure function of
     recorded inputs (agent outputs, budget readings, fixpoint verdicts), so the
     loop replays byte-identically even with no Max_iters. *)
  and run_loop st body until governors =
    (* consecutive non-progress counters per Fixpoint governor (by position). *)
    let fixpoint_counts = Array.make (List.length governors) 0 in
    let rec iter st index =
      if st.terminal <> None then st
      else if index >= max_loop_iters then
        (* hard engine ceiling: [index] iterations (0..index-1) already ran. *)
        emit st (Loop_stopped { iterations = index; reason = "ceiling" })
      else begin
        let st = emit st (Loop_iter { index }) in
        let st = bind_loop_iter st index in
        let st = go st body in
        if st.terminal <> None then st
        else begin
          (* 1. data-driven stop. *)
          let until_stop =
            match until with Some e -> eval st e | None -> false
          in
          if until_stop then
            emit st (Loop_stopped { iterations = index + 1; reason = "until" })
          else
            (* 2. governor checks; record everything they read. *)
            let st, fired =
              List.fold_left
                (fun (st, fired) (gi, gov) ->
                  if fired <> None then (st, fired)
                  else
                    match gov with
                    | Max_iters n ->
                        if index + 1 >= n then (st, Some "max_iters")
                        else (st, None)
                    | Budget ->
                        let v = backend.Backend.budget () in
                        let st = emit st (Budget_read { value = v }) in
                        if v <= 0 then (st, Some "budget") else (st, None)
                    | Fixpoint { window; progress } ->
                        let p = eval st progress in
                        let st = emit st (Fixpoint_progress { progress = p }) in
                        let c =
                          if p then 0 else fixpoint_counts.(gi) + 1
                        in
                        fixpoint_counts.(gi) <- c;
                        if c >= window then (st, Some "fixpoint")
                        else (st, None))
                (st, None)
                (List.mapi (fun i g -> (i, g)) governors)
            in
            match fired with
            | Some reason ->
                emit st (Loop_stopped { iterations = index + 1; reason })
            | None -> iter st (index + 1)
        end
      end
    in
    iter st 0
  in
  finish (go { rev_trace = []; ctx = initial_ctx; attest_counts = [];
               terminal = None } wf.steps)

(* ------------------------------------------------------------------ *)
(* replay: re-interpret from the recorded trace, no backend consulted. *)
(* ------------------------------------------------------------------ *)

exception Replay_mismatch of string

let replay ?(max_loop_iters = default_max_loop_iters) ?(initial_ctx = [])
    ?attestation_verifier ?attestation_session_nonce ~sw:(_sw : Eio.Switch.t)
    ~trace validated =
  let wf = Validate.Validated.workflow validated in
  (match attestation_verifier with
  | Some _ when Validate.Validated.required_attestations validated = [] ->
      raise (Replay_mismatch
        "authenticated replay requires validated required_attestations")
  | _ -> ());
  let pending = ref trace in
  let next () =
    match !pending with
    | [] -> raise (Replay_mismatch "trace exhausted before workflow completed")
    | e :: tl ->
        pending := tl;
        e
  in
  (* During replay we re-feed recorded agent outputs and recorded budget
     readings; we still re-evaluate the pure DSL over the rebuilt ctx (it is
     total and deterministic) and assert it matches the recorded verdict. *)
  let eval_ctx st e = Expr.eval ~ctx:(ctx_for st) e in
  let rec go st steps =
    match (st.terminal, steps) with
    | Some _, _ | _, [] -> st
    | None, step :: rest ->
        let st = go_step st step in
        go st rest
  and go_step st step =
    match step with
    | Agent { id; prompt = _; read_only; output_schema; on_failure;
              protocol = _; brief = _; agent_type; model = _; input } -> (
        match next () with
        | Agent_ran { success; output; id = rid; request_receipt;
            result_receipt } when rid = id -> (
            let validate_request paths request =
              let nonce = match attestation_session_nonce with
                | Some nonce -> nonce
                | None -> raise (Replay_mismatch
                    "structured Agent replay requires session nonce") in
              let canonical, input_digest = match run_input st paths with
                | Ok pair -> pair
                | Error e -> raise (Replay_mismatch
                    ("agent input selection diverged: " ^ e)) in
              let backend_id = match request with
                | `Assoc fields -> (match List.assoc_opt "backend_id" fields with
                    | Some (`String value) -> value
                    | _ -> raise (Replay_mismatch
                        "agent request backend identity missing"))
                | _ -> raise (Replay_mismatch
                    "agent request receipt is not an object") in
              let dispatch_id = sha256 (String.concat "\000"
                [ nonce; id; string_of_int (List.length st.rev_trace) ]) in
              let expected_request = `Assoc
                [ ("schema_version", `String "cwr.agent-request.v1");
                  ("dispatch_id", `String dispatch_id);
                  ("step_id", `String id);
                  ("backend_id", `String backend_id);
                  ("role", match agent_type with None -> `Null
                    | Some r -> `String r);
                  ("read_only", `Bool read_only);
                  ("input_paths", `List
                    (List.map (fun path -> `String path) paths));
                  ("input_digest", `String input_digest);
                  ("input", Yojson.Safe.from_string canonical) ] in
              if expected_request <> request then
                raise (Replay_mismatch "agent request receipt diverged");
              let request_digest =
                snd (Result.get_ok (canonical_digest request)) in
              (dispatch_id, request_digest)
            in
            let receipts = match input, request_receipt, result_receipt with
              | None, None, None -> `Legacy
              | Some paths, Some request, Some result ->
                  let dispatch_id, request_digest =
                    validate_request paths request in
                  let output_digest = match canonical_digest output with
                    | Ok (_, digest) -> digest
                    | Error _ -> raise (Replay_mismatch
                        "canonical Agent output is missing its result receipt") in
                  let expected_result = `Assoc
                    [ ("schema_version", `String "cwr.agent-result.v1");
                      ("dispatch_id", `String dispatch_id);
                      ("step_id", `String id);
                      ("request_digest", `String request_digest);
                      ("success", `Bool success);
                      ("outcome", `String (if success then "success" else "failure"));
                      ("output_digest", `String output_digest);
                      ("output", output) ] in
                  if expected_result <> result then
                    raise (Replay_mismatch "agent result receipt diverged");
                  `Complete (request, result)
              | Some paths, Some request, None ->
                  ignore (validate_request paths request);
                  (match canonical_digest output with
                  | Error _ -> `Noncanonical
                  | Ok _ -> raise (Replay_mismatch
                      "agent result receipt missing for canonical output"))
              | _ -> raise (Replay_mismatch "agent receipt presence mismatch") in
            let st = emit st (Agent_ran { id; success; output;
              request_receipt; result_receipt }) in
            let st = bind_output st id output in
            let st = match receipts with
              | `Complete (request, result) -> bind_receipts st id request result
              | `Legacy | `Noncanonical -> st in
            if receipts = `Noncanonical then
              (match next () with
              | Blocked_at { id = bid; reason } when bid = id ->
                  let st = emit st (Blocked_at { id; reason }) in
                  { st with terminal = Some (Aborted reason) }
              | _ -> raise (Replay_mismatch
                  "non-canonical agent block entry mismatch"))
            else if not success then begin
              match on_failure with
              | Types.Continue ->
                  (* the recorded run SOFT-failed and continued: no Blocked_at was
                     emitted, the walk proceeded. Mirror it exactly. *)
                  st
              | Types.Abort ->
                  (* the recorded run aborted here (fail-closed); consume its
                     Blocked_at and reproduce the Aborted outcome. *)
                  let reason =
                    Printf.sprintf
                      "agent step %S did not produce a successful structured \
                       output"
                      id
                  in
                  (match next () with
                  | Blocked_at { id = bid; reason = _ } when bid = id ->
                      let st = { st with terminal = Some (Aborted reason) } in
                      emit st (Blocked_at { id; reason })
                  | _ -> raise (Replay_mismatch "agent block entry mismatch"))
            end
            else
              match output_schema with
              | Some schema -> (
                  match Schema.validate schema output with
                  | Ok () -> st
                  | Error field ->
                      let reason = Printf.sprintf "schema mismatch: %s" field in
                      (match next () with
                      | Blocked_at { id = bid; reason = recorded }
                        when bid = id && recorded = reason ->
                          let st = emit st (Blocked_at { id; reason }) in
                          { st with terminal = Some (Aborted reason) }
                      | _ -> raise (Replay_mismatch
                          "agent schema block entry mismatch")))
              | None -> st)
        | _ -> raise (Replay_mismatch "agent entry mismatch"))
    | Gate { id; when_ } -> (
        match next () with
        | Gate_evaluated { verdict; id = rid } when rid = id -> (
            let recomputed = if eval_ctx st when_ then Pass else Fail in
            if recomputed <> verdict then
              raise (Replay_mismatch "gate verdict diverged");
            let st = emit st (Gate_evaluated { id; verdict }) in
            match verdict with
            | Pass -> st
            | Fail -> (
                (* the recorded run blocked here; consume its Blocked_at. *)
                match next () with
                | Blocked_at { id = rid; reason } when rid = id ->
                    let st = emit st (Blocked_at { id; reason }) in
                    { st with terminal = Some (Blocked reason) }
                | _ -> raise (Replay_mismatch "gate block entry mismatch")))
        | _ -> raise (Replay_mismatch "gate entry mismatch"))
    | Branch { when_; then_; else_ } -> (
        match next () with
        | Branch_taken { verdict } ->
            let recomputed = if eval_ctx st when_ then Pass else Fail in
            if recomputed <> verdict then
              raise (Replay_mismatch "branch verdict diverged");
            let st = emit st (Branch_taken { verdict }) in
            let chosen = match verdict with Pass -> then_ | Fail -> else_ in
            go st chosen
        | _ -> raise (Replay_mismatch "branch entry mismatch"))
    | Loop { body; until; governors } ->
        replay_loop st body until governors
    | Run { id; cmd = _; working_dir = _; timeout_ms = _; observe = _;
            input; stdout_schema; executable_digest } -> (
        (* NEVER re-execute: re-feed the recorded result (or reproduce the
           recorded allowlist-Blocked). The allowlist is NOT consulted on replay
           (nothing executes), mirroring the Agent_ran replay arm. *)
        match next () with
        | Run_executed { id = rid; input_digest; parsed; result; executable }
          when rid = id ->
            (match executable_digest, executable with
            | None, None -> ()
            | Some expected, Some identity when identity.digest = expected -> ()
            | _ -> raise (Replay_mismatch
                "run executable identity diverged"));
            let expected_digest =
              match input with
              | None -> None
              | Some paths ->
                  (match run_input st paths with
                  | Ok (_, digest) -> Some digest
                  | Error e -> raise (Replay_mismatch ("run input selection diverged: " ^ e)))
            in
            if expected_digest <> input_digest then
              raise (Replay_mismatch "run input digest diverged");
            let st = emit st (Run_executed { id; input_digest; parsed; result;
              executable }) in
            (match stdout_schema, parsed with
            | None, None -> bind_output st id
                (run_output_json ~input_digest ~parsed ~executable result)
            | Some schema, Some json ->
                (match parse_run_stdout schema result with
                | Ok recomputed when recomputed = json ->
                    bind_output st id
                      (run_output_json ~input_digest ~parsed ~executable result)
                | _ -> raise (Replay_mismatch "run parsed stdout diverged"))
            | Some schema, None ->
                (match parse_run_stdout schema result with
                | Ok _ -> raise (Replay_mismatch "run parsed stdout missing")
                | Error _ ->
                    (match next () with
                    | Blocked_at { id = rid; reason } when rid = id ->
                        let st = emit st (Blocked_at { id; reason }) in
                        { st with terminal = Some (Aborted reason) }
                    | _ -> raise (Replay_mismatch "run stdout rejection entry mismatch")))
            | None, Some _ ->
                raise (Replay_mismatch "unexpected run parsed stdout binding"))
        | Blocked_at { id = rid; reason } when rid = id ->
            let st = emit st (Blocked_at { id; reason }) in
            let input_failure_prefix = "run input selection failed:" in
            let is_input_failure =
              String.length reason >= String.length input_failure_prefix
              && String.sub reason 0 (String.length input_failure_prefix)
                 = input_failure_prefix
            in
            { st with
              terminal =
                Some
                  (if is_input_failure then Aborted reason else Blocked reason) }
        | _ -> raise (Replay_mismatch "run entry mismatch"))
    | Commit { id; preflight } -> (
        let replay_committed st expected_receipt expected_digest =
          match next () with
          | Committed_step { id = rid; token_digest; preflight_receipt;
              preflight_receipt_digest } when rid = id ->
              if preflight_receipt <> expected_receipt
                 || preflight_receipt_digest <> expected_digest then
                raise (Replay_mismatch "commit preflight binding diverged");
              let st = emit st (Committed_step { id; token_digest;
                preflight_receipt; preflight_receipt_digest }) in
              { st with terminal = Some (Committed { id; token_digest }) }
          | _ -> raise (Replay_mismatch "commit entry mismatch")
        in
        match preflight with
        | None ->
            (match next () with
            | Committed_step { id = rid; token_digest;
                preflight_receipt = None; preflight_receipt_digest = None }
              when rid = id ->
                let st = emit st (Committed_step { id; token_digest;
                  preflight_receipt = None; preflight_receipt_digest = None }) in
                { st with terminal = Some (Committed { id; token_digest }) }
            | Blocked_at { id = rid; reason } when rid = id ->
                let st = emit st (Blocked_at { id; reason }) in
                { st with terminal = Some (Blocked reason) }
            | _ -> raise (Replay_mismatch "legacy commit entry mismatch"))
        | Some { input; stdout_schema; lock_file; _ } ->
            (match next () with
            | Commit_preflight_executed { id = rid; input_digest; parsed;
                result; receipt; lock_file = recorded_lock_file;
                lock_identity; lock_identity_valid } when rid = id ->
                if recorded_lock_file <> lock_file then
                  raise (Replay_mismatch
                    "commit preflight lock declaration diverged");
                let validated_lock_marker = match lock_file, lock_identity with
                  | None, None -> None
                  | Some path, Some (`Assoc fields as marker) ->
                      let device = match List.assoc_opt "device" fields with
                        | Some (`String value) -> value
                        | _ -> raise (Replay_mismatch
                            "commit preflight lock device missing") in
                      let inode = match List.assoc_opt "inode" fields with
                        | Some (`String value) -> value
                        | _ -> raise (Replay_mismatch
                            "commit preflight lock inode missing") in
                      let expected = commit_lock_json path
                        { Secure_fs.device = device; inode } in
                      if marker <> expected then raise (Replay_mismatch
                        "commit preflight lock marker diverged");
                      Some marker
                  | _ -> raise (Replay_mismatch
                      "commit preflight lock marker presence diverged") in
                let expected_input_digest = match
                    commit_preflight_input st input validated_lock_marker with
                  | Ok (_, digest) -> digest
                  | Error e -> raise (Replay_mismatch
                      ("commit preflight input selection diverged: " ^ e)) in
                if input_digest <> expected_input_digest then
                  raise (Replay_mismatch
                    "commit preflight input digest diverged");
                let recomputed_parsed =
                  if result.exit <> 0 then Error
                    (Printf.sprintf "process exited %d" result.exit)
                  else parse_run_stdout stdout_schema result in
                let receipt_permitted = match lock_file, lock_identity_valid with
                  | None, None -> true
                  | Some _, Some true -> true
                  | Some _, Some false -> false
                  | Some _, None when Result.is_error recomputed_parsed -> false
                  | _ -> raise (Replay_mismatch
                      "commit preflight lock validation verdict diverged") in
                let expected_receipt = match recomputed_parsed,
                    receipt_permitted with
                  | Error _, _ -> None
                  | Ok _, false -> None
                  | Ok json, true ->
                      if parsed <> Some json then
                        raise (Replay_mismatch
                          "commit preflight parsed output diverged");
                      (match commit_preflight_receipt ~id ~input_digest
                          ~result ~parsed:json
                          ~lock_identity:validated_lock_marker with
                      | Ok (value, _) -> Some value
                      | Error e -> raise (Replay_mismatch
                          ("commit preflight receipt invalid: " ^ e))) in
                if receipt <> expected_receipt then
                  raise (Replay_mismatch "commit preflight receipt diverged");
                if Result.is_error recomputed_parsed && parsed <> None then
                  raise (Replay_mismatch
                    "rejected commit preflight unexpectedly has parsed output");
                let st = emit st (Commit_preflight_executed {
                  id; input_digest; parsed; result; receipt;
                  lock_file; lock_identity = validated_lock_marker;
                  lock_identity_valid }) in
                (match expected_receipt with
                | None ->
                    (match next () with
                    | Blocked_at { id = rid; reason } when rid = id ->
                        let st = emit st (Blocked_at { id; reason }) in
                        { st with terminal = Some (Blocked reason) }
                    | _ -> raise (Replay_mismatch
                        "commit preflight rejection entry mismatch"))
                | Some value ->
                    let digest = snd (Result.get_ok (canonical_digest value)) in
                    replay_committed st (Some value) (Some digest))
            | Blocked_at { id = rid; reason } when rid = id ->
                let st = emit st (Blocked_at { id; reason }) in
                { st with terminal = Some (Blocked reason) }
            | _ -> raise (Replay_mismatch
                "commit preflight entry mismatch")))
    | Foreach { over; steps = body } -> (
        (* Consume Parallel_started is wrong here — consume Foreach markers.
           The global [pending] ref is walked in iteration order:
             Foreach_iter_started{index=0; element=...}
             <body sub-trace for iteration 0>
             Foreach_iter_completed{index=0; outcome=...}
             ...
             Foreach_completed{iterations=N}
           We check whether the ctx key was an array and act accordingly. *)
        match List.assoc_opt over st.ctx with
        | Some (`List items) ->
            let prior_item = List.assoc_opt "item" st.ctx in
            let st, n = List.fold_left (fun (st, i) element ->
              match next () with
              | Foreach_iter_started { index = ri; element = re } when ri = i ->
                  if re <> element then
                    raise (Replay_mismatch "foreach element mismatch");
                  let st = emit st (Foreach_iter_started { index = i; element }) in
                  let st = bind st "item" element in
                  let st = go st body in
                  let outcome = match st.terminal with
                    | Some o -> o
                    | None -> Completed_no_commit
                  in
                  (match next () with
                   | Foreach_iter_completed { index = ri; outcome = ro } when ri = i ->
                       if ro <> outcome then
                         raise (Replay_mismatch "foreach iter outcome mismatch");
                       let st = emit st (Foreach_iter_completed { index = i; outcome }) in
                       (st, i + 1)
                   | _ -> raise (Replay_mismatch "foreach_iter_completed entry mismatch"))
              | _ -> raise (Replay_mismatch "foreach_iter_started entry mismatch")
            ) (st, 0) items in
            (* Restore item binding that existed before this foreach (or remove it) *)
            let st = match prior_item with
              | Some v -> bind st "item" v
              | None -> { st with ctx = List.remove_assoc "item" st.ctx }
            in
            (match next () with
             | Foreach_completed { iterations = ri } when ri = n ->
                 emit st (Foreach_completed { iterations = n })
             | _ -> raise (Replay_mismatch "foreach_completed entry mismatch"))
        | Some other ->
            (* Non-array value: run set Blocked; reproduce it here. *)
            let msg = Printf.sprintf "foreach.over=%S is not a JSON array (got %s)"
              over (Yojson.Safe.to_string other) in
            { st with terminal = Some (Blocked msg) }
        | None ->
            (* Missing key: run set Blocked; reproduce it here. *)
            let msg = Printf.sprintf "foreach.over=%S not found in ctx" over in
            { st with terminal = Some (Blocked msg) })
    | Parallel { branches } -> (
        (* Consume: Parallel_started,
                   Parallel_branch_completed{branch_idx=0; trace=[...]; outcome}
                   ...
                   Parallel_branch_completed{branch_idx=n-1; ...}
                   Parallel_completed{outcome}
           Each branch's sub-trace is replayed using a LOCAL ref, not pending. *)
        match next () with
        | Parallel_started ->
            let st = emit st Parallel_started in
            let n = List.length branches in
            (* Replay each branch using its embedded sub-trace. *)
            let st, worst = List.fold_left (fun (st, worst) (i, branch_steps) ->
              match next () with
              | Parallel_branch_completed { branch_idx = ri; trace; outcome; branch_outputs } when ri = i ->
                  let _ = branch_steps in  (* sub-trace already encodes branch body *)
                  let st = emit st (Parallel_branch_completed { branch_idx = i; trace; outcome; branch_outputs }) in
                  (* Restore branch outputs into host ctx using the stored snapshot. *)
                  let st =
                    if branch_outputs = [] then st
                    else
                      let prior =
                        match List.assoc_opt "outputs" st.ctx with
                        | Some (`Assoc fields) -> fields
                        | _ -> []
                      in
                      let merged = List.fold_left (fun acc (k, v) ->
                        (k, v) :: List.remove_assoc k acc
                      ) prior branch_outputs in
                      bind st "outputs" (`Assoc merged)
                  in
                  let worst = match (worst, outcome) with
                    | Aborted r, _ | _, Aborted r -> Aborted r
                    | Blocked r, _ | _, Blocked r -> Blocked r
                    | Committed _ as c, _ | _, (Committed _ as c) -> c
                    | Completed_no_commit, Completed_no_commit -> Completed_no_commit
                  in
                  (st, worst)
              | _ -> raise (Replay_mismatch
                  (Printf.sprintf "parallel_branch_completed[%d] mismatch" i))
            ) (st, Completed_no_commit) (List.mapi (fun i b -> (i, b)) branches) in
            let st = match worst with
              | Aborted _ | Blocked _ ->
                  { st with terminal = Some worst }
              | _ -> st
            in
            let _ = n in
            (match next () with
             | Parallel_completed { outcome = ro } when ro = worst ->
                 emit st (Parallel_completed { outcome = worst })
             | _ -> raise (Replay_mismatch "parallel_completed entry mismatch"))
        | _ -> raise (Replay_mismatch "parallel_started entry mismatch"))
    | Shell { id; commands = _; on_failure } -> (
        match next () with
        | Shell_executed { id = rid; results } when rid = id ->
            let st = emit st (Shell_executed { id; results }) in
            let failed = List.find_opt (fun (_, code) -> code <> 0) results in
            (match failed with
            | None -> st
            | Some (cmd, code) ->
                match on_failure with
                | Types.Continue -> st
                | Types.Abort ->
                    let reason =
                      Printf.sprintf "shell step %S: command %S exited with code %d"
                        id cmd code
                    in
                    (match next () with
                    | Blocked_at { id = bid; reason = _ } when bid = id ->
                        let st = { st with terminal = Some (Aborted reason) } in
                        emit st (Blocked_at { id; reason })
                    | _ -> raise (Replay_mismatch "shell block entry mismatch")))
        | _ -> raise (Replay_mismatch "shell_executed entry mismatch"))
    | Evidence { id; build = _; check = _; zero_admits = _; tier = _; output = _ } -> (
        match next () with
        | Evidence_evaluated { id = rid; tier; passed } when rid = id ->
            let st = emit st (Evidence_evaluated { id; tier; passed }) in
            if not passed then begin
              match next () with
              | Blocked_at { id = bid; reason } when bid = id ->
                  let st = { st with terminal = Some (Aborted reason) } in
                  emit st (Blocked_at { id; reason })
              | _ -> raise (Replay_mismatch "evidence block entry mismatch")
            end else st
        | _ -> raise (Replay_mismatch "evidence_evaluated entry mismatch"))
    | Attest { id; select; replay_domain; output } -> (
        let occurrence = Option.value (List.assoc_opt id st.attest_counts) ~default:0 in
        let st = { st with attest_counts =
          (id, occurrence + 1) :: List.remove_assoc id st.attest_counts } in
        match next () with
        | Attestation_exported { id = rid; envelope } when rid = id ->
            let selected = match Attestation.select_context st.ctx select with
              | Ok values -> values
              | Error e -> raise (Replay_mismatch
                  ("attestation selection failed: " ^ e)) in
            (match (attestation_verifier, attestation_session_nonce) with
            | Some verifier, Some session_nonce when String.trim session_nonce <> "" ->
                let output_path = Attestation.materialize_output_path
                  ~template:output ~occurrence in
                (match Attestation.verify ~verifier ~workflow:wf ~step_id:id
                         ~occurrence ~output_path ~replay_domain ~session_nonce
                         ~selected envelope with
                | Ok () -> emit st (Attestation_exported { id; envelope })
                | Error e -> raise (Replay_mismatch
                    ("attestation verification failed: " ^ e)))
            | _ -> raise (Replay_mismatch
                "attestation replay requires a pinned public key and session nonce"))
        | Blocked_at { id = rid; reason } when rid = id ->
            let st = emit st (Blocked_at { id; reason }) in
            { st with terminal = Some (Blocked reason) }
        | _ -> raise (Replay_mismatch "attestation entry mismatch"))
  and replay_loop st body until governors =
    let fixpoint_counts = Array.make (List.length governors) 0 in
    let rec iter st index =
      if st.terminal <> None then st
      else if index >= max_loop_iters then
        (* mirror run: the ceiling stops the loop here; consume its entry. *)
        consume_stop st index "ceiling"
      else
        match next () with
        | Loop_iter { index = ri } when ri = index ->
            let st = emit st (Loop_iter { index }) in
            let st = bind_loop_iter st index in
            let st = go st body in
            if st.terminal <> None then st
            else
              let until_stop =
                match until with Some e -> eval_ctx st e | None -> false
              in
              if until_stop then consume_stop st (index + 1) "until"
              else
                let st, fired =
                  List.fold_left
                    (fun (st, fired) (gi, gov) ->
                      if fired <> None then (st, fired)
                      else
                        match gov with
                        | Max_iters n ->
                            if index + 1 >= n then (st, Some "max_iters")
                            else (st, None)
                        | Budget -> (
                            match next () with
                            | Budget_read { value } ->
                                let st = emit st (Budget_read { value }) in
                                if value <= 0 then (st, Some "budget")
                                else (st, None)
                            | _ ->
                                raise
                                  (Replay_mismatch "budget reading mismatch"))
                        | Fixpoint { window; progress } -> (
                            match next () with
                            | Fixpoint_progress { progress = recorded } ->
                                let recomputed = eval_ctx st progress in
                                if recomputed <> recorded then
                                  raise
                                    (Replay_mismatch
                                       "fixpoint progress diverged");
                                let st =
                                  emit st
                                    (Fixpoint_progress { progress = recorded })
                                in
                                let c =
                                  if recorded then 0
                                  else fixpoint_counts.(gi) + 1
                                in
                                fixpoint_counts.(gi) <- c;
                                if c >= window then (st, Some "fixpoint")
                                else (st, None)
                            | _ ->
                                raise
                                  (Replay_mismatch "fixpoint verdict mismatch")))
                    (st, None)
                    (List.mapi (fun i g -> (i, g)) governors)
                in
                (match fired with
                | Some reason -> consume_stop st (index + 1) reason
                | None -> iter st (index + 1))
        | _ -> raise (Replay_mismatch "loop iter entry mismatch")
    (* Consume the recorded Loop_stopped entry, asserting it matches. *)
    and consume_stop st iterations reason =
      match next () with
      | Loop_stopped { iterations = ri; reason = rr }
        when ri = iterations && rr = reason ->
          emit st (Loop_stopped { iterations; reason })
      | _ -> raise (Replay_mismatch "loop stop entry mismatch")
    in
    iter st 0
  in
  let outcome, _trace =
    finish (go { rev_trace = []; ctx = initial_ctx; attest_counts = [];
                 terminal = None } wf.steps)
  in
  (* The walk must have consumed the WHOLE trace: a trace that is a valid prefix
     followed by extra (garbage) entries must NOT replay successfully. *)
  if !pending <> [] then
    raise (Replay_mismatch "trailing trace entries after workflow completed");
  outcome
