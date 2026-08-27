open Types
open Engine_state
open Engine_dynamic_parallel

(* ------------------------------------------------------------------ *)
(* run: deterministic interpreter driven by a backend.                 *)
(* ------------------------------------------------------------------ *)

let run ?(max_loop_iters = default_max_loop_iters) ?(run_allowlist = [])
    ?(initial_ctx = []) ?attestation_signer ?attestation_artifact_root
    ?attestation_session_nonce ?agent_backend_id ?deadline
    ?(now = Unix.gettimeofday) ~sw:(_sw : Eio.Switch.t) ~backend ~token
    validated =
  let wf = Validate.Validated.workflow validated in
  let agent ~id ~prompt ~read_only ~agent_type ~model ~output_schema =
    backend.Backend.run_agent ~id ~prompt ~read_only ~agent_type ~model
      ~output_schema
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
        (* The step's declared schema goes to the backend as a native constraint
           (a guide), and is STILL validated below on the way back (the
           guarantee). Belt and braces: a backend may ignore --json-schema, or
           have no such mechanism at all. *)
        let success, output =
          agent ~id ~prompt:effective_prompt ~read_only ~agent_type ~model
            ~output_schema
        in
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
    | Spawn { id; children } ->
        let st = emit st (Spawn_started { id }) in
        let st, completed = List.fold_left (fun (st, n) (child : spawn_child) ->
          if st.terminal <> None then (st, n)
          else
            let st = go st child.steps in
            let outcome = match st.terminal with Some value -> value | None -> Completed_no_commit in
            let st = emit st (Spawn_child_completed { spawn_id = id; child_id = child.id;
              outcome; ctx = st.ctx }) in
            (st, n + 1)) (st, 0) children in
        let outcome = match st.terminal with Some value -> value | None -> Completed_no_commit in
        emit st (Spawn_completed { id; children = completed; outcome })
    | Dynamic_parallel { id; over; steps = body } -> (
        match resolve_dynamic_parallel_over st.ctx over with
        | Error msg ->
            let st = emit st (Blocked_at { id; reason = msg }) in
            { st with terminal = Some (Blocked msg) }
        | Ok keys ->
            let n = List.length keys in
            let template_ids = dynamic_parallel_template_ids body in
            let st = emit st (Dynamic_parallel_started { id; branches = n }) in
            (* None = branch cancelled (a sibling raised) before it completed.
               Each branch's final [attest_counts] is captured too (4th
               element) — inherited from the host's counts at fork time and
               possibly incremented by its own Attest steps — so a repeat
               dispatch of the SAME runtime key (e.g. this whole
               Dynamic_parallel step retried by an enclosing Loop) sees
               occurrence continue from where the previous dispatch left off,
               instead of colliding on occurrence 0 again. *)
            let results
                : (Types.outcome * Types.trace * (string * Yojson.Safe.t) list
                  * (string * int) list)
                  option array
              =
              Array.make n None
            in
            (try
               Eio.Switch.run (fun branch_sw ->
                   List.iteri
                     (fun i key ->
                       Eio.Fiber.fork ~sw:branch_sw (fun () ->
                           let branch_steps =
                             dynamic_parallel_branch_steps ~template_ids ~key
                               ~branch_idx:i body
                           in
                           let branch_st0 =
                             bind { st with rev_trace = [] } "item" (`String key)
                           in
                           let branch_st =
                             try go branch_st0 branch_steps with
                             | Eio.Cancel.Cancelled _ as e -> raise e
                             | exn ->
                                 {
                                   branch_st0 with
                                   terminal = Some (Aborted (Printexc.to_string exn));
                                 }
                           in
                           let outcome =
                             match branch_st.terminal with
                             | Some o -> o
                             | None -> Completed_no_commit
                           in
                           let trace = List.rev branch_st.rev_trace in
                           results.(i) <-
                             Some (outcome, trace, branch_st.ctx, branch_st.attest_counts);
                           match outcome with
                           | Aborted _ | Blocked _ -> Eio.Switch.fail branch_sw Exit
                           | _ -> ()))
                     keys)
             with Exit -> ());
            (* Fill any slots left in flight when the switch failed: a
               genuinely distinct [Cancelled] outcome, never conflated with
               "never started" (this branch DID begin) or [Aborted]/[Blocked]
               (it never got the chance to fail on its own terms). *)
            Array.iteri
              (fun i slot ->
                if slot = None then
                  results.(i) <-
                    Some
                      ( Cancelled
                          "a sibling Dynamic_parallel branch raised an error \
                           before this branch could finish",
                        [], [], [] ))
              results;
            let st =
              List.fold_left
                (fun (st, i) result_opt ->
                  let outcome, trace, branch_ctx, _ = Option.get result_opt in
                  let branch_outputs =
                    match List.assoc_opt "outputs" branch_ctx with
                    | Some (`Assoc fields) -> fields
                    | _ -> []
                  in
                  let st =
                    emit st
                      (Dynamic_parallel_branch_completed
                         { id; branch_idx = i; key = List.nth keys i; trace;
                           outcome; branch_outputs })
                  in
                  (st, i + 1))
                (st, 0) (Array.to_list results)
              |> fst
            in
            (* Every branch's step ids are already namespaced ("<id>#<key>"),
               so merging outputs is a plain union — no collision arbitration
               needed (unlike Parallel's static branches, which can genuinely
               collide on a shared literal id). *)
            let st =
              Array.fold_left
                (fun st result_opt ->
                  let _outcome, _trace, branch_ctx, _ = Option.get result_opt in
                  match List.assoc_opt "outputs" branch_ctx with
                  | None -> st
                  | Some (`Assoc branch_outputs) ->
                      let prior =
                        match List.assoc_opt "outputs" st.ctx with
                        | Some (`Assoc fields) -> fields
                        | _ -> []
                      in
                      let merged =
                        List.fold_left
                          (fun acc (k, v) -> (k, v) :: List.remove_assoc k acc)
                          prior branch_outputs
                      in
                      bind st "outputs" (`Assoc merged)
                  | Some _ -> st)
                st results
            in
            (* Merge every branch's attest occurrence counts back into the
               host, index-ordered (deterministic, replay-reproducible). *)
            let st =
              Array.fold_left
                (fun st result_opt ->
                  let _, _, _, branch_counts = Option.get result_opt in
                  let merged =
                    List.fold_left
                      (fun acc (aid, count) -> (aid, count) :: List.remove_assoc aid acc)
                      st.attest_counts branch_counts
                  in
                  { st with attest_counts = merged })
                st results
            in
            let failed =
              List.mapi (fun i key -> (i, key)) keys
              |> List.filter_map (fun (i, key) ->
                     match Option.get results.(i) with
                     | (Aborted reason, _, _, _) | (Blocked reason, _, _, _) ->
                         Some (key, reason)
                     | _ -> None)
            in
            let outcome =
              if failed = [] then Completed_no_commit
              else
                let summary =
                  String.concat "; "
                    (List.map (fun (key, reason) -> Printf.sprintf "%s: %s" key reason)
                       failed)
                in
                Aborted
                  (Printf.sprintf
                     "dynamic_parallel %S: %d branch(es) raised an error — %s" id
                     (List.length failed) summary)
            in
            let st =
              match outcome with
              | Aborted _ -> { st with terminal = Some outcome }
              | _ -> st
            in
            emit st (Dynamic_parallel_completed { id; branches = n; outcome; failed }))
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
          (* [Cancelled] is a Dynamic_parallel-only outcome; Parallel never
             produces it, but the shared [outcome] type still requires an
             exhaustive match. Treated with the same fail-loud priority as
             [Aborted] — dead code for Parallel today, not a behavior change. *)
          | Aborted r, _ -> Aborted r
          | _, Aborted r -> Aborted r
          | Cancelled r, _ -> Cancelled r
          | _, Cancelled r -> Cancelled r
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
                    | Deadline ->
                        (* Nondeterministic external reading, exactly like
                           Budget: record the VERDICT (a bool, so the ledger
                           round-trips byte-identically — never a float) and let
                           replay re-feed it. No deadline supplied => never
                           fires, the same posture as Budget against a constant
                           stub. *)
                        let expired =
                          match deadline with
                          | Some d -> now () >= d
                          | None -> false
                        in
                        let st = emit st (Deadline_read { expired }) in
                        if expired then (st, Some "deadline") else (st, None)
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
