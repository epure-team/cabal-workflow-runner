open Types
open Engine_state
open Engine_dynamic_parallel

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
  (* A [next] cursor over some trace segment: [make_cursor l] returns a
     [pending] ref (for the trailing-garbage check) and a [next] function that
     pops one entry at a time, failing closed once exhausted. Every
     Dynamic_parallel branch gets its OWN cursor (over that branch's own
     recorded sub-trace) rather than sharing the host [pending] ref, which is
     what lets a branch's Attest genuinely reach Attestation.verify (see the
     Dynamic_parallel arm below) instead of Parallel's discard-and-reproduce
     replay (which intentionally stays untouched). *)
  let make_cursor l =
    let pending = ref l in
    let next () =
      match !pending with
      | [] -> raise (Replay_mismatch "trace exhausted before workflow completed")
      | e :: tl ->
          pending := tl;
          e
    in
    (pending, next)
  in
  (* During replay we re-feed recorded agent outputs and recorded budget
     readings; we still re-evaluate the pure DSL over the rebuilt ctx (it is
     total and deterministic) and assert it matches the recorded verdict. *)
  let eval_ctx st e = Expr.eval ~ctx:(ctx_for st) e in
  (* The walker is parameterized over [next] so it can be instantiated once
     for the host trace and again, independently, for each Dynamic_parallel
     branch's own sub-trace. *)
  let rec make_walker ~next =
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
    | Spawn { id; children } -> (
        match next () with
        | Spawn_started { id = recorded } when recorded = id ->
            let st = emit st (Spawn_started { id }) in
            let st, completed = List.fold_left (fun (st, n) (child : spawn_child) ->
              if st.terminal <> None then (st, n)
              else
                let st = go st child.steps in
                let outcome = match st.terminal with Some value -> value | None -> Completed_no_commit in
                match next () with
                | Spawn_child_completed { spawn_id; child_id; outcome = recorded_outcome; ctx }
                  when spawn_id = id && child_id = child.id && recorded_outcome = outcome && ctx = st.ctx ->
                    (emit st (Spawn_child_completed { spawn_id = id; child_id = child.id;
                       outcome; ctx = st.ctx }), n + 1)
                | _ -> raise (Replay_mismatch "spawn child evidence mismatch")) (st, 0) children in
            let outcome = match st.terminal with Some value -> value | None -> Completed_no_commit in
            (match next () with
             | Spawn_completed { id = recorded; children = count; outcome = recorded_outcome }
               when recorded = id && count = completed && recorded_outcome = outcome ->
                 emit st (Spawn_completed { id; children = completed; outcome })
             | _ -> raise (Replay_mismatch "spawn completion evidence mismatch"))
        | _ -> raise (Replay_mismatch "spawn start evidence mismatch"))
    | Dynamic_parallel { id; over; steps = body } -> (
        match resolve_dynamic_parallel_over st.ctx over with
        | Error msg -> (
            match next () with
            | Blocked_at { id = rid; reason } when rid = id && reason = msg ->
                let st = emit st (Blocked_at { id; reason }) in
                { st with terminal = Some (Blocked reason) }
            | _ -> raise (Replay_mismatch
                "dynamic_parallel resolution-block entry mismatch"))
        | Ok keys -> (
            let n = List.length keys in
            let template_ids = dynamic_parallel_template_ids body in
            match next () with
            | Dynamic_parallel_started { id = recorded; branches = rn }
              when recorded = id && rn = n ->
                let st = emit st (Dynamic_parallel_started { id; branches = n }) in
                (* Each branch replays against its OWN independent local
                   cursor over its own recorded sub-trace (via [make_walker]),
                   recursively dispatched exactly like Foreach/Spawn's shared
                   cursor — never Parallel's discard-and-reproduce. This is
                   what lets a branch's Attest genuinely reach
                   Attestation.verify. *)
                (* [st_before]'s ctx is every branch's baseline (mirroring
                   [run]: all branches fork from the SAME pre-dispatch host
                   ctx — none of them see a SIBLING's merged outputs). Only
                   [attest_counts] threads incrementally across branches (via
                   [st] below), which is safe because disjoint per-branch
                   Attest ids never collide within one dispatch and is
                   required for cross-DISPATCH occurrence continuation (a
                   retried Dynamic_parallel step). *)
                let st_before = st in
                let rec loop st i failed =
                  if i >= n then (st, List.rev failed)
                  else
                    let key = List.nth keys i in
                    match next () with
                    | Dynamic_parallel_branch_completed
                        { id = did; branch_idx; key = rkey; trace = branch_trace;
                          outcome = recorded_outcome; branch_outputs }
                      when did = id && branch_idx = i ->
                        if rkey <> key then
                          raise (Replay_mismatch
                            "dynamic_parallel branch key diverged");
                        let merge_branch_outputs st =
                          if branch_outputs = [] then st
                          else
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
                        in
                        (match recorded_outcome with
                        | Cancelled _ ->
                            (* Never durably ran: nothing to recompute. *)
                            let st = emit st (Dynamic_parallel_branch_completed
                              { id; branch_idx = i; key; trace = branch_trace;
                                outcome = recorded_outcome; branch_outputs }) in
                            loop (merge_branch_outputs st) (i + 1) failed
                        | _ ->
                            let branch_pending, branch_next = make_cursor branch_trace in
                            let branch_go, _ = make_walker ~next:branch_next in
                            let branch_steps =
                              dynamic_parallel_branch_steps ~template_ids ~key
                                ~branch_idx:i body
                            in
                            let branch_st0 =
                              bind
                                { st_before with rev_trace = [];
                                  attest_counts = st.attest_counts }
                                "item" (`String key)
                            in
                            let branch_st = branch_go branch_st0 branch_steps in
                            let recomputed_outcome =
                              match branch_st.terminal with
                              | Some o -> o
                              | None -> Completed_no_commit
                            in
                            if recomputed_outcome <> recorded_outcome then
                              raise (Replay_mismatch
                                "dynamic_parallel branch outcome diverged");
                            if !branch_pending <> [] then
                              raise (Replay_mismatch
                                "dynamic_parallel branch trace has trailing entries");
                            let recomputed_outputs =
                              match List.assoc_opt "outputs" branch_st.ctx with
                              | Some (`Assoc fields) -> fields
                              | _ -> []
                            in
                            if recomputed_outputs <> branch_outputs then
                              raise (Replay_mismatch
                                "dynamic_parallel branch outputs diverged");
                            let st = emit st (Dynamic_parallel_branch_completed
                              { id; branch_idx = i; key; trace = branch_trace;
                                outcome = recorded_outcome; branch_outputs }) in
                            let failed = match recorded_outcome with
                              | Aborted reason | Blocked reason -> (key, reason) :: failed
                              | _ -> failed
                            in
                            (* Merge this branch's attest occurrence counts
                               back into the host, same as [run]: a repeat
                               dispatch of this key sees occurrence continue
                               from here rather than colliding on 0 again. *)
                            let st =
                              { (merge_branch_outputs st) with
                                attest_counts = branch_st.attest_counts }
                            in
                            loop st (i + 1) failed)
                    | _ -> raise (Replay_mismatch
                        "dynamic_parallel branch entry mismatch")
                in
                let st, failed = loop st 0 [] in
                let outcome =
                  if failed = [] then Completed_no_commit
                  else
                    let summary =
                      String.concat "; "
                        (List.map
                           (fun (key, reason) -> Printf.sprintf "%s: %s" key reason)
                           failed)
                    in
                    Aborted
                      (Printf.sprintf
                         "dynamic_parallel %S: %d branch(es) raised an error — %s"
                         id (List.length failed) summary)
                in
                let st =
                  match outcome with
                  | Aborted _ -> { st with terminal = Some outcome }
                  | _ -> st
                in
                (match next () with
                | Dynamic_parallel_completed
                    { id = rid; branches = rbranches; outcome = ro; failed = rf }
                  when rid = id && rbranches = n && ro = outcome && rf = failed ->
                    emit st
                      (Dynamic_parallel_completed { id; branches = n; outcome; failed })
                | _ -> raise (Replay_mismatch
                    "dynamic_parallel completion evidence mismatch"))
            | _ -> raise (Replay_mismatch "dynamic_parallel start evidence mismatch")))
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
                    (* see the identical comment at the live [run] fold above. *)
                    | Aborted r, _ | _, Aborted r -> Aborted r
                    | Cancelled r, _ | _, Cancelled r -> Cancelled r
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
                        | Deadline -> (
                            (* Replay is clock-free BY DESIGN: re-reading the
                               wall clock would make a recorded run stop being
                               reproducible the moment the deadline passed. *)
                            match next () with
                            | Deadline_read { expired } ->
                                let st = emit st (Deadline_read { expired }) in
                                if expired then (st, Some "deadline")
                                else (st, None)
                            | _ ->
                                raise
                                  (Replay_mismatch "deadline verdict mismatch"))
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
  (go, go_step)
  in
  let host_pending, host_next = make_cursor trace in
  let go, _go_step = make_walker ~next:host_next in
  let outcome, _trace =
    finish (go { rev_trace = []; ctx = initial_ctx; attest_counts = [];
                 terminal = None } wf.steps)
  in
  (* The walk must have consumed the WHOLE trace: a trace that is a valid prefix
     followed by extra (garbage) entries must NOT replay successfully. *)
  if !host_pending <> [] then
    raise (Replay_mismatch "trailing trace entries after workflow completed");
  outcome
