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
  terminal : outcome option;
}

let emit st entry = { st with rev_trace = entry :: st.rev_trace }

(* Bind/overwrite a key in ctx (most recent write wins; assoc lookup finds it). *)
let bind st key json = { st with ctx = (key, json) :: st.ctx }

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

let bind_loop_iter st index = bind st "loop" (`Assoc [ ("iter", `Int index) ])

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

let run ?(max_loop_iters = default_max_loop_iters) ?(run_allowlist = []) ?(initial_ctx = [])
    ~sw:(_sw : Eio.Switch.t) ~backend ~token validated =
  let wf = Validate.Validated.workflow validated in
  let agent ~id ~prompt ~read_only =
    backend.Backend.run_agent ~id ~prompt ~read_only
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
    | Agent { id; prompt; read_only; output_schema; on_failure } -> (
        let success, output = agent ~id ~prompt ~read_only in
        let st = emit st (Agent_ran { id; success; output }) in
        let st = bind_output st id output in
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
          | None -> st)
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
    | Run { id; cmd; working_dir; timeout_ms; observe } ->
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
          (* Execute the injected effect exactly ONCE, record the full result,
             and bind it into ctx. Replay re-feeds this without re-executing. *)
          let result =
            backend.Backend.run_command ~id ~argv:cmd ~working_dir ~timeout_ms
              ~observe
          in
          let st = emit st (Run_executed { id; result }) in
          bind_output st id (json_of_run_result result)
        end
    | Commit { id } ->
        if token_is_wellformed token then begin
          let digest = token_digest (Option.get token) in
          let st = emit st (Committed_step { id; token_digest = digest }) in
          { st with terminal = Some (Committed { id; token_digest = digest }) }
        end
        else begin
          let reason =
            Printf.sprintf "Commit %S requires a runtime approval token" id
          in
          let st = emit st (Blocked_at { id; reason }) in
          { st with terminal = Some (Blocked reason) }
        end
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
  finish (go { rev_trace = []; ctx = initial_ctx; terminal = None } wf.steps)

(* ------------------------------------------------------------------ *)
(* replay: re-interpret from the recorded trace, no backend consulted. *)
(* ------------------------------------------------------------------ *)

exception Replay_mismatch of string

let replay ?(max_loop_iters = default_max_loop_iters) ?(initial_ctx = []) ~sw:(_sw : Eio.Switch.t)
    ~trace validated =
  let wf = Validate.Validated.workflow validated in
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
    | Agent { id; prompt = _; read_only = _; output_schema; on_failure } -> (
        match next () with
        | Agent_ran { success; output; id = rid } when rid = id -> (
            let st = emit st (Agent_ran { id; success; output }) in
            let st = bind_output st id output in
            if not success then begin
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
                      (* the recorded run aborted here too; consume its Blocked_at *)
                      let st =
                        { st with terminal = Some (Aborted reason) }
                      in
                      emit st (Blocked_at { id; reason }))
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
    | Run { id; cmd = _; working_dir = _; timeout_ms = _; observe = _ } -> (
        (* NEVER re-execute: re-feed the recorded result (or reproduce the
           recorded allowlist-Blocked). The allowlist is NOT consulted on replay
           (nothing executes), mirroring the Agent_ran replay arm. *)
        match next () with
        | Run_executed { id = rid; result } when rid = id ->
            let st = emit st (Run_executed { id; result }) in
            bind_output st id (json_of_run_result result)
        | Blocked_at { id = rid; reason } when rid = id ->
            let st = emit st (Blocked_at { id; reason }) in
            { st with terminal = Some (Blocked reason) }
        | _ -> raise (Replay_mismatch "run entry mismatch"))
    | Commit { id } -> (
        match next () with
        | Committed_step { id = rid; token_digest } when rid = id ->
            let st = emit st (Committed_step { id; token_digest }) in
            { st with terminal = Some (Committed { id; token_digest }) }
        | Blocked_at { id = rid; reason } when rid = id ->
            let st = emit st (Blocked_at { id; reason }) in
            { st with terminal = Some (Blocked reason) }
        | _ -> raise (Replay_mismatch "commit entry mismatch"))
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
    finish (go { rev_trace = []; ctx = initial_ctx; terminal = None } wf.steps)
  in
  (* The walk must have consumed the WHOLE trace: a trace that is a valid prefix
     followed by extra (garbage) entries must NOT replay successfully. *)
  if !pending <> [] then
    raise (Replay_mismatch "trailing trace entries after workflow completed");
  outcome
