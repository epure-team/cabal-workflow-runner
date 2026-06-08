open Types

let token_digest tok = Digest.to_hex (Digest.string tok)

let token_is_wellformed = function
  | None -> false
  | Some t -> String.length (String.trim t) > 0

(* Execution state threaded through the walk. [rev_trace] accumulates in REVERSE
   order (most recent first) and is reversed at the end. [terminal] is set when a
   Commit or Block ends the run early. *)
type state = { rev_trace : trace_entry list; terminal : outcome option }

let emit st entry = { st with rev_trace = entry :: st.rev_trace }

let finish st =
  let trace = List.rev st.rev_trace in
  let outcome =
    match st.terminal with Some o -> o | None -> Completed_no_commit
  in
  (outcome, trace)

(* ------------------------------------------------------------------ *)
(* run: deterministic interpreter driven by a backend.                 *)
(* ------------------------------------------------------------------ *)

let run ~backend ~token validated =
  let wf = Validate.Validated.workflow validated in
  let gate id = backend.Backend.eval_gate id in
  let agent ~id ~prompt ~read_only = backend.Backend.run_agent ~id ~prompt ~read_only in
  let rec go st steps =
    match (st.terminal, steps) with
    | Some _, _ | _, [] -> st
    | None, step :: rest ->
        let st = go_step st step in
        go st rest
  and go_step st step =
    match step with
    | Agent { id; prompt; read_only } ->
        let success, text = agent ~id ~prompt ~read_only in
        emit st (Agent_ran { id; success; text })
    | Gate { id } ->
        let verdict = gate id in
        emit st (Gate_evaluated { id; verdict })
    | Branch { on; then_; else_ } ->
        let verdict = gate on in
        let st = emit st (Gate_evaluated { id = on; verdict }) in
        let chosen = match verdict with Pass -> then_ | Fail -> else_ in
        go st chosen
    | Loop { max_iters; until; body } ->
        let rec loop st iter =
          if st.terminal <> None then st
          else if iter >= max_iters then st (* hard cap: can never spin *)
          else
            let verdict = gate until in
            let st = emit st (Gate_evaluated { id = until; verdict }) in
            match verdict with
            | Pass -> st
            | Fail ->
                let st = go st body in
                loop st (iter + 1)
        in
        loop st 0
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
  in
  finish (go { rev_trace = []; terminal = None } wf.steps)

(* ------------------------------------------------------------------ *)
(* replay: re-interpret from the recorded trace, no backend consulted. *)
(* ------------------------------------------------------------------ *)

exception Replay_mismatch of string

let replay ~trace validated =
  let wf = Validate.Validated.workflow validated in
  let pending = ref trace in
  let next () =
    match !pending with
    | [] -> raise (Replay_mismatch "trace exhausted before workflow completed")
    | e :: tl ->
        pending := tl;
        e
  in
  let rec go st steps =
    match (st.terminal, steps) with
    | Some _, _ | _, [] -> st
    | None, step :: rest ->
        let st = go_step st step in
        go st rest
  and go_step st step =
    match step with
    | Agent { id; prompt = _; read_only = _ } -> (
        match next () with
        | Agent_ran { success; text; id = rid } when rid = id ->
            emit st (Agent_ran { id; success; text })
        | _ -> raise (Replay_mismatch "agent entry mismatch"))
    | Gate { id } -> (
        match next () with
        | Gate_evaluated { verdict; id = rid } when rid = id ->
            emit st (Gate_evaluated { id; verdict })
        | _ -> raise (Replay_mismatch "gate entry mismatch"))
    | Branch { on; then_; else_ } -> (
        match next () with
        | Gate_evaluated { verdict; id = rid } when rid = on ->
            let st = emit st (Gate_evaluated { id = on; verdict }) in
            let chosen = match verdict with Pass -> then_ | Fail -> else_ in
            go st chosen
        | _ -> raise (Replay_mismatch "branch gate entry mismatch"))
    | Loop { max_iters; until; body } ->
        let rec loop st iter =
          if st.terminal <> None then st
          else if iter >= max_iters then st
          else
            match next () with
            | Gate_evaluated { verdict; id = rid } when rid = until -> (
                let st = emit st (Gate_evaluated { id = until; verdict }) in
                match verdict with
                | Pass -> st
                | Fail ->
                    let st = go st body in
                    loop st (iter + 1))
            | _ -> raise (Replay_mismatch "loop gate entry mismatch")
        in
        loop st 0
    | Commit { id } -> (
        match next () with
        | Committed_step { id = rid; token_digest } when rid = id ->
            let st = emit st (Committed_step { id; token_digest }) in
            { st with terminal = Some (Committed { id; token_digest }) }
        | Blocked_at { id = rid; reason } when rid = id ->
            let st = emit st (Blocked_at { id; reason }) in
            { st with terminal = Some (Blocked reason) }
        | _ -> raise (Replay_mismatch "commit entry mismatch"))
  in
  let outcome, _trace = finish (go { rev_trace = []; terminal = None } wf.steps) in
  outcome
