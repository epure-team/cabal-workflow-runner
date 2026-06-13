(* cabal-backed backend. This is the ONLY place that depends on cabal: the
   [cabal_workflow_runner] library (engine/validate/types/expr) depends on
   yojson only. Agent steps dispatch through cabal's registry and MUST return
   structured JSON; if no parseable JSON is produced we fail closed.

   There is no [eval_gate]: gates, branches and loop stop conditions are pure
   [Expr.t] predicates evaluated by the engine over the recorded agent outputs. *)

open Cabal

(* Initial budget for [Budget] governors. Defaults to a large constant
   (1_000_000) so a cabal run is effectively unbounded unless the embedder sets
   the [CWR_BUDGET] environment variable. *)
let default_budget = 1_000_000

let initial_budget () =
  match Sys.getenv_opt "CWR_BUDGET" with
  | Some s -> ( match int_of_string_opt (String.trim s) with Some n -> n | None -> default_budget)
  | None -> default_budget

(* Extract structured JSON from a cabal task result. We prefer the structured
   report's [raw_json]; failing that we try to parse [agent_text] as JSON. If
   neither yields a JSON object we fail closed (success := false). *)
(* Best-effort extraction of a JSON object/array from agent text. Small/fast
   models often wrap the JSON in prose or a ```json fence; a strict parse would
   fail closed on output that is "JSON plus noise". We try, in order: a direct
   parse; the body of a leading ``` fence; then the substring from the first {/[
   to the last matching }/].

   This is NOT balanced-bracket-aware: it is a heuristic, and it FAILS CLOSED on
   ambiguous input. In particular, valid-JSON-immediately-followed-by-prose that
   itself contains braces (e.g. `{"a":1} note: see {below}`) takes the
   first-{-to-last-} substring, which then fails to parse and yields [None] —
   i.e. the agent step is treated as unsuccessful. That is the safe direction (we
   never silently commit to a partial/misread object). Returns [None] when no JSON
   object/array can be recovered at all. *)
let extract_json (raw : string) : Yojson.Safe.t option =
  let parse str =
    match Yojson.Safe.from_string (String.trim str) with
    | (`Assoc _ | `List _) as j -> Some j
    | _ -> None
    | exception _ -> None
  in
  let strip_fence s =
    let s = String.trim s in
    if String.length s >= 3 && String.sub s 0 3 = "```" then
      match String.index_opt s '\n' with
      | Some nl ->
          let body = String.trim (String.sub s (nl + 1) (String.length s - nl - 1)) in
          let n = String.length body in
          if n >= 3 && String.sub body (n - 3) 3 = "```" then
            String.sub body 0 (n - 3)
          else body
      | None -> s
    else s
  in
  let between_brackets s =
    let pick =
      match (String.index_opt s '{', String.index_opt s '[') with
      | Some a, Some b -> if a <= b then Some ('}', a) else Some (']', b)
      | Some a, None -> Some ('}', a)
      | None, Some b -> Some (']', b)
      | None, None -> None
    in
    match pick with
    | None -> None
    | Some (close, start) -> (
        match String.rindex_opt s close with
        | Some stop when stop >= start -> Some (String.sub s start (stop - start + 1))
        | _ -> None)
  in
  match parse raw with
  | Some j -> Some j
  | None -> (
      match parse (strip_fence raw) with
      | Some j -> Some j
      | None -> ( match between_brackets raw with Some sub -> parse sub | None -> None))

let structured_output (result : Backend_types.task_result) :
    bool * Yojson.Safe.t =
  let base_success =
    match result.Backend_types.status with
    | Backend_types.Success -> true
    | _ -> false
  in
  let from_report =
    match result.Backend_types.report with
    | Some { Backend_types.raw_json = Some j; _ } -> Some j
    | _ -> None
  in
  let from_text () = extract_json result.Backend_types.agent_text in
  match (base_success, from_report) with
  | true, Some j -> (true, j)
  | true, None -> (
      match from_text () with
      | Some j -> (true, j)
      (* successful run but no parseable structured JSON => fail closed *)
      | None -> (false, `Assoc [ ("error", `String "no parseable structured JSON") ]))
  | false, _ ->
      (false, `Assoc [ ("error", `String "agent run did not succeed") ])

(* Build a backend record bound to a live eio environment + switch. Dispatches
   agent work to the first available cabal backend; fails closed if none. *)
let make ~sw ~env ~working_dir : Cabal_workflow_runner.Backend.t =
  (* Populate the registry with the built-in adapters (claude-code, codex,
     gemini, ...). Without this the registry is empty and every dispatch fails
     closed. CWR_BACKEND selects a backend by id (default: first available);
     CWR_MODEL pins the model (e.g. a small/cheap/fast one like "haiku"). *)
  Adapter_loader.register_all ~sw ~env ();
  (* A genuine consumable budget, per-[make] (i.e. per run, shared across all
     loops in that run = a total run budget). Each [Budget]-governor check
     consumes one unit: [budget ()] decrements the counter and returns the
     remaining. With [CWR_BUDGET=N] the counter starts at N, so the readings are
     N-1, N-2, ..., 0; the [Budget] governor stops the loop once a reading is
     <= 0, i.e. on the Nth check. The run therefore performs AT MOST N
     budget-governed loop iterations total. Determinism is unaffected: the engine
     records every [Budget_read] in the trace and replay re-feeds the recorded
     values (replay never calls this). *)
  let budget_counter = ref (initial_budget ()) in
  let budget_mutex = Eio.Mutex.create () in
  let budget () =
    Eio.Mutex.use_rw ~protect:true budget_mutex (fun () ->
      decr budget_counter;
      !budget_counter)
  in
  let select () =
    match Sys.getenv_opt "CWR_BACKEND" with
    | Some name when String.trim name <> "" -> Registry.get (String.trim name)
    | _ -> Registry.first_available ~sw ~env
  in
  let model =
    match Sys.getenv_opt "CWR_MODEL" with
    | Some m when String.trim m <> "" -> Some (String.trim m)
    | _ -> None
  in
  let run_agent ~id ~prompt ~read_only =
    match select () with
    | None ->
        ( false,
          `Assoc
            [ ("error", `String (Printf.sprintf "no cabal backend available (step %s)" id)) ] )
    | Some backend ->
        let spec =
          Backend_types.make_task_spec ~prompt ~working_dir ~read_only ?model
            ~expected_outputs:
              [ Backend_types.Files_changed; Backend_types.Structured_report ]
            ()
        in
        let request = { Backend_types.spec; ctxt = id } in
        let response =
          Agentic_backend.run_task_with_ctxt ~sw ~env backend request
        in
        structured_output response.Backend_types.result
  in
  (* The Run-step effect: process execution + a before/after directory snapshot,
     implemented in [Runner] (bin-side). The lib never spawns a process. *)
  let run_command = Runner.make ~sw ~env ~base:working_dir in
  { Cabal_workflow_runner.Backend.run_agent; budget; run_command }
