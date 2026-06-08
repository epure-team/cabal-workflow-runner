(* cabal-backed backend. This is the ONLY place that depends on cabal: the
   [cabal_workflow_runner] library (engine/validate/types/expr) depends on
   yojson only. Agent steps dispatch through cabal's registry and MUST return
   structured JSON; if no parseable JSON is produced we fail closed.

   There is no [eval_gate]: gates, branches and loop stop conditions are pure
   [Expr.t] predicates evaluated by the engine over the recorded agent outputs. *)

open Cabal

(* Remaining budget for [Budget] governors. Defaults to a large constant
   (1_000_000 iterations) so a cabal run is effectively unbounded unless the
   embedder sets the [CWR_BUDGET] environment variable. *)
let default_budget = 1_000_000

let budget () =
  match Sys.getenv_opt "CWR_BUDGET" with
  | Some s -> ( match int_of_string_opt (String.trim s) with Some n -> n | None -> default_budget)
  | None -> default_budget

(* Extract structured JSON from a cabal task result. We prefer the structured
   report's [raw_json]; failing that we try to parse [agent_text] as JSON. If
   neither yields a JSON object we fail closed (success := false). *)
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
  let from_text () =
    match Yojson.Safe.from_string result.Backend_types.agent_text with
    | (`Assoc _ | `List _) as j -> Some j
    | _ -> None
    | exception _ -> None
  in
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
  let run_agent ~id ~prompt ~read_only =
    match Registry.first_available ~sw ~env with
    | None ->
        ( false,
          `Assoc
            [ ("error", `String (Printf.sprintf "no cabal backend available (step %s)" id)) ] )
    | Some backend ->
        let spec =
          Backend_types.make_task_spec ~prompt ~working_dir ~read_only
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
  { Cabal_workflow_runner.Backend.run_agent; budget }
