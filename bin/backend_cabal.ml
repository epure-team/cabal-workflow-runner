open Cabal
open Cabal_workflow_runner

let ( let* ) result continuation = Result.bind result continuation

let default_budget = 1_000_000

let initial_budget () =
  match Sys.getenv_opt "CWR_BUDGET" with
  | Some value -> (
      match int_of_string_opt (String.trim value) with
      | Some budget -> budget
      | None -> default_budget)
  | None -> default_budget

let required_backend_id () =
  match Sys.getenv_opt "CWR_BACKEND" with
  | None -> Error "CWR_BACKEND is required for live workflow execution"
  | Some value ->
      let backend_id = String.trim value in
      if backend_id = "" then
        Error "CWR_BACKEND is required for live workflow execution"
      else if backend_id <> value then
        Error "CWR_BACKEND must be a canonical backend id without whitespace"
      else Ok backend_id

let default_model () =
  match Sys.getenv_opt "CWR_MODEL" with
  | Some value when String.trim value <> "" -> Some (String.trim value)
  | Some _ | None -> None

let no_attachment_limits : Task_preflight.limits =
  {max_attachments = 0; max_file_size_bytes = 0; max_total_size_bytes = 0}

let strict_json_error message =
  (false, `Assoc [("error", `String message)])

let project_completion response =
  match
    ( Agent_execution.final_status response,
      Agent_execution.final_structured_json response )
  with
  | Agent_execution.Success, Some ((`Assoc _ | `List _) as json) -> (true, json)
  | Agent_execution.Success, None ->
      strict_json_error "backend returned no strict structured JSON"
  | Agent_execution.Success, Some _ ->
      strict_json_error "backend returned unsupported structured JSON"
  | Agent_execution.Failed _, _ -> strict_json_error "agent run failed"
  | Agent_execution.Timed_out, _ -> strict_json_error "agent run timed out"
  | Agent_execution.Cancelled, _ ->
      strict_json_error "agent run was cancelled"

let project_completion_error error =
  (false, Agent_execution.error_to_yojson error)

let make ~sw ~env ~working_dir =
  let* backend_id = required_backend_id () in
  let* () = Cwr_cabal.bootstrap_hardened () in
  let* runtime =
    Cwr_cabal.create ~sw ~env ~limits:no_attachment_limits ~backend_id
      ~working_dir ?default_model:(default_model ()) ()
  in
  let budget_counter = ref (initial_budget ()) in
  let budget_mutex = Eio.Mutex.create () in
  let budget () =
    Eio.Mutex.use_rw ~protect:true budget_mutex (fun () ->
        decr budget_counter;
        !budget_counter)
  in
  let run_agent ~id ~prompt ~read_only ~agent_type ~model ~output_schema =
    let routing =
      match agent_type with
      | Some value when String.trim value <> "" -> Some (String.trim value)
      | Some _ | None -> None
    in
    let model =
      match model with
      | Some value when String.trim value <> "" -> Some (String.trim value)
      | Some _ | None -> None
    in
    let json_schema = Option.map Types.Schema.to_json_schema output_schema in
    match
      Agent_execution.make_request ~id
        ~system_prompt:
          "Return exactly one JSON object or array without prose or a code fence."
        ~user_prompt:prompt ~timeout_s:max_float ?json_schema ?routing ?model
        ~read_only ()
    with
    | Error _ -> strict_json_error "invalid rich agent request"
    | Ok request -> (
        match Runtime.complete runtime request with
        | Ok response -> project_completion response
        | Error error -> project_completion_error error)
  in
  let run_command = Cwr_runner.Runner.make ~sw ~env ~base:working_dir in
  let run_pinned_command =
    Cwr_runner.Runner.make_pinned ~sw ~env ~base:working_dir
  in
  let run_shell_command command =
    try
      let result =
        Backend_process.run_process ~sw ~env ~cmd:["sh"; "-c"; command]
          ~working_dir ~timeout_seconds:60.0 ()
      in
      match result.Backend_process.status with
      | Backend_types.Timeout -> 124
      | Backend_types.Success | Backend_types.Failed _ | Backend_types.Cancelled ->
          result.exit_code
    with _ -> 127
  in
  Ok
    Backend.
      {run_agent; budget; run_command; run_pinned_command; run_shell_command}
