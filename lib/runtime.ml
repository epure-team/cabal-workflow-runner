type capabilities = {
  native_json_schema : bool;
  session_resume : bool;
  attachments : bool;
  maximum_web : Agent_execution.web_level;
  read_only : bool;
  max_turns : bool;
  hard_timeout : bool;
  routing : bool;
  model_selection : bool;
}

let make_capabilities ?(native_json_schema = false) ?(session_resume = false)
    ?(attachments = false) ?(maximum_web = Agent_execution.Web_disabled)
    ?(read_only = false) ?(max_turns = false) ?(hard_timeout = false)
    ?(routing = false) ?(model_selection = false) () =
  {
    native_json_schema;
    session_resume;
    attachments;
    maximum_web;
    read_only;
    max_turns;
    hard_timeout;
    routing;
    model_selection;
  }

let native_json_schema capabilities = capabilities.native_json_schema
let session_resume capabilities = capabilities.session_resume
let attachments capabilities = capabilities.attachments
let maximum_web capabilities = capabilities.maximum_web
let read_only capabilities = capabilities.read_only
let max_turns capabilities = capabilities.max_turns
let hard_timeout capabilities = capabilities.hard_timeout
let routing capabilities = capabilities.routing
let model_selection capabilities = capabilities.model_selection

type t = {
  identity : string option;
  capabilities : capabilities;
  complete_fn :
    Agent_execution.request ->
    (Agent_execution.response, Agent_execution.error) result;
}

let safe_identifier value =
  let safe_character = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' | '.' -> true
    | _ -> false
  in
  value <> ""
  && String.length value <= 128
  && String.for_all safe_character value

let make ?identity ?(capabilities = make_capabilities ()) ~complete () =
  match identity with
  | Some value when not (safe_identifier value) ->
      Error "runtime identity is invalid"
  | _ -> Ok { identity; capabilities; complete_fn = complete }

let identity runtime = runtime.identity
let capabilities runtime = runtime.capabilities
let complete runtime request = runtime.complete_fn request

let legacy_capabilities =
  make_capabilities ~read_only:true ~routing:true ~model_selection:true ()

let unsupported_legacy_request request =
  Option.is_none (Agent_execution.read_only request)
  || Option.is_some (Agent_execution.json_schema request)
  || Option.is_some (Agent_execution.resume_session request)
  || Agent_execution.attachments request <> []
  || Agent_execution.web_level (Agent_execution.web_policy request)
     <> Agent_execution.Web_disabled
  || Option.is_some (Agent_execution.max_turns request)

let compose_legacy_prompt request =
  "## System\n"
  ^ Agent_execution.system_prompt request
  ^ "\n\n## User\n"
  ^ Agent_execution.user_prompt request

let safe_elapsed ~started ~finished =
  let elapsed = finished -. started in
  match classify_float elapsed with
  | (FP_normal | FP_subnormal | FP_zero) when elapsed >= 0.0 -> elapsed
  | FP_normal | FP_subnormal | FP_zero | FP_infinite | FP_nan -> 0.0

let internal_dispatch_error () =
  Agent_execution.redacted_dispatch_error
    Agent_execution.Internal_dispatch_failure

let make_legacy_response ~elapsed_s ~status ~structured_json =
  match
    Agent_execution.make_delivery_intent ~attachment_count:0
      ~attachment_delivery:Agent_execution.Upload_attachments
      ~web_policy:Agent_execution.web_disabled ()
  with
  | Error _ -> Error (internal_dispatch_error ())
  | Ok delivery -> (
      let make_attempt status structured_json =
        Agent_execution.make_attempt ~number:1
          ~kind:Workflow_event.Initial_attempt ~status ~elapsed_s ~delivery
          ?structured_json ()
      in
      let attempt_result = make_attempt status (Some structured_json) in
      let attempt_result, execution_kind, execution_message =
        match attempt_result with
        | Ok attempt ->
            ( Ok attempt,
              Agent_execution.Backend_execution_failed,
              "legacy backend reported failure" )
        | Error _ ->
            ( make_attempt
                (Agent_execution.Failed
                   "legacy backend returned invalid structured JSON") None,
              Agent_execution.Execution_contract_failed,
              "legacy backend returned non-standard JSON" )
      in
      match attempt_result with
      | Error _ -> Error (internal_dispatch_error ())
      | Ok attempt -> (
          match
            Agent_execution.make_response ~attempts:[ attempt ]
              ~status:(Agent_execution.attempt_status attempt)
              ~total_elapsed_s:elapsed_s
              ~cleanup_status:Agent_execution.Cleanup_not_required ()
          with
          | Error _ -> Error (internal_dispatch_error ())
          | Ok response -> (
              match Agent_execution.attempt_status attempt with
              | Agent_execution.Success -> Ok response
              | Agent_execution.Failed _ | Agent_execution.Timed_out
              | Agent_execution.Cancelled -> (
                  match
                    Agent_execution.make_execution_error ~kind:execution_kind
                      ~message:execution_message ~response ()
                  with
                  | Ok error -> Error error
                  | Error _ -> Error (internal_dispatch_error ())))))

let of_legacy_backend ?(now = Unix.gettimeofday) backend =
  let complete_fn request =
    if unsupported_legacy_request request then
      match
        Agent_execution.make_dispatch_error
          ~kind:Agent_execution.Unsupported_request
          ~message:"legacy backend cannot represent this rich request" ()
      with
      | Ok error -> Error error
      | Error _ -> Error (internal_dispatch_error ())
    else
      match Agent_execution.read_only request with
      | None -> Error (internal_dispatch_error ())
      | Some read_only ->
          let started = now () in
          let success, structured_json =
            backend.Backend.run_agent
              ~id:(Agent_execution.id request)
              ~prompt:(compose_legacy_prompt request)
              ~read_only
              ~agent_type:(Agent_execution.routing request)
              ~model:(Agent_execution.model request)
              ~output_schema:None
          in
          let elapsed_s = safe_elapsed ~started ~finished:(now ()) in
          let status =
            if success then Agent_execution.Success
            else Agent_execution.Failed "legacy backend reported failure"
          in
          make_legacy_response ~elapsed_s ~status ~structured_json
  in
  {
    identity = Some "legacy-backend";
    capabilities = legacy_capabilities;
    complete_fn;
  }
