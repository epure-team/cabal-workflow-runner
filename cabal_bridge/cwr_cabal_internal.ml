open Cabal

module Agent_execution = Cabal_workflow_runner.Agent_execution
module Execution_metrics = Cabal_workflow_runner.Execution_metrics
module Runtime = Cabal_workflow_runner.Runtime
module Workflow_event = Cabal_workflow_runner.Workflow_event

let ( let* ) result continuation = Result.bind result continuation

let approved_ids =
  ["claude-code"; "codex"; "copilot-cli"; "gemini-cli"; "opencode"; "pi"]

let expected_origin = function
  | "pi" -> Some Runtime_entry.Yaml
  | "claude-code" | "codex" | "copilot-cli" | "gemini-cli" | "opencode" ->
      Some Runtime_entry.Handwritten
  | _ -> None

let expected_execution_policy = function
  | "copilot-cli" ->
      Some
        (Runtime_entry.Dispatch_quarantined
           Runtime_entry.Incomplete_mcp_isolation)
  | "claude-code" | "codex" | "gemini-cli" | "opencode" | "pi" ->
      Some Runtime_entry.Dispatch_enabled
  | _ -> None

type trusted_entry = {
  id : string;
  entry : Runtime_entry.t;
  backend : Agentic_backend.t;
  descriptor : Backend_registry.descriptor;
  runtime_capabilities : Backend_registry.capabilities;
  origin : Runtime_entry.implementation_origin;
  execution_policy : Runtime_entry.execution_policy;
  version_policy : Runtime_entry.version_policy;
}

type bootstrap = {hardened_entries : trusted_entry list}

type bootstrap_state = Fresh | In_progress | Complete

let bootstrap_state = Atomic.make Fresh

let capture_hardened_entry id =
  match
    ( Registry.find_entry id,
      Backend_registry.find id,
      expected_origin id,
      expected_execution_policy id )
  with
  | ( Some (Registry.Validated entry),
      Some descriptor,
      Some origin,
      Some execution_policy )
    when entry.Runtime_entry.origin = origin
         && entry.execution_policy = execution_policy
         && entry.version_policy = Runtime_entry.Enforce_baseline
         && entry.effective_descriptor = descriptor
         && entry.runtime_capabilities = descriptor.capabilities
         && Agentic_backend.id entry.backend = id ->
      Ok
        {
          id;
          entry;
          backend = entry.backend;
          descriptor = entry.effective_descriptor;
          runtime_capabilities = entry.runtime_capabilities;
          origin = entry.origin;
          execution_policy = entry.execution_policy;
          version_policy = entry.version_policy;
        }
  | _ -> Error "hardened Cabal runtime bootstrap produced an invalid binding"

let capture_hardened_entries () =
  let rec loop captured = function
    | [] -> Ok (List.rev captured)
    | id :: rest ->
        let* entry = capture_hardened_entry id in
        loop (entry :: captured) rest
  in
  loop [] approved_ids

let bootstrap_hardened () =
  if not (Atomic.compare_and_set bootstrap_state Fresh In_progress) then
    Error "CWR hardened Cabal bootstrap is one-shot for the process"
  else
    match
      Runtime_bootstrap.register_runtime
        ~profile:Runtime_bootstrap.Hardened_builtins ()
    with
    | Error error ->
        Atomic.set bootstrap_state Fresh;
        Error (Runtime_bootstrap.render_error error)
    | Ok () -> (
        match capture_hardened_entries () with
        | Ok hardened_entries ->
            Atomic.set bootstrap_state Complete;
            Ok {hardened_entries}
        | Error _ as error ->
            Atomic.set bootstrap_state Complete;
            error)
    | exception error ->
        Atomic.set bootstrap_state Fresh;
        raise error

let trusted_entry_matches trusted current =
  current == trusted.entry
  && current.Runtime_entry.backend == trusted.backend
  && current.effective_descriptor = trusted.descriptor
  && current.runtime_capabilities = trusted.runtime_capabilities
  && current.origin = trusted.origin
  && current.execution_policy = trusted.execution_policy
  && current.version_policy = trusted.version_policy
  && Agentic_backend.id current.backend = trusted.id

let find_hardened_entry bootstrap id =
  List.find_opt (fun trusted -> trusted.id = id) bootstrap.hardened_entries

let hardened_entry bootstrap id =
  match (find_hardened_entry bootstrap id, Registry.find_entry id) with
  | Some trusted, Some (Registry.Validated current)
    when trusted_entry_matches trusted current ->
      Some current
  | None, _ | Some _, None | Some _, Some (Registry.Raw _)
  | Some _, Some (Registry.Validated _) ->
      None

let hardened_registry_intact bootstrap =
  List.for_all
    (fun trusted -> Option.is_some (hardened_entry bootstrap trusted.id))
    bootstrap.hardened_entries

type custom_backend = {
  bootstrap : bootstrap;
  id : string;
  entry : Runtime_entry.t;
  descriptor : Backend_registry.descriptor;
  backend : Agentic_backend.t;
}

let register_custom_backend ~bootstrap ~descriptor ~backend =
  if not (hardened_registry_intact bootstrap) then
    Error "hardened Cabal runtime bootstrap is missing or no longer intact"
  else
    match Runtime_bootstrap.register_custom ~descriptor ~backend with
    | Error error -> Error (Runtime_bootstrap.render_error error)
    | Ok () -> (
        match Registry.find_entry descriptor.id with
        | Some (Registry.Validated entry)
          when entry.backend == backend
               && entry.effective_descriptor = descriptor
               && entry.runtime_capabilities = descriptor.capabilities
               && entry.origin = Runtime_entry.Custom
               && entry.execution_policy = Runtime_entry.Dispatch_enabled
               && entry.version_policy = Runtime_entry.Enforce_baseline ->
            Ok {bootstrap; id = descriptor.id; entry; descriptor; backend}
        | None | Some (Registry.Raw _) | Some (Registry.Validated _) ->
            Error "custom Cabal runtime registration did not remain intact")

type selection_error =
  | Missing
  | Raw
  | Untrusted_binding
  | Custom_authorization_required

let custom_entry_matches bootstrap authorization entry id =
  authorization.bootstrap == bootstrap
  && authorization.id = id
  && authorization.entry == entry
  && authorization.descriptor = entry.Runtime_entry.effective_descriptor
  && authorization.descriptor.capabilities = entry.runtime_capabilities
  && Agentic_backend.id authorization.backend = id
  && Agentic_backend.id entry.backend = id
  && entry.backend == authorization.backend
  && entry.origin = Runtime_entry.Custom
  && entry.execution_policy = Runtime_entry.Dispatch_enabled
  && entry.version_policy = Runtime_entry.Enforce_baseline

let resolve_selection ~bootstrap ?custom_backend ~default_backend id =
  match Registry.find_entry id with
  | None -> Error Missing
  | Some (Registry.Raw _) -> Error Raw
  | Some (Registry.Validated entry) ->
      if List.mem id approved_ids then
        if Option.is_some (hardened_entry bootstrap id) then Ok entry
        else Error Untrusted_binding
      else if id <> default_backend then Error Custom_authorization_required
      else
        match custom_backend with
        | Some authorization
          when custom_entry_matches bootstrap authorization entry id ->
            Ok entry
        | None | Some _ -> Error Custom_authorization_required

let render_selection_error = function
  | Missing -> "requested backend is not registered in the hardened runtime"
  | Raw -> "requested backend has an untrusted raw runtime registration"
  | Untrusted_binding ->
      "requested built-in no longer has its hardened runtime binding"
  | Custom_authorization_required ->
      "custom backend selection requires its explicit registration authorization"

let selection_failure_kind = function
  | Missing -> Agent_execution.Backend_unavailable
  | Raw | Untrusted_binding | Custom_authorization_required ->
      Agent_execution.Capability_mismatch

let internal_mapping_message =
  "central execution telemetry could not be represented safely"

let preserve_trace_or_redact ?event_trace message =
  match event_trace with
  | Some event_trace -> (
      match
        Agent_execution.make_telemetry_mapping_error ~message ~event_trace ()
      with
      | Ok error -> error
      | Error _ ->
          Agent_execution.redacted_dispatch_error
            Agent_execution.Internal_dispatch_failure)
  | None ->
      Agent_execution.redacted_dispatch_error
        Agent_execution.Internal_dispatch_failure

let dispatch_error ?event_trace kind message =
  match Agent_execution.make_dispatch_error ~kind ~message ?event_trace () with
  | Ok error -> error
  | Error _ -> (
      match
        Agent_execution.make_dispatch_error
          ~kind:Agent_execution.Internal_dispatch_failure
          ~message:internal_mapping_message ?event_trace ()
      with
      | Ok error -> error
      | Error _ -> preserve_trace_or_redact ?event_trace internal_mapping_message)

let no_completed_error ?event_trace ~status message =
  match
    Agent_execution.make_no_completed_attempt_error ~status
      ~invocation_may_have_started:true ~message ?event_trace ()
  with
  | Ok error -> error
  | Error _ -> (
      match
        Agent_execution.make_no_completed_attempt_error ~status
          ~invocation_may_have_started:true ~message:internal_mapping_message
          ?event_trace ()
      with
      | Ok error -> error
      | Error _ -> preserve_trace_or_redact ?event_trace internal_mapping_message)

let mapping_failure ?event_trace ?(reason = "") () =
  let suffix = if reason = "" then "" else ": " ^ reason in
  let message = internal_mapping_message ^ suffix in
  match event_trace with
  | Some event_trace -> preserve_trace_or_redact ~event_trace message
  | None ->
      dispatch_error Agent_execution.Internal_dispatch_failure message

let valid_limits (limits : Task_preflight.limits) =
  limits.max_attachments >= 0
  && limits.max_file_size_bytes >= 0
  && limits.max_total_size_bytes >= 0
  &&
  (limits.max_attachments = 0 || limits.max_file_size_bytes = 0
  || limits.max_total_size_bytes = 0
  || limits.max_file_size_bytes <= limits.max_total_size_bytes)

let valid_default_model = function
  | None -> true
  | Some model -> model <> "" && String.trim model = model

let web_level_of_cabal = function
  | Backend_types.Web_disabled -> Agent_execution.Web_disabled
  | Backend_types.Web_search -> Agent_execution.Web_search
  | Backend_types.Web_search_and_fetch -> Agent_execution.Web_search_and_fetch

let runtime_capabilities descriptor =
  let capabilities = descriptor.Backend_registry.capabilities in
  let media_mime_types =
    List.map
      (function
        | Backend_types.Png -> "image/png"
        | Backend_types.Jpeg -> "image/jpeg")
      capabilities.media_support.media_types
  in
  Runtime.make_capabilities
    ~native_json_schema:capabilities.native_json_schema_output
    ~session_resume:capabilities.session_resume ~media_mime_types
    ~maximum_web:(web_level_of_cabal capabilities.web_support.maximum)
    ~restricted_web_domains:false ~read_only:capabilities.read_only_support
    ~max_turns:true ~hard_timeout:true ~routing:true ~model_selection:true ()

let map_media_type mime_type =
  match mime_type with
  | "image/png" -> Ok Backend_types.Png
  | "image/jpeg" -> Ok Backend_types.Jpeg
  | _ -> Error "attachment MIME type is not representable by Cabal"

let map_attachment attachment =
  let size = Agent_execution.attachment_size_bytes attachment in
  if Int64.compare size (Int64.of_int max_int) > 0 then
    Error "attachment size exceeds the Cabal integer range"
  else
    let* media_type =
      map_media_type (Agent_execution.attachment_mime_type attachment)
    in
    Ok
      Backend_types.
        {
          id = Agent_execution.attachment_id attachment;
          path = Agent_execution.attachment_path attachment;
          media_type;
          sha256 = Agent_execution.attachment_sha256 attachment;
          size_bytes = Int64.to_int size;
        }

let map_attachments attachments =
  let rec loop mapped = function
    | [] -> Ok (List.rev mapped)
    | attachment :: rest ->
        let* attachment = map_attachment attachment in
        loop (attachment :: mapped) rest
  in
  loop [] attachments

let map_web_policy policy =
  match Agent_execution.restricted_domains policy with
  | Some _ ->
      Error
        "domain-restricted web access is not representable by the Cabal completion contract"
  | None ->
      Ok
        (match Agent_execution.web_level policy with
        | Agent_execution.Web_disabled -> Backend_types.Web_disabled
        | Agent_execution.Web_search -> Backend_types.Web_search
        | Agent_execution.Web_search_and_fetch ->
            Backend_types.Web_search_and_fetch)

let safe_identifier value =
  let safe_character = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' | '.' -> true
    | _ -> false
  in
  value <> "" && String.length value <= 128
  && String.for_all safe_character value

let mapped_session_id = function
  | None -> None
  | Some value ->
      if String.trim value = "" then None
      else if safe_identifier value then Some value
      else Some "redacted-session"

let map_status = function
  | Backend_types.Success -> Agent_execution.Success
  | Backend_types.Failed _ -> Agent_execution.Failed "backend execution failed"
  | Backend_types.Timeout -> Agent_execution.Timed_out
  | Backend_types.Cancelled -> Agent_execution.Cancelled

let map_attempt_kind = function
  | Backend_types.Initial_attempt -> Workflow_event.Initial_attempt
  | Backend_types.Fresh_attempt -> Workflow_event.Fresh_attempt
  | Backend_types.Resumed_attempt -> Workflow_event.Resumed_attempt

let map_attempt_outcome = function
  | Task_event.Attempt_succeeded -> Workflow_event.Attempt_succeeded
  | Task_event.Attempt_failed -> Workflow_event.Attempt_failed
  | Task_event.Attempt_timed_out -> Workflow_event.Attempt_timed_out
  | Task_event.Attempt_cancelled -> Workflow_event.Attempt_cancelled

let map_retry_kind = function
  | Task_event.Fresh_retry -> Workflow_event.Fresh_retry
  | Task_event.Resume_retry -> Workflow_event.Resume_retry

let int64_option = function
  | None -> Ok None
  | Some value when value < 0 -> Error "negative token usage"
  | Some value -> Ok (Some (Int64.of_int value))

let micros_of_usd = function
  | None -> Ok None
  | Some usd -> (
      match classify_float usd with
      | FP_nan | FP_infinite -> Error "non-finite USD cost"
      | FP_normal | FP_subnormal | FP_zero ->
          if usd < 0.0 then Error "negative USD cost"
          else
            let scaled = usd *. 1_000_000.0 in
            let int64_limit = Float.ldexp 1.0 63 in
            if classify_float scaled = FP_infinite || scaled >= int64_limit then
              Ok (Some Int64.max_int)
            else Ok (Some (Int64.of_float (ceil scaled))))

let map_cost_record (cost : Backend_types.cost) =
  let* input_tokens = int64_option cost.tokens_input in
  let* output_tokens = int64_option cost.tokens_output in
  let* cache_creation_tokens = int64_option cost.cache_creation_input_tokens in
  let* cache_read_tokens = int64_option cost.cache_read_input_tokens in
  let* usd_micros = micros_of_usd cost.cost_usd in
  let* usage =
    Execution_metrics.make_usage ?input_tokens ?output_tokens
      ?cache_creation_tokens ?cache_read_tokens ()
  in
  let* cost = Execution_metrics.make_cost ?usd_micros () in
  Ok (usage, cost)

let map_optional_cost = function
  | None -> Ok (None, None)
  | Some cost ->
      let* usage, cost = map_cost_record cost in
      Ok (Some usage, Some cost)

let standard_object_or_array = function
  | (`Assoc _ | `List _) as json -> (
      match
        Cabal_workflow_runner.Canonical_json.validate_standard
          ~max_depth:Agent_execution.max_json_depth
          ~max_nodes:Agent_execution.max_json_nodes
          ~max_bytes:Agent_execution.max_json_bytes json
      with
      | Ok () -> Some json
      | Error _ -> None)
  | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ | `Tuple _
  | `Variant _ ->
      None

let strict_json text =
  if String.length text > Agent_execution.max_json_bytes then None
  else
    try standard_object_or_array (Yojson.Safe.from_string text)
    with Yojson.Json_error _ | Stack_overflow -> None

let structured_output (result : Backend_types.task_result) =
  let from_text = strict_json result.agent_text in
  let from_report =
    match result.report with
    | Some {Backend_types.raw_json = Some raw_json; _} ->
        standard_object_or_array raw_json
    | None | Some _ -> None
  in
  match (from_report, from_text) with
  | Some report, Some text when not (Yojson.Safe.equal report text) ->
      Error "conflicting structured output sources"
  | Some report, (None | Some _) -> Ok (Some report)
  | None, from_text -> Ok from_text

let web_policy_of_cabal = function
  | Backend_types.Web_disabled -> Agent_execution.web_disabled
  | Backend_types.Web_search -> Agent_execution.web_search
  | Backend_types.Web_search_and_fetch ->
      Agent_execution.web_search_and_fetch

let map_delivery (delivery : Backend_types.attempt_delivery) =
  let attachment_delivery =
    match delivery.attachment_delivery with
    | Backend_types.Upload_attachments -> Agent_execution.Upload_attachments
    | Backend_types.Reuse_session_attachments ->
        Agent_execution.Reuse_session_attachments
  in
  Agent_execution.make_delivery_intent
    ~attachment_count:(List.length delivery.attachment_references)
    ~attachment_delivery
    ~web_policy:(web_policy_of_cabal delivery.web_access_policy) ()

let map_attempt (attempt : Backend_types.task_attempt) =
  let* delivery = map_delivery attempt.delivery in
  let* usage, cost = map_optional_cost attempt.result.cost in
  let* structured_json = structured_output attempt.result in
  let schema_error =
    Option.map
      (fun _ -> "structured output did not satisfy the required schema")
      attempt.schema_validation_error
  in
  Agent_execution.make_attempt ~number:attempt.number
    ~kind:(map_attempt_kind attempt.kind)
    ~status:(map_status attempt.result.status)
    ~elapsed_s:attempt.attempt_elapsed ~delivery ?schema_error
    ?session_id:(mapped_session_id attempt.result.session_id) ?usage ?cost
    ~text:attempt.result.agent_text
    ?structured_json ()

let map_attempts attempts =
  let rec loop mapped = function
    | [] -> Ok (List.rev mapped)
    | attempt :: rest ->
        let* attempt = map_attempt attempt in
        loop (attempt :: mapped) rest
  in
  loop [] attempts

let redacted_identifier value fallback =
  if safe_identifier value then value else fallback

let map_process_exit = function
  | "success" -> Workflow_event.Exited 0
  | "terminated" -> Workflow_event.Signaled
  | "failed" | "timeout" | "cancelled" | _ -> Workflow_event.Unknown

let map_omissions (counts : Task_event.delivery_truncation) =
  Workflow_event.make_omission_counts
    ~text_events:(Int64.of_int counts.agent_text_events)
    ~text_bytes:(Int64.of_int counts.agent_text_bytes)
    ~usage_events:(Int64.of_int counts.token_usage_events)
    ~session_events:(Int64.of_int counts.session_events)
    ~tool_events:(Int64.of_int counts.tool_events)
    ~control_events:(Int64.of_int counts.control_events) ()

let map_tool (tool : Task_event.tool) =
  let id =
    Option.bind tool.id (fun value ->
        if safe_identifier value then Some value else None)
  in
  Workflow_event.make_tool ?id
    ~name:(redacted_identifier tool.name "redacted-tool") ()

let map_event_payload = function
  | Task_event.Task_started -> Ok Workflow_event.Task_started
  | Task_event.Backend_selected {backend_id} ->
      Ok
        (Workflow_event.Backend_selected
           (redacted_identifier backend_id "redacted-backend"))
  | Task_event.Preflight_started -> Ok Workflow_event.Preflight_started
  | Task_event.Preflight_completed -> Ok Workflow_event.Preflight_completed
  | Task_event.Version_probe_started -> Ok Workflow_event.Version_probe_started
  | Task_event.Version_probe_completed ->
      Ok Workflow_event.Version_probe_completed
  | Task_event.Availability_check_started ->
      Ok Workflow_event.Availability_check_started
  | Task_event.Availability_check_completed ->
      Ok Workflow_event.Availability_check_completed
  | Task_event.Attempt_started kind ->
      Ok (Workflow_event.Attempt_started (map_attempt_kind kind))
  | Task_event.Attempt_finished outcome ->
      Ok (Workflow_event.Attempt_finished (map_attempt_outcome outcome))
  | Task_event.Retry_transition {kind; reason = _} ->
      Ok
        (Workflow_event.Retry_transition
             {
               kind = map_retry_kind kind;
              reason = Workflow_event.Other_redacted;
             })
  | Task_event.Process_started _ -> Ok Workflow_event.Process_started
  | Task_event.Process_termination_requested ->
      Ok Workflow_event.Process_termination_requested
  | Task_event.Process_kill_escalated -> Ok Workflow_event.Process_kill_escalated
  | Task_event.Process_exited {exit_status} ->
      Ok (Workflow_event.Process_exited (map_process_exit exit_status))
  | Task_event.Session_id session_id ->
      Ok
        (Workflow_event.Session_id
           (redacted_identifier session_id "redacted-session"))
  | Task_event.Agent_text_delta text -> Ok (Workflow_event.Agent_text_delta text)
  | Task_event.Tool_started tool ->
      let* tool = map_tool tool in
      Ok (Workflow_event.Tool_started tool)
  | Task_event.Tool_finished {id; name} ->
      let id =
        Option.bind id (fun value ->
            if safe_identifier value then Some value else None)
      in
      let name =
        Option.map (fun value -> redacted_identifier value "redacted-tool") name
      in
      Ok (Workflow_event.Tool_finished {id; name})
  | Task_event.Token_usage cost ->
      let* usage, cost = map_cost_record cost in
      Ok
        (Workflow_event.Usage_observed
           {usage = Some usage; cost = Some cost})
  | Task_event.Event_delivery_truncated counts ->
      let* counts = map_omissions counts in
      Ok (Workflow_event.Delivery_truncated counts)
  | Task_event.Terminal terminal ->
      Ok
        (Workflow_event.Terminal
           (match terminal with
           | Task_event.Succeeded -> Workflow_event.Succeeded
           | Task_event.Failed _ -> Workflow_event.Failed
           | Task_event.Timed_out -> Workflow_event.Timed_out
           | Task_event.Cancelled -> Workflow_event.Cancelled))

let map_event ~no_invocation (event : Task_event.t) =
  let* payload = map_event_payload event.payload in
  let attempt =
    match event.payload with
    | Task_event.Task_started | Task_event.Backend_selected _
    | Task_event.Preflight_started | Task_event.Preflight_completed
    | Task_event.Version_probe_started | Task_event.Version_probe_completed
    | Task_event.Availability_check_started
    | Task_event.Availability_check_completed ->
        0
    | Task_event.Terminal _ when no_invocation -> 0
    | Task_event.Attempt_started _ | Task_event.Attempt_finished _
    | Task_event.Retry_transition _ | Task_event.Process_started _
    | Task_event.Process_termination_requested
    | Task_event.Process_kill_escalated | Task_event.Process_exited _
    | Task_event.Session_id _ | Task_event.Agent_text_delta _
    | Task_event.Tool_started _ | Task_event.Tool_finished _
    | Task_event.Token_usage _ | Task_event.Event_delivery_truncated _
    | Task_event.Terminal _ ->
        event.attempt
  in
  Workflow_event.make ~seq:(Int64.of_int event.seq) ~attempt
    ~elapsed_s:event.timestamp payload

let map_trace ?(no_invocation = false)
    (trace : Backend_completer.event_trace) =
  let rec loop mapped = function
    | [] -> Ok (List.rev mapped)
    | event :: rest ->
        let* event = map_event ~no_invocation event in
        loop (event :: mapped) rest
  in
  let* events = loop [] trace.events in
  Workflow_event.make_trace ~omitted_count:(Int64.of_int trace.omitted_events)
    events

let trace_elapsed trace =
  match List.rev (Workflow_event.events trace) with
  | [] -> 0.0
  | event :: _ -> Workflow_event.elapsed_s event

let attempts_elapsed attempts =
  List.fold_left
    (fun total attempt -> total +. Agent_execution.attempt_elapsed_s attempt)
    0.0 attempts

let valid_elapsed value = Float.is_finite value && value >= 0.0

let total_elapsed execution attempts trace =
  if valid_elapsed execution.Backend_types.total_elapsed then
    Ok
      (max execution.Backend_types.total_elapsed
         (max (attempts_elapsed attempts) (trace_elapsed trace)))
  else Error "invalid central total elapsed time"

let map_cleanup = function
  | Backend_types.Cleanup_not_required -> Agent_execution.Cleanup_not_required
  | Backend_types.Cleanup_succeeded -> Agent_execution.Cleanup_succeeded
  | Backend_types.Cleanup_failed -> Agent_execution.Cleanup_failed

let represented_final_result execution =
  match List.rev execution.Backend_types.attempts with
  | [] -> false
  | attempt :: _ ->
      Backend_types.equal_task_result attempt.result execution.final_result

let make_response ~execution ~attempts ?event_trace ~status () =
  let* total_elapsed_s =
    match event_trace with
    | Some trace -> total_elapsed execution attempts trace
    | None ->
        if valid_elapsed execution.Backend_types.total_elapsed then
          Ok
            (max execution.Backend_types.total_elapsed
               (attempts_elapsed attempts))
        else Error "invalid central total elapsed time"
  in
  Agent_execution.make_response ~attempts ~status ~total_elapsed_s
    ~cleanup_status:(map_cleanup execution.cleanup_status) ?event_trace ()

let terminal_attempt trace =
  match List.rev (Workflow_event.events trace) with
  | event :: _ -> Workflow_event.attempt event
  | [] -> 0

let retry_kind_for continuation_number trace =
  List.find_map
    (fun event ->
      if Workflow_event.attempt event <> continuation_number - 1 then None
      else
        match Workflow_event.payload event with
        | Workflow_event.Retry_transition {kind; _} ->
            Some
              (match kind with
              | Workflow_event.Fresh_retry -> Workflow_event.Fresh_attempt
              | Workflow_event.Resume_retry -> Workflow_event.Resumed_attempt)
        | _ -> None)
    (Workflow_event.events trace)

let started_kind_for continuation_number trace =
  List.find_map
    (fun event ->
      if Workflow_event.attempt event <> continuation_number then None
      else
        match Workflow_event.payload event with
        | Workflow_event.Attempt_started kind -> Some kind
        | _ -> None)
    (Workflow_event.events trace)

let continuation_activity continuation_number trace =
  List.exists
    (fun event ->
      Workflow_event.attempt event = continuation_number
      &&
      match Workflow_event.payload event with
      | Workflow_event.Terminal _ | Workflow_event.Delivery_truncated _ -> false
      | _ -> true)
    (Workflow_event.events trace)

let inferred_retry_kind ~descriptor final_attempt =
  if
    descriptor.Backend_registry.capabilities.session_resume
    && Option.is_some (Agent_execution.attempt_session_id final_attempt)
  then Workflow_event.Resumed_attempt
  else Workflow_event.Fresh_attempt

let infer_continuation ~descriptor attempts trace =
  match List.rev attempts with
  | [] -> Ok None
  | final_attempt :: _ ->
      let number = Agent_execution.attempt_number final_attempt + 1 in
      if terminal_attempt trace < number then Ok None
      else if terminal_attempt trace > number then
        Error "outer trace advances beyond one incomplete continuation"
      else
        let kind =
          match retry_kind_for number trace with
          | Some kind -> kind
          | None -> (
              match started_kind_for number trace with
              | Some kind -> kind
              | None -> inferred_retry_kind ~descriptor final_attempt)
        in
        let invocation =
          if continuation_activity number trace then
            Agent_execution.Invocation_started
          else Agent_execution.Invocation_may_have_started
        in
        let* continuation =
          Agent_execution.make_incomplete_continuation ~number ~kind ~invocation
            ()
        in
        Ok (Some continuation)

let make_incomplete_error ~descriptor ~message execution attempts trace =
  let* continuation = infer_continuation ~descriptor attempts trace in
  let outer_status = map_status execution.Backend_types.final_result.status in
  let* total_elapsed_s = total_elapsed execution attempts trace in
  let* incomplete =
    Agent_execution.make_incomplete_execution ~completed_attempts:attempts
      ~outer_status ~total_elapsed_s
      ~cleanup_status:(map_cleanup execution.cleanup_status) ?continuation
      ~outer_event_trace:trace ()
  in
  Agent_execution.make_incomplete_execution_error ~message
    ~execution:incomplete ()

let map_dispatch_kind = function
  | Runtime_dispatch.Invalid_timeout -> Agent_execution.Invalid_request
  | Runtime_dispatch.Backend_not_registered -> Agent_execution.Backend_unavailable
  | Runtime_dispatch.Runtime_registration_untrusted
  | Runtime_dispatch.Backend_quarantined _
  | Runtime_dispatch.Backend_version_unsupported ->
      Agent_execution.Capability_mismatch
  | Runtime_dispatch.Preflight_failed _ -> Agent_execution.Preflight_failed
  | Runtime_dispatch.Backend_unavailable -> Agent_execution.Backend_unavailable
  | Runtime_dispatch.Version_check_failed
  | Runtime_dispatch.Availability_check_failed
  | Runtime_dispatch.Prepared_already_consumed ->
      Agent_execution.Internal_dispatch_failure
  | Runtime_dispatch.Backend_execution_failed
  | Runtime_dispatch.Schema_enforcement_failed _ ->
      Agent_execution.Internal_dispatch_failure

let dispatch_may_have_started = function
  | Runtime_dispatch.Backend_execution_failed
  | Runtime_dispatch.Schema_enforcement_failed _ ->
      true
  | Runtime_dispatch.Invalid_timeout
  | Runtime_dispatch.Backend_not_registered
  | Runtime_dispatch.Runtime_registration_untrusted
  | Runtime_dispatch.Backend_quarantined _
  | Runtime_dispatch.Preflight_failed _
  | Runtime_dispatch.Backend_version_unsupported
  | Runtime_dispatch.Version_check_failed
  | Runtime_dispatch.Backend_unavailable
  | Runtime_dispatch.Availability_check_failed
  | Runtime_dispatch.Prepared_already_consumed ->
      false

let map_plain_dispatch_error failure message trace =
  if dispatch_may_have_started failure then
    no_completed_error ~event_trace:trace
      ~status:(Agent_execution.Failed "backend execution failed") message
  else
    dispatch_error ~event_trace:trace (map_dispatch_kind failure) message

let schema_failure_consistent attempt_2_failure execution =
  match List.rev execution.Backend_types.attempts with
  | [] | [_] -> false
  | second :: _ -> (
      match attempt_2_failure with
      | Backend_types.Schema_validation_failure _ ->
          second.result.status = Backend_types.Success
          && Option.is_some second.schema_validation_error
      | Backend_types.Transport_failure status ->
          status = second.result.status && status <> Backend_types.Success
      | Backend_types.Resume_failure status ->
          second.kind = Backend_types.Resumed_attempt
          && status = second.result.status
          && status <> Backend_types.Success)

let map_execution_failure trace message = function
  | Backend_types.Native_backend_failure_with_schema {execution; message = _} ->
      let* attempts = map_attempts execution.attempts in
      let* response =
        make_response ~execution ~attempts ~event_trace:trace
          ~status:(map_status execution.final_result.status) ()
      in
      Agent_execution.make_execution_error
        ~kind:Agent_execution.Native_backend_failure_with_schema ~message
        ~response ()
  | Backend_types.Schema_retry_failed
      {execution; attempt_1_validation_error = _; attempt_2_failure} ->
      if not (schema_failure_consistent attempt_2_failure execution) then
        Error "incoherent Cabal schema retry failure"
      else
        let* attempts = map_attempts execution.attempts in
        let status =
          match attempt_2_failure with
          | Backend_types.Schema_validation_failure _ ->
              Agent_execution.Failed "schema retry failed"
          | Backend_types.Transport_failure status
          | Backend_types.Resume_failure status ->
              map_status status
        in
        let* response =
          make_response ~execution ~attempts ~event_trace:trace ~status ()
        in
        Agent_execution.make_execution_error
          ~kind:Agent_execution.Schema_retry_failed ~message ~response ()

let map_ok ~descriptor (response : Backend_completer.rich_completion_response) =
  match map_trace response.event_trace with
  | Error reason -> Error (mapping_failure ~reason ())
  | Ok trace ->
      let execution = response.execution in
      if execution.attempts = [] then
        let status = map_status execution.final_result.status in
        if status = Agent_execution.Success then
          Error (mapping_failure ~event_trace:trace ())
        else
          Error
            (no_completed_error ~event_trace:trace ~status
               "central execution ended without a completed backend result")
      else
        match map_attempts execution.attempts with
        | Error reason -> Error (mapping_failure ~event_trace:trace ~reason ())
        | Ok attempts ->
            if represented_final_result execution then
              let status = map_status execution.final_result.status in
              (match
                 make_response ~execution ~attempts ~event_trace:trace ~status ()
               with
              | Ok response -> Ok response
               | Error reason ->
                   Error (mapping_failure ~event_trace:trace ~reason ()))
            else
              (match
                 make_incomplete_error ~descriptor
                   ~message:
                     "central execution ended after incomplete invocation progress"
                   execution attempts trace
               with
              | Ok error -> Error error
                | Error reason ->
                    Error (mapping_failure ~event_trace:trace ~reason ()))

let map_error ~descriptor (error : Backend_completer.rich_completion_error) =
  let message = Backend_completer.render_rich_completion_error error in
  match error.cause with
  | Runtime_dispatch.Dispatch_failure failure -> (
      match
        map_trace ~no_invocation:(not (dispatch_may_have_started failure))
          error.event_trace
      with
      | Error reason -> mapping_failure ~reason ()
      | Ok trace ->
          map_plain_dispatch_error failure message trace
      )
  | Runtime_dispatch.Dispatch_failure_with_execution {failure; execution} -> (
      match map_trace error.event_trace with
      | Error reason -> mapping_failure ~reason ()
      | Ok trace ->
          (match map_attempts execution.attempts with
          | Error reason -> mapping_failure ~event_trace:trace ~reason ()
          | Ok attempts ->
              if represented_final_result execution then
                let status = map_status execution.final_result.status in
                let response = make_response ~execution ~attempts ~status () in
                (match response with
                 | Error reason ->
                     mapping_failure ~event_trace:trace ~reason ()
                | Ok response -> (
                    match
                      Agent_execution.make_post_execution_dispatch_error
                        ~cause:(map_dispatch_kind failure) ~message ~response
                        ~outer_event_trace:trace ()
                    with
                    | Ok error -> error
                     | Error reason ->
                         mapping_failure ~event_trace:trace ~reason ()))
              else
                (match
                   make_incomplete_error ~descriptor ~message execution attempts
                     trace
                 with
                 | Ok error -> error
                  | Error reason ->
                      mapping_failure ~event_trace:trace ~reason ())))
  | Runtime_dispatch.Execution_failure failure -> (
      match map_trace error.event_trace with
      | Error reason -> mapping_failure ~reason ()
      | Ok trace ->
          match map_execution_failure trace message failure with
          | Ok error -> error
          | Error reason -> mapping_failure ~event_trace:trace ~reason ())

let complete_request ~bootstrap ~sw ~env ~limits ~default_backend ?custom_backend
    ~working_dir ~default_model request =
  match Agent_execution.read_only request with
  | None ->
      Error
        (dispatch_error Agent_execution.Invalid_request
           "Cabal bridge requires explicit read-only intent")
  | Some read_only -> (
      match map_web_policy (Agent_execution.web_policy request) with
      | Error message ->
          Error (dispatch_error Agent_execution.Unsupported_request message)
      | Ok web_access -> (
          match map_attachments (Agent_execution.attachments request) with
          | Error message ->
              Error (dispatch_error Agent_execution.Unsupported_request message)
          | Ok attachments ->
              let backend_id =
                Option.value ~default:default_backend
                  (Agent_execution.routing request)
              in
              match
                resolve_selection ~bootstrap ?custom_backend ~default_backend
                  backend_id
              with
              | Error selection_error ->
                  Error
                    (dispatch_error
                       (selection_failure_kind selection_error)
                       (render_selection_error selection_error))
              | Ok entry ->
                  let model =
                    match Agent_execution.model request with
                    | Some _ as model -> model
                    | None -> default_model
                  in
                  match
                    Backend_completer.make_rich ~sw ~env ~limits ~backend_name:backend_id
                      ~working_dir ?model ~read_only ()
                  with
                  | Error message ->
                      Error
                        (dispatch_error Agent_execution.Invalid_request message)
                  | Ok complete ->
                      let completion_request =
                        Backend_completer.make_completion_request
                          ~system_prompt:(Agent_execution.system_prompt request)
                          ~prompt:(Agent_execution.user_prompt request)
                          ?json_schema:(Agent_execution.json_schema request)
                          ?resume_session_id:
                            (Agent_execution.resume_session request)
                          ~attachments ~web_access
                          ~timeout:(Agent_execution.timeout_s request)
                          ?max_turns:(Agent_execution.max_turns request) ()
                      in
                      match complete completion_request with
                      | Ok response ->
                          map_ok ~descriptor:entry.effective_descriptor response
                      | Error error ->
                          Error
                            (map_error ~descriptor:entry.effective_descriptor
                               error)))

let create ~bootstrap ~sw ~env ~limits ~backend_id ~working_dir ?custom_backend
    ?default_model () =
  if not (Runtime_bootstrap.valid_runtime_id backend_id)
     || String.trim backend_id <> backend_id
  then Error "backend id must be explicit, non-blank, and canonical"
  else if String.trim working_dir = "" then Error "working directory is blank"
  else if not (valid_limits limits) then
    Error "caller-provided attachment limits are invalid"
  else if not (valid_default_model default_model) then
    Error "default model must be non-blank and trimmed"
  else
    match
      resolve_selection ~bootstrap ?custom_backend ~default_backend:backend_id
        backend_id
    with
    | Error error -> Error (render_selection_error error)
    | Ok entry ->
        let* capabilities = runtime_capabilities entry.effective_descriptor in
        Runtime.make ~identity:("cabal-" ^ backend_id) ~capabilities
          ~complete:
            (complete_request ~bootstrap ~sw ~env ~limits
               ~default_backend:backend_id ?custom_backend ~working_dir
               ~default_model) ()
