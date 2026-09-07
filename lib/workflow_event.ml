type attempt_kind = Initial_attempt | Fresh_attempt | Resumed_attempt

type attempt_outcome =
  | Attempt_succeeded
  | Attempt_failed
  | Attempt_timed_out
  | Attempt_cancelled

type retry_kind = Fresh_retry | Resume_retry

type retry_reason =
  | Schema_validation
  | Resume_rejected
  | Transport_retry
  | Other_redacted

type process_exit = Exited of int | Signaled | Unknown
type tool = { id : string option; name : string }

type omission_counts = {
  text_events : int64;
  text_bytes : int64;
  usage_events : int64;
  session_events : int64;
  tool_events : int64;
  control_events : int64;
}

type terminal = Succeeded | Failed | Timed_out | Cancelled

type payload =
  | Task_started
  | Backend_selected of string
  | Preflight_started
  | Preflight_completed
  | Version_probe_started
  | Version_probe_completed
  | Availability_check_started
  | Availability_check_completed
  | Attempt_started of attempt_kind
  | Attempt_finished of attempt_outcome
  | Retry_transition of { kind : retry_kind; reason : retry_reason }
  | Process_started
  | Process_termination_requested
  | Process_kill_escalated
  | Process_exited of process_exit
  | Session_id of string
  | Agent_text_delta of string
  | Tool_started of tool
  | Tool_finished of { id : string option; name : string option }
  | Usage_observed of {
      usage : Execution_metrics.usage option;
      cost : Execution_metrics.cost option;
    }
  | Delivery_truncated of omission_counts
  | Opaque_backend_observation
  | Terminal of terminal

type t = { seq : int64; attempt : int; elapsed_s : float; payload : payload }

let max_events = 256
let max_text_bytes = 16 * 1024

let finite_nonnegative value =
  match classify_float value with
  | FP_normal | FP_subnormal | FP_zero -> value >= 0.0
  | FP_infinite | FP_nan -> false

let safe_identifier ?(max_bytes = 128) value =
  let safe_character = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' | '.' -> true
    | _ -> false
  in
  value <> ""
  && String.length value <= max_bytes
  && String.for_all safe_character value

let validate_optional_identifier name = function
  | Some value when not (safe_identifier value) -> Error (name ^ " is invalid")
  | _ -> Ok ()

let nonnegative_count name value =
  if Int64.compare value 0L < 0 then Error (name ^ " must be non-negative")
  else Ok ()

let validate_omissions counts =
  Result.bind (nonnegative_count "text_events" counts.text_events) (fun () ->
      Result.bind (nonnegative_count "text_bytes" counts.text_bytes) (fun () ->
          Result.bind (nonnegative_count "usage_events" counts.usage_events)
            (fun () ->
              Result.bind
                (nonnegative_count "session_events" counts.session_events)
                (fun () ->
                  Result.bind
                    (nonnegative_count "tool_events" counts.tool_events)
                    (fun () ->
                      nonnegative_count "control_events" counts.control_events)))))

let validate_payload = function
  | Backend_selected value ->
      if safe_identifier value then Ok ()
      else Error "backend identifier is invalid"
  | Session_id value ->
      if safe_identifier value then Ok ()
      else Error "session identifier is invalid"
  | Agent_text_delta text ->
      if not (String.is_valid_utf_8 text) then
        Error "agent text is not valid UTF-8"
      else if String.length text > max_text_bytes then
        Error "agent text exceeds the event byte limit"
      else Ok ()
  | Tool_started { id; name } ->
      Result.bind (validate_optional_identifier "tool identifier" id) (fun () ->
          if safe_identifier name then Ok () else Error "tool name is invalid")
  | Tool_finished { id; name } ->
      Result.bind (validate_optional_identifier "tool identifier" id) (fun () ->
          match name with
          | Some value when not (safe_identifier value) ->
              Error "tool name is invalid"
          | _ -> Ok ())
  | Delivery_truncated counts -> validate_omissions counts
  | Task_started | Preflight_started | Preflight_completed
  | Version_probe_started | Version_probe_completed | Availability_check_started
  | Availability_check_completed | Attempt_started _ | Attempt_finished _
  | Retry_transition _ | Process_started | Process_termination_requested
  | Process_kill_escalated | Process_exited _ | Usage_observed _
  | Opaque_backend_observation | Terminal _ ->
      Ok ()

let make ~seq ~attempt ~elapsed_s payload =
  if Int64.compare seq 0L <= 0 then Error "event sequence must be positive"
  else if attempt < 0 then Error "event attempt must be non-negative"
  else if not (finite_nonnegative elapsed_s) then
    Error "event elapsed time must be finite and non-negative"
  else
    Result.map
      (fun () -> { seq; attempt; elapsed_s; payload })
      (validate_payload payload)

let seq event = event.seq
let attempt event = event.attempt
let elapsed_s event = event.elapsed_s
let payload event = event.payload

type trace = { events : t list; omitted_count : int64 }

let is_terminal event =
  match event.payload with Terminal _ -> true | _ -> false

let validate_order events =
  let rec loop previous = function
    | [] -> Ok ()
    | current :: rest -> (
        match previous with
        | None -> loop (Some current) rest
        | Some prior ->
            if Int64.compare current.seq prior.seq <= 0 then
              Error "event sequences must be strictly increasing"
            else if current.attempt < prior.attempt then
              Error "event attempts must be non-decreasing"
            else if current.elapsed_s < prior.elapsed_s then
              Error "event elapsed times must be non-decreasing"
            else loop (Some current) rest)
  in
  loop None events

let validate_terminal events =
  let rec loop count = function
    | [] -> count
    | event :: rest ->
        loop (if is_terminal event then count + 1 else count) rest
  in
  let terminal_count = loop 0 events in
  if terminal_count <> 1 then
    Error "event trace must contain exactly one terminal"
  else
    match List.rev events with
    | last :: _ when is_terminal last -> Ok ()
    | _ -> Error "event trace terminal must be last"

let make_trace ?(omitted_count = 0L) events =
  if Int64.compare omitted_count 0L < 0 then
    Error "omitted event count must be non-negative"
  else if events = [] then Error "event trace must not be empty"
  else if List.length events > max_events then
    Error "event trace exceeds the retained event limit"
  else
    Result.bind (validate_order events) (fun () ->
        Result.map
          (fun () -> { events; omitted_count })
          (validate_terminal events))

let events trace = trace.events
let omitted_count trace = trace.omitted_count
let int64_json value = `Intlit (Int64.to_string value)
let option_json encode = function Some value -> encode value | None -> `Null

let usage_to_yojson usage =
  `Assoc
    [
      ( "input_tokens",
        option_json int64_json (Execution_metrics.input_tokens usage) );
      ( "output_tokens",
        option_json int64_json (Execution_metrics.output_tokens usage) );
      ( "cache_creation_tokens",
        option_json int64_json (Execution_metrics.cache_creation_tokens usage)
      );
      ( "cache_read_tokens",
        option_json int64_json (Execution_metrics.cache_read_tokens usage) );
    ]

let cost_to_yojson cost =
  `Assoc
    [
      ("usd_micros", option_json int64_json (Execution_metrics.usd_micros cost));
    ]

let string_of_attempt_kind = function
  | Initial_attempt -> "initial"
  | Fresh_attempt -> "fresh"
  | Resumed_attempt -> "resumed"

let string_of_attempt_outcome = function
  | Attempt_succeeded -> "success"
  | Attempt_failed -> "failed"
  | Attempt_timed_out -> "timed_out"
  | Attempt_cancelled -> "cancelled"

let string_of_retry_kind = function
  | Fresh_retry -> "fresh"
  | Resume_retry -> "resume"

let string_of_retry_reason = function
  | Schema_validation -> "schema_validation"
  | Resume_rejected -> "resume_rejected"
  | Transport_retry -> "transport_retry"
  | Other_redacted -> "other_redacted"

let string_of_terminal = function
  | Succeeded -> "success"
  | Failed -> "failed"
  | Timed_out -> "timed_out"
  | Cancelled -> "cancelled"

let omission_counts_to_yojson counts =
  `Assoc
    [
      ("text_events", int64_json counts.text_events);
      ("text_bytes", int64_json counts.text_bytes);
      ("usage_events", int64_json counts.usage_events);
      ("session_events", int64_json counts.session_events);
      ("tool_events", int64_json counts.tool_events);
      ("control_events", int64_json counts.control_events);
    ]

let payload_to_yojson = function
  | Task_started -> `Assoc [ ("kind", `String "task_started") ]
  | Backend_selected backend_id ->
      `Assoc
        [
          ("kind", `String "backend_selected");
          ("backend_id", `String backend_id);
        ]
  | Preflight_started -> `Assoc [ ("kind", `String "preflight_started") ]
  | Preflight_completed -> `Assoc [ ("kind", `String "preflight_completed") ]
  | Version_probe_started ->
      `Assoc [ ("kind", `String "version_probe_started") ]
  | Version_probe_completed ->
      `Assoc [ ("kind", `String "version_probe_completed") ]
  | Availability_check_started ->
      `Assoc [ ("kind", `String "availability_check_started") ]
  | Availability_check_completed ->
      `Assoc [ ("kind", `String "availability_check_completed") ]
  | Attempt_started kind ->
      `Assoc
        [
          ("kind", `String "attempt_started");
          ("attempt_kind", `String (string_of_attempt_kind kind));
        ]
  | Attempt_finished outcome ->
      `Assoc
        [
          ("kind", `String "attempt_finished");
          ("outcome", `String (string_of_attempt_outcome outcome));
        ]
  | Retry_transition { kind; reason } ->
      `Assoc
        [
          ("kind", `String "retry_transition");
          ("retry_kind", `String (string_of_retry_kind kind));
          ("reason", `String (string_of_retry_reason reason));
        ]
  | Process_started -> `Assoc [ ("kind", `String "process_started") ]
  | Process_termination_requested ->
      `Assoc [ ("kind", `String "process_termination_requested") ]
  | Process_kill_escalated ->
      `Assoc [ ("kind", `String "process_kill_escalated") ]
  | Process_exited exit ->
      let exit_json =
        match exit with
        | Exited code ->
            `Assoc [ ("kind", `String "exited"); ("code", `Int code) ]
        | Signaled -> `Assoc [ ("kind", `String "signaled") ]
        | Unknown -> `Assoc [ ("kind", `String "unknown") ]
      in
      `Assoc [ ("kind", `String "process_exited"); ("exit", exit_json) ]
  | Session_id session_id ->
      `Assoc
        [ ("kind", `String "session_id"); ("session_id", `String session_id) ]
  | Agent_text_delta text ->
      `Assoc [ ("kind", `String "agent_text_delta"); ("text", `String text) ]
  | Tool_started { id; name } ->
      `Assoc
        [
          ("kind", `String "tool_started");
          ("id", option_json (fun value -> `String value) id);
          ("name", `String name);
        ]
  | Tool_finished { id; name } ->
      `Assoc
        [
          ("kind", `String "tool_finished");
          ("id", option_json (fun value -> `String value) id);
          ("name", option_json (fun value -> `String value) name);
        ]
  | Usage_observed { usage; cost } ->
      `Assoc
        [
          ("kind", `String "usage_observed");
          ("usage", option_json usage_to_yojson usage);
          ("cost", option_json cost_to_yojson cost);
        ]
  | Delivery_truncated counts ->
      `Assoc
        [
          ("kind", `String "delivery_truncated");
          ("omitted", omission_counts_to_yojson counts);
        ]
  | Opaque_backend_observation ->
      `Assoc [ ("kind", `String "opaque_backend_observation") ]
  | Terminal terminal ->
      `Assoc
        [
          ("kind", `String "terminal");
          ("status", `String (string_of_terminal terminal));
        ]

let event_to_yojson event =
  `Assoc
    [
      ("seq", int64_json event.seq);
      ("attempt", `Int event.attempt);
      ("elapsed_s", `Float event.elapsed_s);
      ("payload", payload_to_yojson event.payload);
    ]

let trace_to_yojson trace =
  `Assoc
    [
      ("schema_version", `String "cwr.workflow-event-trace/v1");
      ("events", `List (List.map event_to_yojson trace.events));
      ("omitted_count", int64_json trace.omitted_count);
    ]
