type attachment = {
  id : string;
  path : string;
  mime_type : string;
  sha256 : string;
  size_bytes : int64;
}

let safe_identifier ?(max_bytes = 128) value =
  let safe_character = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' | '.' -> true
    | _ -> false
  in
  value <> ""
  && String.length value <= max_bytes
  && String.for_all safe_character value

let has_forbidden_text_character value =
  String.exists
    (fun character ->
      let code = Char.code character in
      code = 0 || (code < 32 && character <> '\n' && character <> '\t'))
    value

let valid_text value =
  String.is_valid_utf_8 value && not (has_forbidden_text_character value)

let has_no_control_characters value =
  String.for_all
    (fun character ->
      let code = Char.code character in
      code >= 32 && code <> 127)
    value

let valid_path path =
  let components = String.split_on_char '/' path in
  path <> ""
  && String.length path <= 4096
  && path.[0] <> '/'
  && (not (String.contains path '\\'))
  && (not (String.contains path ':'))
  && valid_text path
  && has_no_control_characters path
  && List.for_all
       (fun component ->
         component <> "" && component <> "." && component <> "..")
       components

let mime_token_character = function
  | 'a' .. 'z'
  | '0' .. '9'
  | '!' | '#' | '$' | '&' | '^' | '_' | '.' | '+' | '-' ->
      true
  | _ -> false

let canonical_mime_type value =
  let lowered = String.lowercase_ascii value in
  match String.split_on_char '/' lowered with
  | [ kind; subtype ]
    when kind <> "" && subtype <> ""
         && String.length lowered <= 127
         && String.for_all mime_token_character kind
         && String.for_all mime_token_character subtype ->
      Some lowered
  | _ -> None

let valid_sha256 value =
  String.length value = 64
  && String.for_all
       (function '0' .. '9' | 'a' .. 'f' -> true | _ -> false)
       value

let make_attachment ~id ~path ~mime_type ~sha256 ~size_bytes () =
  if not (safe_identifier id) then Error "attachment identifier is invalid"
  else if not (valid_path path) then Error "attachment path is invalid"
  else
    match canonical_mime_type mime_type with
    | None -> Error "attachment MIME type is invalid"
    | Some mime_type ->
        if not (valid_sha256 sha256) then Error "attachment SHA-256 is invalid"
        else if Int64.compare size_bytes 0L < 0 then
          Error "attachment size must be non-negative"
        else Ok { id; path; mime_type; sha256; size_bytes }

let attachment_id attachment = attachment.id
let attachment_path attachment = attachment.path
let attachment_mime_type attachment = attachment.mime_type
let attachment_sha256 attachment = attachment.sha256
let attachment_size_bytes attachment = attachment.size_bytes

type web_level = Web_disabled | Web_search | Web_search_and_fetch
type web_policy = { level : web_level; restricted_domains : string list option }

let web_disabled = { level = Web_disabled; restricted_domains = None }
let web_search = { level = Web_search; restricted_domains = None }

let web_search_and_fetch =
  { level = Web_search_and_fetch; restricted_domains = None }

let valid_domain_label label =
  let length = String.length label in
  length > 0 && length <= 63
  && label.[0] <> '-'
  && label.[length - 1] <> '-'
  && String.for_all
       (function 'a' .. 'z' | '0' .. '9' | '-' -> true | _ -> false)
       label

let valid_domain domain =
  domain <> ""
  && String.length domain <= 253
  && List.for_all valid_domain_label (String.split_on_char '.' domain)

let max_restricted_domains = 128

let make_restricted_web_policy ~level ~domains () =
  if level = Web_disabled then
    Error "disabled web access cannot have restricted domains"
  else if domains = [] then Error "restricted web domains must not be empty"
  else if List.length domains > max_restricted_domains then
    Error "restricted web domain limit exceeded"
  else if not (List.for_all valid_domain domains) then
    Error "restricted web domain is invalid"
  else if
    List.length domains <> List.length (List.sort_uniq String.compare domains)
  then Error "restricted web domains must be unique"
  else Ok { level; restricted_domains = Some domains }

let web_level policy = policy.level
let restricted_domains policy = policy.restricted_domains

type request = {
  id : string;
  system_prompt : string;
  user_prompt : string;
  json_schema : Yojson.Safe.t option;
  resume_session : string option;
  attachments : attachment list;
  web_policy : web_policy;
  timeout_s : float;
  max_turns : int option;
  routing : string option;
  model : string option;
  read_only : bool option;
}

let finite value =
  match classify_float value with
  | FP_normal | FP_subnormal | FP_zero -> true
  | FP_infinite | FP_nan -> false

let max_json_depth = 64
let max_json_nodes = 10_000
let max_json_bytes = 1024 * 1024
let max_public_text_bytes = 256 * 1024
let max_attempts = 8
let max_response_projection_bytes = 24 * 1024 * 1024
let max_error_projection_bytes = max_response_projection_bytes + 1024

let validate_standard_json json =
  Canonical_json.validate_standard ~max_depth:max_json_depth
    ~max_nodes:max_json_nodes ~max_bytes:max_json_bytes json

let validate_json_schema = function
  | (`Assoc _ | `Bool _) as schema -> validate_standard_json schema
  | `Null | `Int _ | `Intlit _ | `Float _ | `String _ | `List _ | `Tuple _
  | `Variant _ ->
      Error "JSON Schema root must be an object or boolean"

let valid_hint value =
  value <> ""
  && String.length value <= 256
  && valid_text value
  && has_no_control_characters value
  && String.trim value = value

let validate_optional condition error = function
  | Some value when not (condition value) -> Error error
  | _ -> Ok ()

let validate_optional_result validate = function
  | None -> Ok ()
  | Some value -> validate value

let attachment_ids_unique attachments =
  let ids = List.map attachment_id attachments in
  List.length ids = List.length (List.sort_uniq String.compare ids)

let make_request ~id ~system_prompt ~user_prompt ~timeout_s ?json_schema
    ?resume_session ?(attachments = []) ?(web_policy = web_disabled) ?max_turns
    ?routing ?model ?read_only () =
  if not (safe_identifier id) then Error "request identifier is invalid"
  else if not (valid_text system_prompt) then Error "system prompt is invalid"
  else if not (valid_text user_prompt) then Error "user prompt is invalid"
  else if not (finite timeout_s && timeout_s > 0.0) then
    Error "request timeout must be finite and positive"
  else
    Result.bind (validate_optional_result validate_json_schema json_schema)
      (fun () ->
        Result.bind
          (validate_optional safe_identifier "resume session is invalid"
             resume_session) (fun () ->
            if not (attachment_ids_unique attachments) then
              Error "attachment identifiers must be unique"
            else
              Result.bind
                (validate_optional
                   (fun value -> value > 0)
                   "maximum turns must be positive" max_turns)
                (fun () ->
                  Result.bind
                    (validate_optional safe_identifier "routing hint is invalid"
                       routing) (fun () ->
                      Result.map
                        (fun () ->
                          {
                            id;
                            system_prompt;
                            user_prompt;
                            json_schema;
                            resume_session;
                            attachments;
                            web_policy;
                            timeout_s;
                            max_turns;
                            routing;
                            model;
                            read_only;
                          })
                        (validate_optional valid_hint "model hint is invalid"
                           model)))))

let id request = request.id
let system_prompt request = request.system_prompt
let user_prompt request = request.user_prompt
let json_schema request = request.json_schema
let resume_session request = request.resume_session
let attachments request = request.attachments
let web_policy request = request.web_policy
let timeout_s request = request.timeout_s
let max_turns request = request.max_turns
let routing request = request.routing
let model request = request.model
let read_only request = request.read_only

type attachment_delivery = Upload_attachments | Reuse_session_attachments

type delivery_intent = {
  attachment_count : int;
  attachment_delivery : attachment_delivery;
  web_policy : web_policy;
}

let make_delivery_intent ~attachment_count ~attachment_delivery ~web_policy () =
  if attachment_count < 0 then
    Error "delivery attachment count must be non-negative"
  else Ok { attachment_count; attachment_delivery; web_policy }

let delivery_attachment_count delivery = delivery.attachment_count
let delivery_attachment_mode delivery = delivery.attachment_delivery
let delivery_web_policy delivery = delivery.web_policy

type attempt_kind = Workflow_event.attempt_kind =
  | Initial_attempt
  | Fresh_attempt
  | Resumed_attempt

type status = Success | Failed of string | Timed_out | Cancelled

type attempt = {
  number : int;
  kind : attempt_kind;
  status : status;
  text : string;
  structured_json : Yojson.Safe.t option;
  schema_error : string option;
  delivery : delivery_intent;
  elapsed_s : float;
  session_id : string option;
  usage : Execution_metrics.usage option;
  cost : Execution_metrics.cost option;
}

let normalize_line_endings value =
  if not (String.contains value '\r') then value
  else
    let length = String.length value in
    let buffer = Buffer.create length in
    let rec copy index =
      if index < length then
        if value.[index] = '\r' then (
          Buffer.add_char buffer '\n';
          if index + 1 < length && value.[index + 1] = '\n' then copy (index + 2)
          else copy (index + 1))
        else (
          Buffer.add_char buffer value.[index];
          copy (index + 1))
    in
    copy 0;
    Buffer.contents buffer

let normalize_nonempty_diagnostic field value =
  let value = normalize_line_endings value in
  if value = "" || not (valid_text value) then Error (field ^ " is invalid")
  else Ok value

let normalize_status = function
  | Failed message ->
      Result.map
        (fun message -> Failed message)
        (normalize_nonempty_diagnostic "failure diagnostic" message)
  | (Success | Timed_out | Cancelled) as status -> Ok status

let make_attempt ~number ~kind ~status ~elapsed_s ~delivery ?schema_error
    ?session_id ?usage ?cost ?(text = "") ?structured_json () =
  if number <= 0 then Error "attempt number must be positive"
  else if not (finite elapsed_s && elapsed_s >= 0.0) then
    Error "attempt elapsed time must be finite and non-negative"
  else
    Result.bind (normalize_status status) (fun status ->
        Result.bind
          (match schema_error with
          | None -> Ok None
          | Some _ when status <> Success ->
              Error "schema error requires a successful transport status"
          | Some error ->
              Result.map Option.some
                (normalize_nonempty_diagnostic "schema error" error))
          (fun schema_error ->
            Result.bind
              (validate_optional safe_identifier "attempt session is invalid"
                 session_id) (fun () ->
                let text = normalize_line_endings text in
                if not (valid_text text) then Error "attempt text is invalid"
                else if String.length text > max_public_text_bytes then
                  Error "attempt text exceeds the public output byte limit"
                else
                  Result.bind
                    (validate_optional_result validate_standard_json
                       structured_json) (fun () ->
                      Ok
                        {
                          number;
                          kind;
                          status;
                          text;
                          structured_json;
                          schema_error;
                          delivery;
                          elapsed_s;
                          session_id;
                          usage;
                          cost;
                        }))))

let attempt_number attempt = attempt.number
let attempt_kind attempt = attempt.kind
let attempt_status attempt = attempt.status
let attempt_text attempt = attempt.text
let attempt_structured_json attempt = attempt.structured_json
let attempt_schema_error attempt = attempt.schema_error
let attempt_delivery attempt = attempt.delivery
let attempt_elapsed_s attempt = attempt.elapsed_s
let attempt_session_id attempt = attempt.session_id
let attempt_usage attempt = attempt.usage
let attempt_cost attempt = attempt.cost

type cleanup_status =
  | Cleanup_not_required
  | Cleanup_succeeded
  | Cleanup_failed

type response = {
  attempts : attempt list;
  status : status;
  final_attempt : attempt;
  total_elapsed_s : float;
  total_usage : Execution_metrics.usage option;
  total_cost : Execution_metrics.cost option;
  final_session_id : string option;
  cleanup_status : cleanup_status;
  event_trace : Workflow_event.trace option;
}

let validate_attempt_order attempts =
  let rec loop expected_number = function
    | [] -> Ok ()
    | attempt :: rest ->
        if attempt.number <> expected_number then
          Error "attempt numbers must be contiguous and ordered"
        else if expected_number = 1 && attempt.kind <> Initial_attempt then
          Error "the first attempt must be initial"
        else if expected_number > 1 && attempt.kind = Initial_attempt then
          Error "only the first attempt may be initial"
        else loop (expected_number + 1) rest
  in
  loop 1 attempts

let outcome_matches_status outcome status =
  match (outcome, status) with
  | Workflow_event.Attempt_succeeded, Success
  | Attempt_failed, Failed _
  | Attempt_timed_out, Timed_out
  | Attempt_cancelled, Cancelled ->
      true
  | Attempt_succeeded, (Failed _ | Timed_out | Cancelled)
  | Attempt_failed, (Success | Timed_out | Cancelled)
  | Attempt_timed_out, (Success | Failed _ | Cancelled)
  | Attempt_cancelled, (Success | Failed _ | Timed_out) ->
      false

let terminal_matches_status terminal status =
  match (terminal, status) with
  | Workflow_event.Succeeded, Success
  | Failed, Failed _
  | Timed_out, Timed_out
  | Cancelled, Cancelled ->
      true
  | Succeeded, (Failed _ | Timed_out | Cancelled)
  | Failed, (Success | Timed_out | Cancelled)
  | Timed_out, (Success | Failed _ | Cancelled)
  | Cancelled, (Success | Failed _ | Timed_out) ->
      false

let find_attempt attempts number =
  List.find_opt (fun attempt -> attempt.number = number) attempts

let attempt_timing_tolerance_s = 0.001

type retained_metric_dimension = {
  mutable lower_bound : int64 option;
  mutable exact_final : int64 option;
}

type retained_usage_observation = {
  input_tokens : retained_metric_dimension;
  output_tokens : retained_metric_dimension;
  cache_creation_tokens : retained_metric_dimension;
  cache_read_tokens : retained_metric_dimension;
  usd_micros : retained_metric_dimension;
}

let empty_retained_dimension () = { lower_bound = None; exact_final = None }

let empty_retained_usage_observation () =
  {
    input_tokens = empty_retained_dimension ();
    output_tokens = empty_retained_dimension ();
    cache_creation_tokens = empty_retained_dimension ();
    cache_read_tokens = empty_retained_dimension ();
    usd_micros = empty_retained_dimension ();
  }

let clear_exact_usage observation =
  observation.input_tokens.exact_final <- None;
  observation.output_tokens.exact_final <- None;
  observation.cache_creation_tokens.exact_final <- None;
  observation.cache_read_tokens.exact_final <- None;
  observation.usd_micros.exact_final <- None

let retain_metric_dimension ~exact_is_known dimension value =
  (match (dimension.lower_bound, value) with
  | Some previous, Some current when Int64.compare current previous > 0 ->
      dimension.lower_bound <- Some current
  | None, Some current -> dimension.lower_bound <- Some current
  | Some _, (Some _ | None) | None, None -> ());
  dimension.exact_final <- if exact_is_known then value else None

let retain_usage_observation ~exact_is_known observation ~usage ~cost =
  let usage_field get = Option.bind usage get in
  retain_metric_dimension ~exact_is_known observation.input_tokens
    (usage_field Execution_metrics.input_tokens);
  retain_metric_dimension ~exact_is_known observation.output_tokens
    (usage_field Execution_metrics.output_tokens);
  retain_metric_dimension ~exact_is_known observation.cache_creation_tokens
    (usage_field Execution_metrics.cache_creation_tokens);
  retain_metric_dimension ~exact_is_known observation.cache_read_tokens
    (usage_field Execution_metrics.cache_read_tokens);
  retain_metric_dimension ~exact_is_known observation.usd_micros
    (Option.bind cost Execution_metrics.usd_micros)

let retained_dimension_matches dimension aggregate =
  let above_lower_bound =
    match dimension.lower_bound with
    | None -> true
    | Some lower_bound -> (
        match aggregate with
        | Some value -> Int64.compare value lower_bound >= 0
        | None -> false)
  in
  above_lower_bound
  &&
  match dimension.exact_final with
  | None -> true
  | Some exact -> aggregate = Some exact

let retained_usage_matches observation retained =
  let retained_field get = Option.bind retained get in
  retained_dimension_matches observation.input_tokens
    (retained_field Execution_metrics.input_tokens)
  && retained_dimension_matches observation.output_tokens
       (retained_field Execution_metrics.output_tokens)
  && retained_dimension_matches observation.cache_creation_tokens
       (retained_field Execution_metrics.cache_creation_tokens)
  && retained_dimension_matches observation.cache_read_tokens
       (retained_field Execution_metrics.cache_read_tokens)

let retained_cost_matches observation retained =
  retained_dimension_matches observation.usd_micros
    (Option.bind retained Execution_metrics.usd_micros)

let validate_trace ~attempts ~status ~total_elapsed_s trace =
  let starts = Hashtbl.create (List.length attempts) in
  let usage_observations = Hashtbl.create (List.length attempts) in
  let previous_event = ref None in
  let no_unlocated_omissions =
    Int64.compare (Workflow_event.omitted_count trace) 0L = 0
  in
  match List.rev attempts with
  | [] -> Error "event trace requires a response attempt"
  | final_attempt :: _ ->
      let mark_usage_unknown attempt_number =
        match Hashtbl.find_opt usage_observations attempt_number with
        | None -> ()
        | Some observation -> clear_exact_usage observation
      in
      let note_sequence_gap event =
        (match !previous_event with
        | Some previous
          when Int64.compare (Workflow_event.seq event)
                 (Int64.succ (Workflow_event.seq previous))
               > 0 ->
            mark_usage_unknown (Workflow_event.attempt previous)
        | None | Some _ -> ());
        previous_event := Some event
      in
      let validate_event event =
        note_sequence_gap event;
        if Workflow_event.elapsed_s event > total_elapsed_s then
          Error "event trace exceeds the response elapsed time"
        else
          match Workflow_event.payload event with
          | Workflow_event.Terminal terminal ->
              if Workflow_event.attempt event <> final_attempt.number then
                Error "event terminal disagrees with the final attempt"
              else if not (terminal_matches_status terminal status) then
                Error "event terminal disagrees with the response status"
              else Ok ()
          | payload when Workflow_event.attempt event = 0 -> (
              match payload with
              | Workflow_event.Task_started | Backend_selected _
              | Preflight_started | Preflight_completed | Version_probe_started
              | Version_probe_completed | Availability_check_started
              | Availability_check_completed | Delivery_truncated _
              | Opaque_backend_observation ->
                  Ok ()
              | Attempt_started _ | Attempt_finished _ | Retry_transition _
              | Process_started | Process_termination_requested
              | Process_kill_escalated | Process_exited _ | Session_id _
              | Agent_text_delta _ | Tool_started _ | Tool_finished _
              | Usage_observed _ | Terminal _ ->
                  Error "attempt event has no matching response attempt")
          | payload -> (
              match find_attempt attempts (Workflow_event.attempt event) with
              | None -> Error "event refers to an unknown response attempt"
              | Some attempt -> (
                  match payload with
                  | Workflow_event.Attempt_started kind ->
                      if kind <> attempt.kind then
                        Error
                          "event attempt kind disagrees with response telemetry"
                      else (
                        Hashtbl.replace starts attempt.number
                          (Workflow_event.elapsed_s event);
                        Ok ())
                  | Attempt_finished outcome -> (
                      if not (outcome_matches_status outcome attempt.status)
                      then
                        Error
                          "event attempt outcome disagrees with response \
                           telemetry"
                      else
                        match Hashtbl.find_opt starts attempt.number with
                        | Some started
                          when attempt.elapsed_s
                               > Workflow_event.elapsed_s event
                                 -. started +. attempt_timing_tolerance_s ->
                            Error
                              "event attempt timing disagrees with response \
                               telemetry"
                        | Some _ | None -> Ok ())
                  | Session_id session_id ->
                      if attempt.session_id <> Some session_id then
                        Error "event session disagrees with response telemetry"
                      else Ok ()
                  | Usage_observed { usage; cost } ->
                      let observation =
                        match
                          Hashtbl.find_opt usage_observations attempt.number
                        with
                        | Some observation -> observation
                        | None ->
                            let observation =
                              empty_retained_usage_observation ()
                            in
                            Hashtbl.add usage_observations attempt.number
                              observation;
                            observation
                      in
                      retain_usage_observation
                        ~exact_is_known:no_unlocated_omissions observation
                        ~usage ~cost;
                      Ok ()
                  | Retry_transition { kind; reason } -> (
                      match find_attempt attempts (attempt.number + 1) with
                      | None ->
                          Error
                            "retry transition has no following response attempt"
                      | Some next_attempt
                        when next_attempt.kind
                             <>
                             (match kind with
                             | Workflow_event.Fresh_retry -> Fresh_attempt
                             | Resume_retry -> Resumed_attempt) ->
                          Error
                            "retry transition disagrees with response attempt \
                             kind"
                      | Some _
                        when reason = Schema_validation
                             && attempt.schema_error = None ->
                          Error
                            "schema retry event has no matching validation \
                             error"
                      | Some _ -> Ok ())
                  | Task_started | Backend_selected _ | Preflight_started
                  | Preflight_completed | Version_probe_started
                  | Version_probe_completed | Availability_check_started
                  | Availability_check_completed ->
                      Error "pre-dispatch event refers to a response attempt"
                  | Delivery_truncated counts ->
                      if
                        Int64.compare
                          (Workflow_event.omitted_usage_events counts)
                          0L
                        > 0
                      then mark_usage_unknown attempt.number;
                      Ok ()
                  | Process_started | Process_termination_requested
                  | Process_kill_escalated | Process_exited _
                  | Agent_text_delta _ | Tool_started _ | Tool_finished _
                  | Opaque_backend_observation ->
                      Ok ()
                  | Terminal _ -> Error "terminal event validation failed"))
      in
      let rec loop = function
        | [] ->
            Hashtbl.fold
              (fun attempt_number observation result ->
                Result.bind result (fun () ->
                    match find_attempt attempts attempt_number with
                    | None ->
                        Error "usage event has no matching response attempt"
                    | Some attempt ->
                        if
                          not
                            (retained_usage_matches observation attempt.usage)
                        then
                          Error
                            "event usage disagrees with response telemetry"
                        else if
                          not (retained_cost_matches observation attempt.cost)
                        then
                          Error "event cost disagrees with response telemetry"
                        else Ok ()))
              usage_observations (Ok ())
        | event :: rest ->
            Result.bind (validate_event event) (fun () -> loop rest)
      in
      loop (Workflow_event.events trace)

let validate_response_status status (final_attempt : attempt) =
  match (final_attempt.status, final_attempt.schema_error, status) with
  | Success, None, Success -> Ok ()
  | Failed left, None, Failed right when left = right -> Ok ()
  | Timed_out, None, Timed_out | Cancelled, None, Cancelled -> Ok ()
  | Success, Some _, Failed _ -> Ok ()
  | _ -> Error "response status disagrees with the final attempt"

let durations_fit total_elapsed_s attempts =
  let rec loop remaining = function
    | [] -> true
    | attempt :: rest ->
        attempt.elapsed_s <= remaining +. 1e-9
        && loop (remaining -. attempt.elapsed_s) rest
  in
  loop total_elapsed_s attempts

let projection_int64 value = `Intlit (Int64.to_string value)

let projection_option encode = function
  | Some value -> encode value
  | None -> `Null

let usage_projection usage =
  `Assoc
    [
      ( "input_tokens",
        projection_option projection_int64
          (Execution_metrics.input_tokens usage) );
      ( "output_tokens",
        projection_option projection_int64
          (Execution_metrics.output_tokens usage) );
      ( "cache_creation_tokens",
        projection_option projection_int64
          (Execution_metrics.cache_creation_tokens usage) );
      ( "cache_read_tokens",
        projection_option projection_int64
          (Execution_metrics.cache_read_tokens usage) );
    ]

let cost_projection cost =
  `Assoc
    [
      ( "usd_micros",
        projection_option projection_int64 (Execution_metrics.usd_micros cost)
      );
    ]

let string_of_web_level = function
  | Web_disabled -> "disabled"
  | Web_search -> "search"
  | Web_search_and_fetch -> "search_and_fetch"

let web_policy_projection policy =
  `Assoc
    [
      ("level", `String (string_of_web_level policy.level));
      ( "restricted_domains",
        projection_option
          (fun domains ->
            `List (List.map (fun domain -> `String domain) domains))
          policy.restricted_domains );
    ]

let string_of_attachment_delivery = function
  | Upload_attachments -> "upload"
  | Reuse_session_attachments -> "reuse_session"

let delivery_projection delivery =
  `Assoc
    [
      ("attachment_count", `Int delivery.attachment_count);
      ( "attachment_delivery",
        `String (string_of_attachment_delivery delivery.attachment_delivery) );
      ("web_policy", web_policy_projection delivery.web_policy);
    ]

let string_of_attempt_kind = function
  | Initial_attempt -> "initial"
  | Fresh_attempt -> "fresh"
  | Resumed_attempt -> "resumed"

let string_of_status = function
  | Success -> "success"
  | Failed _ -> "failed"
  | Timed_out -> "timed_out"
  | Cancelled -> "cancelled"

let attempt_projection attempt =
  `Assoc
    [
      ("number", `Int attempt.number);
      ("kind", `String (string_of_attempt_kind attempt.kind));
      ("status", `String (string_of_status attempt.status));
      ("text", `String attempt.text);
      ("structured_json", projection_option Fun.id attempt.structured_json);
      ("schema_validation_error", `Bool (Option.is_some attempt.schema_error));
      ("delivery", delivery_projection attempt.delivery);
      ("elapsed_s", `Float attempt.elapsed_s);
      ( "session_id",
        projection_option (fun value -> `String value) attempt.session_id );
      ("usage", projection_option usage_projection attempt.usage);
      ("cost", projection_option cost_projection attempt.cost);
    ]

let string_of_cleanup_status = function
  | Cleanup_not_required -> "not_required"
  | Cleanup_succeeded -> "succeeded"
  | Cleanup_failed -> "failed"

let response_projection response =
  `Assoc
    [
      ("schema_version", `String "cwr.agent-execution.response/v1");
      ("status", `String (string_of_status response.status));
      ("final_text", `String response.final_attempt.text);
      ( "final_structured_json",
        projection_option Fun.id response.final_attempt.structured_json );
      ("attempts", `List (List.map attempt_projection response.attempts));
      ("total_elapsed_s", `Float response.total_elapsed_s);
      ("total_usage", projection_option usage_projection response.total_usage);
      ("total_cost", projection_option cost_projection response.total_cost);
      ( "final_session_id",
        projection_option (fun value -> `String value) response.final_session_id
      );
      ( "cleanup_status",
        `String (string_of_cleanup_status response.cleanup_status) );
      ( "event_trace",
        projection_option Workflow_event.trace_to_yojson response.event_trace );
    ]

let validate_response_projection response =
  match
    Canonical_json.validate_standard ~max_depth:Canonical_json.max_depth
      ~max_nodes:Canonical_json.max_nodes
      ~max_bytes:max_response_projection_bytes
      (response_projection response)
  with
  | Ok () -> Ok ()
  | Error _ -> Error "response exceeds the serialized projection byte limit"

let make_response ~attempts ~status ~total_elapsed_s ~cleanup_status
    ?event_trace () =
  if not (finite total_elapsed_s && total_elapsed_s >= 0.0) then
    Error "response elapsed time must be finite and non-negative"
  else if List.length attempts > max_attempts then
    Error "response attempt limit exceeded"
  else if not (durations_fit total_elapsed_s attempts) then
    Error "response elapsed time cannot be shorter than its attempts"
  else
    match List.rev attempts with
    | [] -> Error "response must contain at least one attempt"
    | final_attempt :: _ ->
        Result.bind (normalize_status status) (fun status ->
            Result.bind (validate_attempt_order attempts) (fun () ->
                Result.bind (validate_response_status status final_attempt)
                  (fun () ->
                    Result.bind
                      (match event_trace with
                      | None -> Ok ()
                      | Some trace ->
                          validate_trace ~attempts ~status ~total_elapsed_s
                            trace)
                      (fun () ->
                        let final_session_id =
                          List.fold_left
                            (fun current attempt ->
                              match attempt.session_id with
                              | Some _ as found -> found
                              | None -> current)
                            None attempts
                        in
                        let response =
                          {
                            attempts;
                            status;
                            final_attempt;
                            total_elapsed_s;
                            total_usage =
                              Execution_metrics.aggregate_usages
                                (List.map attempt_usage attempts);
                            total_cost =
                              Execution_metrics.aggregate_costs
                                (List.map attempt_cost attempts);
                            final_session_id;
                            cleanup_status;
                            event_trace;
                          }
                        in
                        Result.map
                          (fun () -> response)
                          (validate_response_projection response)))))

let attempts response = response.attempts
let final_status response = response.status
let final_text response = response.final_attempt.text
let final_structured_json response = response.final_attempt.structured_json
let total_elapsed_s response = response.total_elapsed_s
let total_usage response = response.total_usage
let total_cost response = response.total_cost
let final_session_id response = response.final_session_id
let cleanup_status response = response.cleanup_status
let event_trace response = response.event_trace

type dispatch_failure_kind =
  | Invalid_request
  | Backend_unavailable
  | Unsupported_request
  | Capability_mismatch
  | Preflight_failed
  | Deadline_before_dispatch
  | Internal_dispatch_failure

type execution_failure_kind =
  | Native_schema_rejection
  | Schema_retry_failed
  | Backend_execution_failed
  | Execution_contract_failed

type error =
  | Dispatch_error of { kind : dispatch_failure_kind; message : string }
  | Post_execution_dispatch_error of {
      cause : dispatch_failure_kind;
      message : string;
      response : response;
    }
  | Execution_error of {
      kind : execution_failure_kind;
      message : string;
      response : response;
    }

type error_view =
  | Dispatch_failure of { kind : dispatch_failure_kind; message : string }
  | Post_execution_dispatch_failed of {
      cause : dispatch_failure_kind;
      message : string;
      response : response;
    }
  | Execution_failure of {
      kind : execution_failure_kind;
      message : string;
      response : response;
    }

let make_dispatch_error ~kind ~message () =
  Result.map
    (fun message -> Dispatch_error { kind; message })
    (normalize_nonempty_diagnostic "dispatch diagnostic" message)

let redacted_dispatch_error kind =
  Dispatch_error { kind; message = "details unavailable" }

let make_post_execution_dispatch_error ~cause ~message ~response () =
  Result.map
    (fun message ->
      Post_execution_dispatch_error { cause; message; response })
    (normalize_nonempty_diagnostic "post-execution dispatch diagnostic" message)

let valid_execution_failure kind response =
  match
    ( kind,
      response.status,
      response.final_attempt.status,
      response.final_attempt.schema_error )
  with
  | Native_schema_rejection, Failed _, Failed _, None
  | Backend_execution_failed, Failed _, Failed _, None
  | Execution_contract_failed, Failed _, Failed _, None ->
      true
  | Schema_retry_failed, _, _, _ -> (
      match response.attempts with
      | [ first; corrective ] -> (
          first.schema_error <> None
          && (corrective.kind = Fresh_attempt
             || corrective.kind = Resumed_attempt)
          &&
          match
            (corrective.status, corrective.schema_error, response.status)
          with
          | Success, Some _, Failed _
          | Failed _, None, Failed _
          | Timed_out, None, Timed_out
          | Cancelled, None, Cancelled ->
              true
          | _ -> false)
      | _ -> false)
  | Native_schema_rejection, _, _, _
  | Backend_execution_failed, _, _, _
  | Execution_contract_failed, _, _, _ ->
      false

let make_execution_error ~kind ~message ~response () =
  if not (valid_execution_failure kind response) then
    Error "execution failure kind disagrees with response telemetry"
  else
    Result.map
      (fun message -> Execution_error { kind; message; response })
      (normalize_nonempty_diagnostic "execution diagnostic" message)

let error_view = function
  | Dispatch_error { kind; message } -> Dispatch_failure { kind; message }
  | Post_execution_dispatch_error { cause; message; response } ->
      Post_execution_dispatch_failed { cause; message; response }
  | Execution_error { kind; message; response } ->
      Execution_failure { kind; message; response }

let response_to_yojson = response_projection

let string_of_dispatch_failure_kind = function
  | Invalid_request -> "invalid_request"
  | Backend_unavailable -> "backend_unavailable"
  | Unsupported_request -> "unsupported_request"
  | Capability_mismatch -> "capability_mismatch"
  | Preflight_failed -> "preflight_failed"
  | Deadline_before_dispatch -> "deadline_before_dispatch"
  | Internal_dispatch_failure -> "internal_dispatch_failure"

let string_of_execution_failure_kind = function
  | Native_schema_rejection -> "native_schema_rejection"
  | Schema_retry_failed -> "schema_retry_failed"
  | Backend_execution_failed -> "backend_execution_failed"
  | Execution_contract_failed -> "execution_contract_failed"

let error_to_yojson = function
  | Dispatch_error { kind; message = _ } ->
      `Assoc
        [
          ("schema_version", `String "cwr.agent-execution.error/v1");
          ("error_kind", `String "dispatch_failure");
          ("failure_kind", `String (string_of_dispatch_failure_kind kind));
        ]
  | Post_execution_dispatch_error { cause; message = _; response } ->
      `Assoc
        [
          ("schema_version", `String "cwr.agent-execution.error/v1");
          ("error_kind", `String "post_execution_dispatch_failed");
          ("cause", `String (string_of_dispatch_failure_kind cause));
          ("response", response_to_yojson response);
        ]
  | Execution_error { kind; message = _; response } ->
      `Assoc
        [
          ("schema_version", `String "cwr.agent-execution.error/v1");
          ("error_kind", `String "execution_failure");
          ("failure_kind", `String (string_of_execution_failure_kind kind));
          ("response", response_to_yojson response);
        ]
