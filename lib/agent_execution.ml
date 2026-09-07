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

let make_restricted_web_policy ~level ~domains () =
  if level = Web_disabled then
    Error "disabled web access cannot have restricted domains"
  else if domains = [] then Error "restricted web domains must not be empty"
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

let valid_json_integer_literal literal =
  let length = String.length literal in
  let first_digit = if length > 0 && literal.[0] = '-' then 1 else 0 in
  if first_digit = length then false
  else
    let digit_count = length - first_digit in
    (digit_count = 1 || literal.[first_digit] <> '0')
    &&
    let rec all_digits index =
      index = length
      ||
      match literal.[index] with
      | '0' .. '9' -> all_digits (index + 1)
      | _ -> false
    in
    all_digits first_digit

let validate_standard_json json =
  let rec validate = function
    | `Null | `Bool _ | `Int _ -> true
    | `Intlit literal -> valid_json_integer_literal literal
    | `Float value -> finite value
    | `String value -> String.is_valid_utf_8 value
    | `List values -> List.for_all validate values
    | `Assoc fields ->
        let keys = List.map fst fields in
        List.for_all
          (fun (key, value) -> String.is_valid_utf_8 key && validate value)
          fields
        && List.length keys = List.length (List.sort_uniq String.compare keys)
    | `Tuple _ | `Variant _ -> false
  in
  validate json

let validate_json_schema = function
  | (`Assoc _ | `Bool _) as schema -> validate_standard_json schema
  | `Null | `Int _ | `Intlit _ | `Float _ | `String _ | `List _ | `Tuple _
  | `Variant _ ->
      false

let valid_hint value =
  value <> ""
  && String.length value <= 256
  && valid_text value
  && has_no_control_characters value
  && String.trim value = value

let validate_optional condition error = function
  | Some value when not (condition value) -> Error error
  | _ -> Ok ()

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
    Result.bind
      (validate_optional validate_json_schema "JSON Schema is invalid"
         json_schema) (fun () ->
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
                else
                  Result.bind
                    (validate_optional validate_standard_json
                       "attempt structured JSON is invalid" structured_json)
                    (fun () ->
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

let make_response ~attempts ~total_elapsed_s ~cleanup_status ?event_trace () =
  if not (finite total_elapsed_s && total_elapsed_s >= 0.0) then
    Error "response elapsed time must be finite and non-negative"
  else if
    List.exists (fun attempt -> attempt.elapsed_s > total_elapsed_s) attempts
  then Error "response elapsed time cannot be shorter than an attempt"
  else
    match List.rev attempts with
    | [] -> Error "response must contain at least one attempt"
    | final_attempt :: _ ->
        Result.map
          (fun () ->
            let final_session_id =
              List.fold_left
                (fun current attempt ->
                  match attempt.session_id with
                  | Some _ as found -> found
                  | None -> current)
                None attempts
            in
            {
              attempts;
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
            })
          (validate_attempt_order attempts)

let attempts response = response.attempts
let final_status response = response.final_attempt.status
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
  | Execution_error of {
      kind : execution_failure_kind;
      message : string;
      response : response;
    }

type error_view =
  | Dispatch_failure of { kind : dispatch_failure_kind; message : string }
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

let make_execution_error ~kind ~message ~response () =
  Result.map
    (fun message -> Execution_error { kind; message; response })
    (normalize_nonempty_diagnostic "execution diagnostic" message)

let error_view = function
  | Dispatch_error { kind; message } -> Dispatch_failure { kind; message }
  | Execution_error { kind; message; response } ->
      Execution_failure { kind; message; response }

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

let string_of_web_level = function
  | Web_disabled -> "disabled"
  | Web_search -> "search"
  | Web_search_and_fetch -> "search_and_fetch"

let web_policy_to_yojson policy =
  `Assoc
    [
      ("level", `String (string_of_web_level policy.level));
      ( "restricted_domains",
        option_json
          (fun domains ->
            `List (List.map (fun domain -> `String domain) domains))
          policy.restricted_domains );
    ]

let string_of_attachment_delivery = function
  | Upload_attachments -> "upload"
  | Reuse_session_attachments -> "reuse_session"

let delivery_to_yojson delivery =
  `Assoc
    [
      ("attachment_count", `Int delivery.attachment_count);
      ( "attachment_delivery",
        `String (string_of_attachment_delivery delivery.attachment_delivery) );
      ("web_policy", web_policy_to_yojson delivery.web_policy);
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

let attempt_to_yojson attempt =
  `Assoc
    [
      ("number", `Int attempt.number);
      ("kind", `String (string_of_attempt_kind attempt.kind));
      ("status", `String (string_of_status attempt.status));
      ("text", `String attempt.text);
      ("structured_json", option_json Fun.id attempt.structured_json);
      ("schema_validation_error", `Bool (Option.is_some attempt.schema_error));
      ("delivery", delivery_to_yojson attempt.delivery);
      ("elapsed_s", `Float attempt.elapsed_s);
      ("session_id", option_json (fun value -> `String value) attempt.session_id);
      ("usage", option_json usage_to_yojson attempt.usage);
      ("cost", option_json cost_to_yojson attempt.cost);
    ]

let string_of_cleanup_status = function
  | Cleanup_not_required -> "not_required"
  | Cleanup_succeeded -> "succeeded"
  | Cleanup_failed -> "failed"

let response_to_yojson response =
  `Assoc
    [
      ("schema_version", `String "cwr.agent-execution.response/v1");
      ("status", `String (string_of_status response.final_attempt.status));
      ("final_text", `String response.final_attempt.text);
      ( "final_structured_json",
        option_json Fun.id response.final_attempt.structured_json );
      ("attempts", `List (List.map attempt_to_yojson response.attempts));
      ("total_elapsed_s", `Float response.total_elapsed_s);
      ("total_usage", option_json usage_to_yojson response.total_usage);
      ("total_cost", option_json cost_to_yojson response.total_cost);
      ( "final_session_id",
        option_json (fun value -> `String value) response.final_session_id );
      ( "cleanup_status",
        `String (string_of_cleanup_status response.cleanup_status) );
      ( "event_trace",
        option_json Workflow_event.trace_to_yojson response.event_trace );
    ]

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
  | Execution_error { kind; message = _; response } ->
      `Assoc
        [
          ("schema_version", `String "cwr.agent-execution.error/v1");
          ("error_kind", `String "execution_failure");
          ("failure_kind", `String (string_of_execution_failure_kind kind));
          ("response", response_to_yojson response);
        ]
