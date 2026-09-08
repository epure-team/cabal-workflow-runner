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
let max_trace_projection_bytes = 8 * 1024 * 1024

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

let make_tool ?id ~name () =
  match id with
  | Some value when not (safe_identifier value) ->
      Error "tool identifier is invalid"
  | _ when not (safe_identifier name) -> Error "tool name is invalid"
  | _ -> Ok { id; name }

let tool_id tool = tool.id
let tool_name tool = tool.name

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

let make_omission_counts ?(text_events = 0L) ?(text_bytes = 0L)
    ?(usage_events = 0L) ?(session_events = 0L) ?(tool_events = 0L)
    ?(control_events = 0L) () =
  let counts =
    {
      text_events;
      text_bytes;
      usage_events;
      session_events;
      tool_events;
      control_events;
    }
  in
  Result.map (fun () -> counts) (validate_omissions counts)

let omitted_text_events counts = counts.text_events
let omitted_text_bytes counts = counts.text_bytes
let omitted_usage_events counts = counts.usage_events
let omitted_session_events counts = counts.session_events
let omitted_tool_events counts = counts.tool_events
let omitted_control_events counts = counts.control_events

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
  | Process_exited (Exited code) when code < 0 ->
      Error "process exit code must be non-negative"
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

type cumulative_metrics = {
  mutable input_tokens : int64 option;
  mutable output_tokens : int64 option;
  mutable cache_creation_tokens : int64 option;
  mutable cache_read_tokens : int64 option;
  mutable usd_micros : int64 option;
}

let empty_cumulative_metrics () =
  {
    input_tokens = None;
    output_tokens = None;
    cache_creation_tokens = None;
    cache_read_tokens = None;
    usd_micros = None;
  }

let update_cumulative_dimension name previous current =
  match current with
  | None -> Ok ()
  | Some current -> (
      match !previous with
      | Some previous_value when Int64.compare current previous_value < 0 ->
          Error (name ^ " cumulative observation decreased")
      | None | Some _ ->
          previous := Some current;
          Ok ())

let validate_usage_snapshots events =
  let current_attempt = ref (-1) in
  let metrics = ref (empty_cumulative_metrics ()) in
  let switch_attempt attempt =
    if attempt <> !current_attempt then (
      current_attempt := attempt;
      metrics := empty_cumulative_metrics ())
  in
  let validate_usage usage =
    let state = !metrics in
    let update field name value =
      let previous = ref field in
      Result.map
        (fun () -> !previous)
        (update_cumulative_dimension name previous value)
    in
    Result.bind
      (update state.input_tokens "input token"
         (Execution_metrics.input_tokens usage))
      (fun input_tokens ->
        state.input_tokens <- input_tokens;
        Result.bind
          (update state.output_tokens "output token"
             (Execution_metrics.output_tokens usage))
          (fun output_tokens ->
            state.output_tokens <- output_tokens;
            Result.bind
              (update state.cache_creation_tokens "cache-creation token"
                 (Execution_metrics.cache_creation_tokens usage))
              (fun cache_creation_tokens ->
                state.cache_creation_tokens <- cache_creation_tokens;
                Result.map
                  (fun cache_read_tokens ->
                    state.cache_read_tokens <- cache_read_tokens)
                  (update state.cache_read_tokens "cache-read token"
                     (Execution_metrics.cache_read_tokens usage)))))
  in
  let validate_cost cost =
    let state = !metrics in
    let previous = ref state.usd_micros in
    Result.map
      (fun () -> state.usd_micros <- !previous)
      (update_cumulative_dimension "cost" previous
         (Execution_metrics.usd_micros cost))
  in
  let rec loop = function
    | [] -> Ok ()
    | event :: rest ->
        switch_attempt event.attempt;
        let result =
          match event.payload with
          | Usage_observed { usage; cost } ->
              Result.bind
                (match usage with
                | None -> Ok ()
                | Some usage -> validate_usage usage)
                (fun () ->
                  match cost with
                  | None -> Ok ()
                  | Some cost -> validate_cost cost)
          | _ -> Ok ()
        in
        Result.bind result (fun () -> loop rest)
  in
  loop events

type phase_state = Not_seen | Started | Completed

type attempt_state = {
  number : int;
  mutable started_kind : attempt_kind option;
  mutable activity_seen : bool;
  mutable finished : attempt_outcome option;
  mutable retry : retry_kind option;
  mutable process_started : bool;
  mutable observation_seen : bool;
  mutable session_seen : bool;
  mutable agent_text_seen : bool;
  mutable usage_seen : bool;
  mutable termination_requested : bool;
  mutable kill_escalated : bool;
  mutable process_exited : bool;
  mutable post_finish_metadata_seen : bool;
  mutable post_finish_metadata_rank : int;
  mutable post_finish_truncation_seen : bool;
  mutable post_finish_fallback_seq : int64 option;
}

let fresh_attempt_state number =
  {
    number;
    started_kind = None;
    activity_seen = false;
    finished = None;
    retry = None;
    process_started = false;
    observation_seen = false;
    session_seen = false;
    agent_text_seen = false;
    usage_seen = false;
    termination_requested = false;
    kill_escalated = false;
    process_exited = false;
    post_finish_metadata_seen = false;
    post_finish_metadata_rank = 0;
    post_finish_truncation_seen = false;
    post_finish_fallback_seq = None;
  }

let retry_attempt_kind = function
  | Fresh_retry -> Fresh_attempt
  | Resume_retry -> Resumed_attempt

let validate_phase name state payload =
  match (payload, !state) with
  | `Start, Not_seen ->
      state := Started;
      Ok ()
  | `Complete, (Not_seen | Started) ->
      state := Completed;
      Ok ()
  | `Start, Started -> Error (name ^ " started more than once")
  | `Start, Completed -> Error (name ^ " started after completion")
  | `Complete, Completed -> Error (name ^ " completed more than once")

let validate_lifecycle events =
  let task_started = ref false in
  let backend_selected = ref false in
  let preflight = ref Not_seen in
  let version_probe = ref Not_seen in
  let availability_check = ref Not_seen in
  let current_attempt = ref None in
  let expected_attempt_kind = ref None in
  let seen_any = ref false in
  let lifecycle_rank = ref (-1) in
  let advance_lifecycle rank name =
    if rank < !lifecycle_rank then Error (name ^ " is out of lifecycle order")
    else (
      lifecycle_rank := rank;
      Ok ())
  in
  let attempt_state number =
    match !current_attempt with
    | Some state when state.number = number -> Ok state
    | Some state when state.number > number ->
        Error "event attempts must be non-decreasing"
    | _ ->
        (match !expected_attempt_kind with
        | Some (expected_number, _) when number > expected_number ->
            expected_attempt_kind := None
        | _ -> ());
        let state = fresh_attempt_state number in
        current_attempt := Some state;
        Ok state
  in
  let require_attempt event =
    if event.attempt = 0 then
      Error "attempt lifecycle event requires an attempt"
    else attempt_state event.attempt
  in
  let before_attempt_end state =
    match (state.finished, state.retry) with
    | None, None -> Ok ()
    | Some _, _ -> Error "attempt activity observed after attempt completion"
    | None, Some _ -> Error "attempt activity observed after retry transition"
  in
  let before_process_exit state =
    if state.process_exited then
      Error "attempt activity observed after process exit"
    else before_attempt_end state
  in
  let valid_final_fallback_truncation counts =
    Int64.compare counts.text_events 1L = 0
    && Int64.compare counts.text_bytes 0L > 0
    && Int64.compare counts.session_events 0L = 0
    && Int64.compare counts.tool_events 0L = 0
    && Int64.compare counts.control_events 0L = 0
  in
  let validate_attempt_started state kind =
    if state.started_kind <> None then Error "attempt started more than once"
    else if state.activity_seen then
      Error "attempt start observed after attempt activity"
    else if state.number = 1 && kind <> Initial_attempt then
      Error "the first attempt must be initial"
    else if state.number > 1 && kind = Initial_attempt then
      Error "only the first attempt may be initial"
    else
      match !expected_attempt_kind with
      | Some (number, expected) when number = state.number && expected <> kind
        ->
          Error "retry transition disagrees with the next attempt kind"
      | _ ->
          state.started_kind <- Some kind;
          Ok ()
  in
  let validate_attempt_event event =
    Result.bind (require_attempt event) (fun state ->
        match event.payload with
        | Attempt_started kind -> validate_attempt_started state kind
        | Attempt_finished outcome ->
            if state.finished <> None then
              Error "attempt finished more than once"
            else if state.retry <> None then
              Error "attempt finished after retry transition"
            else (
              state.activity_seen <- true;
              state.finished <- Some outcome;
              Ok ())
        | Retry_transition { kind; reason } ->
            if state.retry <> None then Error "attempt retried more than once"
            else if state.post_finish_metadata_seen then
              Error "attempt retried after final result metadata"
            else if state.number = max_int then
              Error "attempt number cannot advance"
            else if
              reason = Schema_validation
              &&
              match state.finished with
              | Some outcome -> outcome <> Attempt_succeeded
              | None -> false
            then
              Error
                "schema-validation retry requires a successful transport \
                 attempt"
            else (
              state.activity_seen <- true;
              state.retry <- Some kind;
              expected_attempt_kind :=
                Some (state.number + 1, retry_attempt_kind kind);
              Ok ())
        | Process_started ->
            if state.process_started then Error "process started more than once"
            else if
              state.termination_requested || state.kill_escalated
              || state.process_exited || state.observation_seen
            then Error "process started after process termination"
            else
              Result.map
                (fun () ->
                  state.activity_seen <- true;
                  state.process_started <- true)
                (before_attempt_end state)
        | Process_termination_requested ->
            if state.termination_requested then
              Error "process termination requested more than once"
            else if state.kill_escalated || state.process_exited then
              Error "process termination requested after escalation or exit"
            else
              Result.map
                (fun () ->
                  state.activity_seen <- true;
                  state.termination_requested <- true)
                (before_attempt_end state)
        | Process_kill_escalated ->
            if state.kill_escalated then
              Error "process kill escalated more than once"
            else if state.process_exited then
              Error "process kill escalated after exit"
            else
              Result.map
                (fun () ->
                  state.activity_seen <- true;
                  state.kill_escalated <- true)
                (before_attempt_end state)
        | Process_exited _ ->
            if state.process_exited then Error "process exited more than once"
            else
              Result.map
                (fun () ->
                  state.activity_seen <- true;
                  state.process_exited <- true)
                (before_attempt_end state)
        | Session_id _ ->
            (match (state.finished, state.retry) with
            | Some _, None when state.session_seen ->
                Error "final session metadata was already observed"
            | Some _, None when state.post_finish_metadata_rank > 0 ->
                Error "final session metadata is out of order"
            | Some _, None ->
                state.activity_seen <- true;
                state.observation_seen <- true;
                state.session_seen <- true;
                state.post_finish_metadata_seen <- true;
                state.post_finish_metadata_rank <- 1;
                Ok ()
            | Some _, Some _ | None, Some _ ->
                Error "final result metadata observed after retry transition"
            | None, None ->
                Result.map
                  (fun () ->
                    state.activity_seen <- true;
                    state.observation_seen <- true;
                    state.session_seen <- true)
                  (before_attempt_end state))
        | Agent_text_delta text ->
            (match (state.finished, state.retry) with
            | Some _, None when text = "" ->
                Error "final fallback agent text must be non-empty"
            | Some _, None when state.agent_text_seen ->
                Error "final fallback agent text follows earlier agent text"
            | Some _, None when state.post_finish_metadata_rank >= 3 ->
                Error "final fallback agent text is out of order"
            | Some _, None ->
                state.activity_seen <- true;
                state.observation_seen <- true;
                state.agent_text_seen <- true;
                state.post_finish_metadata_seen <- true;
                state.post_finish_metadata_rank <- 2;
                state.post_finish_fallback_seq <- Some event.seq;
                Ok ()
            | Some _, Some _ | None, Some _ ->
                Error "final result metadata observed after retry transition"
            | None, None ->
                Result.map
                  (fun () ->
                    state.activity_seen <- true;
                    state.observation_seen <- true;
                    state.agent_text_seen <- true)
                  (before_attempt_end state))
        | Usage_observed _ ->
            (match (state.finished, state.retry) with
            | Some _, None when state.usage_seen ->
                Error "final usage metadata was already observed"
            | Some _, None ->
                state.activity_seen <- true;
                state.observation_seen <- true;
                state.usage_seen <- true;
                state.post_finish_metadata_seen <- true;
                state.post_finish_metadata_rank <- 3;
                Ok ()
            | Some _, Some _ | None, Some _ ->
                Error "final result metadata observed after retry transition"
            | None, None ->
                Result.map
                  (fun () ->
                    state.activity_seen <- true;
                    state.observation_seen <- true;
                    state.usage_seen <- true)
                  (before_attempt_end state))
        | Tool_started _ | Tool_finished _ ->
            Result.map
              (fun () ->
                state.activity_seen <- true;
                state.observation_seen <- true)
              (before_process_exit state)
        | Delivery_truncated counts ->
            (match (state.finished, state.retry) with
            | Some _, None when state.post_finish_truncation_seen ->
                Error "final fallback truncation was already observed"
            | Some _, None when state.post_finish_metadata_rank <> 2 ->
                Error "final fallback truncation is out of order"
            | Some _, None
              when
                (match state.post_finish_fallback_seq with
                | Some fallback_seq ->
                    Int64.compare event.seq (Int64.succ fallback_seq) <> 0
                | None -> true) ->
                Error "final fallback truncation is not source-adjacent"
            | Some _, None when not (valid_final_fallback_truncation counts) ->
                Error "final fallback truncation counts are inconsistent"
            | Some _, None ->
                state.post_finish_metadata_seen <- true;
                state.post_finish_truncation_seen <- true;
                Ok ()
            | Some _, Some _ | None, Some _ ->
                Error "final result metadata observed after retry transition"
            | None, None -> before_attempt_end state)
        | Opaque_backend_observation -> before_attempt_end state
        | Task_started | Backend_selected _ | Preflight_started
        | Preflight_completed | Version_probe_started | Version_probe_completed
        | Availability_check_started | Availability_check_completed | Terminal _
          ->
            Ok ())
  in
  let validate_event event =
    let result =
      match event.payload with
      | Task_started ->
          if event.attempt <> 0 then Error "task start must precede attempts"
          else if !task_started then Error "task started more than once"
          else if !seen_any then
            Error "task start must be the first retained event"
          else (
            task_started := true;
            advance_lifecycle 0 "task start")
      | Backend_selected _ ->
          if event.attempt <> 0 then
            Error "backend selection must precede attempts"
          else if !backend_selected then Error "backend selected more than once"
          else (
            backend_selected := true;
            advance_lifecycle 1 "backend selection")
      | Preflight_started ->
          if event.attempt <> 0 then Error "preflight must precede attempts"
          else
            Result.bind (advance_lifecycle 2 "preflight start") (fun () ->
                validate_phase "preflight" preflight `Start)
      | Preflight_completed ->
          if event.attempt <> 0 then Error "preflight must precede attempts"
          else
            Result.bind (advance_lifecycle 3 "preflight completion") (fun () ->
                validate_phase "preflight" preflight `Complete)
      | Version_probe_started ->
          if event.attempt <> 0 then Error "version probe must precede attempts"
          else
            Result.bind (advance_lifecycle 4 "version probe start") (fun () ->
                validate_phase "version probe" version_probe `Start)
      | Version_probe_completed ->
          if event.attempt <> 0 then Error "version probe must precede attempts"
          else
            Result.bind (advance_lifecycle 5 "version probe completion")
              (fun () -> validate_phase "version probe" version_probe `Complete)
      | Availability_check_started ->
          if event.attempt <> 0 then
            Error "availability check must precede attempts"
          else
            Result.bind (advance_lifecycle 6 "availability check start")
              (fun () ->
                validate_phase "availability check" availability_check `Start)
      | Availability_check_completed ->
          if event.attempt <> 0 then
            Error "availability check must precede attempts"
          else
            Result.bind (advance_lifecycle 7 "availability check completion")
              (fun () ->
                validate_phase "availability check" availability_check `Complete)
      | Terminal _ ->
          Result.bind (advance_lifecycle 9 "terminal event") (fun () ->
              match !current_attempt with
              | Some state
                when state.number = event.attempt && state.retry <> None ->
                  Error
                    "terminal event cannot end an attempt with a pending retry"
              | _ -> Ok ())
      | Attempt_started _ | Attempt_finished _ | Retry_transition _
      | Process_started | Process_termination_requested | Process_kill_escalated
      | Process_exited _ | Session_id _ | Agent_text_delta _ | Tool_started _
      | Tool_finished _ | Usage_observed _ ->
          Result.bind (advance_lifecycle 8 "attempt event") (fun () ->
              validate_attempt_event event)
      | (Delivery_truncated _ | Opaque_backend_observation)
        when event.attempt = 0 ->
          Ok ()
      | Delivery_truncated _ | Opaque_backend_observation ->
          Result.bind (advance_lifecycle 8 "attempt event") (fun () ->
              validate_attempt_event event)
    in
    seen_any := true;
    result
  in
  let rec loop = function
    | [] -> Ok ()
    | event :: rest -> Result.bind (validate_event event) (fun () -> loop rest)
  in
  loop events

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

let validate_projection_bound trace =
  match
    Canonical_json.validate_standard ~max_depth:Canonical_json.max_depth
      ~max_nodes:Canonical_json.max_nodes ~max_bytes:max_trace_projection_bytes
      (trace_to_yojson trace)
  with
  | Ok () -> Ok ()
  | Error _ -> Error "event trace exceeds the serialized projection byte limit"

let make_trace ?(omitted_count = 0L) events =
  if Int64.compare omitted_count 0L < 0 then
    Error "omitted event count must be non-negative"
  else if events = [] then Error "event trace must not be empty"
  else if List.length events > max_events then
    Error "event trace exceeds the retained event limit"
  else
    Result.bind (validate_order events) (fun () ->
        Result.bind (validate_terminal events) (fun () ->
            Result.bind (validate_usage_snapshots events) (fun () ->
                Result.bind (validate_lifecycle events) (fun () ->
                    let trace = { events; omitted_count } in
                    Result.map
                      (fun () -> trace)
                      (validate_projection_bound trace)))))
