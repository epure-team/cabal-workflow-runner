open Cabal_workflow_runner

let ok = function Ok value -> value | Error _ -> Alcotest.fail "expected Ok"

let expect_error = function
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected Error"

let contains haystack needle =
  let haystack_length = String.length haystack in
  let needle_length = String.length needle in
  let rec search offset =
    offset + needle_length <= haystack_length
    && (String.sub haystack offset needle_length = needle || search (offset + 1))
  in
  needle_length = 0 || search 0

let check_absent label serialized sentinel =
  Alcotest.(check bool) label false (contains serialized sentinel)

let default_request () =
  ok
    (Agent_execution.make_request ~id:"review-1" ~system_prompt:"system"
       ~user_prompt:"user" ~timeout_s:30.0 ())

let no_delivery () =
  ok
    (Agent_execution.make_delivery_intent ~attachment_count:0
       ~attachment_delivery:Agent_execution.Upload_attachments
       ~web_policy:Agent_execution.web_disabled ())

let attempt ?(number = 1) ?(kind = Workflow_event.Initial_attempt)
    ?(status = Agent_execution.Success) ?(elapsed_s = 0.25) ?schema_error
    ?session_id ?usage ?cost ?(text = "answer") ?structured_json () =
  ok
    (Agent_execution.make_attempt ~number ~kind ~status ~elapsed_s
       ~delivery:(no_delivery ()) ?schema_error ?session_id ?usage ?cost ~text
       ?structured_json ())

let response ?(attempts = [ attempt () ]) ?(total_elapsed_s = 0.5)
    ?(cleanup_status = Agent_execution.Cleanup_not_required) ?event_trace () =
  ok
    (Agent_execution.make_response ~attempts ~total_elapsed_s ~cleanup_status
       ?event_trace ())

let event ~seq ~attempt ~elapsed_s payload =
  ok (Workflow_event.make ~seq ~attempt ~elapsed_s payload)

let terminal_trace () =
  ok
    (Workflow_event.make_trace
       [
         event ~seq:1L ~attempt:0 ~elapsed_s:0.0 Workflow_event.Task_started;
         event ~seq:2L ~attempt:1 ~elapsed_s:0.1
           (Workflow_event.Attempt_started Workflow_event.Initial_attempt);
         event ~seq:3L ~attempt:1 ~elapsed_s:0.2
           (Workflow_event.Attempt_finished Workflow_event.Attempt_succeeded);
         event ~seq:4L ~attempt:1 ~elapsed_s:0.3
           (Workflow_event.Terminal Workflow_event.Succeeded);
       ])

let test_request_defaults () =
  let request = default_request () in
  Alcotest.(check string) "id" "review-1" (Agent_execution.id request);
  Alcotest.(check string)
    "system prompt" "system"
    (Agent_execution.system_prompt request);
  Alcotest.(check string)
    "user prompt" "user"
    (Agent_execution.user_prompt request);
  Alcotest.(check (float 0.0))
    "timeout" 30.0
    (Agent_execution.timeout_s request);
  Alcotest.(check int)
    "no attachments" 0
    (List.length (Agent_execution.attachments request));
  Alcotest.(check bool)
    "schema absent" true
    (Option.is_none (Agent_execution.json_schema request));
  Alcotest.(check bool)
    "session absent" true
    (Option.is_none (Agent_execution.resume_session request));
  Alcotest.(check bool)
    "max turns absent" true
    (Option.is_none (Agent_execution.max_turns request));
  Alcotest.(check bool)
    "routing absent" true
    (Option.is_none (Agent_execution.routing request));
  Alcotest.(check bool)
    "model absent" true
    (Option.is_none (Agent_execution.model request));
  Alcotest.(check bool)
    "read-only unspecified" true
    (Option.is_none (Agent_execution.read_only request));
  Alcotest.(check bool)
    "web disabled" true
    (Agent_execution.web_level (Agent_execution.web_policy request)
    = Agent_execution.Web_disabled)

let test_request_full_and_attachment_order () =
  let first =
    ok
      (Agent_execution.make_attachment ~id:"diagram" ~path:"assets/a.png"
         ~mime_type:"IMAGE/PNG"
         ~sha256:
           "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
         ~size_bytes:12L ())
  in
  let second =
    ok
      (Agent_execution.make_attachment ~id:"photo" ~path:"assets/b.jpg"
         ~mime_type:"image/jpeg"
         ~sha256:
           "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
         ~size_bytes:34L ())
  in
  let web_policy =
    ok
      (Agent_execution.make_restricted_web_policy
         ~level:Agent_execution.Web_search_and_fetch
         ~domains:[ "docs.example.org"; "example.com" ]
         ())
  in
  let request =
    ok
      (Agent_execution.make_request ~id:"author-2" ~system_prompt:"sys"
         ~user_prompt:"usr" ~timeout_s:12.5
         ~json_schema:(`Assoc [ ("type", `String "object") ])
         ~resume_session:"session-7" ~attachments:[ first; second ] ~web_policy
         ~max_turns:4 ~routing:"reviewer" ~model:"vendor/model-1"
         ~read_only:true ())
  in
  Alcotest.(check (list string))
    "attachment order" [ "diagram"; "photo" ]
    (List.map Agent_execution.attachment_id
       (Agent_execution.attachments request));
  Alcotest.(check string)
    "MIME canonicalized" "image/png"
    (Agent_execution.attachment_mime_type first);
  Alcotest.(check (option (list string)))
    "restricted domains"
    (Some [ "docs.example.org"; "example.com" ])
    (Agent_execution.restricted_domains web_policy);
  Alcotest.(check (option bool))
    "read-only" (Some true)
    (Agent_execution.read_only request)

let test_invalid_timeouts () =
  List.iter
    (fun timeout_s ->
      expect_error
        (Agent_execution.make_request ~id:"x" ~system_prompt:"s"
           ~user_prompt:"u" ~timeout_s ()))
    [ 0.0; -0.1; Float.nan; Float.infinity; Float.neg_infinity ]

let test_invalid_max_turns_and_metadata () =
  List.iter
    (fun max_turns ->
      expect_error
        (Agent_execution.make_request ~id:"x" ~system_prompt:"s"
           ~user_prompt:"u" ~timeout_s:1.0 ~max_turns ()))
    [ 0; -1 ];
  expect_error
    (Agent_execution.make_request ~id:"bad id" ~system_prompt:"s"
       ~user_prompt:"u" ~timeout_s:1.0 ());
  expect_error
    (Agent_execution.make_request ~id:"x" ~system_prompt:"s" ~user_prompt:"u"
       ~timeout_s:1.0 ~routing:"" ());
  expect_error
    (Agent_execution.make_request ~id:"x" ~system_prompt:"s" ~user_prompt:"u"
       ~timeout_s:1.0 ~model:"\000model" ());
  expect_error
    (Agent_execution.make_request ~id:"x" ~system_prompt:"s" ~user_prompt:"u"
       ~timeout_s:1.0 ~model:"vendor\nmodel" ());
  expect_error
    (Agent_execution.make_request ~id:"x" ~system_prompt:"s" ~user_prompt:"u"
       ~timeout_s:1.0 ~json_schema:(`Int 3) ());
  expect_error
    (Agent_execution.make_request ~id:"x" ~system_prompt:"s" ~user_prompt:"u"
       ~timeout_s:1.0 ~json_schema:(`Float Float.nan) ())

let test_invalid_attachment_values () =
  let make ?(id = "a") ?(path = "a.png") ?(mime_type = "image/png")
      ?(sha256 =
        "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
      ?(size_bytes = 1L) () =
    Agent_execution.make_attachment ~id ~path ~mime_type ~sha256 ~size_bytes ()
  in
  expect_error (make ~id:"" ());
  expect_error (make ~path:"../secret.png" ());
  expect_error (make ~path:"/secret.png" ());
  expect_error (make ~path:"a\\b.png" ());
  expect_error (make ~path:"C:/secret.png" ());
  expect_error (make ~path:"private\nfile.png" ());
  expect_error (make ~mime_type:"image" ());
  expect_error (make ~sha256:"ABC" ());
  expect_error
    (make
       ~sha256:
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
       ());
  expect_error (make ~size_bytes:(-1L) ());
  let private_path = "../PRIVATE_PATH_DIAGNOSTIC_SENTINEL" in
  let private_digest =
    "PRIVATE_DIGEST_DIAGNOSTIC_SENTINEL___________________________"
  in
  (match make ~path:private_path () with
  | Error diagnostic ->
      check_absent "path omitted from diagnostic" diagnostic private_path
  | Ok _ -> Alcotest.fail "private invalid path accepted");
  match make ~sha256:private_digest () with
  | Error diagnostic ->
      check_absent "digest omitted from diagnostic" diagnostic private_digest
  | Ok _ -> Alcotest.fail "private invalid digest accepted"

let test_invalid_domains_sessions_and_duplicates () =
  List.iter
    (fun domain ->
      expect_error
        (Agent_execution.make_restricted_web_policy
           ~level:Agent_execution.Web_search ~domains:[ domain ] ()))
    [
      "";
      "HTTPS://example.com";
      "Example.com";
      "-bad.example";
      "bad-.example";
      "bad..example";
      "example.com/path";
    ];
  expect_error
    (Agent_execution.make_restricted_web_policy
       ~level:Agent_execution.Web_disabled ~domains:[ "example.com" ] ());
  expect_error
    (Agent_execution.make_request ~id:"x" ~system_prompt:"s" ~user_prompt:"u"
       ~timeout_s:1.0 ~resume_session:"bad/session" ());
  let attachment =
    ok
      (Agent_execution.make_attachment ~id:"same" ~path:"a.png"
         ~mime_type:"image/png"
         ~sha256:
           "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
         ~size_bytes:1L ())
  in
  expect_error
    (Agent_execution.make_request ~id:"x" ~system_prompt:"s" ~user_prompt:"u"
       ~timeout_s:1.0 ~attachments:[ attachment; attachment ] ())

let test_usage_cost_validation_and_unknowns () =
  expect_error (Execution_metrics.make_usage ~input_tokens:(-1L) ());
  expect_error (Execution_metrics.make_cost ~usd_micros:(-1L) ());
  let unknown_usage = ok (Execution_metrics.make_usage ()) in
  let unknown_cost = ok (Execution_metrics.make_cost ()) in
  let aggregate_usage =
    Execution_metrics.aggregate_usages [ None; Some unknown_usage ]
  in
  let aggregate_cost =
    Execution_metrics.aggregate_costs [ None; Some unknown_cost ]
  in
  Alcotest.(check bool)
    "present unknown usage retained" true
    (Option.is_some aggregate_usage);
  Alcotest.(check bool)
    "unknown input remains unknown" true
    (Option.bind aggregate_usage Execution_metrics.input_tokens
    |> Option.is_none);
  Alcotest.(check bool)
    "present unknown cost retained" true
    (Option.is_some aggregate_cost);
  Alcotest.(check bool)
    "unknown cost remains unknown" true
    (Option.bind aggregate_cost Execution_metrics.usd_micros |> Option.is_none)

let test_usage_cost_saturation () =
  let almost_max = Int64.pred Int64.max_int in
  let usage_a =
    ok
      (Execution_metrics.make_usage ~input_tokens:almost_max ~output_tokens:7L
         ())
  in
  let usage_b =
    ok (Execution_metrics.make_usage ~input_tokens:10L ~cache_read_tokens:5L ())
  in
  let cost_a = ok (Execution_metrics.make_cost ~usd_micros:almost_max ()) in
  let cost_b = ok (Execution_metrics.make_cost ~usd_micros:10L ()) in
  let usage =
    match Execution_metrics.aggregate_usages [ Some usage_a; Some usage_b ] with
    | Some value -> value
    | None -> Alcotest.fail "aggregate usage unexpectedly absent"
  in
  let cost =
    match Execution_metrics.aggregate_costs [ Some cost_a; Some cost_b ] with
    | Some value -> value
    | None -> Alcotest.fail "aggregate cost unexpectedly absent"
  in
  Alcotest.(check int64)
    "token count saturates" Int64.max_int
    (Option.value ~default:0L (Execution_metrics.input_tokens usage));
  Alcotest.(check (option int64))
    "unknown cache creation stays unknown" None
    (Execution_metrics.cache_creation_tokens usage);
  Alcotest.(check int64)
    "cost saturates" Int64.max_int
    (Option.value ~default:0L (Execution_metrics.usd_micros cost))

let test_attempt_statuses_and_normalization () =
  let statuses =
    [
      Agent_execution.Success;
      Agent_execution.Failed "failure";
      Agent_execution.Timed_out;
      Agent_execution.Cancelled;
    ]
  in
  List.iter
    (fun status -> ignore (attempt ~status ~text:"line1\r\nline2\rline3" ()))
    statuses;
  let normalized = attempt ~text:"line1\r\nline2\rline3" () in
  Alcotest.(check string)
    "line endings normalized" "line1\nline2\nline3"
    (Agent_execution.attempt_text normalized);
  expect_error
    (Agent_execution.make_attempt ~number:0 ~kind:Initial_attempt
       ~status:Success ~elapsed_s:0.0 ~delivery:(no_delivery ()) ());
  expect_error
    (Agent_execution.make_attempt ~number:1 ~kind:Initial_attempt
       ~status:Success ~elapsed_s:Float.nan ~delivery:(no_delivery ()) ());
  expect_error
    (Agent_execution.make_attempt ~number:1 ~kind:Initial_attempt
       ~status:(Failed "") ~elapsed_s:0.0 ~delivery:(no_delivery ()) ());
  expect_error
    (Agent_execution.make_attempt ~number:1 ~kind:Initial_attempt
       ~status:Timed_out ~elapsed_s:0.0 ~delivery:(no_delivery ())
       ~schema_error:"invalid" ())

let test_response_attempt_ordering () =
  let first = attempt () in
  let fresh = attempt ~number:2 ~kind:Workflow_event.Fresh_attempt () in
  let resumed = attempt ~number:3 ~kind:Workflow_event.Resumed_attempt () in
  ignore (response ~attempts:[ first; fresh; resumed ] ());
  expect_error
    (Agent_execution.make_response ~attempts:[] ~total_elapsed_s:0.0
       ~cleanup_status:Cleanup_not_required ());
  expect_error
    (Agent_execution.make_response ~attempts:[ fresh ] ~total_elapsed_s:1.0
       ~cleanup_status:Cleanup_not_required ());
  expect_error
    (Agent_execution.make_response
       ~attempts:[ first; attempt ~number:3 ~kind:Fresh_attempt () ]
       ~total_elapsed_s:1.0 ~cleanup_status:Cleanup_not_required ());
  expect_error
    (Agent_execution.make_response
       ~attempts:[ first; attempt ~number:2 ~kind:Initial_attempt () ]
       ~total_elapsed_s:1.0 ~cleanup_status:Cleanup_not_required ());
  expect_error
    (Agent_execution.make_response ~attempts:[ first ] ~total_elapsed_s:0.1
       ~cleanup_status:Cleanup_not_required ())

let test_response_aggregates_and_final_session () =
  let usage_1 =
    ok (Execution_metrics.make_usage ~input_tokens:3L ~output_tokens:4L ())
  in
  let usage_2 =
    ok (Execution_metrics.make_usage ~input_tokens:5L ~cache_read_tokens:2L ())
  in
  let cost_1 = ok (Execution_metrics.make_cost ~usd_micros:7L ()) in
  let cost_2 = ok (Execution_metrics.make_cost ~usd_micros:11L ()) in
  let attempts =
    [
      attempt ~session_id:"session-1" ~usage:usage_1 ~cost:cost_1 ();
      attempt ~number:2 ~kind:Workflow_event.Resumed_attempt
        ~session_id:"session-2" ~usage:usage_2 ~cost:cost_2 ();
    ]
  in
  let response =
    response ~attempts ~total_elapsed_s:2.0
      ~cleanup_status:Agent_execution.Cleanup_succeeded
      ~event_trace:(terminal_trace ()) ()
  in
  Alcotest.(check (option string))
    "last session" (Some "session-2")
    (Agent_execution.final_session_id response);
  let usage =
    match Agent_execution.total_usage response with
    | Some value -> value
    | None -> Alcotest.fail "total usage absent"
  in
  let cost =
    match Agent_execution.total_cost response with
    | Some value -> value
    | None -> Alcotest.fail "total cost absent"
  in
  Alcotest.(check (option int64))
    "input aggregate" (Some 8L)
    (Execution_metrics.input_tokens usage);
  Alcotest.(check (option int64))
    "partial field aggregate" (Some 2L)
    (Execution_metrics.cache_read_tokens usage);
  Alcotest.(check (option int64))
    "cost aggregate" (Some 18L)
    (Execution_metrics.usd_micros cost);
  Alcotest.(check bool)
    "trace retained" true
    (Option.is_some (Agent_execution.event_trace response))

let test_event_valid_trace () =
  let trace = terminal_trace () in
  Alcotest.(check int) "events" 4 (List.length (Workflow_event.events trace));
  Alcotest.(check int64)
    "default omitted" 0L
    (Workflow_event.omitted_count trace)

let test_event_sequence_invariants () =
  let started = event ~seq:2L ~attempt:0 ~elapsed_s:0.0 Task_started in
  let terminal_same_seq =
    event ~seq:2L ~attempt:0 ~elapsed_s:0.1 (Terminal Succeeded)
  in
  expect_error (Workflow_event.make_trace [ started; terminal_same_seq ]);
  let terminal_lower_attempt =
    event ~seq:3L ~attempt:0 ~elapsed_s:0.2 (Terminal Succeeded)
  in
  let attempt_started =
    event ~seq:2L ~attempt:1 ~elapsed_s:0.1 (Attempt_started Initial_attempt)
  in
  expect_error
    (Workflow_event.make_trace
       [ started; attempt_started; terminal_lower_attempt ]);
  let backwards_time =
    event ~seq:3L ~attempt:1 ~elapsed_s:0.05 (Terminal Succeeded)
  in
  expect_error
    (Workflow_event.make_trace [ started; attempt_started; backwards_time ])

let test_event_terminal_invariants () =
  let started = event ~seq:1L ~attempt:0 ~elapsed_s:0.0 Task_started in
  let terminal = event ~seq:2L ~attempt:0 ~elapsed_s:0.1 (Terminal Succeeded) in
  expect_error (Workflow_event.make_trace [ started ]);
  expect_error
    (Workflow_event.make_trace
       [ terminal; event ~seq:3L ~attempt:0 ~elapsed_s:0.2 Preflight_started ]);
  expect_error
    (Workflow_event.make_trace
       [
         started;
         terminal;
         event ~seq:3L ~attempt:0 ~elapsed_s:0.2 (Terminal Cancelled);
       ])

let test_event_bounds () =
  expect_error
    (Workflow_event.make ~seq:1L ~attempt:0 ~elapsed_s:Float.infinity
       Task_started);
  expect_error
    (Workflow_event.make ~seq:1L ~attempt:(-1) ~elapsed_s:0.0 Task_started);
  expect_error
    (Workflow_event.make ~seq:1L ~attempt:0 ~elapsed_s:0.0
       (Agent_text_delta (String.make (Workflow_event.max_text_bytes + 1) 'x')));
  expect_error
    (Workflow_event.make_trace ~omitted_count:(-1L)
       [ event ~seq:1L ~attempt:0 ~elapsed_s:0.0 (Terminal Succeeded) ]);
  let invalid_omissions : Workflow_event.omission_counts =
    {
      text_events = -1L;
      text_bytes = 0L;
      usage_events = 0L;
      session_events = 0L;
      tool_events = 0L;
      control_events = 0L;
    }
  in
  expect_error
    (Workflow_event.make ~seq:1L ~attempt:0 ~elapsed_s:0.0
       (Delivery_truncated invalid_omissions));
  let rec controls seq count acc =
    if count = 0 then List.rev acc
    else
      controls (Int64.succ seq) (count - 1)
        (event ~seq ~attempt:0 ~elapsed_s:0.0 Preflight_started :: acc)
  in
  let too_many =
    controls 1L Workflow_event.max_events []
    @ [
        event
          ~seq:(Int64.of_int (Workflow_event.max_events + 1))
          ~attempt:0 ~elapsed_s:0.0 (Terminal Succeeded);
      ]
  in
  expect_error (Workflow_event.make_trace too_many)

let test_event_vocabulary_and_safe_opaque () =
  let usage = ok (Execution_metrics.make_usage ~output_tokens:2L ()) in
  let cost = ok (Execution_metrics.make_cost ~usd_micros:3L ()) in
  let omissions : Workflow_event.omission_counts =
    {
      text_events = 1L;
      text_bytes = 2L;
      usage_events = 3L;
      session_events = 4L;
      tool_events = 5L;
      control_events = 6L;
    }
  in
  let payloads =
    [
      Workflow_event.Backend_selected "backend-1";
      Preflight_started;
      Preflight_completed;
      Version_probe_started;
      Version_probe_completed;
      Availability_check_started;
      Availability_check_completed;
      Attempt_started Initial_attempt;
      Attempt_finished Attempt_succeeded;
      Retry_transition { kind = Fresh_retry; reason = Schema_validation };
      Process_started;
      Process_termination_requested;
      Process_kill_escalated;
      Process_exited (Exited 0);
      Session_id "session-1";
      Agent_text_delta "public output";
      Tool_started { id = Some "tool-1"; name = "reader" };
      Tool_finished { id = Some "tool-1"; name = Some "reader" };
      Usage_observed { usage = Some usage; cost = Some cost };
      Delivery_truncated omissions;
      Opaque_backend_observation;
    ]
  in
  let events =
    List.mapi
      (fun index payload ->
        event ~seq:(Int64.of_int (index + 1)) ~attempt:1 ~elapsed_s:0.0 payload)
      payloads
  in
  let terminal =
    event
      ~seq:(Int64.of_int (List.length events + 1))
      ~attempt:1 ~elapsed_s:0.1 (Terminal Succeeded)
  in
  let trace = ok (Workflow_event.make_trace (events @ [ terminal ])) in
  let serialized =
    Yojson.Safe.to_string (Workflow_event.trace_to_yojson trace)
  in
  Alcotest.(check bool)
    "opaque kind retained" true
    (contains serialized "opaque_backend_observation");
  check_absent "no opaque payload" serialized "PRIVATE_BACKEND_JSON"

let test_serialization_versions_and_statuses () =
  let trace_json =
    Yojson.Safe.to_string (Workflow_event.trace_to_yojson (terminal_trace ()))
  in
  Alcotest.(check bool)
    "trace version" true
    (contains trace_json "cwr.workflow-event-trace/v1");
  List.iter
    (fun (status, tag) ->
      let serialized =
        response ~attempts:[ attempt ~status () ] ()
        |> Agent_execution.response_to_yojson |> Yojson.Safe.to_string
      in
      Alcotest.(check bool)
        ("response status " ^ tag) true
        (contains serialized ("\"status\":\"" ^ tag ^ "\""));
      Alcotest.(check bool)
        "response version" true
        (contains serialized "cwr.agent-execution.response/v1"))
    [
      (Agent_execution.Success, "success");
      (Failed "backend stderr sentinel", "failed");
      (Timed_out, "timed_out");
      (Cancelled, "cancelled");
    ]

let test_serialization_redaction () =
  let prompt_sentinel = "PROMPT_SENTINEL_46f8" in
  let path_sentinel = "private/PATH_SENTINEL_7a2d.png" in
  let digest_sentinel =
    "dededededededededededededededededededededededededededededededede"
  in
  let stdout_sentinel = "STDOUT_SENTINEL_0be1" in
  let stderr_sentinel = "STDERR_SENTINEL_f199" in
  let private_json_sentinel = "PRIVATE_JSON_SENTINEL_41cc" in
  let attachment =
    ok
      (Agent_execution.make_attachment ~id:"private-media" ~path:path_sentinel
         ~mime_type:"image/png" ~sha256:digest_sentinel ~size_bytes:1L ())
  in
  let request =
    ok
      (Agent_execution.make_request ~id:"private-request"
         ~system_prompt:prompt_sentinel ~user_prompt:prompt_sentinel
         ~timeout_s:1.0 ~attachments:[ attachment ] ())
  in
  let schema_rejected_attempt =
    attempt ~status:Success ~schema_error:stderr_sentinel
      ~text:"invalid public attempt output" ()
  in
  let failed_attempt =
    attempt ~number:2 ~kind:Fresh_attempt ~status:(Failed stdout_sentinel)
      ~text:"public final text" ()
  in
  let response =
    response ~attempts:[ schema_rejected_attempt; failed_attempt ] ()
  in
  let runtime = ok (Runtime.make ~complete:(fun _ -> Ok response) ()) in
  let serialized_response =
    ok (Runtime.complete runtime request)
    |> Agent_execution.response_to_yojson |> Yojson.Safe.to_string
  in
  let execution_error =
    ok
      (Agent_execution.make_execution_error
         ~kind:Agent_execution.Schema_retry_failed
         ~message:private_json_sentinel ~response ())
  in
  let serialized_error =
    Agent_execution.error_to_yojson execution_error |> Yojson.Safe.to_string
  in
  List.iter
    (fun sentinel ->
      check_absent "response projection redacts private input/diagnostics"
        serialized_response sentinel;
      check_absent "error projection redacts private input/diagnostics"
        serialized_error sentinel)
    [
      prompt_sentinel;
      path_sentinel;
      digest_sentinel;
      stdout_sentinel;
      stderr_sentinel;
      private_json_sentinel;
    ];
  Alcotest.(check bool)
    "public final text retained" true
    (contains serialized_response "public final text")

let test_error_classification () =
  let dispatch =
    ok
      (Agent_execution.make_dispatch_error
         ~kind:Agent_execution.Backend_unavailable ~message:"not installed" ())
  in
  let execution =
    ok
      (Agent_execution.make_execution_error
         ~kind:Agent_execution.Backend_execution_failed ~message:"failed"
         ~response:(response ()) ())
  in
  (match Agent_execution.error_view dispatch with
  | Dispatch_failure { kind = Backend_unavailable; _ } -> ()
  | _ -> Alcotest.fail "dispatch error classification lost");
  (match Agent_execution.error_view execution with
  | Execution_failure
      { kind = Backend_execution_failed; response = retained; _ } ->
      Alcotest.(check int)
        "execution retains attempts" 1
        (List.length (Agent_execution.attempts retained))
  | _ -> Alcotest.fail "execution error classification lost");
  expect_error
    (Agent_execution.make_dispatch_error ~kind:Backend_unavailable ~message:""
       ());
  let dispatch_kinds =
    [
      (Agent_execution.Invalid_request, "invalid_request");
      (Backend_unavailable, "backend_unavailable");
      (Unsupported_request, "unsupported_request");
      (Capability_mismatch, "capability_mismatch");
      (Preflight_failed, "preflight_failed");
      (Deadline_before_dispatch, "deadline_before_dispatch");
      (Internal_dispatch_failure, "internal_dispatch_failure");
    ]
  in
  List.iter
    (fun (kind, tag) ->
      let serialized =
        ok (Agent_execution.make_dispatch_error ~kind ~message:"safe" ())
        |> Agent_execution.error_to_yojson |> Yojson.Safe.to_string
      in
      Alcotest.(check bool)
        ("dispatch kind " ^ tag) true (contains serialized tag);
      Alcotest.(check bool)
        "error version" true
        (contains serialized "cwr.agent-execution.error/v1"))
    dispatch_kinds;
  let execution_kinds =
    [
      (Agent_execution.Native_schema_rejection, "native_schema_rejection");
      (Schema_retry_failed, "schema_retry_failed");
      (Backend_execution_failed, "backend_execution_failed");
      (Execution_contract_failed, "execution_contract_failed");
    ]
  in
  List.iter
    (fun (kind, tag) ->
      let serialized =
        ok
          (Agent_execution.make_execution_error ~kind ~message:"safe"
             ~response:(response ()) ())
        |> Agent_execution.error_to_yojson |> Yojson.Safe.to_string
      in
      Alcotest.(check bool)
        ("execution kind " ^ tag) true (contains serialized tag))
    execution_kinds

let test_runtime_seam_and_capabilities () =
  let calls = ref 0 in
  let expected = response () in
  let capabilities =
    Runtime.make_capabilities ~native_json_schema:true ~session_resume:true
      ~attachments:true ~maximum_web:Agent_execution.Web_search ~read_only:true
      ~max_turns:true ~hard_timeout:true ~routing:true ~model_selection:true ()
  in
  let runtime =
    ok
      (Runtime.make ~identity:"mock-runtime" ~capabilities
         ~complete:(fun _ ->
           incr calls;
           Ok expected)
         ())
  in
  let actual = ok (Runtime.complete runtime (default_request ())) in
  Alcotest.(check int) "one call" 1 !calls;
  Alcotest.(check int)
    "response retained" 1
    (List.length (Agent_execution.attempts actual));
  Alcotest.(check (option string))
    "identity" (Some "mock-runtime") (Runtime.identity runtime);
  Alcotest.(check bool)
    "schema capability" true
    (Runtime.native_json_schema capabilities);
  expect_error
    (Runtime.make ~identity:"bad identity" ~complete:(fun _ -> Ok expected) ())

let test_legacy_runtime_success () =
  let calls = ref 0 in
  let output = `Assoc [ ("session_id", `String "must-not-be-promoted") ] in
  let backend =
    Backend.stub
      ~agent:(fun ~id ~prompt ~read_only ~agent_type ~model ~output_schema ->
        incr calls;
        Alcotest.(check string) "legacy id" "review-1" id;
        Alcotest.(check bool)
          "prompts composed" true
          (contains prompt "system" && contains prompt "user");
        Alcotest.(check bool) "legacy read-only default" false read_only;
        Alcotest.(check (option string)) "legacy routing" None agent_type;
        Alcotest.(check (option string)) "legacy model" None model;
        Alcotest.(check bool)
          "legacy schema absent" true
          (Option.is_none output_schema);
        (true, output))
      ()
  in
  let runtime = Runtime.of_legacy_backend ~now:(fun () -> 10.0) backend in
  let response = ok (Runtime.complete runtime (default_request ())) in
  Alcotest.(check int) "legacy called once" 1 !calls;
  Alcotest.(check int)
    "one synthetic attempt" 1
    (List.length (Agent_execution.attempts response));
  Alcotest.(check bool)
    "structured JSON retained exactly" true
    (Agent_execution.final_structured_json response = Some output);
  Alcotest.(check bool)
    "structured success retained" true
    (Agent_execution.final_status response = Agent_execution.Success);
  Alcotest.(check (option string))
    "JSON session not injected" None
    (Agent_execution.final_session_id response);
  Alcotest.(check bool)
    "unknown usage" true
    (Option.is_none (Agent_execution.total_usage response));
  Alcotest.(check bool)
    "no events" true
    (Option.is_none (Agent_execution.event_trace response));
  Alcotest.(check bool)
    "cleanup not required" true
    (Agent_execution.cleanup_status response = Cleanup_not_required)

let test_legacy_runtime_failure_and_unsupported () =
  let calls = ref 0 in
  let backend =
    Backend.stub
      ~agent:(fun
          ~id:_
          ~prompt:_
          ~read_only:_
          ~agent_type:_
          ~model:_
          ~output_schema:_
        ->
        incr calls;
        (false, `Assoc [ ("error", `String "legacy failure") ]))
      ()
  in
  let runtime = Runtime.of_legacy_backend ~now:(fun () -> 1.0) backend in
  (match Runtime.complete runtime (default_request ()) with
  | Error error -> (
      match Agent_execution.error_view error with
      | Execution_failure { response; _ } ->
          Alcotest.(check int)
            "failed execution has attempt" 1
            (List.length (Agent_execution.attempts response))
      | Dispatch_failure _ ->
          Alcotest.fail "legacy bool=false is execution, not dispatch failure")
  | Ok _ -> Alcotest.fail "legacy bool=false must be an execution error");
  let schema_request =
    ok
      (Agent_execution.make_request ~id:"review-1" ~system_prompt:"s"
         ~user_prompt:"u" ~timeout_s:1.0 ~json_schema:(`Assoc []) ())
  in
  (match Runtime.complete runtime schema_request with
  | Error error -> (
      match Agent_execution.error_view error with
      | Dispatch_failure { kind = Unsupported_request; _ } -> ()
      | _ -> Alcotest.fail "unsupported legacy request misclassified")
  | Ok _ -> Alcotest.fail "legacy schema request must fail before dispatch");
  Alcotest.(check int) "unsupported request did not dispatch" 1 !calls

let test_legacy_backend_and_engine_compatibility () =
  let calls = ref 0 in
  let backend : Backend.t =
    Backend.stub
      ~agent:(fun
          ~id:_
          ~prompt:_
          ~read_only:_
          ~agent_type:_
          ~model:_
          ~output_schema:_
        ->
        incr calls;
        (true, `Assoc [ ("ok", `Bool true) ]))
      ()
  in
  let workflow : Types.workflow =
    {
      name = "legacy";
      version = None;
      steps =
        [
          Types.Agent
            {
              id = "legacy-agent";
              prompt = "unchanged";
              read_only = false;
              output_schema = None;
              on_failure = Types.Abort;
              protocol = None;
              brief = None;
              agent_type = None;
              model = None;
              input = None;
            };
        ];
    }
  in
  let validated =
    match Validate.workflow ~floor_gates:[] workflow with
    | Ok value -> value
    | Error message -> Alcotest.failf "legacy workflow rejected: %s" message
  in
  let outcome, trace =
    Eio_main.run (fun _ ->
        Eio.Switch.run (fun sw -> Engine.run ~sw ~backend ~token:None validated))
  in
  Alcotest.(check int) "legacy engine dispatched once" 1 !calls;
  Alcotest.(check bool)
    "legacy outcome" true
    (outcome = Types.Completed_no_commit);
  Alcotest.(check int) "legacy trace" 1 (List.length trace)

let test_nonstandard_yojson_is_rejected () =
  expect_error (Canonical_json.validate (`Tuple [ `Int 1 ]));
  expect_error
    (Canonical_json.validate_no_duplicates
       (`Variant ("private", Some (`String "payload"))));
  expect_error (Canonical_json.to_string (`Tuple []))

let () =
  Alcotest.run "rich agent execution"
    [
      ( "request",
        [
          Alcotest.test_case "defaults and accessors" `Quick
            test_request_defaults;
          Alcotest.test_case "full request and attachment order" `Quick
            test_request_full_and_attachment_order;
          Alcotest.test_case "invalid finite positive timeout" `Quick
            test_invalid_timeouts;
          Alcotest.test_case "invalid max turns and metadata" `Quick
            test_invalid_max_turns_and_metadata;
          Alcotest.test_case "invalid attachment fields" `Quick
            test_invalid_attachment_values;
          Alcotest.test_case "invalid domains, sessions, duplicate IDs" `Quick
            test_invalid_domains_sessions_and_duplicates;
        ] );
      ( "metrics",
        [
          Alcotest.test_case "validation and unknown semantics" `Quick
            test_usage_cost_validation_and_unknowns;
          Alcotest.test_case "safe saturating aggregation" `Quick
            test_usage_cost_saturation;
        ] );
      ( "attempts and responses",
        [
          Alcotest.test_case "statuses and normalized text" `Quick
            test_attempt_statuses_and_normalization;
          Alcotest.test_case "ordered attempts" `Quick
            test_response_attempt_ordering;
          Alcotest.test_case "aggregates, final session, cleanup, trace" `Quick
            test_response_aggregates_and_final_session;
        ] );
      ( "events",
        [
          Alcotest.test_case "valid terminal trace" `Quick
            test_event_valid_trace;
          Alcotest.test_case "monotonic sequence/attempt/time" `Quick
            test_event_sequence_invariants;
          Alcotest.test_case "terminal exactly once and last" `Quick
            test_event_terminal_invariants;
          Alcotest.test_case "event, trace, omission, and text bounds" `Quick
            test_event_bounds;
          Alcotest.test_case "typed vocabulary and safe opaque fallback" `Quick
            test_event_vocabulary_and_safe_opaque;
        ] );
      ( "serialization",
        [
          Alcotest.test_case "stable versions and all statuses" `Quick
            test_serialization_versions_and_statuses;
          Alcotest.test_case "sentinel redaction" `Quick
            test_serialization_redaction;
          Alcotest.test_case "dispatch vs execution errors" `Quick
            test_error_classification;
        ] );
      ( "runtime",
        [
          Alcotest.test_case "one-call seam and capabilities" `Quick
            test_runtime_seam_and_capabilities;
          Alcotest.test_case "legacy adapter success" `Quick
            test_legacy_runtime_success;
          Alcotest.test_case "legacy adapter failure and unsupported request"
            `Quick test_legacy_runtime_failure_and_unsupported;
          Alcotest.test_case "legacy Backend/Engine source and behavior" `Quick
            test_legacy_backend_and_engine_compatibility;
          Alcotest.test_case "non-standard Yojson values fail closed" `Quick
            test_nonstandard_yojson_is_rejected;
        ] );
    ]
