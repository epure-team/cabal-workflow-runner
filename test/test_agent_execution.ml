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

let default_request ?read_only () =
  ok
    (Agent_execution.make_request ~id:"review-1" ~system_prompt:"system"
       ~user_prompt:"user" ~timeout_s:30.0 ?read_only ())

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

let response ?(attempts = [ attempt () ]) ?status ?(total_elapsed_s = 0.5)
    ?(cleanup_status = Agent_execution.Cleanup_not_required) ?event_trace () =
  let status =
    match status with
    | Some status -> status
    | None -> (
        match List.rev attempts with
        | final_attempt :: _ -> Agent_execution.attempt_status final_attempt
        | [] -> Agent_execution.Success)
  in
  ok
    (Agent_execution.make_response ~attempts ~status ~total_elapsed_s
       ~cleanup_status ?event_trace ())

let event ~seq ~attempt ~elapsed_s payload =
  ok (Workflow_event.make ~seq ~attempt ~elapsed_s payload)

let terminal_trace () =
  ok
    (Workflow_event.make_trace
       [
         event ~seq:1L ~attempt:0 ~elapsed_s:0.0 Workflow_event.Task_started;
         event ~seq:2L ~attempt:1 ~elapsed_s:0.05
           (Workflow_event.Attempt_started Workflow_event.Initial_attempt);
         event ~seq:3L ~attempt:1 ~elapsed_s:0.3
           (Workflow_event.Attempt_finished Workflow_event.Attempt_succeeded);
         event ~seq:4L ~attempt:1 ~elapsed_s:0.4
           (Workflow_event.Terminal Workflow_event.Succeeded);
       ])

let two_attempt_trace ?(second_kind = Workflow_event.Resumed_attempt)
    ?(first_outcome = Workflow_event.Attempt_succeeded)
    ?(second_outcome = Workflow_event.Attempt_succeeded)
    ?(terminal = Workflow_event.Succeeded) () =
  let retry_kind =
    match second_kind with
    | Workflow_event.Fresh_attempt -> Workflow_event.Fresh_retry
    | Resumed_attempt -> Resume_retry
    | Initial_attempt -> Alcotest.fail "second attempt cannot be initial"
  in
  ok
    (Workflow_event.make_trace
       [
         event ~seq:1L ~attempt:0 ~elapsed_s:0.0 Workflow_event.Task_started;
         event ~seq:2L ~attempt:1 ~elapsed_s:0.05
           (Workflow_event.Attempt_started Workflow_event.Initial_attempt);
         event ~seq:3L ~attempt:1 ~elapsed_s:0.3
           (Workflow_event.Attempt_finished first_outcome);
         event ~seq:4L ~attempt:1 ~elapsed_s:0.31
           (Workflow_event.Retry_transition
              { kind = retry_kind; reason = Workflow_event.Schema_validation });
         event ~seq:5L ~attempt:2 ~elapsed_s:0.35
           (Workflow_event.Attempt_started second_kind);
         event ~seq:6L ~attempt:2 ~elapsed_s:0.6
           (Workflow_event.Attempt_finished second_outcome);
         event ~seq:7L ~attempt:2 ~elapsed_s:0.65
           (Workflow_event.Terminal terminal);
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

let nested_json depth =
  let value = ref `Null in
  for _ = 1 to depth do
    value := `List [ !value ]
  done;
  !value

let test_json_resource_bounds () =
  let too_deep = nested_json (Agent_execution.max_json_depth + 1) in
  expect_error
    (Agent_execution.make_request ~id:"x" ~system_prompt:"s" ~user_prompt:"u"
       ~timeout_s:1.0
       ~json_schema:(`Assoc [ ("allOf", too_deep) ])
       ());
  expect_error
    (Agent_execution.make_attempt ~number:1 ~kind:Initial_attempt
       ~status:Success ~elapsed_s:0.0 ~delivery:(no_delivery ())
       ~structured_json:too_deep ());
  let too_many_nodes =
    `List (List.init Agent_execution.max_json_nodes (fun _ -> `Null))
  in
  expect_error
    (Agent_execution.make_attempt ~number:1 ~kind:Initial_attempt
       ~status:Success ~elapsed_s:0.0 ~delivery:(no_delivery ())
       ~structured_json:too_many_nodes ());
  let too_many_bytes = String.make Agent_execution.max_json_bytes 'x' in
  expect_error
    (Agent_execution.make_attempt ~number:1 ~kind:Initial_attempt
       ~status:Success ~elapsed_s:0.0 ~delivery:(no_delivery ())
       ~structured_json:(`String too_many_bytes) ());
  let too_much_text =
    String.make (Agent_execution.max_public_text_bytes + 1) 'x'
  in
  expect_error
    (Agent_execution.make_attempt ~number:1 ~kind:Initial_attempt
       ~status:Success ~elapsed_s:0.0 ~delivery:(no_delivery ())
       ~text:too_much_text ());
  expect_error
    (Canonical_json.validate (nested_json (Canonical_json.max_depth + 1)));
  expect_error
    (Canonical_json.to_string
       (`String (String.make Canonical_json.max_canonical_bytes 'x')))

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
    (Agent_execution.make_restricted_web_policy
       ~level:Agent_execution.Web_search
       ~domains:
         (List.init (Agent_execution.max_restricted_domains + 1) (fun index ->
              Printf.sprintf "domain-%d.example" index))
       ());
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
  ignore (response ~attempts:[ first; fresh; resumed ] ~total_elapsed_s:1.0 ());
  expect_error
    (Agent_execution.make_response ~attempts:[] ~status:Success
       ~total_elapsed_s:0.0 ~cleanup_status:Cleanup_not_required ());
  expect_error
    (Agent_execution.make_response ~attempts:[ fresh ] ~status:Success
       ~total_elapsed_s:1.0 ~cleanup_status:Cleanup_not_required ());
  expect_error
    (Agent_execution.make_response
       ~attempts:[ first; attempt ~number:3 ~kind:Fresh_attempt () ]
       ~status:Success ~total_elapsed_s:1.0 ~cleanup_status:Cleanup_not_required
       ());
  expect_error
    (Agent_execution.make_response
       ~attempts:[ first; attempt ~number:2 ~kind:Initial_attempt () ]
       ~status:Success ~total_elapsed_s:1.0 ~cleanup_status:Cleanup_not_required
       ());
  expect_error
    (Agent_execution.make_response ~attempts:[ first ] ~status:Success
       ~total_elapsed_s:0.1 ~cleanup_status:Cleanup_not_required ());
  expect_error
    (Agent_execution.make_response ~attempts:[ first; fresh ] ~status:Success
       ~total_elapsed_s:0.4 ~cleanup_status:Cleanup_not_required ());
  expect_error
    (Agent_execution.make_response
       ~attempts:
         [
           attempt ~elapsed_s:Float.max_float ();
           attempt ~number:2 ~kind:Fresh_attempt ~elapsed_s:Float.max_float ();
         ]
       ~status:Success ~total_elapsed_s:Float.max_float
       ~cleanup_status:Cleanup_not_required ());
  let too_many_attempts =
    List.init (Agent_execution.max_attempts + 1) (fun index ->
        let number = index + 1 in
        attempt ~number
          ~kind:
            (if number = 1 then Workflow_event.Initial_attempt
             else Fresh_attempt)
          ())
  in
  expect_error
    (Agent_execution.make_response ~attempts:too_many_attempts ~status:Success
       ~total_elapsed_s:10.0 ~cleanup_status:Cleanup_not_required ())

let test_response_status_and_trace_coherence () =
  let first = attempt () in
  expect_error
    (Agent_execution.make_response ~attempts:[ first ]
       ~status:(Failed "incoherent") ~total_elapsed_s:0.5
       ~cleanup_status:Cleanup_not_required ());
  let schema_attempt = attempt ~schema_error:"not an object" () in
  ignore
    (response ~attempts:[ schema_attempt ]
       ~status:(Failed "schema validation failed") ());
  let mismatched_terminal =
    ok
      (Workflow_event.make_trace
         [
           event ~seq:1L ~attempt:1 ~elapsed_s:0.05
             (Attempt_started Initial_attempt);
           event ~seq:2L ~attempt:1 ~elapsed_s:0.3
             (Attempt_finished Attempt_succeeded);
           event ~seq:3L ~attempt:1 ~elapsed_s:0.4 (Terminal Failed);
         ])
  in
  expect_error
    (Agent_execution.make_response ~attempts:[ first ] ~status:Success
       ~total_elapsed_s:0.5 ~cleanup_status:Cleanup_not_required
       ~event_trace:mismatched_terminal ());
  let wrong_kind = two_attempt_trace ~second_kind:Fresh_attempt () in
  let resumed = attempt ~number:2 ~kind:Resumed_attempt () in
  expect_error
    (Agent_execution.make_response ~attempts:[ first; resumed ] ~status:Success
       ~total_elapsed_s:1.0 ~cleanup_status:Cleanup_not_required
       ~event_trace:wrong_kind ());
  let wrong_outcome =
    ok
      (Workflow_event.make_trace
         [
           event ~seq:1L ~attempt:1 ~elapsed_s:0.05
             (Attempt_started Initial_attempt);
           event ~seq:2L ~attempt:1 ~elapsed_s:0.3
             (Attempt_finished Attempt_failed);
           event ~seq:3L ~attempt:1 ~elapsed_s:0.4 (Terminal Succeeded);
         ])
  in
  expect_error
    (Agent_execution.make_response ~attempts:[ first ] ~status:Success
       ~total_elapsed_s:0.5 ~cleanup_status:Cleanup_not_required
       ~event_trace:wrong_outcome ());
  let late_trace =
    ok
      (Workflow_event.make_trace
         [
           event ~seq:1L ~attempt:1 ~elapsed_s:0.05
             (Attempt_started Initial_attempt);
           event ~seq:2L ~attempt:1 ~elapsed_s:0.3
             (Attempt_finished Attempt_succeeded);
           event ~seq:3L ~attempt:1 ~elapsed_s:0.6 (Terminal Succeeded);
         ])
  in
  expect_error
    (Agent_execution.make_response ~attempts:[ first ] ~status:Success
       ~total_elapsed_s:0.5 ~cleanup_status:Cleanup_not_required
       ~event_trace:late_trace ());
  let second = attempt ~number:2 ~kind:Fresh_attempt () in
  expect_error
    (Agent_execution.make_response ~attempts:[ first; second ] ~status:Success
       ~total_elapsed_s:1.0 ~cleanup_status:Cleanup_not_required
       ~event_trace:(two_attempt_trace ~second_kind:Fresh_attempt ())
       ())

let test_response_trace_sessions_metrics_and_timing () =
  let usage = ok (Execution_metrics.make_usage ~input_tokens:3L ()) in
  let other_usage = ok (Execution_metrics.make_usage ~input_tokens:4L ()) in
  let cost = ok (Execution_metrics.make_cost ~usd_micros:5L ()) in
  let attempt = attempt ~session_id:"session-1" ~usage ~cost () in
  let make_trace ?(session = "session-1") ?(observed_usage = usage)
      ?(observed_cost = cost) ?(finished_at = 0.3) () =
    ok
      (Workflow_event.make_trace
         [
           event ~seq:1L ~attempt:1 ~elapsed_s:0.05
             (Attempt_started Initial_attempt);
           event ~seq:2L ~attempt:1 ~elapsed_s:0.1 (Session_id session);
           event ~seq:3L ~attempt:1 ~elapsed_s:0.2
             (Usage_observed
                { usage = Some observed_usage; cost = Some observed_cost });
           event ~seq:4L ~attempt:1 ~elapsed_s:finished_at
             (Attempt_finished Attempt_succeeded);
           event ~seq:5L ~attempt:1 ~elapsed_s:0.4 (Terminal Succeeded);
         ])
  in
  ignore (response ~attempts:[ attempt ] ~event_trace:(make_trace ()) ());
  expect_error
    (Agent_execution.make_response ~attempts:[ attempt ] ~status:Success
       ~total_elapsed_s:0.5 ~cleanup_status:Cleanup_not_required
       ~event_trace:(make_trace ~session:"session-2" ())
       ());
  expect_error
    (Agent_execution.make_response ~attempts:[ attempt ] ~status:Success
       ~total_elapsed_s:0.5 ~cleanup_status:Cleanup_not_required
       ~event_trace:(make_trace ~observed_usage:other_usage ())
       ());
  expect_error
    (Agent_execution.make_response ~attempts:[ attempt ] ~status:Success
       ~total_elapsed_s:0.5 ~cleanup_status:Cleanup_not_required
       ~event_trace:(make_trace ~finished_at:0.31 ())
       ())

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
      attempt ~session_id:"session-1" ~usage:usage_1 ~cost:cost_1
        ~schema_error:"schema" ();
      attempt ~number:2 ~kind:Workflow_event.Resumed_attempt
        ~session_id:"session-2" ~usage:usage_2 ~cost:cost_2 ();
    ]
  in
  let response =
    response ~attempts ~total_elapsed_s:2.0
      ~cleanup_status:Agent_execution.Cleanup_succeeded
      ~event_trace:(two_attempt_trace ()) ()
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

let trace_of_payloads payloads =
  payloads
  |> List.mapi (fun index (attempt, payload) ->
      event
        ~seq:(Int64.of_int (index + 1))
        ~attempt
        ~elapsed_s:(float_of_int index /. 10.0)
        payload)
  |> Workflow_event.make_trace

let test_event_lifecycle_invariants () =
  let rejects payloads = expect_error (trace_of_payloads payloads) in
  rejects [ (0, Task_started); (0, Task_started); (0, Terminal Succeeded) ];
  rejects
    [
      (0, Preflight_completed); (0, Preflight_started); (0, Terminal Succeeded);
    ];
  rejects
    [
      (0, Version_probe_completed);
      (0, Preflight_completed);
      (0, Terminal Succeeded);
    ];
  rejects
    [
      (0, Task_started);
      (1, Attempt_started Initial_attempt);
      (1, Attempt_started Initial_attempt);
      (1, Terminal Succeeded);
    ];
  rejects
    [
      (1, Attempt_finished Attempt_succeeded);
      (1, Process_started);
      (1, Terminal Succeeded);
    ];
  rejects
    [
      (1, Process_exited (Exited 0));
      (1, Process_termination_requested);
      (1, Terminal Succeeded);
    ];
  rejects
    [
      (1, Agent_text_delta "answer");
      (1, Process_started);
      (1, Terminal Succeeded);
    ];
  rejects
    [
      (1, Attempt_started Initial_attempt);
      (1, Attempt_finished Attempt_succeeded);
      (1, Retry_transition { kind = Resume_retry; reason = Schema_validation });
      (2, Attempt_started Fresh_attempt);
      (2, Terminal Succeeded);
    ];
  rejects
    [
      (1, Attempt_finished Attempt_failed);
      (1, Retry_transition { kind = Fresh_retry; reason = Schema_validation });
      (2, Terminal Failed);
    ];
  rejects
    [
      (1, Attempt_finished Attempt_succeeded);
      (1, Retry_transition { kind = Fresh_retry; reason = Transport_retry });
      (1, Terminal Succeeded);
    ];
  rejects [ (0, Process_started); (0, Terminal Failed) ];
  rejects [ (1, Preflight_started); (1, Terminal Failed) ]

let test_event_omitted_subsequence_is_conservative () =
  ignore
    (ok
       (trace_of_payloads
          [
            (0, Preflight_completed);
            (1, Process_exited (Exited 0));
            (1, Attempt_finished Attempt_succeeded);
            (1, Terminal Succeeded);
          ]));
  ignore
    (ok
       (trace_of_payloads
          [
            (1, Attempt_finished Attempt_succeeded);
            ( 1,
              Retry_transition { kind = Fresh_retry; reason = Transport_retry }
            );
            (3, Attempt_finished Attempt_succeeded);
            (3, Terminal Succeeded);
          ]))

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
  expect_error (Workflow_event.make_omission_counts ~text_events:(-1L) ());
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
  expect_error (Workflow_event.make_trace too_many);
  let escaped_text = String.make Workflow_event.max_text_bytes '\000' in
  let rec text_events seq count acc =
    if count = 0 then List.rev acc
    else
      text_events (Int64.succ seq) (count - 1)
        (event ~seq ~attempt:1 ~elapsed_s:0.0 (Agent_text_delta escaped_text)
        :: acc)
  in
  let oversized_projection =
    text_events 1L (Workflow_event.max_events - 1) []
    @ [
        event
          ~seq:(Int64.of_int Workflow_event.max_events)
          ~attempt:1 ~elapsed_s:0.1 (Terminal Succeeded);
      ]
  in
  expect_error (Workflow_event.make_trace oversized_projection)

let test_event_helper_constructors () =
  let tool = ok (Workflow_event.make_tool ~id:"tool-1" ~name:"reader" ()) in
  Alcotest.(check (option string))
    "tool id" (Some "tool-1")
    (Workflow_event.tool_id tool);
  Alcotest.(check string) "tool name" "reader" (Workflow_event.tool_name tool);
  expect_error (Workflow_event.make_tool ~id:"bad/tool" ~name:"reader" ());
  expect_error (Workflow_event.make_tool ~name:"bad tool" ());
  let omissions =
    ok
      (Workflow_event.make_omission_counts ~text_events:1L ~text_bytes:2L
         ~usage_events:3L ~session_events:4L ~tool_events:5L ~control_events:6L
         ())
  in
  Alcotest.(check int64)
    "text omissions" 1L
    (Workflow_event.omitted_text_events omissions);
  Alcotest.(check int64)
    "control omissions" 6L
    (Workflow_event.omitted_control_events omissions);
  ignore (event ~seq:1L ~attempt:1 ~elapsed_s:0.0 (Tool_started tool));
  ignore
    (event ~seq:2L ~attempt:1 ~elapsed_s:0.0 (Delivery_truncated omissions))

let test_event_vocabulary_and_safe_opaque () =
  let usage = ok (Execution_metrics.make_usage ~output_tokens:2L ()) in
  let cost = ok (Execution_metrics.make_cost ~usd_micros:3L ()) in
  let omissions =
    ok
      (Workflow_event.make_omission_counts ~text_events:1L ~text_bytes:2L
         ~usage_events:3L ~session_events:4L ~tool_events:5L ~control_events:6L
         ())
  in
  let tool = ok (Workflow_event.make_tool ~id:"tool-1" ~name:"reader" ()) in
  let payloads =
    [
      (0, Workflow_event.Task_started);
      (0, Backend_selected "backend-1");
      (0, Preflight_started);
      (0, Preflight_completed);
      (0, Version_probe_started);
      (0, Version_probe_completed);
      (0, Availability_check_started);
      (0, Availability_check_completed);
      (1, Attempt_started Initial_attempt);
      (1, Process_started);
      (1, Session_id "session-1");
      (1, Agent_text_delta "public output");
      (1, Tool_started tool);
      (1, Tool_finished { id = Some "tool-1"; name = Some "reader" });
      (1, Usage_observed { usage = Some usage; cost = Some cost });
      (1, Delivery_truncated omissions);
      (1, Opaque_backend_observation);
      (1, Process_termination_requested);
      (1, Process_kill_escalated);
      (1, Process_exited (Exited 0));
      (1, Attempt_finished Attempt_succeeded);
      (1, Retry_transition { kind = Fresh_retry; reason = Schema_validation });
      (2, Attempt_started Fresh_attempt);
      (2, Attempt_finished Attempt_succeeded);
    ]
  in
  let events =
    List.mapi
      (fun index (attempt, payload) ->
        event ~seq:(Int64.of_int (index + 1)) ~attempt ~elapsed_s:0.0 payload)
      payloads
  in
  let terminal =
    event
      ~seq:(Int64.of_int (List.length events + 1))
      ~attempt:2 ~elapsed_s:0.1 (Terminal Succeeded)
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
         ~kind:Agent_execution.Backend_execution_failed
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

let test_serialized_projection_bounds () =
  let successful_response = response ~event_trace:(terminal_trace ()) () in
  let response_bytes =
    successful_response |> Agent_execution.response_to_yojson
    |> Yojson.Safe.to_string |> String.length
  in
  Alcotest.(check bool)
    "response projection bounded" true
    (response_bytes <= Agent_execution.max_response_projection_bytes);
  let error =
    let response =
      response ~attempts:[ attempt ~status:(Failed "failure") () ] ()
    in
    ok
      (Agent_execution.make_execution_error ~kind:Backend_execution_failed
         ~message:"failure" ~response ())
  in
  let error_bytes =
    error |> Agent_execution.error_to_yojson |> Yojson.Safe.to_string
    |> String.length
  in
  Alcotest.(check bool)
    "error projection bounded" true
    (error_bytes <= Agent_execution.max_error_projection_bytes);
  let trace_bytes =
    terminal_trace () |> Workflow_event.trace_to_yojson |> Yojson.Safe.to_string
    |> String.length
  in
  Alcotest.(check bool)
    "trace projection bounded" true
    (trace_bytes <= Workflow_event.max_trace_projection_bytes)

let test_error_classification () =
  let dispatch =
    ok
      (Agent_execution.make_dispatch_error
         ~kind:Agent_execution.Backend_unavailable ~message:"not installed" ())
  in
  let execution =
    let failed_response =
      response ~attempts:[ attempt ~status:(Failed "failed") () ] ()
    in
    ok
      (Agent_execution.make_execution_error
         ~kind:Agent_execution.Backend_execution_failed ~message:"failed"
         ~response:failed_response ())
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
  let failed_response =
    response ~attempts:[ attempt ~status:(Failed "failed") () ] ()
  in
  let schema_response =
    response
      ~attempts:
        [
          attempt ~schema_error:"schema" ();
          attempt ~number:2 ~kind:Fresh_attempt ~schema_error:"schema" ();
        ]
      ~status:(Failed "schema validation failed") ()
  in
  let execution_kinds =
    [
      ( Agent_execution.Native_schema_rejection,
        "native_schema_rejection",
        failed_response );
      (Schema_retry_failed, "schema_retry_failed", schema_response);
      (Backend_execution_failed, "backend_execution_failed", failed_response);
      (Execution_contract_failed, "execution_contract_failed", failed_response);
    ]
  in
  List.iter
    (fun (kind, tag, response) ->
      let serialized =
        ok
          (Agent_execution.make_execution_error ~kind ~message:"safe" ~response
             ())
        |> Agent_execution.error_to_yojson |> Yojson.Safe.to_string
      in
      Alcotest.(check bool)
        ("execution kind " ^ tag) true (contains serialized tag))
    execution_kinds

let test_execution_error_coherence () =
  let success = response () in
  let failed =
    response ~attempts:[ attempt ~status:(Failed "backend") () ] ()
  in
  let schema_failed =
    response
      ~attempts:
        [
          attempt ~schema_error:"schema" ();
          attempt ~number:2 ~kind:Fresh_attempt ~schema_error:"schema" ();
        ]
      ~status:(Failed "schema validation failed") ()
  in
  List.iter
    (fun kind ->
      expect_error
        (Agent_execution.make_execution_error ~kind ~message:"incoherent"
           ~response:success ()))
    [
      Agent_execution.Native_schema_rejection;
      Schema_retry_failed;
      Backend_execution_failed;
      Execution_contract_failed;
    ];
  expect_error
    (Agent_execution.make_execution_error ~kind:Native_schema_rejection
       ~message:"wrong failure shape" ~response:schema_failed ());
  expect_error
    (Agent_execution.make_execution_error ~kind:Schema_retry_failed
       ~message:"wrong failure shape" ~response:failed ());
  let no_retry =
    response
      ~attempts:[ attempt ~schema_error:"schema" () ]
      ~status:(Failed "schema validation failed") ()
  in
  expect_error
    (Agent_execution.make_execution_error ~kind:Schema_retry_failed
       ~message:"no retry telemetry" ~response:no_retry ());
  ignore
    (ok
       (Agent_execution.make_execution_error ~kind:Native_schema_rejection
          ~message:"native rejection" ~response:failed ()));
  ignore
    (ok
       (Agent_execution.make_execution_error ~kind:Schema_retry_failed
          ~message:"retry exhausted" ~response:schema_failed ()))

let test_runtime_seam_and_capabilities () =
  let calls = ref 0 in
  let expected = response () in
  let capabilities =
    ok
      (Runtime.make_capabilities ~native_json_schema:true ~session_resume:true
         ~media_mime_types:[ "IMAGE/PNG"; "image/jpeg" ]
         ~maximum_web:Agent_execution.Web_search ~restricted_web_domains:true
         ~read_only:true ~max_turns:true ~hard_timeout:true ~routing:true
         ~model_selection:true ())
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
  Alcotest.(check bool)
    "media capability" true
    (Runtime.attachments capabilities);
  Alcotest.(check (list string))
    "canonical media MIME types"
    [ "image/png"; "image/jpeg" ]
    (Runtime.media_mime_types capabilities);
  Alcotest.(check bool)
    "restricted-domain web capability" true
    (Runtime.restricted_web_domains capabilities);
  Alcotest.(check bool)
    "web maximum retained" true
    (Runtime.maximum_web capabilities = Agent_execution.Web_search);
  let defaults = ok (Runtime.make_capabilities ()) in
  Alcotest.(check bool)
    "media disabled by default" false
    (Runtime.attachments defaults);
  Alcotest.(check (list string))
    "no default MIME claims" []
    (Runtime.media_mime_types defaults);
  Alcotest.(check bool)
    "restricted domains disabled by default" false
    (Runtime.restricted_web_domains defaults);
  expect_error
    (Runtime.make_capabilities
       ~media_mime_types:[ "image/png"; "IMAGE/PNG" ]
       ());
  expect_error (Runtime.make_capabilities ~media_mime_types:[ "image" ] ());
  expect_error
    (Runtime.make_capabilities
       ~media_mime_types:
         (List.init (Runtime.max_media_mime_types + 1) (fun index ->
              Printf.sprintf "image/x-%d" index))
       ());
  expect_error (Runtime.make_capabilities ~restricted_web_domains:true ());
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
        Alcotest.(check bool) "legacy read-only" true read_only;
        Alcotest.(check (option string))
          "legacy routing" (Some "reviewer") agent_type;
        Alcotest.(check (option string))
          "legacy model" (Some "vendor/model") model;
        Alcotest.(check bool)
          "legacy schema absent" true
          (Option.is_none output_schema);
        (true, output))
      ()
  in
  let runtime = Runtime.of_legacy_backend ~now:(fun () -> 10.0) backend in
  let request =
    ok
      (Agent_execution.make_request ~id:"review-1" ~system_prompt:"system"
         ~user_prompt:"user" ~timeout_s:30.0 ~read_only:true ~routing:"reviewer"
         ~model:"vendor/model" ())
  in
  let response = ok (Runtime.complete runtime request) in
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
  (match Runtime.complete runtime (default_request ~read_only:false ()) with
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

let test_legacy_read_only_is_explicit () =
  let calls = ref [] in
  let backend =
    Backend.stub
      ~agent:(fun
          ~id:_ ~prompt:_ ~read_only ~agent_type:_ ~model:_ ~output_schema:_ ->
        calls := read_only :: !calls;
        (true, `Null))
      ()
  in
  let runtime = Runtime.of_legacy_backend ~now:(fun () -> 1.0) backend in
  let conservative = Runtime.capabilities runtime in
  Alcotest.(check bool)
    "legacy read-only not claimed by default" false
    (Runtime.read_only conservative);
  Alcotest.(check bool)
    "legacy routing not claimed by default" false
    (Runtime.routing conservative);
  Alcotest.(check bool)
    "legacy model selection not claimed by default" false
    (Runtime.model_selection conservative);
  let attested =
    Runtime.of_legacy_backend ~attested_read_only:true ~attested_routing:true
      ~attested_model_selection:true backend
    |> Runtime.capabilities
  in
  Alcotest.(check bool)
    "caller-attested read-only claim" true
    (Runtime.read_only attested);
  Alcotest.(check bool)
    "caller-attested routing claim" true (Runtime.routing attested);
  Alcotest.(check bool)
    "caller-attested model claim" true
    (Runtime.model_selection attested);
  let expect_unsupported request =
    match Runtime.complete runtime request with
    | Error error -> (
        match Agent_execution.error_view error with
        | Dispatch_failure { kind = Unsupported_request; _ } -> ()
        | _ -> Alcotest.fail "unspecified read-only was misclassified")
    | Ok _ -> Alcotest.fail "unspecified read-only must fail before dispatch"
  in
  expect_unsupported (default_request ());
  ignore (ok (Runtime.complete runtime (default_request ~read_only:false ())));
  ignore (ok (Runtime.complete runtime (default_request ~read_only:true ())));
  Alcotest.(check (list bool)) "false and true forwarded" [ true; false ] !calls

let test_legacy_rejects_each_unsupported_field_before_dispatch () =
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
        (true, `Null))
      ()
  in
  let runtime = Runtime.of_legacy_backend backend in
  let attachment =
    ok
      (Agent_execution.make_attachment ~id:"a" ~path:"a.png"
         ~mime_type:"image/png"
         ~sha256:
           "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
         ~size_bytes:1L ())
  in
  let make ?json_schema ?resume_session ?(attachments = []) ?web_policy
      ?max_turns () =
    ok
      (Agent_execution.make_request ~id:"review-1" ~system_prompt:"system"
         ~user_prompt:"user" ~timeout_s:30.0 ~read_only:false ?json_schema
         ?resume_session ~attachments ?web_policy ?max_turns ())
  in
  let requests =
    [
      make ~json_schema:(`Assoc []) ();
      make ~resume_session:"session-1" ();
      make ~attachments:[ attachment ] ();
      make ~web_policy:Agent_execution.web_search ();
      make ~max_turns:2 ();
    ]
  in
  List.iter
    (fun request ->
      match Runtime.complete runtime request with
      | Error error -> (
          match Agent_execution.error_view error with
          | Dispatch_failure { kind = Unsupported_request; _ } -> ()
          | _ -> Alcotest.fail "unsupported field was misclassified")
      | Ok _ -> Alcotest.fail "unsupported field reached legacy dispatch")
    requests;
  Alcotest.(check int) "no unsupported request dispatched" 0 !calls

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
          Alcotest.test_case "bounded JSON inputs" `Quick
            test_json_resource_bounds;
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
          Alcotest.test_case "response status and trace coherence" `Quick
            test_response_status_and_trace_coherence;
          Alcotest.test_case "trace sessions, metrics, and timing" `Quick
            test_response_trace_sessions_metrics_and_timing;
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
          Alcotest.test_case "lifecycle state machine" `Quick
            test_event_lifecycle_invariants;
          Alcotest.test_case "omitted-event subsequences" `Quick
            test_event_omitted_subsequence_is_conservative;
          Alcotest.test_case "event, trace, omission, and text bounds" `Quick
            test_event_bounds;
          Alcotest.test_case "opaque helper constructors" `Quick
            test_event_helper_constructors;
          Alcotest.test_case "typed vocabulary and safe opaque fallback" `Quick
            test_event_vocabulary_and_safe_opaque;
        ] );
      ( "serialization",
        [
          Alcotest.test_case "stable versions and all statuses" `Quick
            test_serialization_versions_and_statuses;
          Alcotest.test_case "sentinel redaction" `Quick
            test_serialization_redaction;
          Alcotest.test_case "bounded safe projections" `Quick
            test_serialized_projection_bounds;
          Alcotest.test_case "dispatch vs execution errors" `Quick
            test_error_classification;
          Alcotest.test_case "execution error coherence" `Quick
            test_execution_error_coherence;
        ] );
      ( "runtime",
        [
          Alcotest.test_case "one-call seam and capabilities" `Quick
            test_runtime_seam_and_capabilities;
          Alcotest.test_case "legacy adapter success" `Quick
            test_legacy_runtime_success;
          Alcotest.test_case "legacy adapter failure and unsupported request"
            `Quick test_legacy_runtime_failure_and_unsupported;
          Alcotest.test_case "legacy read-only intent must be explicit" `Quick
            test_legacy_read_only_is_explicit;
          Alcotest.test_case "legacy rejects every unsupported rich field"
            `Quick test_legacy_rejects_each_unsupported_field_before_dispatch;
          Alcotest.test_case "legacy Backend/Engine source and behavior" `Quick
            test_legacy_backend_and_engine_compatibility;
          Alcotest.test_case "non-standard Yojson values fail closed" `Quick
            test_nonstandard_yojson_is_rejected;
        ] );
    ]
