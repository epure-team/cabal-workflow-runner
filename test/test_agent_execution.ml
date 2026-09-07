open Cabal_workflow_runner

let ok = function Ok value -> value | Error _ -> Alcotest.fail "expected Ok"

let expect_error = function
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected Error"

let expect_error_message expected = function
  | Error actual -> Alcotest.(check string) "fixed error" expected actual
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

let test_json_size_preflight_is_iterative_and_escape_aware () =
  let validate ?(max_depth = Canonical_json.max_depth)
      ?(max_nodes = Canonical_json.max_nodes) ~max_bytes json =
    try Canonical_json.validate_standard ~max_depth ~max_nodes ~max_bytes json
    with Stack_overflow ->
      Alcotest.fail "JSON size preflight overflowed the stack"
  in
  let escaped = `String "\n" in
  ignore (ok (validate ~max_bytes:4 escaped));
  expect_error_message "$: JSON byte limit exceeded"
    (validate ~max_bytes:3 escaped);
  let escaped_key = `Assoc [ ("\n", `Null) ] in
  ignore (ok (validate ~max_bytes:11 escaped_key));
  expect_error_message "$: JSON byte limit exceeded"
    (validate ~max_bytes:10 escaped_key);
  expect_error
    (validate ~max_bytes:1024 (`Assoc [ ("same", `Int 1); ("same", `Int 2) ]));
  List.iter
    (fun json ->
      let encoded_bytes = String.length (Yojson.Safe.to_string json) in
      ignore (ok (validate ~max_bytes:encoded_bytes json));
      expect_error_message "$: JSON byte limit exceeded"
        (validate ~max_bytes:(encoded_bytes - 1) json))
    [
      `Null;
      `Bool false;
      `Int min_int;
      `Intlit "9223372036854775808";
      `Float (-0.0);
      `Float (Float.of_string "2.2250738585072014e-308");
      `String "quote=\" slash=\\ newline=\n del=\127 utf8=é";
      `List [ `Int 1; `String "two"; `Bool true ];
      `Assoc [ ("escaped\nkey", `String "value\t"); ("empty", `List []) ];
    ];
  let amplified = `String (String.make (8 * 1024 * 1024) '\000') in
  expect_error_message "$: JSON byte limit exceeded"
    (validate ~max_bytes:1024 amplified);
  expect_error_message "$: JSON nesting limit exceeded"
    (validate ~max_depth:64 ~max_nodes:100_000 ~max_bytes:1024
       (nested_json 100_000));
  let private_key = String.make (2 * 1024 * 1024) 'k' in
  (match
     validate ~max_bytes:(3 * 1024 * 1024) (`Assoc [ (private_key, `Tuple []) ])
   with
  | Error diagnostic ->
      check_absent "diagnostic does not copy a hostile key" diagnostic
        private_key
  | Ok _ -> Alcotest.fail "non-standard JSON value accepted");
  expect_error_message "$: canonical JSON byte limit exceeded"
    (try
       Canonical_json.to_string
         (`String (String.make (12 * 1024 * 1024) '\000'))
     with Stack_overflow ->
       Alcotest.fail "canonical JSON size preflight overflowed the stack")

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
  ignore
    (ok
       (Agent_execution.make_response ~attempts:[ attempt ] ~status:Success
          ~total_elapsed_s:0.5 ~cleanup_status:Cleanup_not_required
          ~event_trace:(make_trace ~finished_at:0.31 ())
          ()))

let test_usage_events_are_cumulative_snapshots () =
  let usage_1 =
    ok (Execution_metrics.make_usage ~input_tokens:2L ~output_tokens:1L ())
  in
  let usage_2 =
    ok (Execution_metrics.make_usage ~input_tokens:5L ~output_tokens:4L ())
  in
  let usage_lower =
    ok (Execution_metrics.make_usage ~input_tokens:4L ~output_tokens:4L ())
  in
  let usage_final =
    ok (Execution_metrics.make_usage ~input_tokens:5L ~output_tokens:4L ())
  in
  let cost_1 = ok (Execution_metrics.make_cost ~usd_micros:3L ()) in
  let cost_2 = ok (Execution_metrics.make_cost ~usd_micros:9L ()) in
  let attempt = attempt ~usage:usage_final ~cost:cost_2 () in
  let trace observations =
    let observation_events =
      List.mapi
        (fun index (usage, cost) ->
          event
            ~seq:(Int64.of_int (index + 2))
            ~attempt:1
            ~elapsed_s:(0.1 +. (float_of_int index *. 0.05))
            (Usage_observed { usage = Some usage; cost = Some cost }))
        observations
    in
    let next = List.length observation_events + 2 in
    Workflow_event.make_trace
      (event ~seq:1L ~attempt:1 ~elapsed_s:0.05
         (Attempt_started Initial_attempt)
       :: observation_events
      @ [
          event ~seq:(Int64.of_int next) ~attempt:1 ~elapsed_s:0.35
            (Attempt_finished Attempt_succeeded);
          event
            ~seq:(Int64.of_int (next + 1))
            ~attempt:1 ~elapsed_s:0.4 (Terminal Succeeded);
        ])
  in
  let cumulative = ok (trace [ (usage_1, cost_1); (usage_2, cost_2) ]) in
  ignore (response ~attempts:[ attempt ] ~event_trace:cumulative ());
  expect_error (trace [ (usage_2, cost_2); (usage_lower, cost_2) ]);
  expect_error (trace [ (usage_1, cost_2); (usage_2, cost_1) ]);
  let nonfinal_mismatch =
    ok (trace [ (usage_lower, cost_1); (usage_2, cost_2) ])
  in
  ignore (response ~attempts:[ attempt ] ~event_trace:nonfinal_mismatch ());
  let final_mismatch =
    ok (trace [ (usage_1, cost_1); (usage_lower, cost_2) ])
  in
  expect_error
    (Agent_execution.make_response ~attempts:[ attempt ] ~status:Success
       ~total_elapsed_s:0.5 ~cleanup_status:Cleanup_not_required
       ~event_trace:final_mismatch ());
  let final_cost_mismatch = ok (trace [ (usage_2, cost_1) ]) in
  expect_error
    (Agent_execution.make_response ~attempts:[ attempt ] ~status:Success
       ~total_elapsed_s:0.5 ~cleanup_status:Cleanup_not_required
       ~event_trace:final_cost_mismatch ());
  let omitted_usage =
    ok (Workflow_event.make_omission_counts ~usage_events:1L ())
  in
  let truncated_final_unknown =
    ok
      (Workflow_event.make_trace
         [
           event ~seq:1L ~attempt:1 ~elapsed_s:0.05
             (Attempt_started Initial_attempt);
           event ~seq:2L ~attempt:1 ~elapsed_s:0.1
             (Usage_observed { usage = Some usage_1; cost = Some cost_1 });
           event ~seq:3L ~attempt:1 ~elapsed_s:0.2
             (Delivery_truncated omitted_usage);
           event ~seq:4L ~attempt:1 ~elapsed_s:0.35
             (Attempt_finished Attempt_succeeded);
           event ~seq:5L ~attempt:1 ~elapsed_s:0.4 (Terminal Succeeded);
         ])
  in
  ignore
    (response ~attempts:[ attempt ] ~event_trace:truncated_final_unknown ());
  let gap_final_unknown =
    ok
      (Workflow_event.make_trace
         [
           event ~seq:1L ~attempt:1 ~elapsed_s:0.05
             (Attempt_started Initial_attempt);
           event ~seq:2L ~attempt:1 ~elapsed_s:0.1
             (Usage_observed { usage = Some usage_1; cost = Some cost_1 });
           event ~seq:4L ~attempt:1 ~elapsed_s:0.35
             (Attempt_finished Attempt_succeeded);
           event ~seq:5L ~attempt:1 ~elapsed_s:0.4 (Terminal Succeeded);
         ])
  in
  ignore (response ~attempts:[ attempt ] ~event_trace:gap_final_unknown ());
  let gap_before_final_is_known =
    ok
      (Workflow_event.make_trace
         [
           event ~seq:1L ~attempt:1 ~elapsed_s:0.05
             (Attempt_started Initial_attempt);
           event ~seq:3L ~attempt:1 ~elapsed_s:0.1
             (Usage_observed { usage = Some usage_lower; cost = Some cost_2 });
           event ~seq:4L ~attempt:1 ~elapsed_s:0.35
             (Attempt_finished Attempt_succeeded);
           event ~seq:5L ~attempt:1 ~elapsed_s:0.4 (Terminal Succeeded);
         ])
  in
  expect_error
    (Agent_execution.make_response ~attempts:[ attempt ] ~status:Success
       ~total_elapsed_s:0.5 ~cleanup_status:Cleanup_not_required
       ~event_trace:gap_before_final_is_known ());
  let unlocated_final_unknown =
    ok
      (Workflow_event.make_trace ~omitted_count:1L
         [
           event ~seq:1L ~attempt:1 ~elapsed_s:0.05
             (Attempt_started Initial_attempt);
           event ~seq:2L ~attempt:1 ~elapsed_s:0.1
             (Usage_observed { usage = Some usage_1; cost = Some cost_1 });
           event ~seq:3L ~attempt:1 ~elapsed_s:0.35
             (Attempt_finished Attempt_succeeded);
           event ~seq:4L ~attempt:1 ~elapsed_s:0.4 (Terminal Succeeded);
         ])
  in
  ignore
    (response ~attempts:[ attempt ] ~event_trace:unlocated_final_unknown ())

let test_usage_observations_retain_dimension_lower_bounds () =
  let initial_usage =
    ok (Execution_metrics.make_usage ~input_tokens:5L ~output_tokens:1L ())
  in
  let final_partial_usage =
    ok (Execution_metrics.make_usage ~output_tokens:4L ())
  in
  let initial_cost = ok (Execution_metrics.make_cost ~usd_micros:9L ()) in
  let trace =
    ok
      (Workflow_event.make_trace
         [
           event ~seq:1L ~attempt:1 ~elapsed_s:0.05
             (Attempt_started Initial_attempt);
           event ~seq:2L ~attempt:1 ~elapsed_s:0.1
             (Usage_observed
                { usage = Some initial_usage; cost = Some initial_cost });
           event ~seq:3L ~attempt:1 ~elapsed_s:0.2
             (Usage_observed
                { usage = Some final_partial_usage; cost = None });
           event ~seq:4L ~attempt:1 ~elapsed_s:0.35
             (Attempt_finished Attempt_succeeded);
           event ~seq:5L ~attempt:1 ~elapsed_s:0.4 (Terminal Succeeded);
         ])
  in
  let make ?usage ?cost () =
    let attempt = attempt ?usage ?cost () in
    Agent_execution.make_response ~attempts:[ attempt ] ~status:Success
      ~total_elapsed_s:0.5 ~cleanup_status:Cleanup_not_required ~event_trace:trace
      ()
  in
  let lower_input =
    ok (Execution_metrics.make_usage ~input_tokens:4L ~output_tokens:4L ())
  in
  let valid_usage =
    ok (Execution_metrics.make_usage ~input_tokens:6L ~output_tokens:4L ())
  in
  let wrong_exact_output =
    ok (Execution_metrics.make_usage ~input_tokens:6L ~output_tokens:5L ())
  in
  let lower_cost = ok (Execution_metrics.make_cost ~usd_micros:8L ()) in
  let valid_cost = ok (Execution_metrics.make_cost ~usd_micros:10L ()) in
  expect_error (make ~usage:lower_input ~cost:valid_cost ());
  expect_error (make ~cost:valid_cost ());
  expect_error (make ~usage:valid_usage ());
  expect_error (make ~usage:valid_usage ~cost:lower_cost ());
  expect_error (make ~usage:wrong_exact_output ~cost:valid_cost ());
  ignore (ok (make ~usage:valid_usage ~cost:valid_cost ()));
  let usage_reappears_lower =
    ok (Execution_metrics.make_usage ~input_tokens:4L ())
  in
  expect_error
    (Workflow_event.make_trace
       [
         event ~seq:1L ~attempt:1 ~elapsed_s:0.05
           (Usage_observed { usage = Some initial_usage; cost = None });
         event ~seq:2L ~attempt:1 ~elapsed_s:0.1
           (Usage_observed { usage = Some final_partial_usage; cost = None });
         event ~seq:3L ~attempt:1 ~elapsed_s:0.15
           (Usage_observed { usage = Some usage_reappears_lower; cost = None });
         event ~seq:4L ~attempt:1 ~elapsed_s:0.4 (Terminal Succeeded);
       ]);
  expect_error
    (Workflow_event.make_trace
       [
         event ~seq:1L ~attempt:1 ~elapsed_s:0.05
           (Usage_observed { usage = None; cost = Some initial_cost });
         event ~seq:2L ~attempt:1 ~elapsed_s:0.1
           (Usage_observed { usage = None; cost = None });
         event ~seq:3L ~attempt:1 ~elapsed_s:0.15
           (Usage_observed { usage = None; cost = Some lower_cost });
         event ~seq:4L ~attempt:1 ~elapsed_s:0.4 (Terminal Succeeded);
       ])

let usage_lower_bound_trace omission =
  let observed_usage =
    ok (Execution_metrics.make_usage ~input_tokens:5L ())
  in
  let prefix =
    [
      event ~seq:1L ~attempt:1 ~elapsed_s:0.05
        (Attempt_started Initial_attempt);
      event ~seq:2L ~attempt:1 ~elapsed_s:0.1
        (Usage_observed { usage = Some observed_usage; cost = None });
    ]
  in
  let suffix =
    match omission with
    | `Gap ->
        [
          event ~seq:4L ~attempt:1 ~elapsed_s:0.35
            (Attempt_finished Attempt_succeeded);
          event ~seq:5L ~attempt:1 ~elapsed_s:0.4 (Terminal Succeeded);
        ]
    | `Truncation ->
        let omissions =
          ok (Workflow_event.make_omission_counts ~usage_events:1L ())
        in
        [
          event ~seq:3L ~attempt:1 ~elapsed_s:0.2
            (Delivery_truncated omissions);
          event ~seq:4L ~attempt:1 ~elapsed_s:0.35
            (Attempt_finished Attempt_succeeded);
          event ~seq:5L ~attempt:1 ~elapsed_s:0.4 (Terminal Succeeded);
        ]
  in
  ok (Workflow_event.make_trace (prefix @ suffix))

let check_usage_lower_bound_trace trace =
  let make ?usage () =
    Agent_execution.make_response ~attempts:[ attempt ?usage () ]
      ~status:Success ~total_elapsed_s:0.5
      ~cleanup_status:Cleanup_not_required ~event_trace:trace ()
  in
  let below = ok (Execution_metrics.make_usage ~input_tokens:4L ()) in
  let above = ok (Execution_metrics.make_usage ~input_tokens:6L ()) in
  expect_error (make ~usage:below ());
  expect_error (make ());
  ignore (ok (make ~usage:above ()))

let test_usage_lower_bounds_survive_sequence_gap () =
  check_usage_lower_bound_trace (usage_lower_bound_trace `Gap)

let test_usage_lower_bounds_survive_truncation () =
  check_usage_lower_bound_trace (usage_lower_bound_trace `Truncation)

let test_usage_lower_bounds_survive_unlocated_omissions () =
  let observed_usage =
    ok (Execution_metrics.make_usage ~input_tokens:5L ())
  in
  let unlocated_trace =
    ok
      (Workflow_event.make_trace ~omitted_count:1L
         [
           event ~seq:1L ~attempt:1 ~elapsed_s:0.05
             (Attempt_started Initial_attempt);
           event ~seq:2L ~attempt:1 ~elapsed_s:0.1
             (Usage_observed { usage = Some observed_usage; cost = None });
           event ~seq:3L ~attempt:1 ~elapsed_s:0.35
             (Attempt_finished Attempt_succeeded);
           event ~seq:4L ~attempt:1 ~elapsed_s:0.4 (Terminal Succeeded);
         ])
  in
  let make_unlocated ?usage () =
    Agent_execution.make_response ~attempts:[ attempt ?usage () ]
      ~status:Success ~total_elapsed_s:0.5
      ~cleanup_status:Cleanup_not_required ~event_trace:unlocated_trace ()
  in
  let below = ok (Execution_metrics.make_usage ~input_tokens:4L ()) in
  let above = ok (Execution_metrics.make_usage ~input_tokens:6L ()) in
  expect_error (make_unlocated ~usage:below ());
  expect_error (make_unlocated ());
  ignore (ok (make_unlocated ~usage:above ()))

let test_retry_transition_matches_response_without_retained_start () =
  let make_trace retry_kind =
    ok
      (Workflow_event.make_trace
         [
           event ~seq:1L ~attempt:1 ~elapsed_s:0.05
             (Attempt_started Initial_attempt);
           event ~seq:2L ~attempt:1 ~elapsed_s:0.2
             (Attempt_finished Attempt_succeeded);
           event ~seq:3L ~attempt:1 ~elapsed_s:0.21
             (Retry_transition
                { kind = retry_kind; reason = Schema_validation });
           event ~seq:4L ~attempt:2 ~elapsed_s:0.4
             (Attempt_finished Attempt_succeeded);
           event ~seq:5L ~attempt:2 ~elapsed_s:0.45
             (Terminal Succeeded);
         ])
  in
  let first = attempt ~elapsed_s:0.1 ~schema_error:"schema" () in
  let fresh = attempt ~number:2 ~kind:Fresh_attempt () in
  ignore
    (response ~attempts:[ first; fresh ] ~total_elapsed_s:1.0
       ~event_trace:(make_trace Fresh_retry) ());
  expect_error_message "retry transition disagrees with response attempt kind"
    (Agent_execution.make_response ~attempts:[ first; fresh ] ~status:Success
       ~total_elapsed_s:1.0 ~cleanup_status:Cleanup_not_required
       ~event_trace:(make_trace Resume_retry) ());
  let transition_after_final =
    ok
      (Workflow_event.make_trace
         [
           event ~seq:1L ~attempt:1 ~elapsed_s:0.05
             (Attempt_started Initial_attempt);
           event ~seq:2L ~attempt:1 ~elapsed_s:0.2
             (Attempt_finished Attempt_succeeded);
           event ~seq:3L ~attempt:1 ~elapsed_s:0.21
             (Retry_transition
                { kind = Fresh_retry; reason = Schema_validation });
           event ~seq:4L ~attempt:2 ~elapsed_s:0.45 (Terminal Failed);
         ])
  in
  expect_error_message "retry transition has no following response attempt"
    (Agent_execution.make_response ~attempts:[ first ]
       ~status:(Failed "schema validation failed") ~total_elapsed_s:1.0
       ~cleanup_status:Cleanup_not_required
       ~event_trace:transition_after_final ())

let test_attempt_timing_uses_one_sided_envelope () =
  let attempt = attempt ~elapsed_s:0.25 () in
  let make_trace finished_at =
    ok
      (Workflow_event.make_trace
         [
           event ~seq:1L ~attempt:1 ~elapsed_s:0.05
             (Attempt_started Initial_attempt);
           event ~seq:2L ~attempt:1 ~elapsed_s:finished_at
             (Attempt_finished Attempt_succeeded);
           event ~seq:3L ~attempt:1 ~elapsed_s:0.5 (Terminal Succeeded);
         ])
  in
  ignore
    (response ~attempts:[ attempt ] ~total_elapsed_s:0.5
       ~event_trace:(make_trace 0.35) ());
  ignore
    (response ~attempts:[ attempt ] ~total_elapsed_s:0.5
       ~event_trace:
         (make_trace
            (0.05 +. 0.25 -. (Agent_execution.attempt_timing_tolerance_s /. 2.0)))
       ());
  expect_error
    (Agent_execution.make_response ~attempts:[ attempt ] ~status:Success
       ~total_elapsed_s:0.5 ~cleanup_status:Cleanup_not_required
       ~event_trace:
         (make_trace
            (0.05 +. 0.25 -. (Agent_execution.attempt_timing_tolerance_s *. 2.0)))
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

let test_process_exit_codes () =
  expect_error
    (Workflow_event.make ~seq:1L ~attempt:1 ~elapsed_s:0.0
       (Process_exited (Exited (-1))));
  let trace =
    ok
      (Workflow_event.make_trace
         [
           event ~seq:1L ~attempt:1 ~elapsed_s:0.0 Process_started;
           event ~seq:2L ~attempt:1 ~elapsed_s:0.1
             (Process_exited (Exited max_int));
           event ~seq:3L ~attempt:1 ~elapsed_s:0.2
             (Attempt_finished Attempt_succeeded);
           event ~seq:4L ~attempt:1 ~elapsed_s:0.3 (Terminal Succeeded);
         ])
  in
  let projection =
    trace |> Workflow_event.trace_to_yojson |> Yojson.Safe.to_string
  in
  Alcotest.(check bool)
    "nonnegative host exit code projected" true
    (contains projection (Printf.sprintf "\"code\":%d" max_int))

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
    (trace_bytes <= Workflow_event.max_trace_projection_bytes);
  let escaped_event_text = String.make Workflow_event.max_text_bytes '\000' in
  let event_count = 80 in
  let large_trace =
    let text_events =
      List.init event_count (fun index ->
          event
            ~seq:(Int64.of_int (index + 1))
            ~attempt:1 ~elapsed_s:0.0 (Agent_text_delta escaped_event_text))
    in
    ok
      (Workflow_event.make_trace
         (text_events
         @ [
             event
               ~seq:(Int64.of_int (event_count + 1))
               ~attempt:8 ~elapsed_s:0.1 (Terminal Succeeded);
           ]))
  in
  let escaped_attempt_text =
    String.make Agent_execution.max_public_text_bytes '\127'
  in
  let escaped_json =
    `String (String.make ((Agent_execution.max_json_bytes - 2) / 6) '\000')
  in
  let large_attempts =
    List.init Agent_execution.max_attempts (fun index ->
        let number = index + 1 in
        attempt ~number
          ~kind:(if number = 1 then Initial_attempt else Fresh_attempt)
          ~elapsed_s:0.0 ~text:escaped_attempt_text
          ~structured_json:escaped_json ())
  in
  expect_error
    (Agent_execution.make_response ~attempts:large_attempts ~status:Success
       ~total_elapsed_s:1.0 ~cleanup_status:Cleanup_not_required
       ~event_trace:large_trace ())

let failed_outer_trace ?(attempt = 1) () =
  ok
    (Workflow_event.make_trace
       [ event ~seq:1L ~attempt ~elapsed_s:0.5 (Terminal Failed) ])

let test_error_classification () =
  let dispatch_trace =
    ok
      (Workflow_event.make_trace
         [
           event ~seq:1L ~attempt:0 ~elapsed_s:0.0 Task_started;
           event ~seq:2L ~attempt:0 ~elapsed_s:0.1 (Terminal Failed);
         ])
  in
  let dispatch =
    ok
      (Agent_execution.make_dispatch_error
         ~kind:Agent_execution.Backend_unavailable ~message:"not installed"
         ~event_trace:dispatch_trace ())
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
  | Dispatch_failure
      { kind = Backend_unavailable; event_trace = Some retained; _ } ->
      Alcotest.(check int)
        "dispatch trace retained" 2
        (List.length (Workflow_event.events retained))
  | _ -> Alcotest.fail "dispatch error classification lost");
  let dispatch_projection =
    Agent_execution.error_to_yojson dispatch |> Yojson.Safe.to_string
  in
  Alcotest.(check bool)
    "dispatch trace projected" true
    (contains dispatch_projection "\"event_trace\":{");
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
        (contains serialized "cwr.agent-execution.error/v1");
      check_absent "absent dispatch trace does not alter projection" serialized
        "\"event_trace\"")
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
      ( Agent_execution.Native_backend_failure_with_schema,
        "native_backend_failure_with_schema",
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
      Agent_execution.Native_backend_failure_with_schema;
      Schema_retry_failed;
      Backend_execution_failed;
      Execution_contract_failed;
    ];
  expect_error
    (Agent_execution.make_execution_error
       ~kind:Native_backend_failure_with_schema
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
       (Agent_execution.make_execution_error
          ~kind:Native_backend_failure_with_schema
          ~message:"native backend failure" ~response:failed ()));
  ignore
    (ok
       (Agent_execution.make_execution_error ~kind:Schema_retry_failed
          ~message:"retry exhausted" ~response:schema_failed ()))

let test_post_execution_dispatch_failure_preserves_every_status () =
  let cases =
    [
      ("success", response (), Agent_execution.Success);
      ( "failed",
        response ~attempts:[ attempt ~status:(Failed "backend") () ] (),
        Agent_execution.Failed "backend" );
      ( "timed out",
        response ~attempts:[ attempt ~status:Timed_out () ] (),
        Agent_execution.Timed_out );
      ( "cancelled",
        response ~attempts:[ attempt ~status:Cancelled () ] (),
        Agent_execution.Cancelled );
      ( "schema rejection",
        response ~attempts:[ attempt ~schema_error:"schema" () ]
          ~status:(Failed "schema rejection") (),
        Agent_execution.Failed "schema rejection" );
    ]
  in
  List.iter
    (fun (label, original, expected_status) ->
      let error =
        ok
          (Agent_execution.make_post_execution_dispatch_error
             ~cause:Agent_execution.Preflight_failed
             ~message:"PRIVATE_POST_DISPATCH_DIAGNOSTIC" ~response:original
             ~outer_event_trace:(failed_outer_trace ()) ())
      in
      match Agent_execution.error_view error with
      | Post_execution_dispatch_failed
          {
            cause = Preflight_failed;
            message;
            response = retained;
            outer_event_trace;
          } ->
          Alcotest.(check string)
            (label ^ " diagnostic retained in process")
            "PRIVATE_POST_DISPATCH_DIAGNOSTIC" message;
          Alcotest.(check bool)
            (label ^ " status retained") true
            (Agent_execution.final_status retained = expected_status);
          Alcotest.(check int)
            (label ^ " attempts retained")
            (List.length (Agent_execution.attempts original))
            (List.length (Agent_execution.attempts retained));
          Alcotest.(check int)
            (label ^ " outer trace retained") 1
            (List.length (Workflow_event.events outer_event_trace));
          let serialized =
            Agent_execution.error_to_yojson error |> Yojson.Safe.to_string
          in
          Alcotest.(check bool)
            (label ^ " fixed error kind") true
            (contains serialized
               "\"error_kind\":\"post_execution_dispatch_failed\"");
          Alcotest.(check bool)
            (label ^ " fixed cause") true
            (contains serialized "\"cause\":\"preflight_failed\"");
          check_absent (label ^ " private diagnostic redacted") serialized
            "PRIVATE_POST_DISPATCH_DIAGNOSTIC"
      | _ -> Alcotest.failf "%s post-dispatch failure misclassified" label)
    cases;
  expect_error
    (Agent_execution.make_post_execution_dispatch_error
       ~cause:Agent_execution.Preflight_failed ~message:"" ~response:(response ())
       ~outer_event_trace:(failed_outer_trace ()) ());
  expect_error
    (Agent_execution.make_post_execution_dispatch_error
       ~cause:Agent_execution.Preflight_failed ~message:"outer must fail"
       ~response:(response ()) ~outer_event_trace:(terminal_trace ()) ())

let test_post_execution_cleanup_failure_keeps_success_and_outer_trace () =
  let usage =
    ok (Execution_metrics.make_usage ~input_tokens:7L ~output_tokens:3L ())
  in
  let cost = ok (Execution_metrics.make_cost ~usd_micros:11L ()) in
  let successful_attempt =
    attempt ~elapsed_s:0.2 ~session_id:"session-cleanup" ~usage ~cost ()
  in
  let completed =
    response ~attempts:[ successful_attempt ] ~total_elapsed_s:0.4
      ~cleanup_status:Cleanup_failed ()
  in
  let outer_event_trace =
    ok
      (Workflow_event.make_trace
         [
           event ~seq:1L ~attempt:0 ~elapsed_s:0.0 Task_started;
           event ~seq:2L ~attempt:1 ~elapsed_s:0.05
             (Attempt_started Initial_attempt);
           event ~seq:3L ~attempt:1 ~elapsed_s:0.2
             (Usage_observed { usage = Some usage; cost = Some cost });
           event ~seq:4L ~attempt:1 ~elapsed_s:0.3
             (Session_id "session-cleanup");
           event ~seq:5L ~attempt:1 ~elapsed_s:0.35
             (Attempt_finished Attempt_succeeded);
           event ~seq:6L ~attempt:1 ~elapsed_s:0.8 (Terminal Failed);
         ])
  in
  expect_error
    (Agent_execution.make_response ~attempts:[ successful_attempt ]
       ~status:Success ~total_elapsed_s:0.8 ~cleanup_status:Cleanup_failed
       ~event_trace:outer_event_trace ());
  let error =
    ok
      (Agent_execution.make_post_execution_dispatch_error
         ~cause:Agent_execution.Preflight_failed ~message:"cleanup failed"
         ~response:completed ~outer_event_trace ())
  in
  match Agent_execution.error_view error with
  | Post_execution_dispatch_failed
      { response = retained; outer_event_trace = retained_outer; _ } ->
      Alcotest.(check bool)
        "completed transport remains successful" true
        (Agent_execution.final_status retained = Success);
      Alcotest.(check bool)
        "cleanup failure retained" true
        (Agent_execution.cleanup_status retained = Cleanup_failed);
      Alcotest.(check (option string))
        "session retained" (Some "session-cleanup")
        (Agent_execution.final_session_id retained);
      Alcotest.(check (option int64))
        "usage retained" (Some 7L)
        (Option.bind (Agent_execution.total_usage retained)
           Execution_metrics.input_tokens);
      Alcotest.(check (option int64))
        "cost retained" (Some 11L)
        (Option.bind (Agent_execution.total_cost retained)
           Execution_metrics.usd_micros);
      Alcotest.(check bool)
        "nested response has no mismatched outer trace" true
        (Option.is_none (Agent_execution.event_trace retained));
      let last_outer = List.rev (Workflow_event.events retained_outer) in
      Alcotest.(check bool)
        "outer trace independently failed" true
        (match last_outer with
        | terminal :: _ -> Workflow_event.payload terminal = Terminal Failed
        | [] -> false);
      let serialized =
        Agent_execution.error_to_yojson error |> Yojson.Safe.to_string
      in
      Alcotest.(check bool)
        "nested success projected" true
        (contains serialized "\"status\":\"success\"");
      Alcotest.(check bool)
        "outer failure trace projected" true
        (contains serialized "\"outer_event_trace\"");
      Alcotest.(check bool)
        "post-execution projection bounded" true
        (String.length serialized <= Agent_execution.max_error_projection_bytes)
  | _ -> Alcotest.fail "cleanup failure lost post-execution classification"

let test_no_completed_attempt_error_shapes () =
  let cases =
    [
      (Agent_execution.Failed "PRIVATE_STATUS_FAILURE", Workflow_event.Failed);
      (Agent_execution.Timed_out, Workflow_event.Timed_out);
      (Agent_execution.Cancelled, Workflow_event.Cancelled);
    ]
  in
  List.iter
    (fun (status, terminal) ->
      let event_trace =
        ok
          (Workflow_event.make_trace
             [
               event ~seq:1L ~attempt:1 ~elapsed_s:0.1
                 (Attempt_started Initial_attempt);
               event ~seq:2L ~attempt:1 ~elapsed_s:0.2 (Terminal terminal);
             ])
      in
      let error =
        ok
          (Agent_execution.make_no_completed_attempt_error ~status
             ~invocation_may_have_started:true
             ~message:"PRIVATE_INDETERMINATE_DIAGNOSTIC" ~event_trace ())
      in
      match Agent_execution.error_view error with
      | No_completed_attempt
          {
            status = retained_status;
            invocation_may_have_started;
            event_trace = Some retained_trace;
            _;
          } ->
          Alcotest.(check bool)
            "terminal status retained" true (retained_status = status);
          Alcotest.(check bool)
            "invocation uncertainty retained" true invocation_may_have_started;
          Alcotest.(check int)
            "safe trace retained" 2
            (List.length (Workflow_event.events retained_trace));
          let serialized =
            Agent_execution.error_to_yojson error |> Yojson.Safe.to_string
          in
          Alcotest.(check bool)
            "indeterminate kind projected" true
            (contains serialized "\"error_kind\":\"no_completed_attempt\"");
          Alcotest.(check bool)
            "indeterminate projection version" true
            (contains serialized "cwr.agent-execution.error/v1");
          Alcotest.(check bool)
            "invocation uncertainty projected" true
            (contains serialized "\"invocation_may_have_started\":true");
          check_absent "indeterminate diagnostic redacted" serialized
            "PRIVATE_INDETERMINATE_DIAGNOSTIC";
          check_absent "failed status diagnostic redacted" serialized
            "PRIVATE_STATUS_FAILURE";
          Alcotest.(check bool)
            "indeterminate projection bounded" true
            (String.length serialized
            <= Agent_execution.max_error_projection_bytes)
      | _ -> Alcotest.fail "no-completed-attempt error misclassified")
    cases;
  ignore
    (ok
       (Agent_execution.make_no_completed_attempt_error ~status:Cancelled
          ~invocation_may_have_started:true ~message:"cancelled" ()));
  expect_error
    (Agent_execution.make_no_completed_attempt_error ~status:Success
       ~invocation_may_have_started:true ~message:"invalid success" ());
  expect_error
    (Agent_execution.make_no_completed_attempt_error ~status:Timed_out
       ~invocation_may_have_started:false ~message:"contradictory trace"
       ~event_trace:
         (ok
            (Workflow_event.make_trace
               [
                 event ~seq:1L ~attempt:1 ~elapsed_s:0.1
                   (Attempt_started Initial_attempt);
                 event ~seq:2L ~attempt:1 ~elapsed_s:0.2
                   (Terminal Timed_out);
               ]))
       ());
  expect_error
    (Agent_execution.make_no_completed_attempt_error ~status:Timed_out
       ~invocation_may_have_started:true ~message:"terminal mismatch"
       ~event_trace:(failed_outer_trace ()) ())

let incomplete_continuation ~number ~kind ~invocation =
  ok
    (Agent_execution.make_incomplete_continuation ~number ~kind ~invocation ())

let incomplete_execution ~completed_attempts ~outer_status ~total_elapsed_s
    ~cleanup_status ~continuation ~outer_event_trace =
  ok
    (Agent_execution.make_incomplete_execution ~completed_attempts ~outer_status
       ~total_elapsed_s ~cleanup_status ~continuation ~outer_event_trace ())

let test_incomplete_fresh_retry_deadline () =
  let completed_usage =
    ok (Execution_metrics.make_usage ~input_tokens:7L ~output_tokens:3L ())
  in
  let completed_cost = ok (Execution_metrics.make_cost ~usd_micros:11L ()) in
  let incomplete_usage =
    ok (Execution_metrics.make_usage ~input_tokens:50L ~output_tokens:2L ())
  in
  let incomplete_cost = ok (Execution_metrics.make_cost ~usd_micros:70L ()) in
  let later_incomplete_usage =
    ok (Execution_metrics.make_usage ~input_tokens:55L ())
  in
  let incomplete_omissions =
    ok (Workflow_event.make_omission_counts ~usage_events:1L ())
  in
  let first =
    attempt ~schema_error:"PRIVATE_FIRST_SCHEMA_ERROR" ~session_id:"session-1"
      ~usage:completed_usage ~cost:completed_cost ()
  in
  let outer_event_trace =
    ok
      (Workflow_event.make_trace
         [
           event ~seq:1L ~attempt:0 ~elapsed_s:0.0 Task_started;
           event ~seq:2L ~attempt:1 ~elapsed_s:0.05
             (Attempt_started Initial_attempt);
           event ~seq:3L ~attempt:1 ~elapsed_s:0.15
             (Usage_observed
                { usage = Some completed_usage; cost = Some completed_cost });
           event ~seq:4L ~attempt:1 ~elapsed_s:0.2 (Session_id "session-1");
           event ~seq:5L ~attempt:1 ~elapsed_s:0.3
             (Attempt_finished Attempt_succeeded);
           event ~seq:6L ~attempt:1 ~elapsed_s:0.31
             (Retry_transition
                { kind = Fresh_retry; reason = Schema_validation });
           event ~seq:7L ~attempt:2 ~elapsed_s:0.35
             (Attempt_started Fresh_attempt);
           event ~seq:8L ~attempt:2 ~elapsed_s:0.45
             (Usage_observed
                { usage = Some incomplete_usage; cost = Some incomplete_cost });
           event ~seq:9L ~attempt:2 ~elapsed_s:0.47
             (Session_id "continuation-session");
           event ~seq:10L ~attempt:2 ~elapsed_s:0.49
             (Usage_observed
                { usage = Some later_incomplete_usage; cost = None });
           event ~seq:11L ~attempt:2 ~elapsed_s:0.5
             (Delivery_truncated incomplete_omissions);
           event ~seq:12L ~attempt:2 ~elapsed_s:0.55
             Process_termination_requested;
           event ~seq:13L ~attempt:2 ~elapsed_s:0.6
             (Attempt_finished Attempt_timed_out);
           event ~seq:14L ~attempt:2 ~elapsed_s:0.7 (Terminal Timed_out);
         ])
  in
  let continuation =
    incomplete_continuation ~number:2 ~kind:Fresh_attempt
      ~invocation:Agent_execution.Invocation_started
  in
  let execution =
    incomplete_execution ~completed_attempts:[ first ] ~outer_status:Timed_out
      ~total_elapsed_s:0.7 ~cleanup_status:Cleanup_succeeded ~continuation
      ~outer_event_trace
  in
  let error =
    ok
      (Agent_execution.make_incomplete_execution_error
         ~message:"PRIVATE_DEADLINE_DIAGNOSTIC" ~execution ())
  in
  (match Agent_execution.error_view error with
  | Incomplete_execution { message; execution = retained } ->
      Alcotest.(check string)
        "diagnostic retained in process" "PRIVATE_DEADLINE_DIAGNOSTIC" message;
      Alcotest.(check int)
        "only committed attempt retained" 1
        (List.length
           (Agent_execution.incomplete_completed_attempts retained));
      Alcotest.(check bool)
        "outer timeout retained" true
        (Agent_execution.incomplete_outer_status retained = Timed_out);
      Alcotest.(check (option int64))
        "completed usage excludes incomplete observation" (Some 7L)
        (Option.bind
           (Agent_execution.incomplete_completed_usage retained)
           Execution_metrics.input_tokens);
      Alcotest.(check (option int64))
        "completed cost excludes incomplete observation" (Some 11L)
        (Option.bind
           (Agent_execution.incomplete_completed_cost retained)
           Execution_metrics.usd_micros);
      Alcotest.(check (option int64))
        "incomplete usage is a separate lower bound" (Some 55L)
        (Option.bind
           (Agent_execution.incomplete_continuation_usage_lower_bound retained)
           Execution_metrics.input_tokens);
      Alcotest.(check (option int64))
        "omitted later dimension retains its lower bound" (Some 2L)
        (Option.bind
           (Agent_execution.incomplete_continuation_usage_lower_bound retained)
           Execution_metrics.output_tokens);
      Alcotest.(check (option int64))
        "incomplete cost is a separate lower bound" (Some 70L)
        (Option.bind
           (Agent_execution.incomplete_continuation_cost_lower_bound retained)
           Execution_metrics.usd_micros);
      Alcotest.(check (option string))
        "session derives only from completed attempts" (Some "session-1")
        (Agent_execution.incomplete_final_session_id retained);
      Alcotest.(check bool)
        "cleanup retained" true
        (Agent_execution.incomplete_cleanup_status retained = Cleanup_succeeded);
      let retained_continuation =
        match Agent_execution.incomplete_continuation retained with
        | Some value -> value
        | None -> Alcotest.fail "incomplete continuation lost"
      in
      Alcotest.(check int)
        "continuation number" 2
        (Agent_execution.continuation_number retained_continuation);
      Alcotest.(check bool)
        "continuation kind" true
        (Agent_execution.continuation_kind retained_continuation = Fresh_attempt);
      Alcotest.(check bool)
        "invocation known started" true
        (Agent_execution.continuation_invocation retained_continuation
        = Invocation_started);
      Alcotest.(check int)
        "complete outer trace retained" 14
        (List.length
           (Workflow_event.events
              (Agent_execution.incomplete_outer_event_trace retained)))
  | _ -> Alcotest.fail "deadline interruption lost incomplete classification");
  let serialized =
    Agent_execution.error_to_yojson error |> Yojson.Safe.to_string
  in
  Alcotest.(check bool)
    "partial error kind projected" true
    (contains serialized "\"error_kind\":\"incomplete_execution\"");
  Alcotest.(check bool)
    "outer timeout projected" true
    (contains serialized "\"outer_status\":\"timed_out\"");
  Alcotest.(check bool)
    "separate lower bound projected" true
    (contains serialized "\"continuation_usage_lower_bound\"");
  Alcotest.(check bool)
    "continuation session stays in its trace" true
    (contains serialized "continuation-session");
  check_absent "partial diagnostic redacted" serialized
    "PRIVATE_DEADLINE_DIAGNOSTIC";
  check_absent "schema diagnostic redacted" serialized
    "PRIVATE_FIRST_SCHEMA_ERROR";
  Alcotest.(check bool)
    "partial projection bounded" true
    (String.length serialized <= Agent_execution.max_error_projection_bytes);
  let execution_projection =
    Agent_execution.incomplete_execution_to_yojson execution
    |> Yojson.Safe.to_string
  in
  Alcotest.(check bool)
    "incomplete projection version" true
    (contains execution_projection "cwr.agent-execution.incomplete/v1");
  Alcotest.(check bool)
    "incomplete execution projection bounded" true
    (String.length execution_projection
    <= Agent_execution.max_incomplete_execution_projection_bytes)

let test_spurious_continuation_before_transition_rejected () =
  let first = attempt ~schema_error:"schema rejected" () in
  let cancellation_before_transition =
    ok
      (Workflow_event.make_trace
         [
           event ~seq:1L ~attempt:1 ~elapsed_s:0.05
             (Attempt_started Initial_attempt);
           event ~seq:2L ~attempt:1 ~elapsed_s:0.3
             (Attempt_finished Attempt_succeeded);
           event ~seq:3L ~attempt:1 ~elapsed_s:0.5 (Terminal Cancelled);
         ])
  in
  let fabricated =
    incomplete_continuation ~number:2 ~kind:Fresh_attempt
      ~invocation:Agent_execution.Invocation_may_have_started
  in
  expect_error
    (Agent_execution.make_incomplete_execution ~completed_attempts:[ first ]
       ~outer_status:Cancelled ~total_elapsed_s:0.5
       ~cleanup_status:Cleanup_not_required ~continuation:fabricated
       ~outer_event_trace:cancellation_before_transition ())

let test_started_continuation_requires_explicit_evidence () =
  let first = attempt ~schema_error:"schema rejected" () in
  let omitted_start =
    ok
      (Workflow_event.make_trace ~omitted_count:1L
         [
           event ~seq:1L ~attempt:1 ~elapsed_s:0.3
             (Attempt_finished Attempt_succeeded);
           event ~seq:2L ~attempt:1 ~elapsed_s:0.31
             (Retry_transition
                { kind = Fresh_retry; reason = Schema_validation });
           event ~seq:4L ~attempt:2 ~elapsed_s:0.5 (Terminal Timed_out);
         ])
  in
  let unsupported_started =
    incomplete_continuation ~number:2 ~kind:Fresh_attempt
      ~invocation:Agent_execution.Invocation_started
  in
  expect_error
    (Agent_execution.make_incomplete_execution ~completed_attempts:[ first ]
       ~outer_status:Timed_out ~total_elapsed_s:0.5
       ~cleanup_status:Cleanup_not_required ~continuation:unsupported_started
       ~outer_event_trace:omitted_start ())

let test_uncertain_continuation_requires_omission_and_n_plus_one_terminal () =
  let first = attempt ~schema_error:"schema rejected" () in
  let uncertain =
    incomplete_continuation ~number:2 ~kind:Fresh_attempt
      ~invocation:Agent_execution.Invocation_may_have_started
  in
  let dense_missing_start =
    ok
      (Workflow_event.make_trace
         [
           event ~seq:1L ~attempt:1 ~elapsed_s:0.3
             (Attempt_finished Attempt_succeeded);
           event ~seq:2L ~attempt:1 ~elapsed_s:0.31
             (Retry_transition
                { kind = Fresh_retry; reason = Schema_validation });
           event ~seq:3L ~attempt:2 ~elapsed_s:0.5 (Terminal Timed_out);
         ])
  in
  expect_error
    (Agent_execution.make_incomplete_execution ~completed_attempts:[ first ]
       ~outer_status:Timed_out ~total_elapsed_s:0.5
       ~cleanup_status:Cleanup_not_required ~continuation:uncertain
       ~outer_event_trace:dense_missing_start ());
  let wrong_terminal_attempt =
    ok
      (Workflow_event.make_trace ~omitted_count:1L
         [
           event ~seq:1L ~attempt:1 ~elapsed_s:0.3
             (Attempt_finished Attempt_succeeded);
           event ~seq:3L ~attempt:1 ~elapsed_s:0.5 (Terminal Timed_out);
         ])
  in
  expect_error
    (Agent_execution.make_incomplete_execution ~completed_attempts:[ first ]
       ~outer_status:Timed_out ~total_elapsed_s:0.5
       ~cleanup_status:Cleanup_not_required ~continuation:uncertain
       ~outer_event_trace:wrong_terminal_attempt ())

let test_uncertain_continuation_accepts_omitted_start_at_n_plus_one () =
  let first = attempt ~schema_error:"schema rejected" () in
  let omitted_start =
    ok
      (Workflow_event.make_trace ~omitted_count:1L
         [
           event ~seq:1L ~attempt:1 ~elapsed_s:0.3
             (Attempt_finished Attempt_succeeded);
           event ~seq:2L ~attempt:1 ~elapsed_s:0.31
             (Retry_transition
                { kind = Fresh_retry; reason = Schema_validation });
           event ~seq:4L ~attempt:2 ~elapsed_s:0.5 (Terminal Timed_out);
         ])
  in
  let uncertain =
    incomplete_continuation ~number:2 ~kind:Fresh_attempt
      ~invocation:Agent_execution.Invocation_may_have_started
  in
  let execution =
    ok
      (Agent_execution.make_incomplete_execution ~completed_attempts:[ first ]
         ~outer_status:Timed_out ~total_elapsed_s:0.5
         ~cleanup_status:Cleanup_not_required ~continuation:uncertain
         ~outer_event_trace:omitted_start ())
  in
  let retained =
    match Agent_execution.incomplete_continuation execution with
    | Some value -> value
    | None -> Alcotest.fail "omitted continuation identity was lost"
  in
  Alcotest.(check bool)
    "uncertainty retained" true
    (Agent_execution.continuation_invocation retained
    = Invocation_may_have_started)

let test_uncertain_continuation_rejects_unrelated_earlier_gap () =
  let first = attempt ~schema_error:"schema rejected" () in
  let earlier_gap_then_dense_boundary =
    ok
      (Workflow_event.make_trace ~omitted_count:1L
         [
           event ~seq:1L ~attempt:1 ~elapsed_s:0.05
             (Attempt_started Initial_attempt);
           event ~seq:3L ~attempt:1 ~elapsed_s:0.3
             (Attempt_finished Attempt_succeeded);
           event ~seq:4L ~attempt:1 ~elapsed_s:0.31
             (Retry_transition
                { kind = Fresh_retry; reason = Schema_validation });
           event ~seq:5L ~attempt:2 ~elapsed_s:0.5 (Terminal Timed_out);
         ])
  in
  let uncertain =
    incomplete_continuation ~number:2 ~kind:Fresh_attempt
      ~invocation:Agent_execution.Invocation_may_have_started
  in
  expect_error
    (Agent_execution.make_incomplete_execution ~completed_attempts:[ first ]
       ~outer_status:Timed_out ~total_elapsed_s:0.5
       ~cleanup_status:Cleanup_not_required ~continuation:uncertain
       ~outer_event_trace:earlier_gap_then_dense_boundary ())

let test_uncertain_continuation_rejects_unrelated_earlier_truncation () =
  let first = attempt ~schema_error:"schema rejected" () in
  let omissions =
    ok (Workflow_event.make_omission_counts ~control_events:1L ())
  in
  let earlier_truncation_then_dense_boundary =
    ok
      (Workflow_event.make_trace
         [
           event ~seq:1L ~attempt:1 ~elapsed_s:0.05
             (Attempt_started Initial_attempt);
           event ~seq:2L ~attempt:1 ~elapsed_s:0.1
             (Delivery_truncated omissions);
           event ~seq:3L ~attempt:1 ~elapsed_s:0.3
             (Attempt_finished Attempt_succeeded);
           event ~seq:4L ~attempt:1 ~elapsed_s:0.31
             (Retry_transition
                { kind = Fresh_retry; reason = Schema_validation });
           event ~seq:5L ~attempt:2 ~elapsed_s:0.5 (Terminal Timed_out);
         ])
  in
  let uncertain =
    incomplete_continuation ~number:2 ~kind:Fresh_attempt
      ~invocation:Agent_execution.Invocation_may_have_started
  in
  expect_error
    (Agent_execution.make_incomplete_execution ~completed_attempts:[ first ]
       ~outer_status:Timed_out ~total_elapsed_s:0.5
       ~cleanup_status:Cleanup_not_required ~continuation:uncertain
       ~outer_event_trace:earlier_truncation_then_dense_boundary ())

let test_uncertain_continuation_accepts_boundary_spanning_gap () =
  let first = attempt ~schema_error:"schema rejected" () in
  let omitted_transition_and_start =
    ok
      (Workflow_event.make_trace ~omitted_count:1L
         [
           event ~seq:1L ~attempt:1 ~elapsed_s:0.05
             (Attempt_started Initial_attempt);
           event ~seq:2L ~attempt:1 ~elapsed_s:0.3
             (Attempt_finished Attempt_succeeded);
           event ~seq:4L ~attempt:2 ~elapsed_s:0.5 (Terminal Timed_out);
         ])
  in
  let uncertain =
    incomplete_continuation ~number:2 ~kind:Fresh_attempt
      ~invocation:Agent_execution.Invocation_may_have_started
  in
  ignore
    (ok
       (Agent_execution.make_incomplete_execution ~completed_attempts:[ first ]
          ~outer_status:Timed_out ~total_elapsed_s:0.5
          ~cleanup_status:Cleanup_not_required ~continuation:uncertain
          ~outer_event_trace:omitted_transition_and_start ()))

let test_uncertain_continuation_accepts_post_transition_truncation () =
  let first = attempt ~schema_error:"schema rejected" () in
  let omissions =
    ok (Workflow_event.make_omission_counts ~control_events:1L ())
  in
  let truncated_start =
    ok
      (Workflow_event.make_trace
         [
           event ~seq:1L ~attempt:1 ~elapsed_s:0.05
             (Attempt_started Initial_attempt);
           event ~seq:2L ~attempt:1 ~elapsed_s:0.3
             (Attempt_finished Attempt_succeeded);
           event ~seq:3L ~attempt:1 ~elapsed_s:0.31
             (Retry_transition
                { kind = Fresh_retry; reason = Schema_validation });
           event ~seq:4L ~attempt:2 ~elapsed_s:0.4
             (Delivery_truncated omissions);
           event ~seq:5L ~attempt:2 ~elapsed_s:0.5 (Terminal Timed_out);
         ])
  in
  let uncertain =
    incomplete_continuation ~number:2 ~kind:Fresh_attempt
      ~invocation:Agent_execution.Invocation_may_have_started
  in
  ignore
    (ok
       (Agent_execution.make_incomplete_execution ~completed_attempts:[ first ]
          ~outer_status:Timed_out ~total_elapsed_s:0.5
          ~cleanup_status:Cleanup_not_required ~continuation:uncertain
          ~outer_event_trace:truncated_start ()))

let test_incomplete_completed_telemetry_mismatches () =
  let completed_usage = ok (Execution_metrics.make_usage ~input_tokens:7L ()) in
  let mismatched_usage =
    ok (Execution_metrics.make_usage ~input_tokens:8L ())
  in
  let completed_cost = ok (Execution_metrics.make_cost ~usd_micros:11L ()) in
  let mismatched_cost = ok (Execution_metrics.make_cost ~usd_micros:12L ()) in
  let first =
    attempt ~usage:completed_usage ~cost:completed_cost
      ~session_id:"completed-session" ()
  in
  let trace ~usage ~cost ~session_id =
    ok
      (Workflow_event.make_trace
         [
           event ~seq:1L ~attempt:1 ~elapsed_s:0.05
             (Attempt_started Initial_attempt);
           event ~seq:2L ~attempt:1 ~elapsed_s:0.1
             (Usage_observed { usage = Some usage; cost = Some cost });
           event ~seq:3L ~attempt:1 ~elapsed_s:0.15
             (Session_id session_id);
           event ~seq:4L ~attempt:1 ~elapsed_s:0.3
             (Attempt_finished Attempt_succeeded);
           event ~seq:5L ~attempt:1 ~elapsed_s:0.5 (Terminal Timed_out);
         ])
  in
  let make outer_event_trace =
    Agent_execution.make_incomplete_execution ~completed_attempts:[ first ]
      ~outer_status:Timed_out ~total_elapsed_s:0.5
      ~cleanup_status:Cleanup_not_required ~outer_event_trace ()
  in
  expect_error
    (make
       (trace ~usage:mismatched_usage ~cost:completed_cost
          ~session_id:"completed-session"));
  expect_error
    (make
       (trace ~usage:completed_usage ~cost:mismatched_cost
          ~session_id:"completed-session"));
  expect_error
    (make
       (trace ~usage:completed_usage ~cost:completed_cost
          ~session_id:"different-session"))

let test_incomplete_resumed_retry_cancellation () =
  let first =
    attempt ~schema_error:"schema rejected" ~session_id:"resume-session" ()
  in
  let outer_event_trace =
    ok
      (Workflow_event.make_trace
         [
           event ~seq:1L ~attempt:1 ~elapsed_s:0.05
             (Attempt_started Initial_attempt);
           event ~seq:2L ~attempt:1 ~elapsed_s:0.3
             (Attempt_finished Attempt_succeeded);
           event ~seq:3L ~attempt:1 ~elapsed_s:0.31
             (Retry_transition
                { kind = Resume_retry; reason = Schema_validation });
           event ~seq:4L ~attempt:2 ~elapsed_s:0.35
             (Attempt_started Resumed_attempt);
           event ~seq:5L ~attempt:2 ~elapsed_s:0.5 (Terminal Cancelled);
         ])
  in
  let continuation =
    incomplete_continuation ~number:2 ~kind:Resumed_attempt
      ~invocation:Agent_execution.Invocation_started
  in
  let execution =
    incomplete_execution ~completed_attempts:[ first ] ~outer_status:Cancelled
      ~total_elapsed_s:0.5 ~cleanup_status:Cleanup_failed ~continuation
      ~outer_event_trace
  in
  Alcotest.(check bool)
    "cancellation retained" true
    (Agent_execution.incomplete_outer_status execution = Cancelled);
  Alcotest.(check (option string))
    "completed session retained" (Some "resume-session")
    (Agent_execution.incomplete_final_session_id execution)

let test_incomplete_retry_exception_after_finish_event () =
  let first = attempt ~schema_error:"schema rejected" () in
  let outer_event_trace =
    ok
      (Workflow_event.make_trace
         [
           event ~seq:1L ~attempt:1 ~elapsed_s:0.05
             (Attempt_started Initial_attempt);
           event ~seq:2L ~attempt:1 ~elapsed_s:0.3
             (Attempt_finished Attempt_succeeded);
           event ~seq:3L ~attempt:1 ~elapsed_s:0.31
             (Retry_transition
                { kind = Fresh_retry; reason = Schema_validation });
           event ~seq:4L ~attempt:2 ~elapsed_s:0.35
             (Attempt_started Fresh_attempt);
           event ~seq:5L ~attempt:2 ~elapsed_s:0.45
             (Attempt_finished Attempt_failed);
           event ~seq:6L ~attempt:2 ~elapsed_s:0.5 (Terminal Failed);
         ])
  in
  let continuation =
    incomplete_continuation ~number:2 ~kind:Fresh_attempt
      ~invocation:Agent_execution.Invocation_started
  in
  let execution =
    incomplete_execution ~completed_attempts:[ first ]
      ~outer_status:(Failed "PRIVATE_RETRY_EXCEPTION") ~total_elapsed_s:0.5
      ~cleanup_status:Cleanup_not_required ~continuation ~outer_event_trace
  in
  let serialized =
    ok
      (Agent_execution.make_incomplete_execution_error
         ~message:"PRIVATE_OUTER_EXCEPTION" ~execution ())
    |> Agent_execution.error_to_yojson |> Yojson.Safe.to_string
  in
  Alcotest.(check bool)
    "failed continuation projected without result" true
    (contains serialized "\"invocation\":\"started\"");
  check_absent "outer status diagnostic redacted" serialized
    "PRIVATE_RETRY_EXCEPTION";
  check_absent "outer error diagnostic redacted" serialized
    "PRIVATE_OUTER_EXCEPTION"

let test_incomplete_execution_rejects_contradictions () =
  let first = attempt ~schema_error:"schema rejected" () in
  let trace ?(retry_kind = Workflow_event.Fresh_retry)
      ?(attempt_kind = Workflow_event.Fresh_attempt) ?finish ?third_attempt
      terminal =
    let finish =
      match finish with
      | None -> []
      | Some outcome ->
          [
            event ~seq:5L ~attempt:2 ~elapsed_s:0.45
              (Attempt_finished outcome);
          ]
    in
    let third =
      match third_attempt with
      | None -> []
      | Some kind ->
          [
            event ~seq:6L ~attempt:3 ~elapsed_s:0.47 (Attempt_started kind);
          ]
    in
    let terminal_seq = if third = [] then 6L else 7L in
    let terminal_attempt = if third = [] then 2 else 3 in
    ok
      (Workflow_event.make_trace
         ([
            event ~seq:1L ~attempt:1 ~elapsed_s:0.05
              (Attempt_started Initial_attempt);
            event ~seq:2L ~attempt:1 ~elapsed_s:0.3
              (Attempt_finished Attempt_succeeded);
            event ~seq:3L ~attempt:1 ~elapsed_s:0.31
              (Retry_transition { kind = retry_kind; reason = Schema_validation });
            event ~seq:4L ~attempt:2 ~elapsed_s:0.35
              (Attempt_started attempt_kind);
          ]
         @ finish @ third
         @ [ event ~seq:terminal_seq ~attempt:terminal_attempt ~elapsed_s:0.5
               (Terminal terminal) ]))
  in
  let make continuation outer_event_trace =
    Agent_execution.make_incomplete_execution ~completed_attempts:[ first ]
      ~outer_status:Timed_out ~total_elapsed_s:0.5
      ~cleanup_status:Cleanup_not_required ~continuation ~outer_event_trace ()
  in
  let fresh_started =
    incomplete_continuation ~number:2 ~kind:Fresh_attempt
      ~invocation:Agent_execution.Invocation_started
  in
  expect_error
    (make
       (incomplete_continuation ~number:3 ~kind:Fresh_attempt
          ~invocation:Invocation_started)
       (trace Timed_out));
  expect_error
    (make
       (incomplete_continuation ~number:2 ~kind:Resumed_attempt
          ~invocation:Invocation_started)
       (trace Timed_out));
  expect_error
    (Agent_execution.make_incomplete_continuation ~number:2
       ~kind:Initial_attempt ~invocation:Invocation_started ());
  expect_error
    (make fresh_started (trace ~finish:Attempt_succeeded Timed_out));
  expect_error
    (make fresh_started
       (trace ~third_attempt:Fresh_attempt Timed_out));
  let may_have_started =
    incomplete_continuation ~number:2 ~kind:Fresh_attempt
      ~invocation:Agent_execution.Invocation_may_have_started
  in
  expect_error (make may_have_started (trace Timed_out));
  expect_error (make fresh_started (trace Cancelled));
  let no_retry_context_trace =
    ok
      (Workflow_event.make_trace
         [
           event ~seq:1L ~attempt:1 ~elapsed_s:0.3
             (Attempt_finished Attempt_succeeded);
           event ~seq:2L ~attempt:1 ~elapsed_s:0.5 (Terminal Timed_out);
         ])
  in
  expect_error
    (Agent_execution.make_incomplete_execution
       ~completed_attempts:[ attempt () ] ~outer_status:Timed_out
       ~total_elapsed_s:0.5 ~cleanup_status:Cleanup_not_required
       ~continuation:may_have_started ~outer_event_trace:no_retry_context_trace
       ());
  let omitted_start_trace =
    ok
      (Workflow_event.make_trace ~omitted_count:1L
         [
           event ~seq:1L ~attempt:1 ~elapsed_s:0.3
             (Attempt_finished Attempt_succeeded);
           event ~seq:2L ~attempt:1 ~elapsed_s:0.31
             (Retry_transition
                { kind = Fresh_retry; reason = Schema_validation });
           event ~seq:4L ~attempt:2 ~elapsed_s:0.5 (Terminal Timed_out);
         ])
  in
  ignore (ok (make may_have_started omitted_start_trace));
  let before_retry_trace =
    ok
      (Workflow_event.make_trace
         [
           event ~seq:1L ~attempt:1 ~elapsed_s:0.05
             (Attempt_started Initial_attempt);
           event ~seq:2L ~attempt:1 ~elapsed_s:0.3
             (Attempt_finished Attempt_succeeded);
           event ~seq:3L ~attempt:1 ~elapsed_s:0.5 (Terminal Timed_out);
         ])
  in
  let before_retry =
    ok
      (Agent_execution.make_incomplete_execution
         ~completed_attempts:[ first ] ~outer_status:Timed_out
         ~total_elapsed_s:0.5 ~cleanup_status:Cleanup_not_required
         ~outer_event_trace:before_retry_trace ())
  in
  Alcotest.(check bool)
    "interruption before retry has no synthetic continuation" true
    (Option.is_none (Agent_execution.incomplete_continuation before_retry));
  expect_error
    (Agent_execution.make_incomplete_execution ~completed_attempts:[ first ]
       ~outer_status:Success ~total_elapsed_s:0.5
       ~cleanup_status:Cleanup_not_required ~continuation:fresh_started
       ~outer_event_trace:(trace Succeeded) ())

let test_schema_retry_failure_shapes () =
  let first = attempt ~schema_error:"first schema rejection" () in
  let check ?(kind = Workflow_event.Fresh_attempt) status =
    let second = attempt ~number:2 ~kind ~status () in
    let response = response ~attempts:[ first; second ] () in
    ignore
      (ok
         (Agent_execution.make_execution_error ~kind:Schema_retry_failed
            ~message:"corrective attempt failed" ~response ()))
  in
  check (Failed "backend failure");
  check Timed_out;
  check Cancelled;
  check ~kind:Workflow_event.Resumed_attempt (Failed "resume rejected");
  let schema_again =
    attempt ~number:2 ~kind:Fresh_attempt
      ~schema_error:"second schema rejection" ()
  in
  let schema_response =
    response ~attempts:[ first; schema_again ]
      ~status:(Failed "schema retry exhausted") ()
  in
  ignore
    (ok
       (Agent_execution.make_execution_error ~kind:Schema_retry_failed
          ~message:"schema retry exhausted" ~response:schema_response ()));
  let no_initial_schema =
    response
      ~attempts:
        [
          attempt (); attempt ~number:2 ~kind:Fresh_attempt ~status:Timed_out ();
        ]
      ()
  in
  expect_error
    (Agent_execution.make_execution_error ~kind:Schema_retry_failed
       ~message:"missing first schema rejection" ~response:no_initial_schema ());
  let three_attempts =
    response
      ~attempts:
        [
          first;
          attempt ~number:2 ~kind:Fresh_attempt ~schema_error:"again" ();
          attempt ~number:3 ~kind:Fresh_attempt ~status:Cancelled ();
        ]
      ~total_elapsed_s:1.0 ()
  in
  expect_error
    (Agent_execution.make_execution_error ~kind:Schema_retry_failed
       ~message:"more than one corrective attempt" ~response:three_attempts ())

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
      | Post_execution_dispatch_failed _ ->
          Alcotest.fail
            "legacy bool=false is execution, not post-dispatch failure"
      | No_completed_attempt _ ->
          Alcotest.fail
            "legacy bool=false returned a completed result, not an indeterminate \
             invocation"
      | Incomplete_execution _ ->
          Alcotest.fail
            "legacy bool=false returned a completed result, not partial execution"
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
          Alcotest.test_case "pre-serialization JSON byte accounting" `Quick
            test_json_size_preflight_is_iterative_and_escape_aware;
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
          Alcotest.test_case "cumulative usage snapshots" `Quick
            test_usage_events_are_cumulative_snapshots;
          Alcotest.test_case "usage dimension lower bounds" `Quick
            test_usage_observations_retain_dimension_lower_bounds;
          Alcotest.test_case "usage lower bounds across a sequence gap" `Quick
            test_usage_lower_bounds_survive_sequence_gap;
          Alcotest.test_case "usage lower bounds across truncation" `Quick
            test_usage_lower_bounds_survive_truncation;
          Alcotest.test_case "usage lower bounds across unlocated omissions"
            `Quick test_usage_lower_bounds_survive_unlocated_omissions;
          Alcotest.test_case "retry transition without retained start" `Quick
            test_retry_transition_matches_response_without_retained_start;
          Alcotest.test_case "one-sided attempt timing envelope" `Quick
            test_attempt_timing_uses_one_sided_envelope;
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
          Alcotest.test_case "host-neutral process exit codes" `Quick
            test_process_exit_codes;
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
          Alcotest.test_case "post-execution dispatch status preservation"
            `Quick test_post_execution_dispatch_failure_preserves_every_status;
          Alcotest.test_case "successful execution plus cleanup failure" `Quick
            test_post_execution_cleanup_failure_keeps_success_and_outer_trace;
          Alcotest.test_case "no completed attempt error shapes" `Quick
            test_no_completed_attempt_error_shapes;
          Alcotest.test_case "deadline during fresh retry" `Quick
            test_incomplete_fresh_retry_deadline;
          Alcotest.test_case "reject spurious pre-transition continuation"
            `Quick test_spurious_continuation_before_transition_rejected;
          Alcotest.test_case "started continuation requires evidence" `Quick
            test_started_continuation_requires_explicit_evidence;
          Alcotest.test_case "uncertain continuation evidence requirements"
            `Quick
            test_uncertain_continuation_requires_omission_and_n_plus_one_terminal;
          Alcotest.test_case "omitted continuation start at N+1" `Quick
            test_uncertain_continuation_accepts_omitted_start_at_n_plus_one;
          Alcotest.test_case "reject unrelated earlier continuation gap" `Quick
            test_uncertain_continuation_rejects_unrelated_earlier_gap;
          Alcotest.test_case "reject unrelated earlier continuation truncation"
            `Quick
            test_uncertain_continuation_rejects_unrelated_earlier_truncation;
          Alcotest.test_case "accept continuation boundary gap" `Quick
            test_uncertain_continuation_accepts_boundary_spanning_gap;
          Alcotest.test_case "accept post-transition truncation" `Quick
            test_uncertain_continuation_accepts_post_transition_truncation;
          Alcotest.test_case "incomplete completed telemetry mismatches" `Quick
            test_incomplete_completed_telemetry_mismatches;
          Alcotest.test_case "cancellation during resumed retry" `Quick
            test_incomplete_resumed_retry_cancellation;
          Alcotest.test_case "retry exception after finish event" `Quick
            test_incomplete_retry_exception_after_finish_event;
          Alcotest.test_case "incomplete execution contradictions" `Quick
            test_incomplete_execution_rejects_contradictions;
          Alcotest.test_case "schema retry terminal failures" `Quick
            test_schema_retry_failure_shapes;
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
