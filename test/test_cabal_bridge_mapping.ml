open Cabal
open Cabal_workflow_runner

let fail = Alcotest.fail

let descriptor : Backend_registry.descriptor =
  {
    id = "mapping-fixture";
    display_name = "Mapping fixture";
    binary_name = "true";
    baseline_version = "1.0.0";
    capabilities =
      {
        structured_output = true;
        streaming_output = true;
        session_resume = false;
        mcp_support = Backend_registry.Mcp_none;
        read_only_support = false;
        project_config_surface = Backend_registry.Config_none;
        precedence_confidence = Backend_registry.High;
        generated_lsp_config = false;
        file_reading = false;
        media_support = {media_types = []; evidence = None};
        web_support = {maximum = Backend_types.Web_disabled; evidence = None};
        native_json_schema_output = false;
        native_json_schema_output_evidence = None;
      };
  }

let result ?(status = Backend_types.Success) ?(text = {|{"ok":true}|})
    ?report ?session_id ?cost () =
  Backend_types.make_task_result ~status ~agent_text:text ?report ?session_id
    ?cost ()

let delivery : Backend_types.attempt_delivery =
  {
    attachment_references = [];
    attachment_delivery = Backend_types.Upload_attachments;
    web_access_policy = Backend_types.Web_disabled;
  }

let attempt ?(number = 1) ?(kind = Backend_types.Initial_attempt)
    ?(schema_validation_error = None) ?(attempt_elapsed = 0.2) result =
  Backend_types.{number; kind; result; attempt_elapsed; schema_validation_error; delivery}

let execution ?attempts ?(total_elapsed = 1.0)
    ?(cleanup_status = Backend_types.Cleanup_not_required) final_result =
  Backend_types.make_task_execution ~final_result ?attempts ~total_elapsed
    ~cleanup_status ()

let event ~seq ~attempt ~timestamp payload : Task_event.t =
  {seq; attempt; timestamp; payload}

let trace ?(omitted_events = 0) events : Backend_completer.event_trace =
  {events; omitted_events}

let task_started =
  event ~seq:1 ~attempt:1 ~timestamp:0.0 Task_event.Task_started

let terminal ?(seq = 2) ?(attempt = 1) ?(timestamp = 0.1)
    ?(status = Task_event.Failed "sanitized") () =
  event ~seq ~attempt ~timestamp (Task_event.Terminal status)

let rich_error cause event_trace : Backend_completer.rich_completion_error =
  {cause; event_trace}

let rich_response execution event_trace : Backend_completer.rich_completion_response =
  {text = execution.Backend_types.final_result.agent_text; execution; event_trace}

let check_trace_some_and_attempt label expected_attempt = function
  | Some mapped ->
      Alcotest.(check bool) label true
        (List.for_all
           (fun item -> Workflow_event.attempt item = expected_attempt)
           (Workflow_event.events mapped))
  | None -> fail (label ^ " was dropped")

let check_dispatch_error expected_kind expected_message error =
  match Agent_execution.error_view error with
  | Agent_execution.Dispatch_failure {kind; message; event_trace} ->
      Alcotest.(check bool) "dispatch kind" true (kind = expected_kind);
      Alcotest.(check string) "sanitized message" expected_message message;
      check_trace_some_and_attempt "pre-invocation trace" 0 event_trace;
      (match event_trace with
      | Some trace -> (
          match Workflow_event.events trace with
          | [started; terminal] ->
              Alcotest.(check int64) "start sequence preserved" 1L
                (Workflow_event.seq started);
              Alcotest.(check (float 0.0)) "start timestamp preserved" 0.0
                (Workflow_event.elapsed_s started);
              Alcotest.(check bool) "start payload preserved" true
                (Workflow_event.payload started = Workflow_event.Task_started);
              Alcotest.(check int64) "terminal sequence preserved" 2L
                (Workflow_event.seq terminal);
              Alcotest.(check (float 0.0)) "terminal timestamp preserved" 0.1
                (Workflow_event.elapsed_s terminal);
              Alcotest.(check bool) "terminal payload safely preserved" true
                (Workflow_event.payload terminal
                = Workflow_event.Terminal Workflow_event.Failed)
          | events ->
              Alcotest.failf "expected two pre-dispatch events, got %d"
                (List.length events))
      | None -> fail "pre-invocation trace was dropped")
  | _ -> fail "expected dispatch failure"

let test_dispatch_failure_table () =
  let predispatch_trace = trace [task_started; terminal ()] in
  let preflight =
    Task_preflight.Input (Task_preflight.Too_many_attachments {maximum = 0; actual = 1})
  in
  let cases =
    [
      ( Runtime_dispatch.Invalid_timeout,
        Agent_execution.Invalid_request );
      ( Runtime_dispatch.Backend_not_registered,
        Agent_execution.Backend_unavailable );
      ( Runtime_dispatch.Runtime_registration_untrusted,
        Agent_execution.Capability_mismatch );
      ( Runtime_dispatch.Runtime_entry_invalid Runtime_entry.Runtime_id_mismatch,
        Agent_execution.Capability_mismatch );
      ( Runtime_dispatch.Expected_entry_mismatch,
        Agent_execution.Capability_mismatch );
      ( Runtime_dispatch.Backend_quarantined
          Runtime_entry.Incomplete_mcp_isolation,
        Agent_execution.Capability_mismatch );
      ( Runtime_dispatch.Preflight_failed preflight,
        Agent_execution.Preflight_failed );
      ( Runtime_dispatch.Backend_version_unsupported,
        Agent_execution.Capability_mismatch );
      ( Runtime_dispatch.Version_check_failed,
        Agent_execution.Internal_dispatch_failure );
      ( Runtime_dispatch.Backend_unavailable,
        Agent_execution.Backend_unavailable );
      ( Runtime_dispatch.Availability_check_failed,
        Agent_execution.Internal_dispatch_failure );
      ( Runtime_dispatch.Prepared_already_consumed,
        Agent_execution.Internal_dispatch_failure );
    ]
  in
  List.iter
    (fun (cause, expected_kind) ->
      let source =
        rich_error (Runtime_dispatch.Dispatch_failure cause) predispatch_trace
      in
      let mapped = Cwr_cabal_internal.map_error ~descriptor source in
      check_dispatch_error expected_kind
        (Backend_completer.render_rich_completion_error source)
        mapped)
    cases;
  List.iter
    (fun cause ->
      let source =
        rich_error (Runtime_dispatch.Dispatch_failure cause)
          (trace [task_started; terminal ()])
      in
      let mapped = Cwr_cabal_internal.map_error ~descriptor source in
      match Agent_execution.error_view mapped with
      | Agent_execution.No_completed_attempt
          {
            status = Agent_execution.Failed _;
            invocation_may_have_started = true;
            message;
            event_trace = Some mapped_trace;
          } ->
          Alcotest.(check string) "execution-boundary diagnostic"
            (Backend_completer.render_rich_completion_error source)
            message;
          (match List.rev (Workflow_event.events mapped_trace) with
          | last :: _ ->
              Alcotest.(check int) "terminal attempt retained" 1
                (Workflow_event.attempt last)
          | [] -> fail "execution-boundary trace was empty")
      | _ -> fail "execution-boundary cause was misclassified")
    [
      Runtime_dispatch.Backend_execution_failed;
      Runtime_dispatch.Schema_enforcement_failed "private schema details";
    ]

let one_attempt_trace ?(outcome = Task_event.Attempt_succeeded)
    ?(terminal_status = Task_event.Succeeded) () =
  trace
    [
      task_started;
      event ~seq:2 ~attempt:1 ~timestamp:0.1
        (Task_event.Attempt_started Backend_types.Initial_attempt);
      event ~seq:3 ~attempt:1 ~timestamp:0.3
        (Task_event.Attempt_finished outcome);
      terminal ~seq:4 ~attempt:1 ~timestamp:0.4 ~status:terminal_status ();
    ]

let test_success_and_execution_failure_table () =
  let represented_cases =
    [
      ( "success",
        Backend_types.Success,
        Agent_execution.Success,
        Task_event.Attempt_succeeded,
        Task_event.Succeeded );
      ( "failed",
        Backend_types.Failed "private",
        Agent_execution.Failed "backend execution failed",
        Task_event.Attempt_failed,
        Task_event.Failed "private" );
      ( "timeout",
        Backend_types.Timeout,
        Agent_execution.Timed_out,
        Task_event.Attempt_timed_out,
        Task_event.Timed_out );
      ( "cancel",
        Backend_types.Cancelled,
        Agent_execution.Cancelled,
        Task_event.Attempt_cancelled,
        Task_event.Cancelled );
    ]
  in
  List.iter
    (fun (label, status, expected_status, outcome, terminal_status) ->
      let final_result = result ~status () in
      let completed = execution ~attempts:[attempt final_result] final_result in
      match
        Cwr_cabal_internal.map_ok ~descriptor
          (rich_response completed
             (one_attempt_trace ~outcome ~terminal_status ()))
      with
      | Ok response ->
          Alcotest.(check bool) (label ^ " represented status") true
            (Agent_execution.final_status response = expected_status)
      | Error _ -> fail (label ^ " represented outcome failed mapping"))
    represented_cases;
  let failed_result = result ~status:(Backend_types.Failed "private") () in
  let native_execution =
    execution ~attempts:[attempt failed_result] failed_result
  in
  let native_source =
    rich_error
      (Runtime_dispatch.Execution_failure
         (Backend_types.Native_backend_failure_with_schema
            {execution = native_execution; message = "private"}))
      (one_attempt_trace ~outcome:Task_event.Attempt_failed
         ~terminal_status:(Task_event.Failed "private") ())
  in
  (match
     Agent_execution.error_view
       (Cwr_cabal_internal.map_error ~descriptor native_source)
   with
  | Agent_execution.Execution_failure
      {kind = Agent_execution.Native_backend_failure_with_schema; _} ->
      ()
  | _ -> fail "native schema failure was misclassified");
  let first_result = result ~text:"not-json" () in
  let first =
    attempt ~schema_validation_error:(Some "first invalid") first_result
  in
  let retry_cases =
    [
      ( "schema",
        result ~text:"still-not-json" (),
        Backend_types.Fresh_attempt,
        Some "second invalid",
        Backend_types.Schema_validation_failure "second invalid",
        Task_event.Attempt_succeeded,
        Task_event.Failed "schema" );
      ( "transport failed",
        result ~status:(Backend_types.Failed "private") (),
        Backend_types.Fresh_attempt,
        None,
        Backend_types.Transport_failure (Backend_types.Failed "private"),
        Task_event.Attempt_failed,
        Task_event.Failed "transport" );
      ( "transport timeout",
        result ~status:Backend_types.Timeout (),
        Backend_types.Fresh_attempt,
        None,
        Backend_types.Transport_failure Backend_types.Timeout,
        Task_event.Attempt_timed_out,
        Task_event.Timed_out );
      ( "transport cancel",
        result ~status:Backend_types.Cancelled (),
        Backend_types.Fresh_attempt,
        None,
        Backend_types.Transport_failure Backend_types.Cancelled,
        Task_event.Attempt_cancelled,
        Task_event.Cancelled );
      ( "resume failed",
        result ~status:(Backend_types.Failed "private") (),
        Backend_types.Resumed_attempt,
        None,
        Backend_types.Resume_failure (Backend_types.Failed "private"),
        Task_event.Attempt_failed,
        Task_event.Failed "resume" );
      ( "resume timeout",
        result ~status:Backend_types.Timeout (),
        Backend_types.Resumed_attempt,
        None,
        Backend_types.Resume_failure Backend_types.Timeout,
        Task_event.Attempt_timed_out,
        Task_event.Timed_out );
      ( "resume cancel",
        result ~status:Backend_types.Cancelled (),
        Backend_types.Resumed_attempt,
        None,
        Backend_types.Resume_failure Backend_types.Cancelled,
        Task_event.Attempt_cancelled,
        Task_event.Cancelled );
    ]
  in
  List.iter
    (fun
      ( label,
        second_result,
        second_kind,
        schema_validation_error,
        attempt_2_failure,
        second_outcome,
        terminal_status ) ->
      let second =
        attempt ~number:2 ~kind:second_kind ~schema_validation_error second_result
      in
      let retry_execution =
        execution ~attempts:[first; second] second_result
      in
      let retry_kind =
        match second_kind with
        | Backend_types.Fresh_attempt -> Task_event.Fresh_retry
        | Backend_types.Resumed_attempt -> Task_event.Resume_retry
        | Backend_types.Initial_attempt -> fail "invalid corrective attempt kind"
      in
      let retry_trace =
        trace
          [
            task_started;
            event ~seq:2 ~attempt:1 ~timestamp:0.1
              (Task_event.Attempt_started Backend_types.Initial_attempt);
            event ~seq:3 ~attempt:1 ~timestamp:0.3
              (Task_event.Attempt_finished Task_event.Attempt_succeeded);
            event ~seq:4 ~attempt:1 ~timestamp:0.31
              (Task_event.Retry_transition
                 {kind = retry_kind; reason = "schema details"});
            event ~seq:5 ~attempt:2 ~timestamp:0.4
              (Task_event.Attempt_started second_kind);
            event ~seq:6 ~attempt:2 ~timestamp:0.6
              (Task_event.Attempt_finished second_outcome);
            terminal ~seq:7 ~attempt:2 ~timestamp:0.7
              ~status:terminal_status ();
          ]
      in
      let source =
        rich_error
          (Runtime_dispatch.Execution_failure
             (Backend_types.Schema_retry_failed
                {
                  execution = retry_execution;
                  attempt_1_validation_error = "first invalid";
                  attempt_2_failure;
                }))
          retry_trace
      in
      match
        Agent_execution.error_view
          (Cwr_cabal_internal.map_error ~descriptor source)
      with
      | Agent_execution.Execution_failure
          {kind = Agent_execution.Schema_retry_failed; response; _} ->
          Alcotest.(check int) (label ^ " attempts") 2
            (List.length (Agent_execution.attempts response))
      | _ -> fail (label ^ " schema retry was misclassified"))
    retry_cases

let test_cleanup_failure_after_success () =
  let successful_result = result () in
  let successful_attempt = attempt successful_result in
  let completed =
    execution ~attempts:[successful_attempt]
      ~cleanup_status:Backend_types.Cleanup_failed successful_result
  in
  let source =
    rich_error
      (Runtime_dispatch.Dispatch_failure_with_execution
         {
           failure =
             Runtime_dispatch.Preflight_failed
               (Task_preflight.Input Task_preflight.Attachment_cleanup_failed);
           execution = completed;
         })
      (one_attempt_trace ~terminal_status:(Task_event.Failed "cleanup") ())
  in
  match
    Agent_execution.error_view
      (Cwr_cabal_internal.map_error ~descriptor source)
  with
  | Agent_execution.Post_execution_dispatch_failed
      {cause = Agent_execution.Preflight_failed; response; outer_event_trace; _}
    ->
      Alcotest.(check bool) "nested success retained" true
        (Agent_execution.final_status response = Agent_execution.Success);
      Alcotest.(check bool) "cleanup failure retained" true
        (Agent_execution.cleanup_status response = Agent_execution.Cleanup_failed);
      Alcotest.(check int) "outer trace retained" 4
        (List.length (Workflow_event.events outer_event_trace))
  | _ -> fail "cleanup failure after success was misclassified"

let test_dispatch_failure_after_partial_progress () =
  let completed_result = result ~text:"not-json" () in
  let completed =
    attempt ~schema_validation_error:(Some "invalid") completed_result
  in
  let synthetic_failure = result ~status:(Backend_types.Failed "private") () in
  let partial = execution ~attempts:[completed] synthetic_failure in
  let event_trace =
    trace
      [
        task_started;
        event ~seq:2 ~attempt:1 ~timestamp:0.1
          (Task_event.Attempt_started Backend_types.Initial_attempt);
        event ~seq:3 ~attempt:1 ~timestamp:0.3
          (Task_event.Attempt_finished Task_event.Attempt_succeeded);
        event ~seq:4 ~attempt:1 ~timestamp:0.31
          (Task_event.Retry_transition
             {kind = Task_event.Fresh_retry; reason = "schema"});
        event ~seq:5 ~attempt:2 ~timestamp:0.4
          (Task_event.Attempt_started Backend_types.Fresh_attempt);
        terminal ~seq:6 ~attempt:2 ~timestamp:0.7
          ~status:(Task_event.Failed "private") ();
      ]
  in
  let source =
    rich_error
      (Runtime_dispatch.Dispatch_failure_with_execution
         {failure = Runtime_dispatch.Backend_execution_failed; execution = partial})
      event_trace
  in
  match
    Agent_execution.error_view
      (Cwr_cabal_internal.map_error ~descriptor source)
  with
  | Agent_execution.Incomplete_execution {execution; _} ->
      Alcotest.(check int) "completed prefix" 1
        (List.length (Agent_execution.incomplete_completed_attempts execution));
      (match Agent_execution.incomplete_continuation execution with
      | Some continuation ->
          Alcotest.(check int) "fresh continuation" 2
            (Agent_execution.continuation_number continuation)
      | None -> fail "partial retry invocation was discarded")
  | _ -> fail "post-progress dispatch failure was misclassified"

let test_incoherent_retry_success_fails_with_trace () =
  let first_result = result ~text:"not-json" () in
  let first =
    attempt ~schema_validation_error:(Some "first invalid") first_result
  in
  List.iter
    (fun (label, second_kind, attempt_2_failure, retry_kind) ->
      let second_result = result () in
      let second = attempt ~number:2 ~kind:second_kind second_result in
      let retry_execution =
        execution ~attempts:[first; second] second_result
      in
      let source_trace =
        trace
          [
            task_started;
            event ~seq:2 ~attempt:1 ~timestamp:0.1
              (Task_event.Attempt_started Backend_types.Initial_attempt);
            event ~seq:3 ~attempt:1 ~timestamp:0.3
              (Task_event.Attempt_finished Task_event.Attempt_succeeded);
            event ~seq:4 ~attempt:1 ~timestamp:0.31
              (Task_event.Retry_transition {kind = retry_kind; reason = "schema"});
            event ~seq:5 ~attempt:2 ~timestamp:0.4
              (Task_event.Attempt_started second_kind);
            event ~seq:6 ~attempt:2 ~timestamp:0.6
              (Task_event.Attempt_finished Task_event.Attempt_succeeded);
            terminal ~seq:7 ~attempt:2 ~timestamp:0.7
              ~status:(Task_event.Failed "incoherent") ();
          ]
      in
      let source =
        rich_error
          (Runtime_dispatch.Execution_failure
             (Backend_types.Schema_retry_failed
                {
                  execution = retry_execution;
                  attempt_1_validation_error = "first invalid";
                  attempt_2_failure;
                }))
          source_trace
      in
      match
        Agent_execution.error_view
          (Cwr_cabal_internal.map_error ~descriptor source)
      with
      | Agent_execution.Telemetry_mapping_failure {event_trace; _} ->
          Alcotest.(check int) (label ^ " trace") 7
            (List.length (Workflow_event.events event_trace))
      | _ -> fail (label ^ " incoherent success did not retain its safe trace"))
    [
      ( "transport success",
        Backend_types.Fresh_attempt,
        Backend_types.Transport_failure Backend_types.Success,
        Task_event.Fresh_retry );
      ( "resume success",
        Backend_types.Resumed_attempt,
        Backend_types.Resume_failure Backend_types.Success,
        Task_event.Resume_retry );
    ]

let test_synthetic_timeout_and_cancel_after_progress () =
  let completed_result = result ~text:"not-json" () in
  let completed =
    attempt ~schema_validation_error:(Some "invalid") completed_result
  in
  List.iter
    (fun (label, status, terminal_status) ->
      let synthetic = result ~status () in
      let partial = execution ~attempts:[completed] synthetic in
      let event_trace =
        trace
          [
            task_started;
            event ~seq:2 ~attempt:1 ~timestamp:0.1
              (Task_event.Attempt_started Backend_types.Initial_attempt);
            event ~seq:3 ~attempt:1 ~timestamp:0.3
              (Task_event.Attempt_finished Task_event.Attempt_succeeded);
            event ~seq:4 ~attempt:1 ~timestamp:0.31
              (Task_event.Retry_transition
                 {kind = Task_event.Fresh_retry; reason = "schema"});
            event ~seq:5 ~attempt:2 ~timestamp:0.4
              (Task_event.Attempt_started Backend_types.Fresh_attempt);
            terminal ~seq:6 ~attempt:2 ~timestamp:0.7
              ~status:terminal_status ();
          ]
      in
      match
        Cwr_cabal_internal.map_ok ~descriptor
          (rich_response partial event_trace)
      with
      | Error error -> (
          match Agent_execution.error_view error with
          | Agent_execution.Incomplete_execution {execution; _} ->
              Alcotest.(check int) (label ^ " completed attempts") 1
                (List.length
                   (Agent_execution.incomplete_completed_attempts execution))
          | _ -> fail (label ^ " partial progress was misclassified"))
      | Ok _ -> fail (label ^ " synthetic result became a response"))
    [
      ("timeout", Backend_types.Timeout, Task_event.Timed_out);
      ("cancel", Backend_types.Cancelled, Task_event.Cancelled);
    ]

let test_zero_progress_terminal_outcomes () =
  List.iter
    (fun (label, status, expected_status, terminal_status) ->
      let final_result = result ~status () in
      let source =
        rich_response (execution ~attempts:[] final_result)
          (trace
             [
               task_started;
               terminal ~status:terminal_status ();
             ])
      in
      match Cwr_cabal_internal.map_ok ~descriptor source with
      | Error error -> (
          match Agent_execution.error_view error with
          | Agent_execution.No_completed_attempt
              {
                status = mapped_status;
                invocation_may_have_started = true;
                event_trace = Some mapped_trace;
                _;
              } ->
              Alcotest.(check bool) (label ^ " status") true
                (mapped_status = expected_status);
              Alcotest.(check int) (label ^ " trace") 2
                (List.length (Workflow_event.events mapped_trace))
          | _ -> fail (label ^ " zero-progress outcome was misclassified"))
      | Ok _ -> fail (label ^ " zero-progress outcome became a response"))
    [
      ( "failed",
        Backend_types.Failed "private",
        Agent_execution.Failed "backend execution failed",
        Task_event.Failed "private" );
      ( "timeout",
        Backend_types.Timeout,
        Agent_execution.Timed_out,
        Task_event.Timed_out );
      ( "cancel",
        Backend_types.Cancelled,
        Agent_execution.Cancelled,
        Task_event.Cancelled );
    ];
  let impossible_success = result () in
  match
    Cwr_cabal_internal.map_ok ~descriptor
      (rich_response (execution ~attempts:[] impossible_success)
         (trace [task_started; terminal ~status:Task_event.Succeeded ()]))
  with
  | Error error -> (
      match Agent_execution.error_view error with
      | Agent_execution.Telemetry_mapping_failure {event_trace; _} ->
          Alcotest.(check int) "impossible success trace retained" 2
            (List.length (Workflow_event.events event_trace))
      | _ -> fail "zero-attempt success was misclassified")
  | Ok _ -> fail "zero-attempt success unexpectedly mapped"

let expect_internal_mapping_error label = function
  | Error error -> (
      match Agent_execution.error_view error with
      | Agent_execution.Dispatch_failure
          {kind = Agent_execution.Internal_dispatch_failure; _} ->
          ()
      | Agent_execution.Telemetry_mapping_failure _ -> ()
      | _ -> fail (label ^ " was not an internal mapping failure"))
  | Ok _ -> fail (label ^ " unexpectedly mapped")

let test_negative_and_invalid_conversions () =
  let negative_cost : Backend_types.cost =
    {
      tokens_input = Some (-1);
      tokens_output = None;
      cost_usd = Some (-1.0);
      cache_creation_input_tokens = None;
      cache_read_input_tokens = None;
    }
  in
  let negative_result = result ~cost:negative_cost () in
  expect_internal_mapping_error "negative cost"
    (Cwr_cabal_internal.map_ok ~descriptor
       (rich_response
          (execution ~attempts:[attempt negative_result] negative_result)
          (one_attempt_trace ())));
  let bad_number_result = result () in
  expect_internal_mapping_error "attempt number"
    (Cwr_cabal_internal.map_ok ~descriptor
       (rich_response
          (execution ~attempts:[attempt ~number:0 bad_number_result] bad_number_result)
          (one_attempt_trace ())));
  expect_internal_mapping_error "attempt elapsed"
    (Cwr_cabal_internal.map_ok ~descriptor
       (rich_response
          (execution
             ~attempts:[attempt ~attempt_elapsed:(-0.1) bad_number_result]
             bad_number_result)
          (one_attempt_trace ())));
  expect_internal_mapping_error "total elapsed"
    (Cwr_cabal_internal.map_ok ~descriptor
       (rich_response
          (execution ~attempts:[attempt bad_number_result] ~total_elapsed:(-0.1)
             bad_number_result)
          (one_attempt_trace ())));
  expect_internal_mapping_error "trace sequence"
    (Cwr_cabal_internal.map_ok ~descriptor
       (rich_response
          (execution ~attempts:[attempt bad_number_result] bad_number_result)
          (trace
             [
               event ~seq:0 ~attempt:1 ~timestamp:0.0 Task_event.Task_started;
               terminal ();
             ])));
  expect_internal_mapping_error "trace omissions"
    (Cwr_cabal_internal.map_ok ~descriptor
       (rich_response
          (execution ~attempts:[attempt bad_number_result] bad_number_result)
          (one_attempt_trace () |> fun value -> {value with omitted_events = -1})));
  List.iter
    (fun (label, session_id, expected) ->
      let unsafe_session_result = result ~session_id () in
      match
        Cwr_cabal_internal.map_ok ~descriptor
          (rich_response
             (execution ~attempts:[attempt unsafe_session_result]
                unsafe_session_result)
             (one_attempt_trace ()))
      with
      | Ok response ->
          Alcotest.(check (option string)) label expected
            (Agent_execution.final_session_id response)
      | Error _ -> fail (label ^ " did not map conservatively"))
    [
      ("unsafe session redacted", "private/session", Some "redacted-session");
      ("noncanonical session redacted", " private ", Some "redacted-session");
      ("blank session dropped", "   ", None);
    ]

let test_constructor_failure_retains_valid_source_trace () =
  let successful_result = result () in
  let source_trace =
    one_attempt_trace ~terminal_status:(Task_event.Failed "mismatch") ()
  in
  let source =
    rich_response
      (execution ~attempts:[attempt successful_result] successful_result)
      source_trace
  in
  match Cwr_cabal_internal.map_ok ~descriptor source with
  | Error error -> (
      match Agent_execution.error_view error with
      | Agent_execution.Telemetry_mapping_failure
          {event_trace = retained; _} ->
          let expected =
            match Cwr_cabal_internal.map_trace source_trace with
            | Ok trace -> trace
            | Error reason -> fail reason
          in
          Alcotest.(check bool) "known-valid source trace retained exactly" true
            (Workflow_event.trace_to_yojson expected
            = Workflow_event.trace_to_yojson retained)
      | _ -> fail "constructor failure discarded or misclassified its trace")
  | Ok _ -> fail "mismatched response unexpectedly passed construction"

let test_error_constructor_fallback_retains_trace () =
  let source_trace =
    trace
      [
        task_started;
        terminal ~attempt:1 ~status:Task_event.Timed_out ();
      ]
  in
  let source =
    rich_error
      (Runtime_dispatch.Dispatch_failure
         Runtime_dispatch.Backend_execution_failed)
      source_trace
  in
  match
    Agent_execution.error_view
      (Cwr_cabal_internal.map_error ~descriptor source)
  with
  | Agent_execution.Telemetry_mapping_failure {event_trace; _} ->
      let expected =
        match Cwr_cabal_internal.map_trace source_trace with
        | Ok trace -> trace
        | Error reason -> fail reason
      in
      Alcotest.(check bool) "fallback retained exact mapped trace" true
        (Workflow_event.trace_to_yojson expected
        = Workflow_event.trace_to_yojson event_trace)
  | _ -> fail "error-constructor fallback discarded its valid source trace"

let test_event_envelopes_are_not_reassociated () =
  let usage =
    Backend_types.
      {
        tokens_input = Some 3;
        tokens_output = None;
        cost_usd = Some 0.25;
        cache_creation_input_tokens = None;
        cache_read_input_tokens = None;
      }
  in
  let sources =
    [
      event ~seq:40 ~attempt:1 ~timestamp:1.0 Task_event.Task_started;
      event ~seq:41 ~attempt:1 ~timestamp:1.1
        (Task_event.Attempt_started Backend_types.Initial_attempt);
      event ~seq:42 ~attempt:1 ~timestamp:1.2
        (Task_event.Attempt_finished Task_event.Attempt_succeeded);
      event ~seq:43 ~attempt:1 ~timestamp:1.25
        (Task_event.Session_id "session-3");
      event ~seq:44 ~attempt:1 ~timestamp:1.5 (Task_event.Token_usage usage);
      event ~seq:45 ~attempt:1 ~timestamp:1.75
        (Task_event.Terminal Task_event.Succeeded);
    ]
  in
  let mapped =
    match Cwr_cabal_internal.map_trace (trace sources) with
    | Ok trace -> Workflow_event.events trace
    | Error error -> fail error
  in
  Alcotest.(check int) "event count" (List.length sources) (List.length mapped);
  List.iter2
    (fun source mapped ->
      let expected_attempt =
        match source.Task_event.payload with
        | Task_event.Task_started -> 0
        | _ -> source.attempt
      in
      Alcotest.(check int64) "sequence" (Int64.of_int source.seq)
        (Workflow_event.seq mapped);
      Alcotest.(check int) "attempt" expected_attempt
        (Workflow_event.attempt mapped);
      Alcotest.(check (float 0.0)) "elapsed" source.timestamp
        (Workflow_event.elapsed_s mapped))
    sources mapped;
  let payload_at expected_seq =
    List.find_map
      (fun item ->
        if Workflow_event.seq item = expected_seq then
          Some (Workflow_event.payload item)
        else None)
      mapped
  in
  Alcotest.(check bool) "session stayed on its source envelope" true
    (payload_at 43L = Some (Workflow_event.Session_id "session-3"));
  Alcotest.(check bool) "usage stayed on its source envelope" true
    (match payload_at 44L with
    | Some (Workflow_event.Usage_observed _) -> true
    | _ -> false);
  let predispatch_sources =
    [
      event ~seq:71 ~attempt:1 ~timestamp:2.0 Task_event.Task_started;
      event ~seq:72 ~attempt:1 ~timestamp:2.1 Task_event.Preflight_started;
      event ~seq:73 ~attempt:1 ~timestamp:2.2 Task_event.Preflight_completed;
      event ~seq:74 ~attempt:1 ~timestamp:2.3
        (Task_event.Terminal (Task_event.Failed "private"));
    ]
  in
  let predispatch_mapped =
    match
      Cwr_cabal_internal.map_trace ~no_invocation:true
        (trace predispatch_sources)
    with
    | Ok trace -> Workflow_event.events trace
    | Error error -> fail error
  in
  List.iter2
    (fun source mapped ->
      Alcotest.(check int64) "predispatch sequence"
        (Int64.of_int source.Task_event.seq)
        (Workflow_event.seq mapped);
      Alcotest.(check int) "predispatch attempt" 0
        (Workflow_event.attempt mapped);
      Alcotest.(check (float 0.0)) "predispatch elapsed" source.timestamp
        (Workflow_event.elapsed_s mapped))
    predispatch_sources predispatch_mapped;
  Alcotest.(check bool) "predispatch payload order" true
    (List.map Workflow_event.payload predispatch_mapped
    = [
        Workflow_event.Task_started;
        Workflow_event.Preflight_started;
        Workflow_event.Preflight_completed;
        Workflow_event.Terminal Workflow_event.Failed;
      ])

let () =
  Alcotest.run "CWR Cabal mapping tables"
    [
      ( "outcomes",
        [
          Alcotest.test_case "all plain dispatch causes" `Quick
            test_dispatch_failure_table;
          Alcotest.test_case "success and execution failures" `Quick
            test_success_and_execution_failure_table;
          Alcotest.test_case "cleanup failure after success" `Quick
            test_cleanup_failure_after_success;
          Alcotest.test_case "dispatch failure after partial progress" `Quick
            test_dispatch_failure_after_partial_progress;
          Alcotest.test_case "incoherent retry success" `Quick
            test_incoherent_retry_success_fails_with_trace;
          Alcotest.test_case "synthetic timeout and cancel" `Quick
            test_synthetic_timeout_and_cancel_after_progress;
          Alcotest.test_case "zero-progress terminal outcomes" `Quick
            test_zero_progress_terminal_outcomes;
        ] );
      ( "conversion",
        [
          Alcotest.test_case "negative and invalid telemetry" `Quick
            test_negative_and_invalid_conversions;
          Alcotest.test_case "constructor failure retains source trace" `Quick
            test_constructor_failure_retains_valid_source_trace;
          Alcotest.test_case "error fallback retains source trace" `Quick
            test_error_constructor_fallback_retains_trace;
          Alcotest.test_case "event envelopes remain exact" `Quick
            test_event_envelopes_are_not_reassociated;
        ] );
    ]
