open Cabal
open Cabal_workflow_runner

let fail error = Alcotest.fail error

let ok = function Ok value -> value | Error error -> fail error

let bootstrap = ok (Cwr_cabal_internal.bootstrap_hardened ())

let execution_ok = function
  | Ok value -> value
  | Error error -> (
      match Agent_execution.error_view error with
      | Agent_execution.Dispatch_failure {message; _}
      | Agent_execution.No_completed_attempt {message; _}
      | Agent_execution.Incomplete_execution {message; _}
      | Agent_execution.Post_execution_dispatch_failed {message; _}
      | Agent_execution.Execution_failure {message; _}
      | Agent_execution.Telemetry_mapping_failure {message; _} ->
          Alcotest.fail message)

let contains value needle =
  let value_length = String.length value in
  let needle_length = String.length needle in
  let rec loop index =
    index + needle_length <= value_length
    &&
    (String.sub value index needle_length = needle || loop (index + 1))
  in
  needle_length = 0 || loop 0

let no_attachment_limits : Task_preflight.limits =
  {max_attachments = 0; max_file_size_bytes = 0; max_total_size_bytes = 0}

let media_limits : Task_preflight.limits =
  {max_attachments = 4; max_file_size_bytes = 4096; max_total_size_bytes = 8192}

let feature_evidence : Backend_types.feature_evidence =
  {
    tested_at_version = "1.0.0";
    test_method = Backend_types.E2e_test;
    evidence_url = None;
    notes = "deterministic CWR bridge fake backend";
  }

let native_evidence : Backend_types.capability_evidence =
  {
    tested_at_version = "1.0.0";
    json_schema_draft = "2020-12";
    test_method = Backend_types.E2e_test;
  }

let descriptor ?(binary_name = "true") ?(baseline_version = "1.0.0")
    ?(session_resume = false) ?(native = false)
    ?(read_only = false) ?(media_types = [])
    ?(web = Backend_types.Web_disabled) id =
  let open Backend_registry in
  {
    id;
    display_name = "CWR deterministic fake backend";
    binary_name;
    baseline_version;
    capabilities =
      {
        structured_output = true;
        streaming_output = true;
        session_resume;
        mcp_support = Mcp_none;
        read_only_support = read_only;
        project_config_surface = Config_none;
        precedence_confidence = High;
        generated_lsp_config = false;
        file_reading = false;
        media_support =
          {
            media_types;
            evidence =
              (if media_types = [] then None else Some feature_evidence);
          };
        web_support =
          {
            maximum = web;
            evidence =
              (if web = Backend_types.Web_disabled then None
               else Some feature_evidence);
          };
        native_json_schema_output = native;
        native_json_schema_output_evidence =
          (if native then Some native_evidence else None);
      };
  }

type observation = {
  calls : int ref;
  availability_calls : int ref;
  specs : Backend_types.task_spec list ref;
}

let make_backend ?(session_resume = false) ?(native = false)
    ?(emit_result_text = true)
    ?(available = fun () -> true) ~id run =
  let observation =
    {calls = ref 0; availability_calls = ref 0; specs = ref []}
  in
  let module Backend = struct
    let id = id
    let name = "CWR deterministic fake backend"
    let models = []
    let models_probe = None

    let available ~sw:_ ~env:_ =
      incr observation.availability_calls;
      available ()

    let supports_session_resume = session_resume
    let native_json_schema_output = native
    let is_resume_failure _ = false

    let check_project_config ~sw:_ ~env:_ ~project_dir:_ ~setup_result:_ =
      Agentic_backend.Config_check_unsupported "deterministic fake"

    let run_task ~sw:_ ~env ?context ?on_raw_line:_ spec =
      incr observation.calls;
      observation.specs := spec :: !(observation.specs);
      let result = run ~env ~context ~call:!(observation.calls) spec in
      Option.iter
        (fun context ->
          if
            emit_result_text && result.Backend_types.agent_text <> ""
            && not (Task_execution_context.agent_text_emitted context)
          then
            Task_execution_context.emit context
              (Task_event.Agent_text_delta result.agent_text))
        context;
      result
  end in
  ((module Backend : Agentic_backend.S), observation)

let result ?(status = Backend_types.Success) ?(text = {|{"ok":true}|})
    ?report ?session_id ?cost () =
  Backend_types.make_task_result ~status ~agent_text:text ?report ?session_id
    ?cost ()

let cost ?input ?output ?cache_creation ?cache_read ?usd () :
    Backend_types.cost =
  {
    tokens_input = input;
    tokens_output = output;
    cost_usd = usd;
    cache_creation_input_tokens = cache_creation;
    cache_read_input_tokens = cache_read;
  }

let register ?binary_name ?baseline_version ?session_resume ?native ?read_only
    ?media_types ?web id backend =
  ok
    (Cwr_cabal_internal.register_custom_backend ~bootstrap
       ~descriptor:
         (descriptor ?binary_name ?baseline_version ?session_resume ?native
            ?read_only ?media_types ?web id)
       ~backend)

let create ~sw ~env ?(limits = no_attachment_limits) ?custom_backend ?default_model
    ~backend_id ~working_dir () =
  ok
    (Cwr_cabal_internal.create ~bootstrap ~sw ~env ~limits ~backend_id
       ~working_dir ?custom_backend ?default_model ())

let request ?(id = "bridge-request") ?(system_prompt = "system")
    ?(user_prompt = "user") ?json_schema ?resume_session ?(attachments = [])
    ?(web_policy = Agent_execution.web_disabled) ?max_turns ?routing ?model
    ?(read_only = Some false) ?(timeout_s = 5.0) () =
  ok
    (Agent_execution.make_request ~id ~system_prompt ~user_prompt ~timeout_s
       ?json_schema ?resume_session ~attachments ~web_policy ?max_turns ?routing
       ?model ?read_only ())

let with_temp_dir label f =
  let root = Filename.temp_dir ("cwr-cabal-" ^ label ^ "-") "" in
  let rec remove path =
    if Sys.file_exists path then
      if Sys.is_directory path then begin
        Array.iter (fun name -> remove (Filename.concat path name))
          (Sys.readdir path);
        Unix.rmdir path
      end
      else Unix.unlink path
  in
  Fun.protect ~finally:(fun () -> remove root) (fun () -> f root)

let write_file path contents =
  let channel = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr channel) (fun () ->
      output_string channel contents)

let make_attachment ~workspace ~id ~name ~mime_type contents =
  write_file (Filename.concat workspace name) contents;
  ok
    (Agent_execution.make_attachment ~id ~path:name ~mime_type
       ~sha256:Digestif.SHA256.(to_hex (digest_string contents))
       ~size_bytes:(Int64.of_int (String.length contents)) ())

let status_equal left right =
  match (left, right) with
  | Agent_execution.Success, Agent_execution.Success
  | Agent_execution.Timed_out, Agent_execution.Timed_out
  | Agent_execution.Cancelled, Agent_execution.Cancelled ->
      true
  | Agent_execution.Failed _, Agent_execution.Failed _ -> true
  | _ -> false

let check_predispatch_error ~kind ~diagnostic error =
  match Agent_execution.error_view error with
  | Agent_execution.Dispatch_failure
      {kind = actual; message; event_trace = Some trace} ->
      Alcotest.(check bool) "dispatch kind" true (actual = kind);
      Alcotest.(check bool) "sanitized diagnostic" true
        (contains message diagnostic);
      Alcotest.(check bool) "every pre-invocation envelope is attempt zero" true
        (List.for_all
           (fun event -> Workflow_event.attempt event = 0)
           (Workflow_event.events trace))
  | _ -> fail "pre-invocation error lost its typed trace"

let test_bootstrap_conflict_is_clear () =
  match Cwr_cabal.bootstrap_hardened () with
  | Error message ->
      Alcotest.(check bool)
        "second bootstrap explains the process lifecycle" true
        (contains message "one-shot for the process")
  | Ok _ -> fail "a second hardened bootstrap unexpectedly succeeded"

let validated_entry id =
  match Registry.find_entry id with
  | Some (Registry.Validated entry) -> entry
  | Some (Registry.Raw _) -> Alcotest.failf "%s is raw" id
  | None -> Alcotest.failf "%s is missing" id

let clone_entry ?backend entry =
  let backend = Option.value ~default:entry.Runtime_entry.backend backend in
  match
    Runtime_entry.create ~backend ~descriptor:entry.effective_descriptor
      ~runtime_capabilities:entry.runtime_capabilities ~origin:entry.origin
      ~execution_policy:entry.execution_policy
      ~version_policy:entry.version_policy
  with
  | Ok entry -> entry
  | Error error -> fail (Runtime_entry.render_validation_error error)

let test_hardened_entry_identity_is_pinned () =
  let id = "codex" in
  let original = validated_entry id in
  let expect_rejected ~sw ~env label =
    match
      Cwr_cabal_internal.create ~bootstrap ~sw ~env ~limits:no_attachment_limits
        ~backend_id:id ~working_dir:"/tmp" ()
    with
    | Error _ -> ()
    | Ok _ -> Alcotest.fail (label ^ " unexpectedly retained trust")
  in
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  Registry.register original.backend;
  expect_rejected ~sw ~env "raw replacement";
  Registry.register_validated original;
  let cloned = clone_entry original in
  Registry.register_validated cloned;
  expect_rejected ~sw ~env "equal validated replacement";
  Registry.register_validated original;
  let replacement_backend, _ =
    make_backend ~session_resume:true ~native:true ~id
      (fun ~env:_ ~context:_ ~call:_ _ -> result ())
  in
  let replacement = clone_entry ~backend:replacement_backend original in
  Registry.register_validated replacement;
  expect_rejected ~sw ~env "physical backend replacement";
  Registry.register_validated original;
  ignore (create ~sw ~env ~backend_id:id ~working_dir:"/tmp" ())

let test_custom_token_binds_exact_entry_and_bootstrap () =
  let first_id = "cwr-token-first" in
  let first_backend, _ =
    make_backend ~id:first_id (fun ~env:_ ~context:_ ~call:_ _ -> result ())
  in
  let first_token = register first_id first_backend in
  let second_id = "cwr-token-second" in
  let second_backend, _ =
    make_backend ~id:second_id (fun ~env:_ ~context:_ ~call:_ _ -> result ())
  in
  let second_token = register second_id second_backend in
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  (match
     Cwr_cabal_internal.create ~bootstrap ~sw ~env ~limits:no_attachment_limits
       ~backend_id:first_id ~working_dir:"/tmp" ~custom_backend:second_token ()
   with
  | Error _ -> ()
  | Ok _ -> fail "a token authorized another custom backend");
  let original = validated_entry first_id in
  Registry.register_validated (clone_entry original);
  (match
     Cwr_cabal_internal.create ~bootstrap ~sw ~env ~limits:no_attachment_limits
       ~backend_id:first_id ~working_dir:"/tmp" ~custom_backend:first_token ()
   with
  | Error _ -> ()
  | Ok _ -> fail "a token survived validated entry replacement");
  Registry.register_validated original;
  ignore
    (create ~sw ~env ~custom_backend:first_token ~backend_id:first_id
       ~working_dir:"/tmp" ())

let test_concurrent_create_uses_immutable_bootstrap () =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let make_runtime () =
    Cwr_cabal_internal.create ~bootstrap ~sw ~env ~limits:no_attachment_limits
      ~backend_id:"codex" ~working_dir:"/tmp" ()
  in
  let left = ref None in
  let right = ref None in
  Eio.Fiber.both
    (fun () -> left := Some (make_runtime ()))
    (fun () -> right := Some (make_runtime ()));
  match (!left, !right) with
  | Some (Ok _), Some (Ok _) -> ()
  | Some (Error error), _ | _, Some (Error error) -> Alcotest.fail error
  | None, _ | _, None -> fail "concurrent create did not complete"

let test_exact_request_mapping () =
  with_temp_dir "mapping" @@ fun root ->
  let workspace = Filename.concat root "workspace with spaces" in
  Unix.mkdir workspace 0o700;
  let png = "\x89PNG\r\n\x1a\nfixture" in
  let jpeg = "\xff\xd8\xfffixture" in
  let attachments =
    [
      make_attachment ~workspace ~id:"first" ~name:"first image.png"
        ~mime_type:"image/png" png;
      make_attachment ~workspace ~id:"second" ~name:"second image.jpg"
        ~mime_type:"image/jpeg" jpeg;
    ]
  in
  let id = "cwr-map-exact" in
  let backend, observation =
    make_backend ~session_resume:true ~native:true ~id
      (fun ~env:_ ~context:_ ~call:_ _ ->
        result ~session_id:"returned-session" ())
  in
  let custom_backend =
    register ~session_resume:true ~native:true ~read_only:true
      ~media_types:[Backend_types.Png; Backend_types.Jpeg]
      ~web:Backend_types.Web_search_and_fetch id backend
  in
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let runtime =
    create ~sw ~env ~limits:media_limits ~custom_backend ~backend_id:id
      ~working_dir:workspace ()
  in
  let capabilities = Runtime.capabilities runtime in
  Alcotest.(check bool) "native schema matches the bound entry" true
    (Runtime.native_json_schema capabilities);
  Alcotest.(check bool) "session resume matches the bound entry" true
    (Runtime.session_resume capabilities);
  Alcotest.(check bool) "attachments match the bound entry" true
    (Runtime.attachments capabilities);
  Alcotest.(check (list string)) "media types match the bound entry"
    ["image/png"; "image/jpeg"]
    (Runtime.media_mime_types capabilities);
  Alcotest.(check bool) "maximum web matches the bound entry" true
    (Runtime.maximum_web capabilities
    = Agent_execution.Web_search_and_fetch);
  Alcotest.(check bool) "domain restrictions remain unsupported" false
    (Runtime.restricted_web_domains capabilities);
  Alcotest.(check bool) "read only matches the bound entry" true
    (Runtime.read_only capabilities);
  Alcotest.(check bool) "max turns are accepted and forwarded" true
    (Runtime.max_turns capabilities);
  Alcotest.(check bool) "central deadline is hard" true
    (Runtime.hard_timeout capabilities);
  Alcotest.(check bool) "fixed runtime does not advertise routing" false
    (Runtime.routing capabilities);
  Alcotest.(check bool) "per-request model selection is forwarded" true
    (Runtime.model_selection capabilities);
  let schema = `Assoc [("type", `String "object")] in
  let mapped =
    request ~system_prompt:" system prompt with spaces "
      ~user_prompt:" user prompt with spaces " ~json_schema:schema
      ~resume_session:"resume-session" ~attachments
      ~web_policy:Agent_execution.web_search_and_fetch ~timeout_s:12.5
      ~max_turns:7 ~routing:id ~model:"vendor/model name" ~read_only:(Some true)
      ()
    |> Runtime.complete runtime |> execution_ok
  in
  Alcotest.(check int) "central backend called once" 1 !(observation.calls);
  (match List.rev !(observation.specs) with
  | [ spec ] ->
      Alcotest.(check string) "resume omits system replay"
        " user prompt with spaces " spec.prompt;
      Alcotest.(check (option string)) "model" (Some "vendor/model name")
        spec.model;
      Alcotest.(check bool) "read only" true spec.read_only;
      Alcotest.(check (option int)) "max turns" (Some 7) spec.max_turns;
      Alcotest.(check (float 0.0)) "finite timeout" 12.5 spec.timeout;
      Alcotest.(check (option string)) "resume" (Some "resume-session")
        spec.resume_session_id;
      Alcotest.(check int) "ordered attachments" 2
        (List.length spec.attachments);
      Alcotest.(check bool) "web" true
        (spec.web_access = Backend_types.Web_search_and_fetch);
      Alcotest.(check bool) "schema" true (spec.json_schema = Some schema)
  | specs ->
      Alcotest.failf "expected one central spec, got %d" (List.length specs));
  let trace =
    match Agent_execution.event_trace mapped with
    | Some trace -> trace
    | None -> fail "central completion omitted its normalized trace"
  in
  Alcotest.(check bool) "central preflight was observed" true
    (List.exists
       (fun event ->
         Workflow_event.payload event = Workflow_event.Preflight_started)
       (Workflow_event.events trace));
  let rec event_index predicate index = function
    | [] -> None
    | event :: rest ->
        if predicate (Workflow_event.payload event) then Some index
        else event_index predicate (index + 1) rest
  in
  let events = Workflow_event.events trace in
  let required_index label predicate =
    match event_index predicate 0 events with
    | Some index -> index
    | None -> fail (label ^ " event was absent")
  in
  let required_at label index =
    match List.nth_opt events index with
    | Some event -> event
    | None -> fail (label ^ " event index was invalid")
  in
  let session_index =
    required_index "session"
      (function Workflow_event.Session_id _ -> true | _ -> false)
  in
  let finish_index =
    required_index "attempt finish"
      (function Workflow_event.Attempt_finished _ -> true | _ -> false)
  in
  Alcotest.(check bool) "fallback metadata keeps its post-finish envelope" true
    (session_index > finish_index);
  let finish_event = required_at "attempt finish" finish_index in
  let session_event = required_at "session" session_index in
  Alcotest.(check int64) "source sequence envelope is unchanged"
    (Int64.succ (Workflow_event.seq finish_event))
    (Workflow_event.seq session_event);
  Alcotest.(check int) "source attempt envelope is unchanged"
    (Workflow_event.attempt finish_event)
    (Workflow_event.attempt session_event);
  Alcotest.(check bool) "source elapsed envelope remains ordered" true
    (Workflow_event.elapsed_s session_event
    >= Workflow_event.elapsed_s finish_event);
  Alcotest.(check (option string)) "session retained"
    (Some "returned-session")
    (Agent_execution.final_session_id mapped);
  Alcotest.(check bool) "sealed attachment cleanup retained" true
    (Agent_execution.cleanup_status mapped = Agent_execution.Cleanup_succeeded);
  (match Runtime.complete runtime (request ~routing:"another-backend" ()) with
  | Error error -> (
      match Agent_execution.error_view error with
      | Agent_execution.Dispatch_failure
          {kind = Agent_execution.Capability_mismatch; message; _} ->
          Alcotest.(check bool) "cross-backend routing diagnostic" true
            (contains message "bound backend")
      | _ -> fail "cross-backend routing was misclassified")
  | Ok _ -> fail "fixed runtime switched backend per request");
  Alcotest.(check int) "routing mismatch precedes Cabal dispatch" 1
    !(observation.calls)

let test_guarded_dispatch_rejects_selected_entry_replacement () =
  let id = "cwr-guarded-race" in
  let original_backend, original_observation =
    make_backend ~id (fun ~env:_ ~context:_ ~call:_ _ ->
        result ~text:{|{"original":true}|} ())
  in
  let token = register id original_backend in
  let original_entry = validated_entry id in
  let replacement_backend, replacement_observation =
    make_backend ~id (fun ~env:_ ~context:_ ~call:_ _ ->
        result ~text:{|{"replacement":true}|} ())
  in
  let replacement_entry =
    clone_entry ~backend:replacement_backend original_entry
  in
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let runtime =
    ok
      (Cwr_cabal_internal.Private.create_with_selection_hook ~bootstrap ~sw ~env
         ~limits:no_attachment_limits ~backend_id:id ~working_dir:"/tmp"
         ~custom_backend:token
         ~after_selection:(fun () -> Registry.register_validated replacement_entry)
         ())
  in
  let outcome =
    Fun.protect
      ~finally:(fun () -> Registry.register_validated original_entry)
      (fun () -> Runtime.complete runtime (request ()))
  in
  (match outcome with
  | Error error -> (
      match Agent_execution.error_view error with
      | Agent_execution.Dispatch_failure
          {kind = Agent_execution.Capability_mismatch; message; _} ->
          Alcotest.(check bool) "guard mismatch is sanitized" true
            (contains message "expected entry identity")
      | _ -> fail "guarded replacement was misclassified")
  | Ok response ->
      Alcotest.failf "replacement race executed: %s"
        (Agent_execution.final_text response));
  Alcotest.(check int) "captured original not invoked after replacement" 0
    !(original_observation.calls);
  Alcotest.(check int) "replacement never invoked" 0
    !(replacement_observation.calls)

let test_guarded_dispatch_keeps_snapshot_after_capture () =
  let id = "cwr-guarded-snapshot" in
  let replacement_entry = ref None in
  let original_backend, original_observation =
    make_backend
      ~available:(fun () ->
        Option.iter Registry.register_validated !replacement_entry;
        true)
      ~id (fun ~env:_ ~context:_ ~call:_ _ ->
        result ~text:{|{"original":true}|} ())
  in
  let token = register id original_backend in
  let original_entry = validated_entry id in
  let replacement_backend, replacement_observation =
    make_backend ~id (fun ~env:_ ~context:_ ~call:_ _ ->
        result ~text:{|{"replacement":true}|} ())
  in
  replacement_entry :=
    Some (clone_entry ~backend:replacement_backend original_entry);
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let runtime =
    create ~sw ~env ~custom_backend:token ~backend_id:id ~working_dir:"/tmp" ()
  in
  let outcome =
    Fun.protect
      ~finally:(fun () -> Registry.register_validated original_entry)
      (fun () -> Runtime.complete runtime (request ()))
  in
  let response = execution_ok outcome in
  Alcotest.(check (option bool)) "captured original produced the result"
    (Some true)
    (match Agent_execution.final_structured_json response with
    | Some (`Assoc fields) -> (
        match List.assoc_opt "original" fields with
        | Some (`Bool value) -> Some value
        | _ -> None)
    | _ -> None);
  Alcotest.(check int) "captured original invoked once" 1
    !(original_observation.calls);
  Alcotest.(check int) "late replacement never invoked" 0
    !(replacement_observation.calls)

let test_final_agent_text_fallback_is_preserved () =
  let id = "cwr-final-text-fallback" in
  let final_text = {|{"fallback":true}|} in
  let backend, observation =
    make_backend ~emit_result_text:false ~id
      (fun ~env:_ ~context:_ ~call:_ _ -> result ~text:final_text ())
  in
  let custom_backend = register id backend in
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let runtime =
    create ~sw ~env ~custom_backend ~backend_id:id ~working_dir:"/tmp" ()
  in
  let response = Runtime.complete runtime (request ()) |> execution_ok in
  let trace =
    match Agent_execution.event_trace response with
    | Some trace -> trace
    | None -> fail "final fallback trace was absent"
  in
  let events = Workflow_event.events trace in
  Alcotest.(check int) "one final fallback text event" 1
    (List.fold_left
       (fun count event ->
         match Workflow_event.payload event with
         | Workflow_event.Agent_text_delta _ -> count + 1
         | _ -> count)
       0 events);
  let rec adjacent = function
    | finish :: text :: terminal :: rest -> (
        match
          ( Workflow_event.payload finish,
            Workflow_event.payload text,
            Workflow_event.payload terminal )
        with
        | ( Workflow_event.Attempt_finished Workflow_event.Attempt_succeeded,
            Workflow_event.Agent_text_delta actual,
            Workflow_event.Terminal Workflow_event.Succeeded ) ->
            Some (finish, text, actual)
        | _ -> adjacent (text :: terminal :: rest))
    | _ -> None
  in
  (match adjacent events with
  | Some (finish, text, actual) ->
      Alcotest.(check string) "fallback text is source-preserved" final_text actual;
      Alcotest.(check bool) "fallback text is nonempty" true (actual <> "");
      Alcotest.(check bool) "fallback text is bounded" true
        (String.length actual <= Workflow_event.max_text_bytes);
      Alcotest.(check int64) "fallback sequence follows attempt finish"
        (Int64.succ (Workflow_event.seq finish))
        (Workflow_event.seq text);
      Alcotest.(check int) "fallback remains on the finished attempt"
        (Workflow_event.attempt finish) (Workflow_event.attempt text);
      Alcotest.(check bool) "fallback timestamp remains ordered" true
        (Workflow_event.elapsed_s text >= Workflow_event.elapsed_s finish)
  | None -> fail "final fallback was not immediately before the terminal");
  Alcotest.(check int) "fallback backend called once" 1 !(observation.calls)

let test_nonresume_prompt_and_default_model () =
  let id = "cwr-map-prompt" in
  let backend, observation =
    make_backend ~id (fun ~env:_ ~context:_ ~call:_ _ -> result ())
  in
  let custom_backend = register id backend in
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let runtime =
    create ~sw ~env ~custom_backend ~default_model:"default/model"
      ~backend_id:id ~working_dir:"/tmp" ()
  in
  ignore
    (Runtime.complete runtime
       (request ~system_prompt:"sys token" ~user_prompt:"user token" ())
    |> execution_ok);
  match !(observation.specs) with
  | [ spec ] ->
      Alcotest.(check string) "canonical prompt composition"
        "SYSTEM INSTRUCTIONS:\nsys token\n\n---\n\nUSER REQUEST:\nuser token"
        spec.prompt;
      Alcotest.(check (option string)) "constructor model fallback"
        (Some "default/model") spec.model
  | specs -> Alcotest.failf "expected one spec, got %d" (List.length specs)

let test_statuses_are_exhaustive () =
  let statuses =
    ref
      [
        Backend_types.Success;
        Backend_types.Failed "/private/backend detail";
        Backend_types.Timeout;
        Backend_types.Cancelled;
      ]
  in
  let id = "cwr-statuses" in
  let backend, _ =
    make_backend ~id (fun ~env:_ ~context:_ ~call:_ _ ->
        match !statuses with
        | status :: rest ->
            statuses := rest;
            result ~status ()
        | [] -> result ())
  in
  let custom_backend = register id backend in
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let runtime =
    create ~sw ~env ~custom_backend ~backend_id:id ~working_dir:"/tmp" ()
  in
  List.iter
    (fun expected ->
      let response = Runtime.complete runtime (request ()) |> execution_ok in
      Alcotest.(check bool) "status mapped" true
        (status_equal expected (Agent_execution.final_status response));
      match Agent_execution.final_status response with
      | Agent_execution.Failed message ->
          Alcotest.(check bool) "private backend detail redacted" false
            (contains message "/private")
      | Agent_execution.Success | Agent_execution.Timed_out
      | Agent_execution.Cancelled -> ())
    [
      Agent_execution.Success;
      Agent_execution.Failed "redacted";
      Agent_execution.Timed_out;
      Agent_execution.Cancelled;
    ]

let report raw_json : Backend_types.structured_report =
  {verdict = None; issues = []; questions = []; suggestions = []; raw_json}

let test_strict_structured_output () =
  let results =
    ref
      [
        result ~text:"prose" ~report:(report (Some (`Assoc [("raw", `Bool true)]))) ();
        result ~text:{|{"text":true}|}
          ~report:(report (Some (`Assoc [("raw", `Bool true)]))) ();
        result ~text:{|{"fallback":true}|} ~report:(report (Some (`String "bad"))) ();
        result ~text:"```json\n{\"bad\":true}\n```" ();
        result ~text:"prefix {\"bad\":true}" ();
        result ~text:"42" ();
      ]
  in
  let id = "cwr-json-strict" in
  let backend, _ =
    make_backend ~id (fun ~env:_ ~context:_ ~call:_ _ ->
        match !results with
        | value :: rest ->
            results := rest;
            value
        | [] -> result ())
  in
  let custom_backend = register id backend in
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let runtime =
    create ~sw ~env ~custom_backend ~backend_id:id ~working_dir:"/tmp" ()
  in
  let structured () =
    Runtime.complete runtime (request ()) |> execution_ok
    |> Agent_execution.final_structured_json
  in
  Alcotest.(check bool) "valid raw report preferred" true
    (structured () = Some (`Assoc [("raw", `Bool true)]));
  (match Runtime.complete runtime (request ()) with
  | Error error -> (
      match Agent_execution.error_view error with
      | Agent_execution.Telemetry_mapping_failure {event_trace; _} ->
          Alcotest.(check bool) "safe source trace retained" true
            (Workflow_event.events event_trace <> [])
      | _ -> fail "conflicting structured sources were misclassified")
  | Ok _ -> fail "conflicting structured sources were accepted");
  Alcotest.(check bool) "invalid report root yields strict text" true
    (structured () = Some (`Assoc [("fallback", `Bool true)]));
  List.iter
    (fun label ->
      Alcotest.(check (option bool)) label None
        (Option.map (fun _ -> true) (structured ())))
    ["fence rejected"; "prose rejected"; "scalar rejected"]

let object_schema =
  `Assoc
    [
      ("type", `String "object");
      ("required", `List [`String "ok"]);
      ("properties", `Assoc [("ok", `Assoc [("type", `String "boolean")])]);
    ]

let test_schema_retry_and_native_failure () =
  let retry_id = "cwr-schema-resume" in
  let retry_backend, observation =
    make_backend ~session_resume:true ~id:retry_id
      (fun ~env:_ ~context:_ ~call spec ->
        if call = 1 then result ~text:"not-json" ~session_id:"session-one" ()
        else begin
          Alcotest.(check (option string)) "retry resumes session"
            (Some "session-one") spec.Backend_types.resume_session_id;
          result ~text:"[]" ~session_id:"session-two" ()
        end)
  in
  let retry_custom =
    register ~session_resume:true retry_id retry_backend
  in
  let native_id = "cwr-native-failure" in
  let native_backend, native_observation =
    make_backend ~native:true ~id:native_id
      (fun ~env:_ ~context:_ ~call:_ _ ->
        result ~status:(Backend_types.Failed "schema or transport") ())
  in
  let native_custom = register ~native:true native_id native_backend in
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let retry_runtime =
    create ~sw ~env ~custom_backend:retry_custom ~backend_id:retry_id
      ~working_dir:"/tmp" ()
  in
  (match Runtime.complete retry_runtime (request ~json_schema:object_schema ()) with
  | Error error -> (
      match Agent_execution.error_view error with
      | Agent_execution.Execution_failure
          {kind = Agent_execution.Schema_retry_failed; response; _} ->
          Alcotest.(check int) "both retries retained" 2
            (List.length (Agent_execution.attempts response));
          let trace =
            match Agent_execution.event_trace response with
            | Some trace -> trace
            | None -> fail "schema retry trace was absent"
          in
          Alcotest.(check bool) "opaque retry reason stays redacted" true
            (List.exists
               (fun event ->
                 match Workflow_event.payload event with
                 | Workflow_event.Retry_transition
                     {reason = Workflow_event.Other_redacted; _} ->
                     true
                 | _ -> false)
               (Workflow_event.events trace));
          (match Agent_execution.attempts response with
          | [ first; second ] ->
              Alcotest.(check bool) "initial kind" true
                (Agent_execution.attempt_kind first
                = Workflow_event.Initial_attempt);
              Alcotest.(check bool) "resumed kind" true
                (Agent_execution.attempt_kind second
                = Workflow_event.Resumed_attempt);
              Alcotest.(check bool) "both schema errors" true
                (Option.is_some (Agent_execution.attempt_schema_error first)
                && Option.is_some
                     (Agent_execution.attempt_schema_error second))
          | _ -> fail "schema retry did not retain exactly two attempts")
      | _ -> fail "schema retry failure was misclassified")
  | Ok _ -> fail "double schema failure unexpectedly succeeded");
  Alcotest.(check int) "two central calls" 2 !(observation.calls);
  let native_runtime =
    create ~sw ~env ~custom_backend:native_custom ~backend_id:native_id
      ~working_dir:"/tmp" ()
  in
  (match Runtime.complete native_runtime (request ~json_schema:object_schema ()) with
  | Error error -> (
      match Agent_execution.error_view error with
      | Agent_execution.Execution_failure
          {
            kind = Agent_execution.Native_backend_failure_with_schema;
            response;
            _;
          } ->
          Alcotest.(check int) "native call retained" 1
            (List.length (Agent_execution.attempts response))
      | _ -> fail "native failure lost neutral causality")
  | Ok _ -> fail "native failed result unexpectedly succeeded");
  Alcotest.(check int) "native path called once" 1 !(native_observation.calls)

let test_retry_exception_is_incomplete () =
  let id = "cwr-retry-incomplete" in
  let backend, _observation =
    make_backend ~id (fun ~env:_ ~context:_ ~call _ ->
        if call = 1 then result ~text:"not-json" ()
        else raise (Failure "private retry exception"))
  in
  let custom_backend = register id backend in
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let runtime =
    create ~sw ~env ~custom_backend ~backend_id:id ~working_dir:"/tmp" ()
  in
  match Runtime.complete runtime (request ~json_schema:object_schema ()) with
  | Error error -> (
      match Agent_execution.error_view error with
      | Agent_execution.Incomplete_execution {message; execution} ->
          Alcotest.(check int) "one completed attempt retained" 1
            (List.length
               (Agent_execution.incomplete_completed_attempts execution));
          Alcotest.(check bool) "private exception redacted" false
            (contains message "private retry");
          (match Agent_execution.incomplete_continuation execution with
          | Some continuation ->
              Alcotest.(check int) "continuation number" 2
                (Agent_execution.continuation_number continuation);
              Alcotest.(check bool) "continuation was observed" true
                (Agent_execution.continuation_invocation continuation
                = Agent_execution.Invocation_started)
          | None -> fail "retry invocation evidence was dropped")
      | _ -> fail "retry exception was not mapped as incomplete")
  | Ok _ -> fail "retry exception unexpectedly succeeded"

let test_capability_and_routing_rejections_precede_spawn () =
  let id = "cwr-preflight-rejections" in
  let backend, observation =
    make_backend ~id (fun ~env:_ ~context:_ ~call:_ _ -> result ())
  in
  let custom_backend = register id backend in
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let runtime =
    create ~sw ~env ~custom_backend ~backend_id:id ~working_dir:"/tmp" ()
  in
  let expect_dispatch request =
    match Runtime.complete runtime request with
    | Error error -> error
    | Ok _ -> fail "unsupported request reached the fake backend"
  in
  let restricted =
    ok
      (Agent_execution.make_restricted_web_policy
         ~level:Agent_execution.Web_search ~domains:["example.com"] ())
  in
  let domain_error = expect_dispatch (request ~web_policy:restricted ()) in
  (match Agent_execution.error_view domain_error with
  | Agent_execution.Dispatch_failure
      {kind = Agent_execution.Unsupported_request; _} ->
      ()
  | _ -> fail "restricted domains were misclassified");
  let read_only_error = expect_dispatch (request ~read_only:(Some true) ()) in
  (match Agent_execution.error_view read_only_error with
  | Agent_execution.Dispatch_failure
      {
        kind = Agent_execution.Preflight_failed;
        message;
        event_trace = Some trace;
      } ->
      Alcotest.(check bool) "preflight diagnostic is sanitized" true
        (contains message "read-only");
      Alcotest.(check bool) "preflight trace remains pre-invocation" true
        (List.for_all
           (fun event -> Workflow_event.attempt event = 0)
           (Workflow_event.events trace))
  | _ -> fail "read-only capability rejection was misclassified");
  Alcotest.(check int) "no backend spawn" 0 !(observation.calls);
  Alcotest.(check int) "capability rejection precedes availability" 0
    !(observation.availability_calls)

let test_missing_blank_untrusted_and_quarantined_backends () =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let expect_create_error ?custom_backend backend_id =
    match
      Cwr_cabal_internal.create ~bootstrap ~sw ~env ~limits:no_attachment_limits
        ~backend_id ~working_dir:"/tmp" ?custom_backend ()
    with
    | Error _ -> ()
    | Ok _ -> Alcotest.failf "backend %S unexpectedly passed create" backend_id
  in
  expect_create_error "";
  expect_create_error "not-registered";
  let raw_backend, raw_observation =
    make_backend ~id:"cwr-raw" (fun ~env:_ ~context:_ ~call:_ _ -> result ())
  in
  Registry.register raw_backend;
  expect_create_error "cwr-raw";
  let quarantined =
    create ~sw ~env ~backend_id:"copilot-cli" ~working_dir:"/tmp" ()
  in
  (match Runtime.complete quarantined (request ()) with
  | Error error -> (
      match Agent_execution.error_view error with
      | Agent_execution.Dispatch_failure
          {
            kind = Agent_execution.Capability_mismatch;
            message;
            event_trace = Some trace;
          } ->
          Alcotest.(check bool) "quarantine diagnostic" true
            (contains message "quarantined");
          Alcotest.(check bool) "quarantine terminal is attempt zero" true
            (List.for_all
               (fun event -> Workflow_event.attempt event = 0)
               (Workflow_event.events trace))
      | _ -> fail "quarantine was misclassified or lost its trace")
  | Ok _ -> fail "quarantined backend executed");
  Alcotest.(check int) "raw backend not called" 0 !(raw_observation.calls)

let test_unavailable_and_zero_attempt_timeout () =
  let unavailable_id = "cwr-unavailable" in
  let unavailable, unavailable_observation =
    make_backend ~available:(fun () -> false) ~id:unavailable_id
      (fun ~env:_ ~context:_ ~call:_ _ -> result ())
  in
  let unavailable_custom = register unavailable_id unavailable in
  let timeout_id = "cwr-zero-attempt-timeout" in
  let timeout_backend, timeout_observation =
    make_backend
      ~available:(fun () ->
        Unix.sleepf 0.05;
        true)
      ~id:timeout_id (fun ~env:_ ~context:_ ~call:_ _ -> result ())
  in
  let timeout_custom = register timeout_id timeout_backend in
  let exception_id = "cwr-zero-attempt-exception" in
  let exception_backend, exception_observation =
    make_backend ~id:exception_id (fun ~env:_ ~context:_ ~call:_ _ ->
        raise (Failure "private backend exception"))
  in
  let exception_custom = register exception_id exception_backend in
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let unavailable_runtime =
    create ~sw ~env ~custom_backend:unavailable_custom ~backend_id:unavailable_id
      ~working_dir:"/tmp" ()
  in
  (match Runtime.complete unavailable_runtime (request ()) with
  | Error error -> (
      match Agent_execution.error_view error with
      | Agent_execution.Dispatch_failure
          {
            kind = Agent_execution.Backend_unavailable;
            message;
            event_trace = Some trace;
          } ->
          Alcotest.(check bool) "availability diagnostic" true
            (contains message "not available");
          Alcotest.(check bool) "unavailable trace is attempt zero" true
            (List.for_all
               (fun event -> Workflow_event.attempt event = 0)
               (Workflow_event.events trace))
      | _ -> fail "unavailable backend was misclassified")
  | Ok _ -> fail "unavailable backend succeeded");
  Alcotest.(check int) "unavailable backend not called" 0
    !(unavailable_observation.calls);
  let timeout_runtime =
    create ~sw ~env ~custom_backend:timeout_custom ~backend_id:timeout_id
      ~working_dir:"/tmp" ()
  in
  (match Runtime.complete timeout_runtime (request ~timeout_s:0.001 ()) with
  | Error error -> (
      match Agent_execution.error_view error with
      | Agent_execution.No_completed_attempt
          {
            status = Agent_execution.Timed_out;
            invocation_may_have_started = true;
            _;
          } ->
          ()
      | _ -> fail "zero-attempt timeout was misclassified")
  | Ok _ -> fail "zero-attempt timeout unexpectedly succeeded");
  Alcotest.(check int) "timed-out backend not called" 0
    !(timeout_observation.calls);
  let exception_runtime =
    create ~sw ~env ~custom_backend:exception_custom ~backend_id:exception_id
      ~working_dir:"/tmp" ()
  in
  (match Runtime.complete exception_runtime (request ()) with
  | Error error -> (
      match Agent_execution.error_view error with
      | Agent_execution.No_completed_attempt
          {
            status = Agent_execution.Failed _;
            invocation_may_have_started = true;
            _;
          } ->
          ()
      | _ -> fail "zero-attempt backend exception was misclassified")
  | Ok _ -> fail "zero-attempt backend exception unexpectedly succeeded");
  Alcotest.(check int) "exception backend called once" 1
    !(exception_observation.calls)

let test_predispatch_registry_version_and_availability_errors () =
  let raw_id = "cwr-race-raw" in
  let raw_backend, _ =
    make_backend ~id:raw_id (fun ~env:_ ~context:_ ~call:_ _ -> result ())
  in
  let raw_token = register raw_id raw_backend in
  let missing_id = "cwr-race-missing" in
  let missing_backend, _ =
    make_backend ~id:missing_id (fun ~env:_ ~context:_ ~call:_ _ -> result ())
  in
  let missing_token = register missing_id missing_backend in
  let availability_id = "cwr-availability-error" in
  let availability_backend, availability_observation =
    make_backend ~available:(fun () -> raise (Failure "private availability"))
      ~id:availability_id
      (fun ~env:_ ~context:_ ~call:_ _ -> result ())
  in
  let availability_token = register availability_id availability_backend in
  let run_race ~runtime mutate =
    let outcome = ref None in
    Eio.Fiber.both
      (fun () ->
        Eio.Fiber.yield ();
        mutate ())
      (fun () -> outcome := Some (Runtime.complete runtime (request ())));
    match !outcome with
    | Some (Error error) -> error
    | Some (Ok _) -> fail "registry race unexpectedly dispatched"
    | None -> fail "registry race did not complete"
  in
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let raw_runtime =
    create ~sw ~env ~custom_backend:raw_token ~backend_id:raw_id
      ~working_dir:"/tmp" ()
  in
  let raw_original = validated_entry raw_id in
  let raw_error = run_race ~runtime:raw_runtime (fun () -> Registry.register raw_backend) in
  Registry.register_validated raw_original;
  check_predispatch_error ~kind:Agent_execution.Capability_mismatch
    ~diagnostic:"raw-registered" raw_error;
  let missing_runtime =
    create ~sw ~env ~custom_backend:missing_token ~backend_id:missing_id
      ~working_dir:"/tmp" ()
  in
  let registry_snapshot =
    Registry.list_ids ()
    |> List.filter_map (fun id ->
           match Registry.find_entry id with
           | Some (Registry.Validated entry) -> Some entry
           | None | Some (Registry.Raw _) -> None)
  in
  let missing_error = run_race ~runtime:missing_runtime Registry.clear in
  Registry.replace_all_validated registry_snapshot;
  check_predispatch_error ~kind:Agent_execution.Backend_unavailable
    ~diagnostic:"not registered" missing_error;
  let availability_runtime =
    create ~sw ~env ~custom_backend:availability_token
      ~backend_id:availability_id ~working_dir:"/tmp" ()
  in
  let availability_error =
    match Runtime.complete availability_runtime (request ()) with
    | Error error -> error
    | Ok _ -> fail "availability exception unexpectedly dispatched"
  in
  check_predispatch_error ~kind:Agent_execution.Internal_dispatch_failure
    ~diagnostic:"availability check failed" availability_error;
  Alcotest.(check int) "availability failure did not invoke backend" 0
    !(availability_observation.calls)

let test_predispatch_version_rejection () =
  with_temp_dir "version" @@ fun root ->
  let binary = "cwr-old-version" in
  let binary_path = Filename.concat root binary in
  write_file binary_path "#!/bin/sh\nprintf 'cwr-old-version 0.1.0\\n'\n";
  Unix.chmod binary_path 0o700;
  let original_path = Option.value ~default:"" (Sys.getenv_opt "PATH") in
  Fun.protect
    ~finally:(fun () -> Unix.putenv "PATH" original_path)
    (fun () ->
      Unix.putenv "PATH" (root ^ ":" ^ original_path);
      let id = "cwr-version-rejected" in
      let backend, observation =
        make_backend ~id (fun ~env:_ ~context:_ ~call:_ _ -> result ())
      in
      let token =
        register ~binary_name:binary ~baseline_version:"1.0.0" id backend
      in
      Eio_posix.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      let runtime =
        create ~sw ~env ~custom_backend:token ~backend_id:id ~working_dir:"/tmp"
          ()
      in
      let error =
        match Runtime.complete runtime (request ()) with
        | Error error -> error
        | Ok _ -> fail "unsupported version unexpectedly dispatched"
      in
      check_predispatch_error ~kind:Agent_execution.Capability_mismatch
        ~diagnostic:"stable baseline" error;
      Alcotest.(check int) "version rejection did not invoke backend" 0
        !(observation.calls))

let test_clear_and_rebootstrap_do_not_refresh_trust () =
  Registry.clear ();
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let expect_old_handle_rejected label =
    match
      Cwr_cabal_internal.create ~bootstrap ~sw ~env ~limits:no_attachment_limits
        ~backend_id:"codex" ~working_dir:"/tmp" ()
    with
    | Error _ -> ()
    | Ok _ -> Alcotest.fail (label ^ " trusted a new registry generation")
  in
  expect_old_handle_rejected "cleared registry";
  (match Cwr_cabal.bootstrap_hardened () with
  | Error message ->
      Alcotest.(check bool) "CWR bootstrap stays one-shot" true
        (contains message "one-shot")
  | Ok _ -> fail "CWR bootstrap succeeded twice");
  (match
     Runtime_bootstrap.register_runtime
       ~profile:Runtime_bootstrap.Hardened_builtins ()
   with
  | Ok () -> ()
  | Error error -> fail (Runtime_bootstrap.render_error error));
  expect_old_handle_rejected "direct Cabal rebootstrap"

let test_cost_rounding_sessions_and_event_truncation () =
  let id = "cwr-cost-events" in
  let first_cost =
    cost ~input:3 ~output:5 ~cache_creation:7 ~cache_read:11 ~usd:0.0000001 ()
  in
  let overflow_cost = cost ~input:13 ~usd:max_float () in
  let backend, observation =
    make_backend ~id (fun ~env:_ ~context ~call:_ _ ->
        Option.iter
          (fun context ->
            for _ = 1 to 400 do
              Task_execution_context.emit context
                (Task_event.Token_usage first_cost)
            done)
          context;
        result ~session_id:"cost-session" ~cost:first_cost ())
  in
  let custom_backend = register id backend in
  let overflow_id = "cwr-cost-overflow" in
  let overflow_backend, _ =
    make_backend ~id:overflow_id
      (fun ~env:_ ~context:_ ~call:_ _ -> result ~cost:overflow_cost ())
  in
  let overflow_custom = register overflow_id overflow_backend in
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let runtime =
    create ~sw ~env ~custom_backend ~backend_id:id ~working_dir:"/tmp" ()
  in
  let response = Runtime.complete runtime (request ()) |> execution_ok in
  let attempt =
    match Agent_execution.attempts response with
    | [ attempt ] -> attempt
    | attempts ->
        Alcotest.failf "expected one attempt, got %d" (List.length attempts)
  in
  (match Agent_execution.attempt_cost attempt with
  | Some mapped ->
      Alcotest.(check (option int64)) "ceil sub-micro USD" (Some 1L)
        (Execution_metrics.usd_micros mapped)
  | None -> fail "attempt cost missing");
  Alcotest.(check (option string)) "session aggregate"
    (Some "cost-session")
    (Agent_execution.final_session_id response);
  let trace =
    match Agent_execution.event_trace response with
    | Some trace -> trace
    | None -> fail "cost/session trace was absent"
  in
  Alcotest.(check bool) "bounded trace" true
    (List.length (Workflow_event.events trace) <= Workflow_event.max_events);
  Alcotest.(check bool) "omissions recorded" true
    (Int64.compare (Workflow_event.omitted_count trace) 0L > 0
    || List.exists
         (fun event ->
           match Workflow_event.payload event with
           | Workflow_event.Delivery_truncated _ -> true
           | _ -> false)
         (Workflow_event.events trace));
  Alcotest.(check int) "backend called" 1 !(observation.calls);
  let overflow_runtime =
    create ~sw ~env ~custom_backend:overflow_custom ~backend_id:overflow_id
      ~working_dir:"/tmp" ()
  in
  let overflow_response =
    Runtime.complete overflow_runtime (request ()) |> execution_ok
  in
  let overflow_attempt =
    match Agent_execution.attempts overflow_response with
    | [ attempt ] -> attempt
    | attempts ->
        Alcotest.failf "expected one overflow attempt, got %d"
          (List.length attempts)
  in
  (match Agent_execution.attempt_cost overflow_attempt with
  | Some mapped ->
      Alcotest.(check (option int64)) "overflow saturates" (Some Int64.max_int)
        (Execution_metrics.usd_micros mapped)
  | None -> fail "overflow cost missing")

let test_process_and_tool_events_are_normalized () =
  let id = "cwr-process-tool-events" in
  let observed_cost = cost ~input:2 ~output:3 ~usd:0.25 () in
  let backend, _observation =
    make_backend ~id (fun ~env:_ ~context ~call:_ _ ->
        Option.iter
          (fun context ->
            Task_execution_context.emit context
              (Task_event.Process_started {pid = Some 4242});
            Task_execution_context.emit context
              (Task_event.Session_id "event-session");
            Task_execution_context.emit context
              (Task_event.Agent_text_delta {|{"ok":true}|});
            Task_execution_context.emit context
              (Task_event.Tool_started {id = Some "tool-1"; name = "reader"});
            Task_execution_context.emit context
              (Task_event.Tool_finished
                 {id = Some "tool-1"; name = Some "reader"});
            Task_execution_context.emit context
              (Task_event.Token_usage observed_cost);
            Task_execution_context.emit context
              Task_event.Process_termination_requested;
            Task_execution_context.emit context Task_event.Process_kill_escalated;
            Task_execution_context.emit context
              (Task_event.Process_exited {exit_status = "private-status"}))
          context;
        result ~text:{|{"ok":true}|} ~session_id:"event-session"
          ~cost:observed_cost ())
  in
  let custom_backend = register id backend in
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let runtime =
    create ~sw ~env ~custom_backend ~backend_id:id ~working_dir:"/tmp" ()
  in
  let response = Runtime.complete runtime (request ()) |> execution_ok in
  let trace =
    match Agent_execution.event_trace response with
    | Some trace -> trace
    | None -> fail "process/tool trace was absent"
  in
  let payloads = List.map Workflow_event.payload (Workflow_event.events trace) in
  let has predicate = List.exists predicate payloads in
  Alcotest.(check bool) "process start" true
    (has (function Workflow_event.Process_started -> true | _ -> false));
  Alcotest.(check bool) "tool start" true
    (has (function Workflow_event.Tool_started _ -> true | _ -> false));
  Alcotest.(check bool) "tool finish" true
    (has (function Workflow_event.Tool_finished _ -> true | _ -> false));
  Alcotest.(check bool) "termination requested" true
    (has (function
      | Workflow_event.Process_termination_requested -> true
      | _ -> false));
  Alcotest.(check bool) "kill escalation" true
    (has (function Workflow_event.Process_kill_escalated -> true | _ -> false));
  Alcotest.(check bool) "unknown exit detail redacted" true
    (has (function
      | Workflow_event.Process_exited Workflow_event.Unknown -> true
      | _ -> false));
  let projection =
    Agent_execution.response_to_yojson response |> Yojson.Safe.to_string
  in
  Alcotest.(check bool) "process id omitted" false (contains projection "4242");
  Alcotest.(check bool) "private exit detail omitted" false
    (contains projection "private-status")

let () =
  Alcotest.run "CWR Cabal rich bridge"
    [
      ( "bootstrap and routing",
        [
          Alcotest.test_case "second bootstrap conflict" `Quick
            test_bootstrap_conflict_is_clear;
          Alcotest.test_case "hardened entry identity is pinned" `Quick
            test_hardened_entry_identity_is_pinned;
          Alcotest.test_case "custom token binds exact entry" `Quick
            test_custom_token_binds_exact_entry_and_bootstrap;
          Alcotest.test_case "concurrent create" `Quick
            test_concurrent_create_uses_immutable_bootstrap;
          Alcotest.test_case "selected entry replacement is guarded" `Quick
            test_guarded_dispatch_rejects_selected_entry_replacement;
          Alcotest.test_case "captured entry survives later replacement" `Quick
            test_guarded_dispatch_keeps_snapshot_after_capture;
          Alcotest.test_case "missing blank raw and quarantined" `Quick
            test_missing_blank_untrusted_and_quarantined_backends;
          Alcotest.test_case "unavailable and zero-attempt timeout" `Quick
            test_unavailable_and_zero_attempt_timeout;
          Alcotest.test_case "registry and availability failures retain trace"
            `Quick test_predispatch_registry_version_and_availability_errors;
          Alcotest.test_case "version rejection retains trace" `Quick
            test_predispatch_version_rejection;
        ] );
      ( "request mapping",
        [
          Alcotest.test_case "all rich fields" `Quick test_exact_request_mapping;
          Alcotest.test_case "prompt and default model" `Quick
            test_nonresume_prompt_and_default_model;
          Alcotest.test_case "capability and domain prechecks" `Quick
            test_capability_and_routing_rejections_precede_spawn;
        ] );
      ( "response mapping",
        [
          Alcotest.test_case "all statuses" `Quick test_statuses_are_exhaustive;
          Alcotest.test_case "strict object and array JSON" `Quick
            test_strict_structured_output;
          Alcotest.test_case "schema retry and native failure" `Quick
            test_schema_retry_and_native_failure;
          Alcotest.test_case "retry exception is incomplete" `Quick
            test_retry_exception_is_incomplete;
          Alcotest.test_case "cost session events and truncation" `Quick
            test_cost_rounding_sessions_and_event_truncation;
          Alcotest.test_case "process and tool event normalization" `Quick
            test_process_and_tool_events_are_normalized;
          Alcotest.test_case "final agent text fallback" `Quick
            test_final_agent_text_fallback_is_preserved;
        ] );
      ( "destructive lifecycle",
        [
          Alcotest.test_case "clear and rebootstrap cannot refresh trust" `Quick
            test_clear_and_rebootstrap_do_not_refresh_trust;
        ] );
    ]
