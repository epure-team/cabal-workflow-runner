open Cabal_workflow_runner

let with_path f =
  let path = Filename.temp_file "cwr-observations-" ".jsonl" in
  Fun.protect ~finally:(fun () -> try Sys.remove path with _ -> ()) (fun () -> f path)
let get = function Ok value -> value | Error error -> Alcotest.fail error
let events path = get (Secure_fs.read_regular path) |> String.split_on_char '\n'
  |> List.filter ((<>) "") |> List.map Yojson.Safe.from_string
let usage = {Observation.input_tokens=Some 0;output_tokens=Some 7;cache_read_tokens=Some 3;cache_write_tokens=Some 2}

let test_metadata_and_resume () = with_path @@ fun path ->
  let sink = get (Observation.open_sink ~path ~run_id:"run-1") in
  let call = Observation.start sink ~step:"probe" ~backend:(Some "claude-code") ~model:(Some "haiku") in
  Alcotest.(check int) "start durable before finish" 1 (List.length (events path));
  Observation.finish sink call ~outcome:"failed" ~exit_code:(Some 9)
    ~session_id:(Some "session-1") ~duration_ms:(Some 123.) ~usage:(Some usage);
  get (Observation.close sink);
  let sink = get (Observation.open_sink ~path ~run_id:"run-1") in
  let call = Observation.start sink ~step:"probe" ~backend:None ~model:None in
  Observation.finish sink call ~outcome:"ok" ~exit_code:(Some 0)
    ~session_id:None ~duration_ms:None ~usage:None;
  get (Observation.close sink);
  let rows = events path in
  let open Yojson.Safe.Util in
  Alcotest.(check int) "history retained" 4 (List.length rows);
  let failed = List.nth rows 1 and resumed = List.nth rows 2 in
  Alcotest.(check string) "new occurrence" "run-1:call:2" (resumed |> member "call_id" |> to_string);
  Alcotest.(check int) "observed zero retained" 0 (failed |> member "usage" |> member "input_tokens" |> to_int);
  Alcotest.(check string) "failure completeness explicit" "partial" (failed |> member "usage" |> member "usage_basis" |> to_string);
  Alcotest.(check bool) "cost unknown" true (failed |> member "cost" |> member "cost_usd" = `Null);
  Alcotest.(check string) "session" "session-1" (failed |> member "session_id" |> to_string);
  Alcotest.(check (float 0.001)) "elapsed" 123. (failed |> member "timing" |> member "duration_ms" |> to_float)

let test_sanitization () = with_path @@ fun path ->
  let sink = get (Observation.open_sink ~path ~run_id:"run-safe") in
  let call = Observation.start sink ~step:"body includes credentials" ~backend:None ~model:(Some "sk-fixture") in
  Observation.finish sink call ~outcome:"provider prose" ~exit_code:None
    ~session_id:(Some "Bearer fixture") ~duration_ms:(Some nan)
    ~usage:(Some {usage with input_tokens=Some (-1)});
  get (Observation.close sink);
  let row = List.nth (events path) 1 in
  let open Yojson.Safe.Util in
  List.iter (fun key -> Alcotest.(check bool) key true (member key row = `Null))
    ["step";"requested_model";"session_id"];
  Alcotest.(check bool) "negative tokens dropped" true (row |> member "usage" |> member "input_tokens" = `Null);
  Alcotest.(check bool) "nonfinite duration dropped" true (row |> member "timing" |> member "duration_ms" = `Null)

let test_lock_and_failure () = with_path @@ fun path ->
  let sink = get (Observation.open_sink ~path ~run_id:"run-lock") in
  Alcotest.(check bool) "second writer refused" true (Result.is_error (Observation.open_sink ~path ~run_id:"run-lock"));
  Unix.putenv "CWR_TEST_FAIL_LEDGER_APPEND" "1";
  Fun.protect ~finally:(fun () -> Unix.putenv "CWR_TEST_FAIL_LEDGER_APPEND" "") (fun () ->
    try ignore (Observation.start sink ~step:"probe" ~backend:None ~model:None);
      Alcotest.fail "write failure was hidden"
    with Observation.Sink_error -> ());
  Alcotest.(check bool) "sticky failed close" true (Result.is_error (Observation.close sink))

let test_append_safety () = with_path @@ fun path ->
  let alias = path ^ ".alias" in
  Unix.symlink path alias;
  Fun.protect ~finally:(fun () -> Sys.remove alias) (fun () ->
    Alcotest.(check bool) "symlink refused" true
      (Result.is_error (Observation.open_sink ~path:alias ~run_id:"safe-run")));
  let sink = get (Observation.open_sink ~path ~run_id:"safe-run") in
  Alcotest.(check int) "private permissions" 0o600 (Unix.stat path).Unix.st_perm;
  let call = Observation.start sink ~step:"probe" ~backend:None ~model:None in
  Observation.finish sink call ~outcome:"ok" ~exit_code:(Some 0)
    ~session_id:None ~duration_ms:None ~usage:None;
  (try Observation.finish sink call ~outcome:"ok" ~exit_code:(Some 0)
    ~session_id:None ~duration_ms:None ~usage:None;
    Alcotest.fail "duplicate final accepted" with Observation.Sink_error -> ());
  Alcotest.(check int) "no duplicate event" 2 (List.length (events path));
  ignore (Observation.close sink)

let test_missing_final_lf () = with_path @@ fun path ->
  let original = {|{"run_id":"resume-run","call_seq":1,"type":"call.started"}|} in
  let oc = open_out path in output_string oc original; close_out oc;
  Alcotest.(check bool) "unterminated JSONL refused" true
    (Result.is_error (Observation.open_sink ~path ~run_id:"resume-run"));
  Alcotest.(check string) "prior bytes preserved" original (get (Secure_fs.read_regular path))

let () = Alcotest.run "provider observations" ["sink", [
  Alcotest.test_case "metadata and resume" `Quick test_metadata_and_resume;
  Alcotest.test_case "allowlist" `Quick test_sanitization;
  Alcotest.test_case "exclusive lock and write failure" `Quick test_lock_and_failure;
  Alcotest.test_case "append path and duplicate safety" `Quick test_append_safety;
  Alcotest.test_case "restart requires final LF" `Quick test_missing_final_lf]]
