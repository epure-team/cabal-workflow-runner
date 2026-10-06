open Cabal_workflow_runner

let contains text needle =
  let rec loop offset =
    offset + String.length needle <= String.length text
    && (String.sub text offset (String.length needle) = needle || loop (offset + 1)) in
  loop 0

let test_flags_and_unknown_failure () =
  let dir = Filename.temp_file "cwr-observation-" "" in
  Sys.remove dir; Unix.mkdir dir 0o700;
  let wf = Filename.concat dir "workflow.json" in
  let out = Filename.concat dir "events.jsonl" in
  let oc = open_out wf in
  output_string oc {|{"name":"metadata","steps":[{"kind":"agent","id":"probe","agent_type":"not-a-backend","read_only":true,"prompt":"PRIVATE_PROMPT_MARKER"}]}|};
  close_out oc;
  let executable = Sys.argv.(1) in
  let command = Printf.sprintf "%s run %s --telemetry-file %s --telemetry-run-id fixture-run >/dev/null 2>&1"
      (Filename.quote executable) (Filename.quote wf) (Filename.quote out) in
  let rc = Sys.command command in
  Alcotest.(check int) "workflow fails, flags accepted" 2 rc;
  let raw = match Secure_fs.read_regular out with Ok raw -> raw | Error e -> Alcotest.fail e in
  Alcotest.(check bool) "no prompt" false
    (contains raw "PRIVATE_PROMPT_MARKER");
  let events = String.split_on_char '\n' raw |> List.filter ((<>) "") |> List.map Yojson.Safe.from_string in
  let open Yojson.Safe.Util in
  Alcotest.(check int) "start and finish" 2 (List.length events);
  Alcotest.(check string) "started" "call.started" (List.hd events |> member "type" |> to_string);
  let finished = List.nth events 1 in
  Alcotest.(check string) "failed" "failed" (finished |> member "outcome" |> to_string);
  Alcotest.(check bool) "usage unknown" true (finished |> member "usage" |> member "input_tokens" = `Null);
  List.iter Sys.remove [wf; out]; Unix.rmdir dir

let test_provider_metadata code =
  let dir = Filename.temp_file "cwr-fake-provider-" "" in
  Sys.remove dir; Unix.mkdir dir 0o700;
  let wf = Filename.concat dir "workflow.json" in
  let out = Filename.concat dir "events.jsonl" in
  let cli = Filename.concat dir "claude" in
  let write path content = let oc = open_out path in output_string oc content; close_out oc in
  write wf {|{"name":"fixture","steps":[{"kind":"agent","id":"probe","agent_type":"claude-code","read_only":true,"model":"haiku","prompt":"PRIVATE_PROMPT_MARKER"}]}|};
  write cli ("#!/bin/sh\nif [ \"$1\" = --version ]; then echo '2.1.117 (Claude Code)'; exit 0; fi\n"
    ^ "case \" $* \" in *' --disallowedTools '*) ;; *) exit 42;; esac\n"
    ^ "printf '%s\\n' '{\"type\":\"result\",\"result\":\"{\\\"ok\\\":true}\",\"session_id\":\"fixture-session\",\"usage\":{\"input_tokens\":0,\"output_tokens\":7,\"cache_read_input_tokens\":3,\"cache_creation_input_tokens\":2}}'\n"
    ^ Printf.sprintf "exit %d\n" code);
  Unix.chmod cli 0o700;
  let command = Printf.sprintf "PATH=%s %s run %s --telemetry-file %s --telemetry-run-id fixture-provider >/dev/null 2>&1"
      (Filename.quote (dir ^ ":" ^ Sys.getenv "PATH")) (Filename.quote Sys.argv.(1)) (Filename.quote wf) (Filename.quote out) in
  let rc = Sys.command command in
  Alcotest.(check int) "workflow outcome" (if code=0 then 0 else 2) rc;
  let raw = match Secure_fs.read_regular out with Ok raw -> raw | Error e -> Alcotest.fail e in
  Alcotest.(check bool) "no prompt" false (contains raw "PRIVATE_PROMPT_MARKER");
  let rows = String.split_on_char '\n' raw |> List.filter ((<>) "") |> List.map Yojson.Safe.from_string in
  let open Yojson.Safe.Util in
  Alcotest.(check int) "two events" 2 (List.length rows);
  let row = List.nth rows 1 in
  Alcotest.(check string) "outcome" (if code=0 then "ok" else "failed") (member "outcome" row |> to_string);
  Alcotest.(check int) "exit preserved" code (member "exit_code" row |> to_int);
  Alcotest.(check string) "session preserved" "fixture-session" (member "session_id" row |> to_string);
  Alcotest.(check int) "zero preserved" 0 (member "usage" row |> member "input_tokens" |> to_int);
  Alcotest.(check int) "output preserved" 7 (member "usage" row |> member "output_tokens" |> to_int);
  Alcotest.(check int) "cache preserved" 3 (member "usage" row |> member "cache_read_tokens" |> to_int);
  Alcotest.(check bool) "elapsed present" true (member "timing" row |> member "duration_ms" <> `Null);
  List.iter Sys.remove [wf; out; cli]; Unix.rmdir dir

let () = Alcotest.run ~argv:[|Sys.argv.(0)|] "observation CLI" ["metadata", [
  Alcotest.test_case "unknown backend" `Quick test_flags_and_unknown_failure;
  Alcotest.test_case "provider success" `Quick (fun () -> test_provider_metadata 0);
  Alcotest.test_case "provider failure" `Quick (fun () -> test_provider_metadata 9)]]
