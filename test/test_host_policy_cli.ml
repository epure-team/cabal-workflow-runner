let with_fixture ~backend ~read_only f =
  let dir = Filename.temp_file "cwr-host-policy-" "" in
  Sys.remove dir; Unix.mkdir dir 0o700;
  let wf = Filename.concat dir "workflow.json" in
  let marker = Filename.concat dir "dispatch" in
  let cli = Filename.concat dir backend in
  let write path text = let oc = open_out path in output_string oc text; close_out oc in
  write wf (Printf.sprintf {|{"name":"fixture","steps":[{"kind":"agent","id":"probe","agent_type":"%s","read_only":%b,"prompt":"Return JSON"}]}|}
    (if backend="claude" then "claude-code" else backend) read_only);
  write cli ("#!/bin/sh\nif [ \"$1\" = --version ]; then echo '2.1.117'; exit 0; fi\n"
    ^ "printf '%s\\n' \"$@\" > " ^ Filename.quote marker ^ "\n"
    ^ "printf '%s\\n' '{\"type\":\"result\",\"result\":\"{\\\"ok\\\":true}\"}'\n");
  Unix.chmod cli 0o700;
  Fun.protect ~finally:(fun () -> List.iter (fun path -> if Sys.file_exists path then Sys.remove path) [wf;cli;marker]; Unix.rmdir dir)
    (fun () -> f dir wf marker)

let run dir wf flags =
  Sys.command (Printf.sprintf "PATH=%s %s run %s %s >/dev/null 2>&1"
    (Filename.quote (dir ^ ":" ^ Sys.getenv "PATH")) (Filename.quote Sys.argv.(1)) (Filename.quote wf) flags)
let flags = "--read-only-tools Read,Glob,Grep --agent-max-budget-usd 1 --agent-max-turns 8"
let read path = let ic=open_in path in Fun.protect ~finally:(fun () -> close_in ic)
  (fun () -> really_input_string ic (in_channel_length ic))
let test_policy () = with_fixture ~backend:"claude" ~read_only:true @@ fun dir wf marker ->
  Alcotest.(check int) "policy run succeeds" 0 (run dir wf flags);
  let args=String.split_on_char '\n' (read marker) in
  let pairs flag value =
    let rec seek = function a::b::_ when a=flag -> b=value | _::rest -> seek rest | [] -> false in seek args in
  List.iter (fun flag -> Alcotest.(check bool) flag true (List.mem flag args))
    ["--safe-mode";"--restricted";"--strict-mcp-config"];
  List.iter (fun (flag,value) -> Alcotest.(check bool) flag true (pairs flag value))
    ["--tools","Read,Glob,Grep";"--max-budget-usd","1";"--max-turns","8";
     "--permission-mode","dontAsk";"--mcp-config",{|{"mcpServers":{}}|}];
  Alcotest.(check bool) "no bypass" false (List.mem "--dangerously-skip-permissions" args);
  Alcotest.(check bool) "no deny/allow ambiguity" false (List.mem "--disallowedTools" args)

let test_rejected ~backend ~read_only flags = with_fixture ~backend ~read_only @@ fun dir wf marker ->
  Alcotest.(check bool) "run refused" true (run dir wf flags <> 0);
  Alcotest.(check bool) "no provider dispatched" false (Sys.file_exists marker)

let () = Alcotest.run ~argv:[|Sys.argv.(0)|] "host read-only policy" ["CLI", [
  Alcotest.test_case "native bounded toolset" `Quick test_policy;
  Alcotest.test_case "mutable refused" `Quick (fun () -> test_rejected ~backend:"claude" ~read_only:false flags);
  Alcotest.test_case "Codex refused" `Quick (fun () -> test_rejected ~backend:"codex" ~read_only:true flags);
  Alcotest.test_case "unsafe tools refused" `Quick (fun () -> test_rejected ~backend:"claude" ~read_only:true "--read-only-tools Read,Bash --agent-max-budget-usd 1 --agent-max-turns 8");
  Alcotest.test_case "partial policy refused" `Quick (fun () -> test_rejected ~backend:"claude" ~read_only:true "--read-only-tools Read");
  Alcotest.test_case "zero budget refused" `Quick (fun () -> test_rejected ~backend:"claude" ~read_only:true "--read-only-tools Read --agent-max-budget-usd 0 --agent-max-turns 8");
  Alcotest.test_case "NaN budget refused" `Quick (fun () -> test_rejected ~backend:"claude" ~read_only:true "--read-only-tools Read --agent-max-budget-usd nan --agent-max-turns 8");
  Alcotest.test_case "zero turns refused" `Quick (fun () -> test_rejected ~backend:"claude" ~read_only:true "--read-only-tools Read --agent-max-budget-usd 1 --agent-max-turns 0")]]
