(* Integration test for examples/proof-carrying-change.workflow.json (Jalon 3, Workstream B).

   Unlike test_pcc.ml (which stubs BOTH run_command and run_agent — pure engine-interpretation
   coverage against hand-written JSON matching arch-index's documented contract), this test uses
   a REAL run_command: pcc-index / pcc-dossier / pcc-preflight / arch-impact / arch-rules run as
   actual subprocesses against a real git+dune fixture, via the same process-execution path
   `cwr run` uses in production (Cwr_runner.Runner.make — see bin/dune). Only run_agent stays
   simulated (table-driven, no LLM/cabal) — but "author" genuinely WRITES to the fixture's files
   on disk before returning its structured summary, so the next real arch-impact/arch-rules call
   sees exactly what a real agent's edit would produce ("fixer" stays pure JSON in every scenario
   here: none of IT1/IT2/IT3 need it to make a further edit — IT1/IT3 converge on iteration 1,
   IT2's fixer never converges by design). An agent stub that only returns {"done":true} without
   touching the filesystem would not exercise this seam at all (see docs/proof-carrying-change.md
   and this milestone's JALON3-rapport.md, "the wrong path").

   REQUIRES a SIBLING arch-index checkout, built (`dune build`), with both its repo root and
   scripts/pcc/ prepended to PATH — see this repo's "pcc-integration" CI job for the exact setup.
   Deliberately NOT part of the default @runtest alias (see test/dune's data_only_dirs +
   this file's own (alias pcc-integration) in test/dune): plain `dune test`/`dune runtest` must
   stay green without arch-index anywhere nearby, same as before this milestone. *)

open Cabal_workflow_runner
open Types

let floor_gates =
  [ "g-computed"; "g-sound"; "g-no-new-findings"; "g-rules-pass"; "g-independent" ]

let pcc_allowlist =
  [ "pcc-index"; "arch-impact"; "arch-rules"; "pcc-dossier"; "pcc-preflight" ]

let required_tools = [ "arch-impact"; "arch-rules"; "pcc-index"; "pcc-dossier"; "pcc-preflight" ]

(* ---- locate arch-index's tools on PATH, or hard-skip LOUDLY (never silently) --------------- *)

let is_executable path =
  (try Unix.access path [ Unix.X_OK ]; true with Unix.Unix_error _ -> false)
  && Sys.file_exists path && not (Sys.is_directory path)

let which name =
  match Sys.getenv_opt "PATH" with
  | None -> None
  | Some path ->
      String.split_on_char ':' path
      |> List.find_map (fun dir ->
             let cand = Filename.concat dir name in
             if is_executable cand then Some cand else None)

let () =
  let missing = List.filter (fun n -> which n = None) required_tools in
  if missing <> [] then begin
    Printf.eprintf
      "\n\
       *** PCC-INTEGRATION-SKIP: %s not found on PATH. ***\n\
       This integration test requires a SIBLING arch-index checkout, built with `dune build`,\n\
       with BOTH its repo root and scripts/pcc/ prepended to PATH (pcc-index/pcc-dossier/\n\
       pcc-preflight resolve arch-impact via `command -v`, then everything else relative to it).\n\
       See this repo's \"pcc-integration\" CI job for the exact setup. Exiting nonzero rather than\n\
       reporting 0 tests run as a pass — a silent skip here would look identical to a green\n\
       integration run and is exactly the failure mode this milestone exists to rule out.\n\n\
       %!"
      (String.concat ", " missing);
    exit 3
  end

(* ---- small test-only shell/filesystem helpers (fixture setup, not the measured pipeline) --- *)

let project_path rel =
  match Sys.getenv_opt "DUNE_SOURCEROOT" with
  | Some root -> Filename.concat root rel
  | None -> if Sys.file_exists rel then rel else Filename.concat ".." rel

let sh dir cmd =
  let full = Printf.sprintf "cd %s && %s" (Filename.quote dir) cmd in
  match Sys.command full with
  | 0 -> ()
  | n -> Alcotest.failf "command failed (exit %d): %s (in %s)" n cmd dir

let git dir args = sh dir ("git " ^ String.concat " " (List.map Filename.quote args))

let rec copy_tree src dst =
  Unix.mkdir dst 0o755;
  Sys.readdir src
  |> Array.iter (fun name ->
         let s = Filename.concat src name and d = Filename.concat dst name in
         if Sys.is_directory s then copy_tree s d
         else
           let ic = open_in_bin s and oc = open_out_bin d in
           let n = in_channel_length ic in
           output_string oc (really_input_string ic n);
           close_in ic; close_out oc)

let write_file path contents =
  let oc = open_out path in
  output_string oc contents; close_out oc

let read_file path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic; s

(* A fresh COPY of test/fixtures/pcc-repo, git-initialized with one commit — NEVER the checked-in
   template itself (the workflow commits/writes into the tree it runs against). *)
let fresh_fixture () =
  let dir = Filename.temp_file "pcc-it-" "" in
  Sys.remove dir;
  copy_tree (project_path "test/fixtures/pcc-repo") dir;
  git dir [ "init"; "-q" ];
  git dir [ "config"; "user.email"; "t@t" ];
  git dir [ "config"; "user.name"; "t" ];
  git dir [ "add"; "-A" ];
  git dir [ "commit"; "-q"; "-m"; "init" ];
  (* the operator writes .pcc/task.md before invoking `cwr run` (docs/proof-carrying-change.md,
     "How author/fixer actually receive context") — real engine file read, not backend-mediated,
     so it must exist on disk before Engine.run, same as test_pcc.ml's ensure_pcc_files. *)
  Unix.mkdir (Filename.concat dir ".pcc") 0o755;
  write_file (Filename.concat dir ".pcc/task.md") "integration test: apply the scenario's change.\n";
  dir

let cleanup dir = sh (Filename.dirname dir) (Printf.sprintf "rm -rf %s" (Filename.quote dir))

(* ---- the three "author" edits under test — REAL file writes, never a JSON-only stub -------- *)

(* IT1: an anodin, compliant addition. Does not touch src/misc.ml's pre-existing decision-lint
   finding, does not call Db.write from src/ui.ml, does not change test_pccfix's assertions. *)
let author_clean_change dir =
  let path = Filename.concat dir "src/ui.ml" in
  write_file path (read_file path ^ "let handle2 (x : int) : int = x + 2\n")

(* IT2: a REAL architecture violation — ui now reaches db, which arch-rules.txt forbids. *)
let author_rule_violation dir =
  let path = Filename.concat dir "src/ui.ml" in
  write_file path "let handle (x : int) : int = Db.write (x + 1)\n"

(* IT3: breaks test_pccfix's "handle 1 = 2" assertion — no rule violation, no new decision
   finding, so every tooled gate before Commit stays clean; only the REAL `dune test` inside
   pcc-preflight can catch this. *)
let author_break_test dir =
  let path = Filename.concat dir "src/ui.ml" in
  write_file path "let handle (x : int) : int = x + 999\n"

(* ---- workflow plumbing (mirrors test_pcc.ml's pattern, duplicated rather than shared: this
   file must build and behave identically whether or not test_pcc.ml exists) ------------------ *)

let load_workflow () =
  match Workflow_json.of_file (project_path "examples/proof-carrying-change.workflow.json") with
  | Ok wf -> wf
  | Error e -> Alcotest.failf "proof-carrying-change.workflow.json failed to parse: %s" e

let validated () =
  match Validate.workflow ~floor_gates (load_workflow ()) with
  | Ok v -> v
  | Error e -> Alcotest.failf "proof-carrying-change.workflow.json failed validation: %s" e

let has_blocked_at ~id trace =
  List.exists (function Blocked_at { id = rid; _ } -> rid = id | _ -> false) trace

let outcome_eq a b = Types.string_of_outcome a = Types.string_of_outcome b

let outcome_testable =
  Alcotest.testable
    (fun fmt o -> Format.pp_print_string fmt (Types.string_of_outcome o))
    outcome_eq

(* Runs the REAL workflow against [fixture_dir]: run_command is Cwr_runner.Runner.make (real
   subprocesses, base = fixture_dir, exactly what `cwr run` builds in production); run_agent is
   table-driven EXCEPT "author", which calls [author_edit fixture_dir] to mutate real files
   before returning its structured summary.

   [Agent]'s [brief]/[protocol] file reads (lib/engine.ml) are NOT base-relative like
   [run_command]'s [working_dir] — they are plain [In_channel.with_open_text] reads against
   whatever the PROCESS's cwd is at the time [Engine.run] executes (see test_pcc.ml's own
   comment on [ensure_pcc_files]). So this chdirs into [fixture_dir] for the duration of the
   run and restores the original cwd afterward — [cleanup] then removes the (no longer current)
   fixture directory safely. *)
let engine_run ~fixture_dir ~author_edit ~fixer_seq
    ?(reviewer = `Assoc [ ("verdict", `String "approved"); ("reason", `String "coherent") ])
    ~token validated =
  let original_cwd = Sys.getcwd () in
  Fun.protect
    ~finally:(fun () -> Sys.chdir original_cwd)
    (fun () ->
      Sys.chdir fixture_dir;
      Eio_main.run (fun env ->
      Eio.Switch.run (fun sw ->
          let run_command = Cwr_runner.Runner.make ~sw ~env ~base:fixture_dir in
          let run_pinned_command = Cwr_runner.Runner.make_pinned ~sw ~env ~base:fixture_dir in
          let calls = Hashtbl.create 8 in
          let run_agent ~id ~prompt:_ ~read_only:_ ~agent_type:_ ~model:_ ~output_schema:_ =
            let n = Option.value ~default:0 (Hashtbl.find_opt calls id) in
            Hashtbl.replace calls id (n + 1);
            match id with
            | "author" ->
                author_edit fixture_dir;
                (true, `Assoc [ ("summary", `String "applied the scenario's fixture change") ])
            | "fixer" ->
                let last = List.length fixer_seq - 1 in
                (true, List.nth fixer_seq (min n last))
            | "reviewer" -> (true, reviewer)
            | _ -> (true, `Assoc [])
          in
          let backend = Backend.stub ~agent:run_agent ~run_command ~run_pinned_command () in
          Engine.run ~run_allowlist:pcc_allowlist ~attestation_session_nonce:"pcc-it-session"
            ~agent_backend_id:"pcc-it-backend" ~sw ~backend ~token validated)))

(* ---- IT1: happy path — Committed, real ledger round-trips --------------------------------- *)

let test_it1_happy_path () =
  let dir = fresh_fixture () in
  Fun.protect ~finally:(fun () -> cleanup dir) (fun () ->
      let fixer_seq = [ `Assoc [ ("progressed", `Bool true); ("done", `Bool true) ] ] in
      let outcome, trace =
        engine_run ~fixture_dir:dir ~author_edit:author_clean_change ~fixer_seq
          ~token:(Some "integration-test-token") (validated ())
      in
      (match outcome with
      | Committed _ -> ()
      | o -> Alcotest.failf "IT1: expected Committed, got %s" (Types.string_of_outcome o));
      Alcotest.(check bool) "IT1: g-independent (reviewer) actually ran" true
        (List.exists (function Agent_ran { id = "reviewer"; _ } -> true | _ -> false) trace);
      let ledger = Ledger.to_ndjson trace in
      match Ledger.of_ndjson ledger with
      | Error e -> Alcotest.failf "IT1: real-run ledger did not round-trip: %s" e
      | Ok replay_trace ->
          let replayed =
            Eio_main.run (fun _env ->
                Eio.Switch.run (fun sw ->
                    Engine.replay ~attestation_session_nonce:"pcc-it-session" ~sw
                      ~trace:replay_trace (validated ())))
          in
          Alcotest.(check outcome_testable)
            "IT1: replay of the REAL run's ledger reproduces the same terminal outcome" outcome
            replayed)

(* ---- IT2: a real architecture-rules violation — Blocked at g-rules-pass, specifically ------ *)

let test_it2_rules_violation () =
  let dir = fresh_fixture () in
  Fun.protect ~finally:(fun () -> cleanup dir) (fun () ->
      (* progressed:true keeps the Fixpoint governor from firing early (mirrors test_pcc.ml's
         T2): done:false forever means only Max_iters(4) can stop the loop, so this scenario
         drives 4 real reindex/impact/rules/dossier iterations before the post-loop pass. *)
      let fixer_seq = [ `Assoc [ ("progressed", `Bool true); ("done", `Bool false) ] ] in
      let outcome, trace =
        engine_run ~fixture_dir:dir ~author_edit:author_rule_violation ~fixer_seq
          ~token:(Some "integration-test-token") (validated ())
      in
      (match outcome with
      | Blocked _ -> ()
      | o -> Alcotest.failf "IT2: expected Blocked, got %s" (Types.string_of_outcome o));
      Alcotest.(check bool) "IT2: blocked at g-rules-pass, specifically" true
        (has_blocked_at ~id:"g-rules-pass" trace);
      Alcotest.(check bool) "IT2: g-computed was NOT the block point (only rules failed)" false
        (has_blocked_at ~id:"g-computed" trace);
      Alcotest.(check bool) "IT2: g-sound was NOT the block point (contract_ok stayed true)" false
        (has_blocked_at ~id:"g-sound" trace);
      Alcotest.(check bool) "IT2: reviewer/commit never reached" false
        (List.exists (function Agent_ran { id = "reviewer"; _ } -> true | _ -> false) trace))

(* ---- IT3: preflight (real `dune test`) catches a real regression --------------------------- *)

let test_it3_preflight_fails () =
  let dir = fresh_fixture () in
  Fun.protect ~finally:(fun () -> cleanup dir) (fun () ->
      let fixer_seq = [ `Assoc [ ("progressed", `Bool true); ("done", `Bool true) ] ] in
      let outcome, trace =
        engine_run ~fixture_dir:dir ~author_edit:author_break_test ~fixer_seq
          ~token:(Some "integration-test-token") (validated ())
      in
      (match outcome with
      | Blocked _ -> ()
      | o -> Alcotest.failf "IT3: expected Blocked, got %s" (Types.string_of_outcome o));
      Alcotest.(check bool) "IT3: blocked at the commit step (real preflight failed)" true
        (has_blocked_at ~id:"submit" trace);
      Alcotest.(check bool)
        "IT3: g-independent passed first (every tooled gate was clean — only the real test \
         suite caught this)"
        false (has_blocked_at ~id:"g-independent" trace);
      Alcotest.(check bool) "IT3: g-rules-pass passed (no architecture violation here)" false
        (has_blocked_at ~id:"g-rules-pass" trace))

let () =
  Alcotest.run "proof-carrying-change (real subprocesses)"
    [ ( "scenarios",
        [ Alcotest.test_case "IT1 happy path -> Committed, real ledger replays" `Quick
            test_it1_happy_path;
          Alcotest.test_case "IT2 real arch-rules violation -> Blocked g-rules-pass" `Quick
            test_it2_rules_violation;
          Alcotest.test_case "IT3 real dune-test regression -> Blocked at commit (preflight)"
            `Quick test_it3_preflight_fails
        ] ) ]
