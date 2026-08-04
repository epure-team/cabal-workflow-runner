(* Scenarios T1-T8 for examples/proof-carrying-change.workflow.json (WR-02, Workstream B).
   No backend dependency: agents and Run commands are entirely stubbed, so these tests exercise
   only the engine's interpretation of the workflow against arch-index's documented JSON
   contract (docs/change-impact.md, docs/fitness-functions.md in epure-team/arch-index) — never
   a real subprocess, never a real LLM. *)

open Cabal_workflow_runner
open Types

let project_path rel =
  match Sys.getenv_opt "DUNE_SOURCEROOT" with
  | Some root -> Filename.concat root rel
  | None -> if Sys.file_exists rel then rel else Filename.concat ".." rel

let floor_gates =
  [ "g-computed"; "g-sound"; "g-no-new-findings"; "g-rules-pass"; "g-independent" ]

let pcc_allowlist =
  [ "pcc-index"; "pcc-baseline"; "arch-impact"; "arch-rules"; "pcc-preflight" ]

let load_workflow () =
  match Workflow_json.of_file (project_path "examples/proof-carrying-change.workflow.json") with
  | Ok wf -> wf
  | Error e -> Alcotest.failf "proof-carrying-change.workflow.json failed to parse: %s" e

let validated () =
  match Validate.workflow ~floor_gates (load_workflow ()) with
  | Ok v -> v
  | Error e -> Alcotest.failf "proof-carrying-change.workflow.json failed validation: %s" e

let outcome_eq a b = Types.string_of_outcome a = Types.string_of_outcome b

let outcome_testable =
  Alcotest.testable
    (fun fmt o -> Format.pp_print_string fmt (Types.string_of_outcome o))
    outcome_eq

(* The reviewer step has structured [input] (read-only, selecting predecessor context), which
   requires both a runtime agent-backend id and a fresh attestation session nonce — see
   lib/engine.ml's [structured Agent requires runtime agent_backend_id / a fresh runtime
   session nonce]. Neither is workflow-file-supplied; both are operator/runtime-only, mirroring
   the token. *)
let engine_run ?(run_allowlist = pcc_allowlist) ~backend ~token validated =
  Eio_main.run (fun _env ->
    Eio.Switch.run (fun sw ->
      Engine.run ~run_allowlist ~attestation_session_nonce:"test-session-nonce"
        ~agent_backend_id:"pcc-test-backend" ~sw ~backend ~token validated))

let engine_replay ~trace validated =
  Eio_main.run (fun _env ->
    Eio.Switch.run (fun sw ->
      Engine.replay ~attestation_session_nonce:"test-session-nonce" ~sw ~trace validated))

(* ---- stub JSON matching arch-index's --format json machine-output contract ---- *)

let jstr s = `String s
let jint n = `Int n
let jbool b = `Bool b

let index_ok functions =
  `Assoc [ ("computed", jbool true); ("functions", jint functions); ("contract_ok", jbool true) ]

let baseline_ok findings =
  `Assoc [ ("computed", jbool true); ("findings", jint findings) ]

let impact_json ~computed ~contract_ok ~verdict ~new_findings =
  `Assoc
    [ ("computed", jbool computed); ("contract_ok", jbool contract_ok);
      ("verdict", jstr verdict); ("new_findings", jint new_findings) ]

let rules_json ~computed ~contract_ok ~verdict ~failing =
  `Assoc
    [ ("computed", jbool computed); ("contract_ok", jbool contract_ok);
      ("verdict", jstr verdict); ("failing", jint failing) ]

let preflight_ok tests = `Assoc [ ("ok", jbool true); ("tests", jint tests) ]

let run_result_of ?(exit = 0) json : run_result =
  { exit; stdout = Yojson.Safe.to_string json; stderr = ""; truncated = false; files = [] }

(* A run_command backend keyed by step id, with a call counter per id so a scenario can supply
   a call log (T8's "no subprocess on replay" assertion) or per-iteration variation. *)
let run_command_stub ?(calls = Hashtbl.create 8) table
    ~id ~argv:_ ~working_dir:_ ~timeout_ms:_ ~observe:_ ~stdin_content:_ : run_result =
  Hashtbl.replace calls id (1 + Option.value ~default:0 (Hashtbl.find_opt calls id));
  match List.assoc_opt id table with
  | Some result -> result
  | None -> { exit = 0; stdout = "{}"; stderr = ""; truncated = false; files = [] }

(* An agent stub: "author" and "reviewer" return a fixed answer; "fixer" consumes one entry
   per call from [fixer_seq], repeating the last entry once exhausted (so max_iters can run past
   a short sequence without raising). *)
let agent_stub ?(fixer_seq = [ `Assoc [ ("progressed", jbool true); ("done", jbool true) ] ])
    ?(reviewer = `Assoc [ ("verdict", jstr "approved"); ("reason", jstr "coherent") ])
    ?(author = `Assoc [ ("summary", jstr "renamed a function") ]) ?(calls = Hashtbl.create 8)
    () ~id ~prompt:_ ~read_only:_ ~agent_type:_ ~model:_ =
  let n = Option.value ~default:0 (Hashtbl.find_opt calls id) in
  Hashtbl.replace calls id (n + 1);
  match id with
  | "author" -> (true, author)
  | "fixer" ->
      let last = List.length fixer_seq - 1 in
      (true, List.nth fixer_seq (min n last))
  | "reviewer" -> (true, reviewer)
  | _ -> (true, `Assoc [])

let backend ?fixer_seq ?reviewer ?author ?agent_calls ?run_calls run_table =
  Backend.stub
    ~agent:(agent_stub ?fixer_seq ?reviewer ?author ?calls:agent_calls ())
    ~run_command:(run_command_stub ?calls:run_calls run_table)
    ()

let has_blocked_at ~id trace =
  List.exists (function Blocked_at { id = rid; _ } -> rid = id | _ -> false) trace

let ran_agent id trace =
  List.exists (function Agent_ran { id = rid; _ } -> rid = id | _ -> false) trace

(* substring test (no Str/Re dependency needed in this test exe). *)
let contains_substring hay needle =
  let nh = String.length hay and nn = String.length needle in
  let rec at i = i + nn <= nh && (String.sub hay i nn = needle || at (i + 1)) in
  nn = 0 || at 0

(* ---- T1: nominal path — outcome = Committed, ledger replays byte-identically without
   re-executing anything ---- *)

let test_t1_nominal () =
  let run_calls = Hashtbl.create 8 and agent_calls = Hashtbl.create 8 in
  let table =
    [ ("index", run_result_of (index_ok 42)); ("baseline", run_result_of (baseline_ok 0));
      ("reindex", run_result_of (index_ok 42));
      ("impact",
       run_result_of (impact_json ~computed:true ~contract_ok:true ~verdict:"pass" ~new_findings:0));
      ("rules",
       run_result_of (rules_json ~computed:true ~contract_ok:true ~verdict:"pass" ~failing:0));
      ("submit", run_result_of (preflight_ok 12)) ]
  in
  let b = backend ~run_calls ~agent_calls table in
  let outcome, trace = engine_run ~backend:b ~token:(Some "high-entropy-test-token") (validated ()) in
  (match outcome with
  | Committed _ -> ()
  | o -> Alcotest.failf "T1: expected Committed, got %s" (Types.string_of_outcome o));
  Alcotest.(check int) "T1: fixer ran exactly once (converged on iteration 1)" 1
    (Option.value ~default:0 (Hashtbl.find_opt agent_calls "fixer"));
  let ledger = Ledger.to_ndjson trace in
  match Ledger.of_ndjson ledger with
  | Error e -> Alcotest.failf "T1: ledger did not round-trip: %s" e
  | Ok replay_trace ->
      let before_index = Option.value ~default:0 (Hashtbl.find_opt run_calls "index") in
      let replayed = engine_replay ~trace:replay_trace (validated ()) in
      Alcotest.(check outcome_testable) "T1: replay is byte-identical" outcome replayed;
      Alcotest.(check int) "T1: replay executed NO subprocess" before_index
        (Option.value ~default:0 (Hashtbl.find_opt run_calls "index"))

(* ---- T2: new_findings > 0 on every iteration — the fixer never converges, the loop stops on
   its max_iters governor, and g-no-new-findings blocks (named explicitly, not a crash) ---- *)

let test_t2_findings_persist () =
  let table =
    [ ("index", run_result_of (index_ok 10)); ("baseline", run_result_of (baseline_ok 0));
      ("reindex", run_result_of (index_ok 10));
      ("impact",
       run_result_of (impact_json ~computed:true ~contract_ok:true ~verdict:"fail" ~new_findings:1));
      ("rules",
       run_result_of (rules_json ~computed:true ~contract_ok:true ~verdict:"pass" ~failing:0)) ]
  in
  (* progressed=true every time (so Fixpoint never fires) and done=false always, so only
     Max_iters(4) can stop the loop. *)
  let fixer_seq = [ `Assoc [ ("progressed", jbool true); ("done", jbool false) ] ] in
  let agent_calls = Hashtbl.create 8 in
  let b = backend ~fixer_seq ~agent_calls table in
  let outcome, trace = engine_run ~backend:b ~token:(Some "tok") (validated ()) in
  (match outcome with
  | Blocked _ -> ()
  | o -> Alcotest.failf "T2: expected Blocked, got %s" (Types.string_of_outcome o));
  Alcotest.(check bool) "T2: blocked at g-no-new-findings, not some other gate" true
    (has_blocked_at ~id:"g-no-new-findings" trace);
  Alcotest.(check bool) "T2: g-computed and g-sound were NOT the block point" false
    (has_blocked_at ~id:"g-computed" trace || has_blocked_at ~id:"g-sound" trace);
  Alcotest.(check bool) "T2: the loop ran to its Max_iters governor, not once"
    true
    (4 <= Option.value ~default:0 (Hashtbl.find_opt agent_calls "fixer"));
  Alcotest.(check bool) "T2: reviewer/commit never reached" false (ran_agent "reviewer" trace)

(* ---- T3: index carries no decision analysis — arch-impact refuses (computed:false); Blocked
   on g-computed specifically, never a crash, never a silent pass ---- *)

let test_t3_no_decision_analysis () =
  let table =
    [ ("index", run_result_of (index_ok 5)); ("baseline", run_result_of (baseline_ok 0));
      ("reindex", run_result_of (index_ok 5));
      (* arch-impact prints its JSON THEN exits 3 on --fail-on-new-findings refusal — the Run
         step binds it regardless of exit code (cwr SPEC.md: a Run's result is always bound; the
         Gate is the only line of defense). *)
      ("impact",
       run_result_of ~exit:3
         (impact_json ~computed:false ~contract_ok:true ~verdict:"refused" ~new_findings:0));
      ("rules",
       run_result_of (rules_json ~computed:true ~contract_ok:true ~verdict:"pass" ~failing:0)) ]
  in
  let fixer_seq = [ `Assoc [ ("progressed", jbool false); ("done", jbool true) ] ] in
  let b = backend ~fixer_seq table in
  let outcome, trace = engine_run ~backend:b ~token:(Some "tok") (validated ()) in
  (match outcome with
  | Blocked _ -> ()
  | o -> Alcotest.failf "T3: expected Blocked, got %s" (Types.string_of_outcome o));
  Alcotest.(check bool) "T3: blocked specifically at g-computed" true
    (has_blocked_at ~id:"g-computed" trace);
  (* the Run step itself must have executed cleanly (Run_executed, not a crash/Abort) *)
  let impact_ran =
    List.exists
      (function Run_executed { id = "impact"; parsed = Some _; _ } -> true | _ -> false)
      trace
  in
  Alcotest.(check bool) "T3: impact's refusal was a clean Run_executed, not an Abort" true
    impact_ran

(* ---- T4: index is not (contract) sound — Blocked on g-sound ---- *)

let test_t4_not_sound () =
  let table =
    [ ("index", run_result_of (index_ok 5)); ("baseline", run_result_of (baseline_ok 0));
      ("reindex", run_result_of (index_ok 5));
      ("impact",
       run_result_of
         (impact_json ~computed:true ~contract_ok:false ~verdict:"pass" ~new_findings:0));
      ("rules",
       run_result_of (rules_json ~computed:true ~contract_ok:false ~verdict:"pass" ~failing:0)) ]
  in
  let fixer_seq = [ `Assoc [ ("progressed", jbool false); ("done", jbool true) ] ] in
  let b = backend ~fixer_seq table in
  let outcome, trace = engine_run ~backend:b ~token:(Some "tok") (validated ()) in
  (match outcome with
  | Blocked _ -> ()
  | o -> Alcotest.failf "T4: expected Blocked, got %s" (Types.string_of_outcome o));
  Alcotest.(check bool) "T4: blocked specifically at g-sound" true (has_blocked_at ~id:"g-sound" trace);
  Alcotest.(check bool) "T4: g-computed passed first" false (has_blocked_at ~id:"g-computed" trace)

(* ---- T5: every tooled gate passes, but the reviewer rejects — Blocked on g-independent ---- *)

let test_t5_reviewer_rejects () =
  let table =
    [ ("index", run_result_of (index_ok 5)); ("baseline", run_result_of (baseline_ok 0));
      ("reindex", run_result_of (index_ok 5));
      ("impact",
       run_result_of (impact_json ~computed:true ~contract_ok:true ~verdict:"pass" ~new_findings:0));
      ("rules",
       run_result_of (rules_json ~computed:true ~contract_ok:true ~verdict:"pass" ~failing:0)) ]
  in
  let reviewer = `Assoc [ ("verdict", jstr "rejected"); ("reason", jstr "not convinced") ] in
  let b = backend ~reviewer table in
  let outcome, trace = engine_run ~backend:b ~token:(Some "tok") (validated ()) in
  (match outcome with
  | Blocked _ -> ()
  | o -> Alcotest.failf "T5: expected Blocked, got %s" (Types.string_of_outcome o));
  Alcotest.(check bool) "T5: blocked specifically at g-independent" true
    (has_blocked_at ~id:"g-independent" trace);
  Alcotest.(check bool) "T5: reviewer DID run (input was well-formed)" true
    (ran_agent "reviewer" trace)

(* ---- T6: everything approved, but no runtime token — Commit blocks; nobody else can approve
   on its behalf ---- *)

let test_t6_no_token () =
  let table =
    [ ("index", run_result_of (index_ok 5)); ("baseline", run_result_of (baseline_ok 0));
      ("reindex", run_result_of (index_ok 5));
      ("impact",
       run_result_of (impact_json ~computed:true ~contract_ok:true ~verdict:"pass" ~new_findings:0));
      ("rules",
       run_result_of (rules_json ~computed:true ~contract_ok:true ~verdict:"pass" ~failing:0));
      ("submit", run_result_of (preflight_ok 12)) ]
  in
  let b = backend table in
  let outcome, trace = engine_run ~backend:b ~token:None (validated ()) in
  (match outcome with
  | Blocked _ -> ()
  | o -> Alcotest.failf "T6: expected Blocked, got %s" (Types.string_of_outcome o));
  Alcotest.(check bool) "T6: blocked specifically at the commit step" true
    (has_blocked_at ~id:"submit" trace);
  Alcotest.(check bool) "T6: g-independent passed (reviewer did approve)" false
    (has_blocked_at ~id:"g-independent" trace)

(* ---- T7: pcc-index prints a float where the schema declares int — the run aborts, never a
   silent green. [VÉRIFIER] (see final report): cwr's Run-stdout path rejects floats/Intlit
   UNCONDITIONALLY via Canonical_json.to_string inside parse_run_stdout (engine.ml ~line 124),
   before the declared Schema.validate type check ever runs — this is stronger than SPEC.md §
   "Authenticated export trust boundary" implies ("Floats and Intlit are rejected only for
   attestation-bearing workflows..."), which describes a DIFFERENT, additional restriction on
   Attest's own selected/signed values. A workflow with no Attest step still gets this rejection
   on every structured Run/Commit-preflight stdout. Confirmed empirically: the actual Aborted
   reason is "... $.functions: floats are not canonical; use an integer", not a
   "schema mismatch: functions" — arch-index's own float-free JSON contract (arch-index PR
   feat(json): machine-output contract) was the right call independent of this. ---- *)

let test_t7_float_in_json () =
  let bad_index : run_result =
    { exit = 0;
      stdout = {|{"computed":true,"functions":10.5,"contract_ok":true}|};
      stderr = ""; truncated = false; files = [] }
  in
  let table = [ ("index", bad_index) ] in
  let b = backend table in
  let outcome, trace = engine_run ~backend:b ~token:(Some "tok") (validated ()) in
  (match outcome with
  | Aborted reason ->
      Alcotest.(check bool) "T7: abort names the offending field and rejects the float" true
        (contains_substring reason "functions" && contains_substring reason "not canonical")
  | o -> Alcotest.failf "T7: expected Aborted on a float where int is required, got %s"
      (Types.string_of_outcome o));
  Alcotest.(check bool) "T7: the walk never reached baseline/author" false
    (ran_agent "author" trace)

(* ---- T8: replay of T1 and T2 from a serialised ledger is byte-identical, and executes no
   subprocess — see per-scenario ledger checks above (T1); this scenario re-confirms it holds for
   the Blocked outcome shape too (T2), not just Committed. ---- *)

let test_t8_replay_t2_from_ledger () =
  let run_calls = Hashtbl.create 8 in
  let table =
    [ ("index", run_result_of (index_ok 10)); ("baseline", run_result_of (baseline_ok 0));
      ("reindex", run_result_of (index_ok 10));
      ("impact",
       run_result_of (impact_json ~computed:true ~contract_ok:true ~verdict:"fail" ~new_findings:1));
      ("rules",
       run_result_of (rules_json ~computed:true ~contract_ok:true ~verdict:"pass" ~failing:0)) ]
  in
  let fixer_seq = [ `Assoc [ ("progressed", jbool true); ("done", jbool false) ] ] in
  let b = backend ~fixer_seq ~run_calls table in
  let outcome, trace = engine_run ~backend:b ~token:(Some "tok") (validated ()) in
  (match outcome with Blocked _ -> () | o ->
    Alcotest.failf "T8 setup: expected Blocked (see T2), got %s" (Types.string_of_outcome o));
  let ledger = Ledger.to_ndjson trace in
  match Ledger.of_ndjson ledger with
  | Error e -> Alcotest.failf "T8: ledger did not round-trip: %s" e
  | Ok replay_trace ->
      let before = Option.value ~default:0 (Hashtbl.find_opt run_calls "impact") in
      let replayed = engine_replay ~trace:replay_trace (validated ()) in
      Alcotest.(check outcome_testable) "T8: replay of a Blocked run is byte-identical" outcome
        replayed;
      Alcotest.(check int) "T8: replay executed NO subprocess" before
        (Option.value ~default:0 (Hashtbl.find_opt run_calls "impact"))

let () =
  Alcotest.run "proof-carrying-change"
    [ ( "workflow",
        [ Alcotest.test_case "lints/validates against the 5 floor gates" `Quick (fun () ->
              ignore (validated ())) ] );
      ( "scenarios",
        [ Alcotest.test_case "T1 nominal path -> Committed, replay byte-identical" `Quick
            test_t1_nominal;
          Alcotest.test_case "T2 persistent findings -> governor stop, Blocked g-no-new-findings"
            `Quick test_t2_findings_persist;
          Alcotest.test_case "T3 no decision analysis -> Blocked g-computed, not a crash" `Quick
            test_t3_no_decision_analysis;
          Alcotest.test_case "T4 not ⊤-marked -> Blocked g-sound" `Quick test_t4_not_sound;
          Alcotest.test_case "T5 reviewer rejects -> Blocked g-independent" `Quick
            test_t5_reviewer_rejects;
          Alcotest.test_case "T6 no token -> Blocked at commit" `Quick test_t6_no_token;
          Alcotest.test_case "T7 float where int required -> Aborted, not a silent pass" `Quick
            test_t7_float_in_json;
          Alcotest.test_case "T8 replay of a Blocked (T2) run from ledger" `Quick
            test_t8_replay_t2_from_ledger
        ] ) ]
