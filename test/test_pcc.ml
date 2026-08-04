(* Scenarios for examples/proof-carrying-change.workflow.json (WR-02, Workstream B).
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
  [ "pcc-index"; "arch-impact"; "arch-rules"; "pcc-dossier"; "pcc-preflight" ]

(* [author]/[fixer]'s [brief] is a REAL file read done by the engine itself
   (lib/engine.ml: [In_channel.with_open_text p In_channel.input_all]), resolved against the
   process's actual cwd — it is NOT mediated by [Backend.t] the way [run_agent]/[run_command] are,
   so no stub can fake it. In a live run these files are produced by real steps (the operator
   writes .pcc/task.md before invoking `cwr run`; the "dossier" Run step writes .pcc/dossier.md
   each iteration) — here they must simply exist on disk before any scenario that reaches
   author/fixer runs. Created once, idempotently, relative to wherever this test binary's cwd
   happens to be (the dune sandbox or repo root) — same directory the workflow's own relative
   ".pcc/..." paths resolve against when the process is launched from the project root. *)
let ensure_pcc_files () =
  if not (Sys.file_exists ".pcc") then Unix.mkdir ".pcc" 0o755 ;
  let write_if_absent path contents =
    if not (Sys.file_exists path) then Out_channel.with_open_text path (fun oc ->
      Out_channel.output_string oc contents)
  in
  write_if_absent ".pcc/task.md" "test fixture: apply a trivial change.\n" ;
  write_if_absent ".pcc/dossier.md" "test fixture: no findings.\n"

let read_file path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic ; s

let raw_workflow_json () =
  Yojson.Safe.from_string
    (read_file (project_path "examples/proof-carrying-change.workflow.json"))

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

(* The real arch-impact ALWAYS prints root "computed":true when it prints an object at all — the
   whole object, including a --fail-on-new-findings refusal, is assembled and printed BEFORE the
   refusal check runs (bin/arch_impact/arch_impact.ml: the JSON block precedes the
   --fail-on-new-findings block). So root `computed` can never distinguish a refusal from a clean
   run; only `verdict` (and, for the refusal specifically, `findings.computed`) can. This helper
   makes that impossible to get wrong by construction: there is no `~computed` parameter to set to
   `false` by mistake — see the workflow's g-computed fix (round-1 review) for what this cost when
   a test stub encoded the wrong shape. *)
let impact_json ?(findings_computed = true) ~contract_ok ~verdict ~new_findings () =
  `Assoc
    [ ("computed", jbool true); ("contract_ok", jbool contract_ok);
      ("verdict", jstr verdict); ("new_findings", jint new_findings);
      ("findings",
       `Assoc
         [ ("computed", jbool findings_computed);
           ("reason",
            if findings_computed then `Null
            else jstr "no decision analysis in this index — absence of data, not absence of findings") ]) ]

let rules_json ~computed ~contract_ok ~verdict ~failing =
  `Assoc
    [ ("computed", jbool computed); ("contract_ok", jbool contract_ok);
      ("verdict", jstr verdict); ("failing", jint failing) ]

let preflight_ok tests = `Assoc [ ("ok", jbool true); ("tests", jint tests) ]

let run_result_of ?(exit = 0) json : run_result =
  { exit; stdout = Yojson.Safe.to_string json; stderr = ""; truncated = false; files = [] }

let empty_result : run_result =
  { exit = 0; stdout = ""; stderr = ""; truncated = false; files = [] }

(* The workflow's default "everything is clean" answer for each Run step id, overridable per
   scenario. round-2 review (F1): impact-final/rules-final are the POST-LOOP re-verification pass
   the gates actually read from — they are deliberately separate parameters from impact/rules (the
   in-loop pass the fixer sees via its dossier), so a scenario can make them disagree, exactly as
   a fixer's un-reindexed last edit could in a real run. *)
let default_table
    ?(index = index_ok 10) ?(reindex = index_ok 10)
    ?(impact = impact_json ~contract_ok:true ~verdict:"pass" ~new_findings:0 ())
    ?(rules = rules_json ~computed:true ~contract_ok:true ~verdict:"pass" ~failing:0)
    ?(reindex_final = index_ok 10)
    ?(impact_final = impact_json ~contract_ok:true ~verdict:"pass" ~new_findings:0 ())
    ?(rules_final = rules_json ~computed:true ~contract_ok:true ~verdict:"pass" ~failing:0)
    ?(preflight = Some (preflight_ok 12)) () =
  [ ("index", run_result_of index); ("reindex", run_result_of reindex);
    ("impact", run_result_of impact); ("rules", run_result_of rules);
    ("dossier", empty_result);
    ("reindex-final", run_result_of reindex_final);
    ("impact-final", run_result_of impact_final); ("rules-final", run_result_of rules_final) ]
  @ (match preflight with None -> [] | Some p -> [ ("submit", run_result_of p) ])

(* A run_command backend keyed by step id, with a call counter per id so a scenario can supply
   a call log (byte-identical-replay assertions, F5's per-iteration variation) or per-iteration
   variation via [run_seq]: a table mapping id -> response LIST, consumed one per call and
   repeating the last entry once exhausted (mirrors [fixer_seq] below for Agent steps). *)
let run_command_stub ?(calls = Hashtbl.create 8) ?(run_seq = []) table
    ~id ~argv:_ ~working_dir:_ ~timeout_ms:_ ~observe:_ ~stdin_content:_ : run_result =
  let n = Option.value ~default:0 (Hashtbl.find_opt calls id) in
  Hashtbl.replace calls id (n + 1);
  match List.assoc_opt id run_seq with
  | Some seq when seq <> [] -> List.nth seq (min n (List.length seq - 1))
  | _ -> ( match List.assoc_opt id table with
      | Some result -> result
      | None -> { exit = 0; stdout = "{}"; stderr = ""; truncated = false; files = [] })

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

let backend ?fixer_seq ?reviewer ?author ?agent_calls ?run_calls ?run_seq run_table =
  Backend.stub
    ~agent:(agent_stub ?fixer_seq ?reviewer ?author ?calls:agent_calls ())
    ~run_command:(run_command_stub ?calls:run_calls ?run_seq run_table)
    ()

(* Removes the top-level gate step with the given id from the workflow's raw JSON, so
   test_each_floor_gate_is_load_bearing can confirm the validator actually depends on it — rather
   than asserting a claim about the validator's behavior without exercising it. *)
let workflow_json_without_gate ~id =
  match raw_workflow_json () with
  | `Assoc fields ->
      let steps =
        match List.assoc "steps" fields with `List l -> l | _ -> assert false
      in
      let is_target = function
        | `Assoc step_fields ->
            List.assoc_opt "kind" step_fields = Some (`String "gate")
            && List.assoc_opt "id" step_fields = Some (`String id)
        | _ -> false
      in
      let steps' = List.filter (fun s -> not (is_target s)) steps in
      Alcotest.(check bool)
        (Printf.sprintf "gate %S was present to remove" id)
        true (List.length steps' = List.length steps - 1) ;
      `Assoc (List.map (fun (k, v) -> if k = "steps" then (k, `List steps') else (k, v)) fields)
  | _ -> Alcotest.fail "workflow root is not a JSON object"

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
  let table = default_table () in
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
      (* The byte-identical guarantee is Engine.replay succeeding at all — it raises
         Replay_mismatch on any structural divergence (a trace entry out of order, ill-typed, or
         left over after the walk completes). This outcome_testable check is a coarser,
         additional confirmation that the terminal outcome STRING also matches; it does not by
         itself prove byte-identity (two structurally different traces could share an outcome
         string), so don't read its name as the whole proof. *)
      let replayed = engine_replay ~trace:replay_trace (validated ()) in
      Alcotest.(check outcome_testable) "T1: replay reproduces the same terminal outcome" outcome
        replayed;
      Alcotest.(check int) "T1: replay executed NO subprocess" before_index
        (Option.value ~default:0 (Hashtbl.find_opt run_calls "index"))

(* ---- T2: new_findings > 0 on every iteration — the fixer never converges, the loop stops on
   its max_iters governor, and g-computed blocks (round-1 review fix: a non-"pass" verdict —
   "fail" here, "refused" in T3 — is exactly what g-computed's impact-final.parsed.verdict
   conjunct catches; g-no-new-findings never gets a turn to fire on THIS scenario, because
   g-computed is strictly upstream of it and arch-impact's own verdict already reflects
   new_findings > 0 given the workflow's fixed --fail-on-new-findings invocation. See T2b for a
   scenario that actually exercises g-no-new-findings as a distinct defense.) The post-loop pass
   sees the SAME persistent problem as the in-loop one (this is a structural property of the
   diff, unaffected by the fixer's inability to fix it), so impact_final mirrors impact. ---- *)

let test_t2_findings_persist () =
  let dirty = impact_json ~contract_ok:true ~verdict:"fail" ~new_findings:1 () in
  let table = default_table ~impact:dirty ~impact_final:dirty () in
  (* progressed=true every time (so Fixpoint never fires) and done=false always, so only
     Max_iters(4) can stop the loop. *)
  let fixer_seq = [ `Assoc [ ("progressed", jbool true); ("done", jbool false) ] ] in
  let agent_calls = Hashtbl.create 8 in
  let b = backend ~fixer_seq ~agent_calls table in
  let outcome, trace = engine_run ~backend:b ~token:(Some "tok") (validated ()) in
  (match outcome with
  | Blocked _ -> ()
  | o -> Alcotest.failf "T2: expected Blocked, got %s" (Types.string_of_outcome o));
  Alcotest.(check bool) "T2: blocked at g-computed (verdict != pass), not some other gate" true
    (has_blocked_at ~id:"g-computed" trace);
  Alcotest.(check bool) "T2: g-sound was NOT the block point" false (has_blocked_at ~id:"g-sound" trace);
  Alcotest.(check bool) "T2: the loop ran to its Max_iters governor, not once"
    true
    (4 <= Option.value ~default:0 (Hashtbl.find_opt agent_calls "fixer"));
  Alcotest.(check bool) "T2: reviewer/commit never reached" false (ran_agent "reviewer" trace)

(* ---- T2b: defense-in-depth for g-no-new-findings. arch-impact's real code cannot actually
   produce a "pass" verdict alongside new_findings > 0 — the two are computed from the same `decs`
   list (see arch_impact.ml) — so this scenario is deliberately SYNTHETIC: it feeds the engine an
   inconsistent stub (a hypothetical arch-impact regression, or a tampered/forged Run result) to
   prove g-no-new-findings still catches what g-computed's verdict check would, by construction,
   miss. This is the concrete justification for keeping BOTH gates. ---- *)

let test_t2b_belt_and_braces_no_new_findings () =
  (* verdict LIES "pass", but new_findings says otherwise — not a real arch-impact output. *)
  let lying = impact_json ~contract_ok:true ~verdict:"pass" ~new_findings:1 () in
  let table = default_table ~impact_final:lying () in
  let outcome, trace =
    engine_run ~backend:(backend table) ~token:(Some "tok") (validated ())
  in
  (match outcome with
  | Blocked _ -> ()
  | o -> Alcotest.failf "T2b: expected Blocked, got %s" (Types.string_of_outcome o));
  Alcotest.(check bool) "T2b: g-computed passed (verdict says pass — that's the point)" false
    (has_blocked_at ~id:"g-computed" trace);
  Alcotest.(check bool) "T2b: g-no-new-findings caught what verdict alone would have missed" true
    (has_blocked_at ~id:"g-no-new-findings" trace);
  Alcotest.(check bool) "T2b: reviewer/commit never reached" false (ran_agent "reviewer" trace)

(* ---- T3: index carries no decision analysis — arch-impact refuses. This is a structural
   property of the index (no decisions table), unaffected by the fixer's edits, so impact_final
   mirrors impact: both the in-loop and post-loop pass see the same refusal. Blocked on
   g-computed specifically, never a crash, never a silent pass. ---- *)

let test_t3_no_decision_analysis () =
  (* arch-impact prints its JSON THEN exits 3 on --fail-on-new-findings refusal — the Run step
     binds it regardless of exit code (cwr SPEC.md: a Run's result is always bound; the Gate is
     the only line of defense). *)
  let refused =
    impact_json ~findings_computed:false ~contract_ok:true ~verdict:"refused" ~new_findings:0 ()
  in
  let table = default_table ~impact:refused ~impact_final:refused () in
  let fixer_seq = [ `Assoc [ ("progressed", jbool false); ("done", jbool true) ] ] in
  let b = backend ~fixer_seq table in
  let outcome, trace = engine_run ~backend:b ~token:(Some "tok") (validated ()) in
  (match outcome with
  | Blocked _ -> ()
  | o -> Alcotest.failf "T3: expected Blocked, got %s" (Types.string_of_outcome o));
  Alcotest.(check bool) "T3: blocked specifically at g-computed" true
    (has_blocked_at ~id:"g-computed" trace);
  (* the Run step itself must have executed cleanly (Run_executed, not a crash/Abort) *)
  let impact_final_ran =
    List.exists
      (function Run_executed { id = "impact-final"; parsed = Some _; _ } -> true | _ -> false)
      trace
  in
  Alcotest.(check bool) "T3: impact-final's refusal was a clean Run_executed, not an Abort" true
    impact_final_ran

(* ---- T4: index is not (contract) sound — Blocked on g-sound. Structural DB property, same
   in-loop and post-loop. ---- *)

let test_t4_not_sound () =
  let unsound = impact_json ~contract_ok:false ~verdict:"pass" ~new_findings:0 () in
  let table =
    default_table ~impact:unsound ~impact_final:unsound
      ~rules:(rules_json ~computed:true ~contract_ok:false ~verdict:"pass" ~failing:0)
      ~rules_final:(rules_json ~computed:true ~contract_ok:false ~verdict:"pass" ~failing:0) ()
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
  let table = default_table () in
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
  let table = default_table () in
  let outcome, trace = engine_run ~backend:(backend table) ~token:None (validated ()) in
  (match outcome with
  | Blocked _ -> ()
  | o -> Alcotest.failf "T6: expected Blocked, got %s" (Types.string_of_outcome o));
  Alcotest.(check bool) "T6: blocked specifically at the commit step" true
    (has_blocked_at ~id:"submit" trace);
  Alcotest.(check bool) "T6: g-independent passed (reviewer did approve)" false
    (has_blocked_at ~id:"g-independent" trace)

(* ---- T7: pcc-index prints a float where the schema declares int — the run aborts, never a
   silent green. cwr's Run-stdout path rejects floats/Intlit unconditionally, via
   Canonical_json.to_string inside parse_run_stdout (engine.ml, ahead of Schema.validate) — for
   every structured Run/preflight, with or without an Attest step in the workflow. The Aborted
   reason names the field and says "not canonical", not "schema mismatch". ---- *)

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
  Alcotest.(check bool) "T7: the walk never reached author" false (ran_agent "author" trace)

(* ---- T8: replay of a Blocked run (T2's shape) from a serialised ledger is byte-identical, and
   executes no subprocess — see per-scenario ledger checks above (T1); this scenario re-confirms
   it holds for the Blocked outcome shape too, not just Committed. ---- *)

let test_t8_replay_blocked_from_ledger () =
  let run_calls = Hashtbl.create 8 in
  let dirty = impact_json ~contract_ok:true ~verdict:"fail" ~new_findings:1 () in
  let table = default_table ~impact:dirty ~impact_final:dirty () in
  let fixer_seq = [ `Assoc [ ("progressed", jbool true); ("done", jbool false) ] ] in
  let b = backend ~fixer_seq ~run_calls table in
  let outcome, trace = engine_run ~backend:b ~token:(Some "tok") (validated ()) in
  (match outcome with Blocked _ -> () | o ->
    Alcotest.failf "T8 setup: expected Blocked (see T2), got %s" (Types.string_of_outcome o));
  let ledger = Ledger.to_ndjson trace in
  match Ledger.of_ndjson ledger with
  | Error e -> Alcotest.failf "T8: ledger did not round-trip: %s" e
  | Ok replay_trace ->
      let before = Option.value ~default:0 (Hashtbl.find_opt run_calls "impact-final") in
      (* See T1's comment: Replay_mismatch-free consumption is the byte-identical proof; this is
         the coarser terminal-outcome-string confirmation on top of it. *)
      let replayed = engine_replay ~trace:replay_trace (validated ()) in
      Alcotest.(check outcome_testable) "T8: replay reproduces the same terminal outcome" outcome
        replayed;
      Alcotest.(check int) "T8: replay executed NO subprocess" before
        (Option.value ~default:0 (Hashtbl.find_opt run_calls "impact-final"))

(* ---- T-rules-fail: a real architecture-fitness violation, everything else clean — the ONE
   scenario (before round 2) that actually made g-rules-pass the blocking gate rather than an
   untested floor. ---- *)

let test_rules_fail () =
  let broken = rules_json ~computed:true ~contract_ok:true ~verdict:"fail" ~failing:2 in
  let table = default_table ~rules:broken ~rules_final:broken () in
  let fixer_seq = [ `Assoc [ ("progressed", jbool true); ("done", jbool false) ] ] in
  let outcome, trace =
    engine_run ~backend:(backend ~fixer_seq table) ~token:(Some "tok") (validated ())
  in
  (match outcome with
  | Blocked _ -> ()
  | o -> Alcotest.failf "T-rules-fail: expected Blocked, got %s" (Types.string_of_outcome o));
  Alcotest.(check bool) "T-rules-fail: blocked specifically at g-rules-pass" true
    (has_blocked_at ~id:"g-rules-pass" trace);
  Alcotest.(check bool) "T-rules-fail: g-computed/g-sound/g-no-new-findings all passed first"
    false
    (has_blocked_at ~id:"g-computed" trace
    || has_blocked_at ~id:"g-sound" trace
    || has_blocked_at ~id:"g-no-new-findings" trace)

(* ---- T-converge-iter-3: the fixer genuinely needs three tries — red on iterations 1 and 2 (it
   reports progressed but not done), green on iteration 3 (done) — AND the post-loop
   re-verification agrees the final state is clean. Proves multi-iteration convergence actually
   reaches Committed end-to-end, not just the single-iteration case T1 covers. Uses [run_seq] to
   vary the in-loop impact/rules verdict BY CALL NUMBER, exercising the per-iteration-variation
   stub infrastructure the round-1 review noted was present but unused. ---- *)

let test_converge_iter3 () =
  let dirty = impact_json ~contract_ok:true ~verdict:"fail" ~new_findings:1 () in
  let clean = impact_json ~contract_ok:true ~verdict:"pass" ~new_findings:0 () in
  let table = default_table ~impact_final:clean () in
  let run_seq =
    [ ("impact", [ run_result_of dirty; run_result_of dirty; run_result_of clean ]) ]
  in
  let fixer_seq =
    [ `Assoc [ ("progressed", jbool true); ("done", jbool false) ];
      `Assoc [ ("progressed", jbool true); ("done", jbool false) ];
      `Assoc [ ("progressed", jbool true); ("done", jbool true) ] ]
  in
  let agent_calls = Hashtbl.create 8 in
  let outcome, _trace =
    engine_run ~backend:(backend ~fixer_seq ~agent_calls ~run_seq table) ~token:(Some "tok")
      (validated ())
  in
  (match outcome with
  | Committed _ -> ()
  | o -> Alcotest.failf "T-converge-iter-3: expected Committed, got %s"
      (Types.string_of_outcome o));
  Alcotest.(check int) "T-converge-iter-3: the fixer genuinely took 3 iterations" 3
    (Option.value ~default:0 (Hashtbl.find_opt agent_calls "fixer"))

(* ---- T-late-regression (the round-2 review's F1 regression test): the fixer's OWN LAST edit
   introduces a problem, and the loop exits right after that edit because the fixer's own verdict
   was read BEFORE it, from the state it inherited — exactly the shape of the bug the round-1
   workflow had. impact/rules (in-loop, what the fixer's dossier sees on iteration 1) are clean;
   the fixer declares done=true anyway on iteration 1 (simulating a fixer that made one further
   edit after reading a clean dossier and — wrongly — believes it is still fine, or simply an
   edit made without re-running anything, which is exactly what a real fixer agent does: it does
   not re-invoke arch-impact itself). impact-final/rules-final (the POST-loop pass, run against
   the tree AFTER that last edit) are dirty. Before the F1 fix, the gates read the in-loop
   "impact"/"rules" outputs and would have seen only the clean pre-edit verdicts — Committed, a
   false positive. After the fix, the gates read impact-final/rules-final and correctly block.
   This is the single most important test in this file: without it, nothing distinguishes "the
   proof was re-verified against the committed tree" from "the proof was verified against
   whatever the tree happened to be one step earlier". ---- *)

let test_late_regression_caught_by_post_loop_pass () =
  let clean = impact_json ~contract_ok:true ~verdict:"pass" ~new_findings:0 () in
  let dirty = impact_json ~contract_ok:true ~verdict:"fail" ~new_findings:1 () in
  let table = default_table ~impact:clean ~impact_final:dirty () in
  let fixer_seq = [ `Assoc [ ("progressed", jbool true); ("done", jbool true) ] ] in
  let outcome, trace =
    engine_run ~backend:(backend ~fixer_seq table) ~token:(Some "tok") (validated ())
  in
  (match outcome with
  | Blocked _ -> ()
  | o ->
      Alcotest.failf
        "T-late-regression: expected Blocked (post-loop pass must catch the fixer's last, \
         never-reindexed edit), got %s"
        (Types.string_of_outcome o));
  Alcotest.(check bool)
    "T-late-regression: blocked at g-computed via the POST-loop impact-final, not the stale \
     in-loop impact"
    true (has_blocked_at ~id:"g-computed" trace);
  let impact_final_ran =
    List.exists
      (function Run_executed { id = "impact-final"; _ } -> true | _ -> false)
      trace
  in
  Alcotest.(check bool) "T-late-regression: impact-final actually ran (the re-verification pass \
                         is not skipped)" true impact_final_ran

(* ---- T-preflight-fails: pcc-preflight (the test suite) exits nonzero — Commit blocks, same
   family as T6 but via a failing test suite rather than a missing token. Round-2 review (F7):
   the engine gates preflight ONLY on exit code and schema-conformance, never on the semantic
   VALUE of `ok` — an exit-0 preflight reporting {"ok":false,...} would NOT block. This scenario
   exercises the actual gate (nonzero exit), the one that is real. ---- *)

let test_preflight_fails () =
  let table = default_table () in
  let run_seq = [ ("submit", [ run_result_of ~exit:1 (preflight_ok 12) ]) ] in
  let outcome, trace =
    engine_run ~backend:(backend ~run_seq table) ~token:(Some "tok") (validated ())
  in
  (match outcome with
  | Blocked _ -> ()
  | o -> Alcotest.failf "T-preflight-fails: expected Blocked, got %s"
      (Types.string_of_outcome o));
  Alcotest.(check bool) "T-preflight-fails: blocked at the commit step" true
    (has_blocked_at ~id:"submit" trace);
  Alcotest.(check bool) "T-preflight-fails: g-independent passed (reviewer did approve)" false
    (has_blocked_at ~id:"g-independent" trace)

(* Each of the five floor gates is load-bearing: removing ANY one of them from the workflow file
   must make Validate.workflow fail (naming the missing gate), never silently accept a Commit
   path that skips it. This is the automated form of the manual "remove a gate, watch every
   scenario fail at validation" check done during round-1 review — implemented as a real test
   rather than left as an unverified claim in the docs. *)
let test_each_floor_gate_is_load_bearing () =
  List.iter
    (fun id ->
      let mutated = workflow_json_without_gate ~id in
      match Workflow_json.of_json mutated with
      | Error e ->
          Alcotest.failf "removing gate %S should still leave a PARSEABLE workflow: %s" id e
      | Ok wf -> (
          match Validate.workflow ~floor_gates wf with
          | Ok _ ->
              Alcotest.failf
                "removing floor gate %S must fail validation, not silently validate" id
          | Error msg ->
              Alcotest.(check bool)
                (Printf.sprintf "validation error for a missing %S names it" id)
                true (contains_substring msg id)))
    floor_gates

let () =
  ensure_pcc_files () ;
  Alcotest.run "proof-carrying-change"
    [ ( "workflow",
        [ Alcotest.test_case "lints/validates against the 5 floor gates" `Quick (fun () ->
              ignore (validated ()));
          Alcotest.test_case "each floor gate is load-bearing (removing it fails validation)"
            `Quick test_each_floor_gate_is_load_bearing ] );
      ( "scenarios",
        [ Alcotest.test_case "T1 nominal path -> Committed, replay byte-identical" `Quick
            test_t1_nominal;
          Alcotest.test_case "T2 persistent findings -> governor stop, Blocked g-computed"
            `Quick test_t2_findings_persist;
          Alcotest.test_case "T2b belt-and-braces: g-no-new-findings catches what verdict misses"
            `Quick test_t2b_belt_and_braces_no_new_findings;
          Alcotest.test_case "T3 no decision analysis -> Blocked g-computed, not a crash" `Quick
            test_t3_no_decision_analysis;
          Alcotest.test_case "T4 not ⊤-marked -> Blocked g-sound" `Quick test_t4_not_sound;
          Alcotest.test_case "T5 reviewer rejects -> Blocked g-independent" `Quick
            test_t5_reviewer_rejects;
          Alcotest.test_case "T6 no token -> Blocked at commit" `Quick test_t6_no_token;
          Alcotest.test_case "T7 float where int required -> Aborted, not a silent pass" `Quick
            test_t7_float_in_json;
          Alcotest.test_case "T8 replay of a Blocked run from ledger" `Quick
            test_t8_replay_blocked_from_ledger;
          Alcotest.test_case "T-rules-fail -> Blocked g-rules-pass" `Quick test_rules_fail;
          Alcotest.test_case "T-converge-iter-3: 3 real iterations -> Committed" `Quick
            test_converge_iter3;
          Alcotest.test_case
            "T-late-regression: fixer's last edit unverified in-loop -> caught post-loop" `Quick
            test_late_regression_caught_by_post_loop_pass;
          Alcotest.test_case "T-preflight-fails -> Blocked at commit" `Quick test_preflight_fails
        ] ) ]
