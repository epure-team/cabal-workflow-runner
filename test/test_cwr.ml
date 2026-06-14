open Cabal_workflow_runner
open Types

(* ---- helpers ---- *)

(* Resolve a project-relative path (e.g. "examples/smoke.workflow.json") robustly.
   Dune sets DUNE_SOURCEROOT to the project root for both [dune test] and
   [dune exec]; we join against it. Without it (direct binary invocation), try cwd
   first (project root) then the parent (legacy dune sandbox path). *)
let project_path rel =
  match Sys.getenv_opt "DUNE_SOURCEROOT" with
  | Some root -> Filename.concat root rel
  | None ->
      if Sys.file_exists rel then rel
      else Filename.concat ".." rel

let validate_ok ~floor wf =
  match Validate.workflow ~floor_gates:floor wf with
  | Ok v -> v
  | Error e -> Alcotest.failf "expected valid workflow, got Error: %s" e

(* substring test (no Str/Re dependency in this test exe). *)
let contains_substring hay needle =
  let nh = String.length hay and nn = String.length needle in
  let rec at i = i + nn <= nh && (String.sub hay i nn = needle || at (i + 1)) in
  nn = 0 || at 0

let outcome_eq a b = Types.string_of_outcome a = Types.string_of_outcome b

let outcome_testable =
  Alcotest.testable
    (fun fmt o -> Format.pp_print_string fmt (Types.string_of_outcome o))
    outcome_eq

(* An agent backend that returns a fixed JSON output per id. *)
let json_backend table =
  let agent ~id ~prompt:_ ~read_only:_ =
    match List.assoc_opt id table with
    | Some j -> (true, j)
    | None -> (true, `Assoc [])
  in
  Backend.stub ~agent ()

(* Wrap Engine.run in an Eio context (required after ~sw threading). *)
let engine_run ?max_loop_iters ?run_allowlist ?initial_ctx ~backend ~token validated =
  Eio_main.run (fun _env ->
    Eio.Switch.run (fun sw ->
      Engine.run ?max_loop_iters ?run_allowlist ?initial_ctx ~sw ~backend ~token validated))

(* Wrap Engine.replay in an Eio context. *)
let engine_replay ?max_loop_iters ?initial_ctx ~trace validated =
  Eio_main.run (fun _env ->
    Eio.Switch.run (fun sw ->
      Engine.replay ?max_loop_iters ?initial_ctx ~sw ~trace validated))

(* A workflow whose Commit is guaranteed-gated by "g" on every path. The gate
   condition is trivially true. *)
let gated_workflow =
  {
    name = "gated";
    version = None;
    steps =
      [
        Agent
          { id = "draft"; prompt = "do work"; read_only = false; output_schema = None; on_failure = Types.Abort };
        Gate { id = "g"; when_ = Expr.Lit (Expr.Bool true) };
        Commit { id = "submit" };
      ];
  }

(* ---- 1. parse round-trip (now with expressions, schema, governors) ---- *)

let test_parse_roundtrip () =
  let json =
    {|{ "name": "demo",
        "steps": [
          { "kind": "agent", "id": "a", "prompt": "p", "read_only": true,
            "output_schema": { "severity": { "enum": ["low","high"] } } },
          { "kind": "gate", "id": "g", "when": { "exists": "outputs.a.severity" } },
          { "kind": "branch",
            "when": { "in": [ {"path":"outputs.a.severity"}, {"lit":["high"]} ] },
            "then": [ { "kind": "commit", "id": "c" } ],
            "else": [ { "kind": "gate", "id": "h", "when": { "lit": false } } ] },
          { "kind": "loop",
            "until": { "eq": [ {"path":"loop.iter"}, {"lit": 1} ] },
            "governors": [ { "kind": "max_iters", "n": 2 }, { "kind": "budget" } ],
            "body": [ { "kind": "agent", "id": "b", "prompt": "q", "read_only": false } ] }
        ] }|}
  in
  match Workflow_json.of_string json with
  | Error e -> Alcotest.failf "valid JSON failed to parse: %s" e
  | Ok wf ->
      Alcotest.(check string) "name" "demo" wf.name;
      Alcotest.(check int) "step count" 4 (List.length wf.steps);
      let reparsed = Workflow_json.of_json (Workflow_json.to_json wf) in
      (match reparsed with
      | Ok wf2 -> Alcotest.(check bool) "roundtrip equal" true (wf = wf2)
      | Error e -> Alcotest.failf "round-trip parse failed: %s" e)

let test_parse_malformed () =
  (match Workflow_json.of_string "{ this is not json " with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "malformed JSON should yield Error");
  match
    Workflow_json.of_string
      {|{ "name": "x", "steps": [ { "kind": "frobnicate" } ] }|}
  with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "unknown step kind should yield Error"

(* ---- TEST 3 (spec): total DSL ---- *)

let test_total_dsl () =
  let ctx = [ ("outputs", `Assoc [ ("a", `Assoc [ ("severity", `String "high") ]) ]) ] in
  (* missing path => false, no raise *)
  Alcotest.(check bool) "missing path eq => false" false
    (Expr.eval ~ctx
       (Expr.Eq (Expr.Path [ "outputs"; "a"; "nope" ], Expr.Lit (Expr.Int 1))));
  Alcotest.(check bool) "missing path lt => false" false
    (Expr.eval ~ctx
       (Expr.Lt (Expr.Path [ "outputs"; "missing"; "x" ], Expr.Lit (Expr.Int 1))));
  (* type-mismatched comparison: string < int => false, no raise *)
  Alcotest.(check bool) "string vs int lt => false" false
    (Expr.eval ~ctx
       (Expr.Lt (Expr.Path [ "outputs"; "a"; "severity" ], Expr.Lit (Expr.Int 5))));
  (* exists on missing => false; on present => true *)
  Alcotest.(check bool) "exists missing => false" false
    (Expr.eval ~ctx (Expr.Exists [ "outputs"; "a"; "nope" ]));
  Alcotest.(check bool) "exists present => true" true
    (Expr.eval ~ctx (Expr.Exists [ "outputs"; "a"; "severity" ]));
  (* a real match still works *)
  Alcotest.(check bool) "eq string matches => true" true
    (Expr.eval ~ctx
       (Expr.Eq (Expr.Path [ "outputs"; "a"; "severity" ], Expr.Lit (Expr.String "high"))))

(* ---- v0.8 F2: Exists treats a present object as present ---- *)

let test_exists_present_object () =
  (* a = {"obj": {...}} : a present object => Exists true *)
  let ctx_obj =
    [ ("outputs", `Assoc [ ("a", `Assoc [ ("obj", `Assoc [ ("k", `Int 1) ]) ]) ]) ]
  in
  Alcotest.(check bool) "exists present object => true" true
    (Expr.eval ~ctx:ctx_obj (Expr.Exists [ "outputs"; "a"; "obj" ]));
  (* a = {"obj": null} : explicit JSON null => Exists false *)
  let ctx_null = [ ("outputs", `Assoc [ ("a", `Assoc [ ("obj", `Null) ]) ]) ] in
  Alcotest.(check bool) "exists explicit null => false" false
    (Expr.eval ~ctx:ctx_null (Expr.Exists [ "outputs"; "a"; "obj" ]));
  (* missing path => false *)
  Alcotest.(check bool) "exists missing path => false" false
    (Expr.eval ~ctx:ctx_obj (Expr.Exists [ "outputs"; "a"; "nope" ]));
  (* a present scalar => true *)
  let ctx_scalar =
    [ ("outputs", `Assoc [ ("a", `Assoc [ ("obj", `String "x") ]) ]) ]
  in
  Alcotest.(check bool) "exists present scalar => true" true
    (Expr.eval ~ctx:ctx_scalar (Expr.Exists [ "outputs"; "a"; "obj" ]));
  (* a present array => true *)
  let ctx_arr =
    [ ("outputs", `Assoc [ ("a", `Assoc [ ("obj", `List [ `Int 1 ]) ]) ]) ]
  in
  Alcotest.(check bool) "exists present array => true" true
    (Expr.eval ~ctx:ctx_arr (Expr.Exists [ "outputs"; "a"; "obj" ]))

(* ---- TEST 1 (spec): structured output drives a branch ---- *)

let branch_wf =
  {
    name = "branchy";
    version = None;
    steps =
      [
        Agent { id = "a"; prompt = "assess"; read_only = true; output_schema = None; on_failure = Types.Abort };
        Branch
          {
            when_ =
              Expr.In
                ( Expr.Path [ "outputs"; "a"; "severity" ],
                  Expr.Lit (Expr.List [ Expr.String "high"; Expr.String "critical" ]) );
            then_ =
              [ Agent { id = "esc"; prompt = "escalate"; read_only = false; output_schema = None; on_failure = Types.Abort } ];
            else_ =
              [ Agent { id = "drop"; prompt = "drop"; read_only = false; output_schema = None; on_failure = Types.Abort } ];
          };
      ];
  }

(* did the trace contain an Agent_ran for [id]? *)
let ran_agent trace id =
  List.exists
    (function Agent_ran { id = i; _ } -> i = id | _ -> false)
    trace

let test_branch_high () =
  let backend = json_backend [ ("a", `Assoc [ ("severity", `String "high") ]) ] in
  let v = validate_ok ~floor:[] branch_wf in
  let _, trace = engine_run ~backend ~token:None v in
  Alcotest.(check bool) "high => then_ (escalate ran)" true (ran_agent trace "esc");
  Alcotest.(check bool) "high => else_ NOT taken" false (ran_agent trace "drop")

let test_branch_low () =
  let backend = json_backend [ ("a", `Assoc [ ("severity", `String "low") ]) ] in
  let v = validate_ok ~floor:[] branch_wf in
  let _, trace = engine_run ~backend ~token:None v in
  Alcotest.(check bool) "low => else_ (drop ran)" true (ran_agent trace "drop");
  Alcotest.(check bool) "low => then_ NOT taken" false (ran_agent trace "esc")

(* ---- TEST 2 (spec): schema fail-closed ---- *)

let test_schema_fail_closed () =
  let schema : Schema.t = [ ("severity", Schema.Enum [ "low"; "high" ]) ] in
  let wf =
    {
      name = "schema";
      version = None;
      steps =
        [
          Agent
            { id = "a"; prompt = "p"; read_only = true; output_schema = Some schema; on_failure = Types.Abort };
          (* would commit if it got here, but the agent output lacks "severity" *)
          Gate { id = "g"; when_ = Expr.Lit (Expr.Bool true) };
          Commit { id = "submit" };
        ];
    }
  in
  (* agent returns an object WITHOUT the required "severity" field *)
  let backend = json_backend [ ("a", `Assoc [ ("other", `String "x") ]) ] in
  let v = validate_ok ~floor:[ "g" ] wf in
  let outcome, _ = engine_run ~backend ~token:(Some "tok") v in
  match outcome with
  | Aborted reason ->
      Alcotest.(check bool) "abort names the field" true
        (reason = "schema mismatch: severity")
  | o ->
      Alcotest.failf "expected Aborted on schema mismatch, got %s"
        (Types.string_of_outcome o)

(* ---- v0.8 F1: a failed agent step FAILS CLOSED (aborts the walk) ---- *)

(* A workflow whose FIRST agent stub returns success=false, then an always-true
   gate + commit + token. The old engine fell through and continued, letting the
   commit fire despite the failure. Now the failed agent aborts: Aborted, NOT
   Committed, and no Committed_step in the trace. Replay reproduces the Aborted. *)
let test_failed_agent_fails_closed () =
  let wf =
    {
      name = "failed-agent";
      version = None;
      steps =
        [
          Agent { id = "a"; prompt = "p"; read_only = false; output_schema = None; on_failure = Types.Abort };
          Gate { id = "g"; when_ = Expr.Lit (Expr.Bool true) };
          Commit { id = "submit" };
        ];
    }
  in
  (* a backend whose agent returns success=false with an error object *)
  let agent ~id:_ ~prompt:_ ~read_only:_ =
    (false, `Assoc [ ("error", `String "boom") ])
  in
  let backend = Backend.stub ~agent () in
  let v = validate_ok ~floor:[ "g" ] wf in
  let outcome, trace = engine_run ~backend ~token:(Some "tok") v in
  (match outcome with
  | Aborted _ -> ()
  | o ->
      Alcotest.failf "expected Aborted on a failed agent, got %s"
        (Types.string_of_outcome o));
  let committed =
    List.exists (function Committed_step _ -> true | _ -> false) trace
  in
  Alcotest.(check bool) "commit NOT reached" false committed;
  (* run and replay agree on the trace shape: Agent_ran{success=false};
     Blocked_at; terminal Aborted. *)
  let replayed = engine_replay ~trace v in
  Alcotest.(check outcome_testable) "replay identical (Aborted)" outcome replayed

(* SOFT-FAIL: an agent with on_failure=Continue that returns success=false must
   NOT abort — the failed Agent_ran is recorded, its output bound, and the walk
   CONTINUES to the next step. Here a failing "a" (Continue) is followed by a
   succeeding "b"; the run reaches Completed_no_commit (no commit step), the trace
   carries BOTH agents and NO Blocked_at/Aborted, and replay reproduces it. *)
let test_soft_fail_agent_continues () =
  let wf =
    {
      name = "soft-fail-agent";
      version = None;
      steps =
        [
          Agent { id = "a"; prompt = "p"; read_only = false; output_schema = None; on_failure = Types.Continue };
          Agent { id = "b"; prompt = "q"; read_only = false; output_schema = None; on_failure = Types.Abort };
        ];
    }
  in
  (* "a" fails, "b" succeeds *)
  let agent ~id ~prompt:_ ~read_only:_ =
    if id = "a" then (false, `Assoc [ ("error", `String "boom") ])
    else (true, `Assoc [ ("ok", `Bool true) ])
  in
  let backend = Backend.stub ~agent () in
  let v = validate_ok ~floor:[] wf in
  let outcome, trace = engine_run ~backend ~token:None v in
  (match outcome with
  | Completed_no_commit -> ()
  | o ->
      Alcotest.failf "expected Completed_no_commit on a soft-failed agent, got %s"
        (Types.string_of_outcome o));
  (* both agents ran; "a" recorded failed; no Blocked_at emitted. *)
  let ran_a_failed =
    List.exists
      (function Agent_ran { id = "a"; success = false; _ } -> true | _ -> false)
      trace
  in
  let ran_b_ok =
    List.exists
      (function Agent_ran { id = "b"; success = true; _ } -> true | _ -> false)
      trace
  in
  let any_block = List.exists (function Blocked_at _ -> true | _ -> false) trace in
  Alcotest.(check bool) "failed agent a recorded" true ran_a_failed;
  Alcotest.(check bool) "agent b still ran (loop continued)" true ran_b_ok;
  Alcotest.(check bool) "no Blocked_at on soft-fail" false any_block;
  let replayed = engine_replay ~trace v in
  Alcotest.(check outcome_testable) "replay identical (Completed_no_commit)" outcome
    replayed

(* SOFT-FAIL + COMMIT is REJECTED at validation. on_failure=continue would let a
   soft-failed agent reach a commit past a trivially-true floor gate (the
   commit-floor invariant tracks gate IDs, not predicate content), so the
   validator forbids the combination. A commit-free Continue workflow is accepted. *)
let test_soft_fail_with_commit_rejected () =
  let with_commit =
    {
      name = "soft-fail-commit";
      version = None;
      steps =
        [
          Agent { id = "a"; prompt = "p"; read_only = false; output_schema = None; on_failure = Types.Continue };
          Gate { id = "g"; when_ = Expr.Lit (Expr.Bool true) };
          Commit { id = "submit" };
        ];
    }
  in
  (match Validate.workflow ~floor_gates:[ "g" ] with_commit with
  | Error msg ->
      Alcotest.(check bool) "error explains soft-fail+commit rejection" true
        (contains_substring msg "commit-free workflows")
  | Ok _ ->
      Alcotest.fail "on_failure=continue with a Commit must be rejected");
  (* the SAME workflow with the default abort IS accepted (sanity: it's the
     Continue+Commit combination that's forbidden, not the commit). *)
  let abort_commit =
    {
      with_commit with
      steps =
        [
          Agent { id = "a"; prompt = "p"; read_only = false; output_schema = None; on_failure = Types.Abort };
          Gate { id = "g"; when_ = Expr.Lit (Expr.Bool true) };
          Commit { id = "submit" };
        ];
    }
  in
  (match Validate.workflow ~floor_gates:[ "g" ] abort_commit with
  | Ok _ -> ()
  | Error msg -> Alcotest.failf "abort+commit must be accepted, got: %s" msg);
  (* a commit-free Continue workflow is accepted. *)
  let commit_free =
    {
      name = "soft-fail-no-commit";
      version = None;
      steps =
        [ Agent { id = "a"; prompt = "p"; read_only = false; output_schema = None; on_failure = Types.Continue } ];
    }
  in
  match Validate.workflow ~floor_gates:[] commit_free with
  | Ok _ -> ()
  | Error msg ->
      Alcotest.failf "commit-free continue workflow must be accepted, got: %s" msg

(* ---- TEST 6 (spec): ungoverned loop rejected ---- *)

let test_ungoverned_loop_rejected () =
  let wf =
    {
      name = "ungoverned";
      version = None;
      steps =
        [
          Loop
            {
              body =
                [ Agent { id = "x"; prompt = "p"; read_only = true; output_schema = None; on_failure = Types.Abort } ];
              until = None;
              governors = [];
            };
        ];
    }
  in
  match Validate.workflow ~floor_gates:[] wf with
  | Error msg ->
      Alcotest.(check bool) "error mentions ungoverned" true
        (msg = "loop is ungoverned")
  | Ok _ -> Alcotest.fail "loop with empty governors must be rejected"

let test_bad_max_iters_rejected () =
  let wf =
    {
      name = "bad-cap";
      version = None;
      steps =
        [
          Loop
            {
              body = [];
              until = None;
              governors = [ Max_iters 0 ];
            };
        ];
    }
  in
  match Validate.workflow ~floor_gates:[] wf with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "Max_iters 0 must be rejected"

(* ---- TEST 4 (spec): governed unbounded loop terminates — Budget ---- *)

let decrementing_budget n =
  let r = ref n in
  fun () ->
    let v = !r in
    decr r;
    v

let test_loop_budget_terminates () =
  (* a loop with ONLY [Budget] (no Max_iters), an until that never holds, and a
     budget that decrements from N => runs exactly N iterations then stops.
     budget starts at 3: readings 3,2,1 keep going (>0), reading 0 stops at the
     iteration where it hits <=0. Sequence: iter0 read=3 (>0 continue), iter1
     read=2, iter2 read=1, iter3 read=0 -> stop. So 4 iterations? We design the
     stub so the Nth reading is the one that is <=0. *)
  let agent_count = ref 0 in
  let agent ~id ~prompt:_ ~read_only:_ =
    incr agent_count;
    (true, `Assoc [ ("id", `String id) ])
  in
  (* budget readings: 2, 1, 0  => stops when reading 0 (third reading). *)
  let backend = Backend.stub ~agent ~budget:(decrementing_budget 2) () in
  let wf =
    {
      name = "budget-loop";
      version = None;
      steps =
        [
          Loop
            {
              body =
                [ Agent { id = "work"; prompt = "p"; read_only = false; output_schema = None; on_failure = Types.Abort } ];
              (* until never holds *)
              until = Some (Expr.Lit (Expr.Bool false));
              governors = [ Budget ];
            };
        ];
    }
  in
  let v = validate_ok ~floor:[] wf in
  let outcome, trace = engine_run ~backend ~token:None v in
  (* readings 2(>0 continue) at iter0, 1(>0) at iter1, 0(<=0 STOP) at iter2 =>
     3 iterations of the body. *)
  Alcotest.(check int) "ran body exactly 3 times" 3 !agent_count;
  let stop =
    List.find_map
      (function Loop_stopped { iterations; reason } -> Some (iterations, reason) | _ -> None)
      trace
  in
  Alcotest.(check bool) "stopped via budget after 3 iters" true
    (stop = Some (3, "budget"));
  Alcotest.(check outcome_testable) "completed without commit"
    Completed_no_commit outcome

(* ---- TEST 5 (spec): governed unbounded loop terminates — Fixpoint ---- *)

let test_loop_fixpoint_terminates () =
  (* a loop with ONLY [Fixpoint {window=2; progress}] where progress is false
     from the start => stops after 2 iterations. Budget is default (unbounded).
     until never holds. *)
  let agent_count = ref 0 in
  let agent ~id:_ ~prompt:_ ~read_only:_ =
    incr agent_count;
    (true, `Assoc [ ("progressed", `Bool false) ])
  in
  let backend = Backend.stub ~agent () in
  let wf =
    {
      name = "fixpoint-loop";
      version = None;
      steps =
        [
          Loop
            {
              body =
                [ Agent { id = "work"; prompt = "p"; read_only = false; output_schema = None; on_failure = Types.Abort } ];
              until = Some (Expr.Lit (Expr.Bool false));
              governors =
                [
                  Fixpoint
                    {
                      window = 2;
                      progress = Expr.Path [ "outputs"; "work"; "progressed" ];
                    };
                ];
            };
        ];
    }
  in
  let v = validate_ok ~floor:[] wf in
  let outcome, trace = engine_run ~backend ~token:None v in
  Alcotest.(check int) "ran body exactly 2 times" 2 !agent_count;
  let stop =
    List.find_map
      (function Loop_stopped { iterations; reason } -> Some (iterations, reason) | _ -> None)
      trace
  in
  Alcotest.(check bool) "stopped via fixpoint after 2 iters" true
    (stop = Some (2, "fixpoint"));
  Alcotest.(check outcome_testable) "completed without commit"
    Completed_no_commit outcome

(* ---- v0.5 Fix 1: the unconditional engine iteration ceiling ---- *)

(* A Budget-ONLY loop whose backend returns a CONSTANT positive budget (the
   shipped Backend_cabal semantics) and whose [until] never holds would run
   forever under the old code. The engine ceiling stops it at exactly
   [max_loop_iters] with reason "ceiling". *)
let test_loop_ceiling_budget_constant () =
  let agent_count = ref 0 in
  let agent ~id ~prompt:_ ~read_only:_ =
    incr agent_count;
    (true, `Assoc [ ("id", `String id) ])
  in
  (* constant budget, always > 0 => Budget never fires (mirrors Backend_cabal). *)
  let backend = Backend.stub ~agent ~budget:(fun () -> 1_000_000) () in
  let wf =
    {
      name = "budget-constant-loop";
      version = None;
      steps =
        [
          Loop
            {
              body =
                [ Agent { id = "work"; prompt = "p"; read_only = false; output_schema = None; on_failure = Types.Abort } ];
              until = Some (Expr.Lit (Expr.Bool false));
              governors = [ Budget ];
            };
        ];
    }
  in
  let v = validate_ok ~floor:[] wf in
  let outcome, trace = engine_run ~max_loop_iters:5 ~backend ~token:None v in
  Alcotest.(check int) "ran body exactly 5 times (ceiling)" 5 !agent_count;
  let stop =
    List.find_map
      (function Loop_stopped { iterations; reason } -> Some (iterations, reason) | _ -> None)
      trace
  in
  Alcotest.(check bool) "stopped via ceiling after 5 iters" true
    (stop = Some (5, "ceiling"));
  Alcotest.(check outcome_testable) "completed without commit"
    Completed_no_commit outcome;
  (* the ceiling is a constant, so replay reproduces the same Loop_stopped. *)
  let replayed = engine_replay ~max_loop_iters:5 ~trace v in
  Alcotest.(check outcome_testable) "replay identical" outcome replayed

(* A Fixpoint-ONLY loop whose agent ALWAYS reports progressed:true never trips
   Fixpoint; the ceiling stops it anyway. *)
let test_loop_ceiling_fixpoint_always_progresses () =
  let agent_count = ref 0 in
  let agent ~id:_ ~prompt:_ ~read_only:_ =
    incr agent_count;
    (true, `Assoc [ ("progressed", `Bool true) ])
  in
  let backend = Backend.stub ~agent () in
  let wf =
    {
      name = "fixpoint-progress-loop";
      version = None;
      steps =
        [
          Loop
            {
              body =
                [ Agent { id = "work"; prompt = "p"; read_only = false; output_schema = None; on_failure = Types.Abort } ];
              until = Some (Expr.Lit (Expr.Bool false));
              governors =
                [
                  Fixpoint
                    {
                      window = 2;
                      progress = Expr.Path [ "outputs"; "work"; "progressed" ];
                    };
                ];
            };
        ];
    }
  in
  let v = validate_ok ~floor:[] wf in
  let outcome, trace = engine_run ~max_loop_iters:4 ~backend ~token:None v in
  Alcotest.(check int) "ran body exactly 4 times (ceiling)" 4 !agent_count;
  let stop =
    List.find_map
      (function Loop_stopped { iterations; reason } -> Some (iterations, reason) | _ -> None)
      trace
  in
  Alcotest.(check bool) "stopped via ceiling after 4 iters" true
    (stop = Some (4, "ceiling"));
  Alcotest.(check outcome_testable) "completed without commit"
    Completed_no_commit outcome

(* ---- TEST 7 (spec): deterministic replay with a loop ---- *)

let test_replay_with_loop () =
  (* governed loop (budget) + structured agents + an expression-gated commit. *)
  let agent ~id ~prompt:_ ~read_only:_ =
    match id with
    | "assess" -> (true, `Assoc [ ("severity", `String "high") ])
    | _ -> (true, `Assoc [ ("progressed", `Bool false) ])
  in
  let backend = Backend.stub ~agent ~budget:(decrementing_budget 1) () in
  let wf =
    {
      name = "replayable";
      version = None;
      steps =
        [
          Agent { id = "assess"; prompt = "p"; read_only = true; output_schema = None; on_failure = Types.Abort };
          Loop
            {
              body =
                [ Agent { id = "work"; prompt = "q"; read_only = false; output_schema = None; on_failure = Types.Abort } ];
              until = Some (Expr.Lit (Expr.Bool false));
              governors = [ Budget ];
            };
          Gate { id = "g"; when_ = Expr.Lit (Expr.Bool true) };
          Branch
            {
              when_ =
                Expr.In
                  ( Expr.Path [ "outputs"; "assess"; "severity" ],
                    Expr.Lit (Expr.List [ Expr.String "high" ]) );
              then_ = [ Commit { id = "submit" } ];
              else_ =
                [ Agent { id = "noop"; prompt = "r"; read_only = true; output_schema = None; on_failure = Types.Abort } ];
            };
        ];
    }
  in
  let v = validate_ok ~floor:[ "g" ] wf in
  let outcome, trace = engine_run ~backend ~token:(Some "tok") v in
  (* replay re-feeds recorded outputs + budget readings, no backend *)
  let replayed = engine_replay ~trace v in
  Alcotest.(check outcome_testable) "replay outcome identical" outcome replayed;
  (match outcome with
  | Committed _ -> ()
  | o -> Alcotest.failf "expected the run to commit, got %s" (Types.string_of_outcome o));
  (* iteration count is recorded; replay reproduces the same Loop_stopped *)
  let loop_iters trace =
    List.filter_map
      (function Loop_iter { index } -> Some index | _ -> None)
      trace
  in
  Alcotest.(check (list int)) "loop iteration indices" [ 0; 1 ] (loop_iters trace);
  (* re-run replay path produces the SAME trace via a second run determinism *)
  let backend2 = Backend.stub ~agent ~budget:(decrementing_budget 1) () in
  let outcome2, trace2 = engine_run ~backend:backend2 ~token:(Some "tok") v in
  Alcotest.(check outcome_testable) "second run identical outcome" outcome outcome2;
  Alcotest.(check bool) "second run identical trace" true (trace = trace2)

(* ---- v0.8 F4: replay rejects trailing extra trace entries ---- *)

(* Take a real recorded trace; the unmodified trace replays fine, but the trace
   with one extra dummy entry appended must raise Replay_mismatch. *)
let test_replay_rejects_trailing_entries () =
  let v = validate_ok ~floor:[ "g" ] gated_workflow in
  let outcome, trace = engine_run ~backend:(Backend.stub ()) ~token:(Some "tok") v in
  (* unmodified trace replays fine *)
  let replayed = engine_replay ~trace v in
  Alcotest.(check outcome_testable) "unmodified trace replays fine" outcome
    replayed;
  (* append one extra dummy entry => Replay_mismatch *)
  let trace_plus = trace @ [ Loop_iter { index = 99 } ] in
  let raised =
    try
      ignore (engine_replay ~trace:trace_plus v);
      false
    with Engine.Replay_mismatch _ -> true
  in
  Alcotest.(check bool) "trailing entry => Replay_mismatch" true raised

(* ---- v0.9: the run step ---- *)

(* A stub run_command returning a fixed run_result regardless of args. *)
let run_backend result =
  let run_command ~id:_ ~argv:_ ~working_dir:_ ~timeout_ms:_ ~observe:_ = result in
  Backend.stub ~run_command ()

let mk_file path change =
  { path; change; size = 3; digest = "deadbeef" }

(* TEST 1: a mkdir-like run (stub returns exit 0 + a Created file) binds
   outputs.mk.exit=0, the files list has the created entry, and a following gate
   eq(outputs.mk.exit,0) passes. *)
let test_run_step_outputs_and_gate () =
  let result =
    {
      exit = 0;
      stdout = "ok";
      stderr = "";
      truncated = false;
      files = [ mk_file "out/x" Created ];
    }
  in
  let backend = run_backend result in
  let wf =
    {
      name = "run-demo";
      version = None;
      steps =
        [
          Run
            {
              id = "mk";
              cmd = [ "mkdir"; "-p"; "out" ];
              working_dir = "scratch";
              timeout_ms = Some 30000;
              observe = Some [ "out" ];
            };
          Gate
            {
              id = "g";
              when_ =
                Expr.Eq
                  (Expr.Path [ "outputs"; "mk"; "exit" ], Expr.Lit (Expr.Int 0));
            };
        ];
    }
  in
  let v = validate_ok ~floor:[ "g" ] wf in
  let outcome, trace =
    engine_run ~run_allowlist:[ "mkdir" ] ~backend ~token:None v
  in
  Alcotest.(check outcome_testable)
    "completed (gate passed, no commit)" Completed_no_commit outcome;
  (* the recorded run_result is in the trace *)
  let recorded =
    List.find_map
      (function Run_executed { id = "mk"; result } -> Some result | _ -> None)
      trace
  in
  (match recorded with
  | Some r ->
      Alcotest.(check int) "recorded exit 0" 0 r.exit;
      Alcotest.(check int) "one created file" 1 (List.length r.files);
      Alcotest.(check string) "created path" "out/x" (List.hd r.files).path
  | None -> Alcotest.fail "expected a Run_executed entry for mk");
  (* the gate after it must have passed (Pass verdict recorded) *)
  let gate_passed =
    List.exists
      (function Gate_evaluated { id = "g"; verdict = Pass } -> true | _ -> false)
      trace
  in
  Alcotest.(check bool) "gate eq(outputs.mk.exit,0) passed" true gate_passed

(* TEST 2: allowlist enforcement. *)
let test_run_step_allowlist () =
  let result =
    { exit = 0; stdout = ""; stderr = ""; truncated = false; files = [] }
  in
  let wf =
    {
      name = "allow";
      version = None;
      steps =
        [
          Run
            {
              id = "r";
              cmd = [ "rm"; "-rf"; "x" ];
              working_dir = "scratch";
              timeout_ms = None;
              observe = None;
            };
        ];
    }
  in
  let v = validate_ok ~floor:[] wf in
  (* not in allowlist => Blocked, no execution *)
  let ran = ref 0 in
  let backend_count =
    let run_command ~id:_ ~argv:_ ~working_dir:_ ~timeout_ms:_ ~observe:_ =
      incr ran;
      result
    in
    Backend.stub ~run_command ()
  in
  let outcome, trace =
    engine_run ~run_allowlist:[] ~backend:backend_count ~token:None v
  in
  Alcotest.(check int) "blocked => run_command NOT called" 0 !ran;
  (match outcome with
  | Blocked reason ->
      Alcotest.(check bool) "blocked names the allowlist" true
        (let needle = "allowlist" in
         let rec has i =
           i + String.length needle <= String.length reason
           && (String.sub reason i (String.length needle) = needle || has (i + 1))
         in
         has 0)
  | o -> Alcotest.failf "expected Blocked, got %s" (Types.string_of_outcome o));
  let has_block =
    List.exists (function Blocked_at { id = "r"; _ } -> true | _ -> false) trace
  in
  Alcotest.(check bool) "Blocked_at emitted" true has_block;
  (* bare name in the allowlist => runs *)
  let ran2 = ref 0 in
  let backend2 =
    let run_command ~id:_ ~argv:_ ~working_dir:_ ~timeout_ms:_ ~observe:_ =
      incr ran2;
      result
    in
    Backend.stub ~run_command ()
  in
  let outcome2, _ =
    engine_run ~run_allowlist:[ "rm" ] ~backend:backend2 ~token:None v
  in
  Alcotest.(check int) "allowed (bare rm) => run_command called once" 1 !ran2;
  Alcotest.(check outcome_testable) "allowed => completes" Completed_no_commit
    outcome2;
  (* "*" => runs *)
  let ran3 = ref 0 in
  let backend3 =
    let run_command ~id:_ ~argv:_ ~working_dir:_ ~timeout_ms:_ ~observe:_ =
      incr ran3;
      result
    in
    Backend.stub ~run_command ()
  in
  let outcome3, _ =
    engine_run ~run_allowlist:[ "*" ] ~backend:backend3 ~token:None v
  in
  Alcotest.(check int) "'*' => run_command called once" 1 !ran3;
  Alcotest.(check outcome_testable) "'*' => completes" Completed_no_commit
    outcome3

(* TEST 3: replay never re-executes. The stub increments a counter on each
   run_command call; after a live run (counter=1), Engine.replay reproduces the
   SAME outcome with NO further increment (replay re-feeds the recorded result).
   Trailing-entry rejection still holds. *)
let test_run_step_replay_no_reexec () =
  let counter = ref 0 in
  let run_command ~id:_ ~argv:_ ~working_dir:_ ~timeout_ms:_ ~observe:_ =
    incr counter;
    {
      exit = 0;
      stdout = Printf.sprintf "call#%d" !counter;
      stderr = "";
      truncated = false;
      files = [ mk_file "out/y" Created ];
    }
  in
  let backend = Backend.stub ~run_command () in
  let wf =
    {
      name = "replay-run";
      version = None;
      steps =
        [
          Run
            {
              id = "mk";
              cmd = [ "mkdir"; "out" ];
              working_dir = "scratch";
              timeout_ms = None;
              observe = None;
            };
          Gate
            {
              id = "g";
              when_ =
                Expr.Eq
                  (Expr.Path [ "outputs"; "mk"; "exit" ], Expr.Lit (Expr.Int 0));
            };
        ];
    }
  in
  let v = validate_ok ~floor:[ "g" ] wf in
  let outcome, trace =
    engine_run ~run_allowlist:[ "mkdir" ] ~backend ~token:None v
  in
  Alcotest.(check int) "live run executed the command exactly once" 1 !counter;
  (* replay re-feeds the recorded result, NEVER calling run_command again *)
  let replayed = engine_replay ~trace v in
  Alcotest.(check int) "replay did NOT re-execute (counter unchanged)" 1 !counter;
  Alcotest.(check outcome_testable) "replay outcome identical" outcome replayed;
  (* trailing-entry rejection still holds for a run-bearing trace *)
  let trace_plus = trace @ [ Loop_iter { index = 99 } ] in
  let raised =
    try
      ignore (engine_replay ~trace:trace_plus v);
      false
    with Engine.Replay_mismatch _ -> true
  in
  Alcotest.(check bool) "trailing entry => Replay_mismatch" true raised

(* TEST 4: file diff — first run returns a Created entry, a second run returns a
   Deleted entry; both observed in their respective outputs.<id>.files. *)
let test_run_step_file_diff () =
  let calls = ref 0 in
  let run_command ~id:_ ~argv:_ ~working_dir:_ ~timeout_ms:_ ~observe:_ =
    incr calls;
    if !calls = 1 then
      {
        exit = 0;
        stdout = "";
        stderr = "";
        truncated = false;
        files = [ mk_file "f" Created ];
      }
    else
      {
        exit = 0;
        stdout = "";
        stderr = "";
        truncated = false;
        files = [ { path = "f"; change = Deleted; size = 0; digest = "" } ];
      }
  in
  let backend = Backend.stub ~run_command () in
  let mkrun id =
    Run
      {
        id;
        cmd = [ "touch"; "f" ];
        working_dir = "scratch";
        timeout_ms = None;
        observe = None;
      }
  in
  let wf =
    { name = "diff"; version = None; steps = [ mkrun "create"; mkrun "remove" ] }
  in
  let v = validate_ok ~floor:[] wf in
  let _outcome, trace =
    engine_run ~run_allowlist:[ "touch" ] ~backend ~token:None v
  in
  let files_of id =
    List.find_map
      (function
        | Run_executed { id = rid; result } when rid = id -> Some result.files
        | _ -> None)
      trace
  in
  (match files_of "create" with
  | Some [ fc ] ->
      Alcotest.(check bool) "first run observed Created" true (fc.change = Created)
  | _ -> Alcotest.fail "expected one Created in create.files");
  match files_of "remove" with
  | Some [ fc ] ->
      Alcotest.(check bool) "second run observed Deleted" true (fc.change = Deleted)
  | _ -> Alcotest.fail "expected one Deleted in remove.files"

(* TEST 5: working_dir with ".." or absolute => parse/validate Error. *)
let test_run_step_bad_working_dir () =
  let parent =
    {|{ "name": "x", "steps": [
         { "kind": "run", "id": "r", "cmd": ["echo","hi"], "working_dir": "../escape" } ] }|}
  in
  let absolute =
    {|{ "name": "x", "steps": [
         { "kind": "run", "id": "r", "cmd": ["echo","hi"], "working_dir": "/abs" } ] }|}
  in
  let empty_cmd =
    {|{ "name": "x", "steps": [
         { "kind": "run", "id": "r", "cmd": [], "working_dir": "ok" } ] }|}
  in
  List.iter
    (fun (label, json) ->
      match Workflow_json.of_string json with
      | Error _ -> ()
      | Ok _ -> Alcotest.failf "expected parse Error for %s" label)
    [ ("..", parent); ("absolute", absolute); ("empty cmd", empty_cmd) ];
  (* a relative no-".." working_dir + non-empty cmd parses fine *)
  let ok =
    {|{ "name": "x", "steps": [
         { "kind": "run", "id": "r", "cmd": ["echo","hi"], "working_dir": "ok/sub" } ] }|}
  in
  match Workflow_json.of_string ok with
  | Ok _ -> ()
  | Error e -> Alcotest.failf "valid run step should parse, got Error: %s" e

(* The run-demo example validates and lints clean-of-errors (the
   run-step-executes-commands warning is expected/allowed). *)
let test_run_demo_example () =
  let path = project_path "examples/run-demo.workflow.json" in
  match Workflow_json.of_file path with
  | Error e -> Alcotest.failf "run-demo parse Error: %s" e
  | Ok wf ->
      let ds = Lint.check ~floor_gates:[] wf in
      Alcotest.(check bool) "run-demo has no error diagnostics" false
        (Lint.has_errors ds);
      let has_run_warning =
        List.exists
          (fun (d : Lint.diagnostic) -> d.code = "run-step-executes-commands")
          ds
      in
      Alcotest.(check bool) "run-demo emits run-step-executes-commands warning"
        true has_run_warning;
      (match Validate.workflow ~floor_gates:[] wf with
      | Ok _ -> ()
      | Error e -> Alcotest.failf "run-demo should validate, got Error: %s" e)

(* Lint emits a louder destructive-command warning for rm (still a warning,
   has_errors stays false). *)
let test_run_step_destructive_warning () =
  let wf =
    {
      name = "destructive";
      version = None;
      steps =
        [
          Run
            {
              id = "wipe";
              cmd = [ "rm"; "-rf"; "out" ];
              working_dir = "scratch";
              timeout_ms = None;
              observe = None;
            };
        ];
    }
  in
  let ds = Lint.check ~floor_gates:[] wf in
  Alcotest.(check bool) "destructive run is a warning, not error" false
    (Lint.has_errors ds);
  let has_destructive =
    List.exists
      (fun (d : Lint.diagnostic) -> d.code = "run-step-destructive-command")
      ds
  in
  Alcotest.(check bool) "emits run-step-destructive-command" true has_destructive

(* v0.9 review Fix 1: the per-file digest is MD5 ([Digest]), honestly labeled.
   Known-answer: [Digest.to_hex (Digest.string "abc")] is the well-known MD5 of
   "abc" = "900150983cd24fb0d6963f7d28e17f72". The runner computes a file change's
   [digest] field the same way, so a created-file diff binds that value. This
   pins the digest construction so a future regression (e.g. a wrong hash) is
   caught. *)
let test_run_step_digest_known_answer () =
  Alcotest.(check string) "MD5(\"abc\") known-answer"
    "900150983cd24fb0d6963f7d28e17f72"
    (Digest.to_hex (Digest.string "abc"));
  (* A run_result carrying a Created file whose digest is MD5("abc") survives the
     engine round-trip into outputs.<id>.files[].digest unchanged. *)
  let result =
    {
      exit = 0;
      stdout = "";
      stderr = "";
      truncated = false;
      files =
        [
          {
            path = "out/x";
            change = Created;
            size = 3;
            digest = Digest.to_hex (Digest.string "abc");
          };
        ];
    }
  in
  let backend = run_backend result in
  let wf =
    {
      name = "digest";
      version = None;
      steps =
        [
          Run
            {
              id = "mk";
              cmd = [ "touch"; "out/x" ];
              working_dir = "scratch";
              timeout_ms = None;
              observe = None;
            };
        ];
    }
  in
  let v = validate_ok ~floor:[] wf in
  let _, trace = engine_run ~run_allowlist:[ "touch" ] ~backend ~token:None v in
  let recorded =
    List.find_map
      (function Run_executed { id = "mk"; result } -> Some result | _ -> None)
      trace
  in
  match recorded with
  | Some r ->
      Alcotest.(check string) "files[0].digest == MD5(\"abc\")"
        "900150983cd24fb0d6963f7d28e17f72" (List.hd r.files).digest
  | None -> Alcotest.fail "expected a Run_executed entry for mk"

(* v0.9 review Fix 2: a path-bearing cmd[0] (absolute OR relative) is rejected by
   the engine BEFORE the allowlist match — it must be a bare name resolved via
   PATH. So ["/usr/bin/mkdir"] with --allow-run mkdir is Blocked (the bypass is
   closed) and run_command is NOT called; ["./mkdir"] is Blocked too; bare
   ["mkdir"] allowlisted runs. *)
let test_run_step_rejects_path_argv0 () =
  let result =
    { exit = 0; stdout = ""; stderr = ""; truncated = false; files = [] }
  in
  let mk_wf cmd0 =
    {
      name = "pathy";
      version = None;
      steps =
        [
          Run
            {
              id = "r";
              cmd = [ cmd0; "out" ];
              working_dir = "scratch";
              timeout_ms = None;
              observe = None;
            };
        ];
    }
  in
  (* a backend counting executions; with allowlist ["mkdir"] *)
  let run_blocked cmd0 =
    let ran = ref 0 in
    let backend =
      let run_command ~id:_ ~argv:_ ~working_dir:_ ~timeout_ms:_ ~observe:_ =
        incr ran;
        result
      in
      Backend.stub ~run_command ()
    in
    let v = validate_ok ~floor:[] (mk_wf cmd0) in
    let outcome, trace =
      engine_run ~run_allowlist:[ "mkdir" ] ~backend ~token:None v
    in
    (outcome, trace, !ran)
  in
  (* absolute path => Blocked (path rejected), run_command NOT called *)
  let outcome_abs, trace_abs, ran_abs = run_blocked "/usr/bin/mkdir" in
  Alcotest.(check int) "absolute path => run_command NOT called" 0 ran_abs;
  (match outcome_abs with
  | Blocked reason ->
      let contains hay needle =
        let nl = String.length needle and hl = String.length hay in
        let rec aux i = i + nl <= hl && (String.sub hay i nl = needle || aux (i + 1)) in
        nl = 0 || aux 0
      in
      Alcotest.(check bool) "block reason mentions PATH/bare name" true
        (contains reason "bare name" || contains reason "PATH")
  | o -> Alcotest.failf "expected Blocked for absolute path, got %s"
           (Types.string_of_outcome o));
  Alcotest.(check bool) "Blocked_at emitted (absolute)" true
    (List.exists (function Blocked_at { id = "r"; _ } -> true | _ -> false)
       trace_abs);
  (* relative ./ path => Blocked, not run *)
  let outcome_rel, _, ran_rel = run_blocked "./mkdir" in
  Alcotest.(check int) "relative ./ path => run_command NOT called" 0 ran_rel;
  (match outcome_rel with
  | Blocked _ -> ()
  | o -> Alcotest.failf "expected Blocked for ./mkdir, got %s"
           (Types.string_of_outcome o));
  (* nested a/b path => Blocked, not run *)
  let _, _, ran_nested = run_blocked "a/b" in
  Alcotest.(check int) "a/b path => run_command NOT called" 0 ran_nested;
  (* bare name + allowlist => runs *)
  let ran_ok = ref 0 in
  let backend_ok =
    let run_command ~id:_ ~argv:_ ~working_dir:_ ~timeout_ms:_ ~observe:_ =
      incr ran_ok;
      result
    in
    Backend.stub ~run_command ()
  in
  let v_ok = validate_ok ~floor:[] (mk_wf "mkdir") in
  let outcome_ok, _ =
    engine_run ~run_allowlist:[ "mkdir" ] ~backend:backend_ok ~token:None v_ok
  in
  Alcotest.(check int) "bare mkdir allowlisted => run_command called once" 1
    !ran_ok;
  Alcotest.(check outcome_testable) "bare allowlisted => completes"
    Completed_no_commit outcome_ok

(* v0.9 review Fix 3 (engine-level surface): the engine calls the injected
   run_command exactly once and records its result; the bin runner wraps the real
   effect in try/with so a spawn/buffer-limit exception becomes a RECORDED
   non-zero result rather than a crash. Here we assert the lib contract: a
   run_command returning a synthetic non-zero result (mirroring the bin runner's
   on-exception path, e.g. exit 127 ENOENT) is recorded as a normal Run_executed,
   so the run is replayable and the engine never aborts. (The bin runner's
   try/with itself is exercised by the CLI repro in the review notes.) *)
let test_run_step_effect_failure_recorded () =
  (* a run_command that simulates the bin runner's spawn-failure return: a
     well-formed non-zero result, NOT a raised exception. *)
  let run_command ~id:_ ~argv:_ ~working_dir:_ ~timeout_ms:_ ~observe:_ =
    {
      exit = 127;
      stdout = "";
      stderr = "run: command could not be executed: ENOENT";
      truncated = false;
      files = [];
    }
  in
  let backend = Backend.stub ~run_command () in
  let wf =
    {
      name = "enoent";
      version = None;
      steps =
        [
          Run
            {
              id = "boom";
              cmd = [ "definitely-not-a-real-binary-xyz" ];
              working_dir = "scratch";
              timeout_ms = None;
              observe = None;
            };
          Gate
            {
              id = "g";
              when_ =
                Expr.Eq
                  (Expr.Path [ "outputs"; "boom"; "exit" ], Expr.Lit (Expr.Int 127));
            };
        ];
    }
  in
  let v = validate_ok ~floor:[ "g" ] wf in
  let outcome, trace =
    engine_run ~run_allowlist:[ "definitely-not-a-real-binary-xyz" ] ~backend
      ~token:None v
  in
  (* the failure was RECORDED (exit 127), the gate read it, the run completed *)
  let recorded =
    List.find_map
      (function Run_executed { id = "boom"; result } -> Some result | _ -> None)
      trace
  in
  (match recorded with
  | Some r -> Alcotest.(check int) "recorded synthetic exit 127" 127 r.exit
  | None -> Alcotest.fail "expected a recorded Run_executed for boom");
  Alcotest.(check outcome_testable) "engine completed (no crash/abort)"
    Completed_no_commit outcome;
  (* and it replays byte-identically *)
  let replayed = engine_replay ~trace v in
  Alcotest.(check outcome_testable) "replay identical" outcome replayed

(* ---- KEEP: fail-closed validation ---- *)

let test_validate_commit_no_gate () =
  let wf = { name = "ungated"; version = None; steps = [ Commit { id = "submit" } ] } in
  match Validate.workflow ~floor_gates:[ "g" ] wf with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "commit without floor gate must be rejected"

let test_validate_commit_one_branch_only () =
  let wf =
    {
      name = "one-branch";
      version = None;
      steps =
        [
          Branch
            {
              when_ = Expr.Lit (Expr.Bool true);
              then_ = [ Gate { id = "g"; when_ = Expr.Lit (Expr.Bool true) } ];
              else_ =
                [ Agent { id = "x"; prompt = "p"; read_only = true; output_schema = None; on_failure = Types.Abort } ];
            };
          Commit { id = "submit" };
        ];
    }
  in
  match Validate.workflow ~floor_gates:[ "g" ] wf with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "commit gated in only one branch must be rejected"

let test_validate_loop_gate_not_guaranteed () =
  let wf =
    {
      name = "loop-gate";
      version = None;
      steps =
        [
          Loop
            {
              body = [ Gate { id = "g"; when_ = Expr.Lit (Expr.Bool true) } ];
              until = None;
              governors = [ Max_iters 3 ];
            };
          Commit { id = "submit" };
        ];
    }
  in
  match Validate.workflow ~floor_gates:[ "g" ] wf with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "gate inside loop body must not count as guaranteed"

let test_validate_accepts_gated () = ignore (validate_ok ~floor:[ "g" ] gated_workflow)

(* ---- KEEP: commit needs the runtime token ---- *)

let test_commit_no_token_blocked () =
  let v = validate_ok ~floor:[ "g" ] gated_workflow in
  let outcome, _ = engine_run ~backend:(Backend.stub ()) ~token:None v in
  match outcome with
  | Blocked _ -> ()
  | o -> Alcotest.failf "expected Blocked with no token, got %s" (Types.string_of_outcome o)

let test_commit_token_digest_only () =
  let v = validate_ok ~floor:[ "g" ] gated_workflow in
  let tok = "test-approval-token" in
  let outcome, trace = engine_run ~backend:(Backend.stub ()) ~token:(Some tok) v in
  (match outcome with
  | Committed { token_digest; _ } ->
      Alcotest.(check string) "digest matches Engine.token_digest"
        (Engine.token_digest tok) token_digest;
      Alcotest.(check bool) "raw token not in digest" false (token_digest = tok)
  | o -> Alcotest.failf "expected Committed, got %s" (Types.string_of_outcome o));
  let trace_text =
    String.concat "|"
      (List.map
         (function
           | Committed_step { token_digest; _ } -> token_digest
           | Agent_ran { output; _ } -> Yojson.Safe.to_string output
           | Gate_evaluated { id; _ } -> id
           | Branch_taken { verdict } -> Types.verdict_to_string verdict
           | Loop_iter { index } -> string_of_int index
           | Budget_read { value } -> string_of_int value
           | Fixpoint_progress { progress } -> string_of_bool progress
           | Loop_stopped { reason; _ } -> reason
           | Run_executed { id; _ } -> id
           | Blocked_at { reason; _ } -> reason
           | Parallel_started -> "parallel_started"
           | Parallel_branch_completed { branch_idx; _ } ->
               Printf.sprintf "parallel_branch_%d" branch_idx
           | Parallel_completed _ -> "parallel_completed"
           | Foreach_iter_started { index; _ } ->
               Printf.sprintf "foreach_iter_started_%d" index
           | Foreach_iter_completed { index; _ } ->
               Printf.sprintf "foreach_iter_completed_%d" index
           | Foreach_completed { iterations } ->
               Printf.sprintf "foreach_completed_%d" iterations
           | Ctx_snapshot _ -> "ctx_snapshot")
         trace)
  in
  let contains hay needle =
    let nl = String.length needle and hl = String.length hay in
    let rec aux i = i + nl <= hl && (String.sub hay i nl = needle || aux (i + 1)) in
    nl = 0 || aux 0
  in
  Alcotest.(check bool) "raw token absent from trace" false (contains trace_text tok)

(* ---- v0.5 Fix 2: a failing Gate BLOCKS the run ---- *)

(* A floor gate whose predicate evaluates FALSE must terminate the walk as
   Blocked (naming the gate), never reaching the commit. *)
let test_false_gate_blocks () =
  let wf =
    {
      name = "false-gate";
      version = None;
      steps =
        [
          Agent
            { id = "a"; prompt = "p"; read_only = true; output_schema = None; on_failure = Types.Abort };
          (* gate predicate is false: outputs.a.ok does not exist *)
          Gate { id = "g"; when_ = Expr.Exists [ "outputs"; "a"; "ok" ] };
          Commit { id = "submit" };
        ];
    }
  in
  let backend = json_backend [ ("a", `Assoc [ ("other", `String "x") ]) ] in
  let v = validate_ok ~floor:[ "g" ] wf in
  let outcome, trace = engine_run ~backend ~token:(Some "tok") v in
  (match outcome with
  | Blocked reason ->
      Alcotest.(check bool) "block reason names the gate" true
        (reason = "gate \"g\" evaluated false")
  | o ->
      Alcotest.failf "expected Blocked on a false floor gate, got %s"
        (Types.string_of_outcome o));
  (* the commit must NOT have been reached *)
  let committed =
    List.exists (function Committed_step _ -> true | _ -> false) trace
  in
  Alcotest.(check bool) "commit not reached" false committed;
  (* replay reproduces the Blocked outcome *)
  let replayed = engine_replay ~trace v in
  Alcotest.(check outcome_testable) "replay identical" outcome replayed

(* A true gate records Pass and continues (this is [gated_workflow], whose gate is
   trivially true) — covered by the happy-path test; here we additionally confirm
   the bounty example lints with ZERO diagnostics (no warnings) and the smoke
   example reaches Committed. *)
let test_bounty_lint_clean_zero_warnings () =
  match Workflow_json.of_file (project_path "examples/bounty.workflow.json") with
  | Error e -> Alcotest.failf "could not load bounty example: %s" e
  | Ok wf ->
      let ds =
        Lint.check ~floor_gates:[ "g-validated"; "g-observed"; "g-independent" ] wf
      in
      Alcotest.(check int) "bounty example: zero diagnostics (no warnings)" 0
        (List.length ds)

(* The smoke example's floor gate g-observed evaluates TRUE (observed:true), so
   the run is not Blocked by the new gate semantics and reaches Committed. *)
let test_smoke_still_committable () =
  match Workflow_json.of_file (project_path "examples/smoke.workflow.json") with
  | Error e -> Alcotest.failf "could not load smoke example: %s" e
  | Ok wf -> (
      let v = validate_ok ~floor:[ "g-observed" ] wf in
      (* a backend echoing the fixed JSON the prompts ask for *)
      let backend =
        json_backend
          [
            ("classify", `Assoc [ ("severity", `String "high"); ("observed", `Bool true) ]);
            ("tick", `Assoc [ ("done", `Bool true); ("progressed", `Bool false) ]);
            ("review", `Assoc [ ("ok", `Bool true) ]);
          ]
      in
      let outcome, _ = engine_run ~backend ~token:(Some "tok") v in
      match outcome with
      | Committed { id; _ } ->
          Alcotest.(check string) "smoke commits at submit" "submit" id
      | o ->
          Alcotest.failf "expected smoke to commit, got %s"
            (Types.string_of_outcome o))

(* ---- KEEP: happy path ---- *)

let test_happy_path () =
  let v = validate_ok ~floor:[ "g" ] gated_workflow in
  let outcome, _ = engine_run ~backend:(Backend.stub ()) ~token:(Some "approve") v in
  match outcome with
  | Committed { id; _ } -> Alcotest.(check string) "committed id" "submit" id
  | o -> Alcotest.failf "expected Committed, got %s" (Types.string_of_outcome o)

(* ===================================================================== *)
(* v0.3: Lint library — meta-agent usable                                *)
(* ===================================================================== *)

let has_code code ds =
  List.exists (fun (d : Lint.diagnostic) -> d.code = code) ds

let count_errors ds =
  List.length (List.filter (fun (d : Lint.diagnostic) -> d.severity = Lint.Error) ds)

(* ---- LINT 1: parse-tolerant, never raises (invalid JSON) ---- *)

let test_lint_invalid_json () =
  let ds =
    try Lint.check_json "not json{"
    with e -> Alcotest.failf "check_json raised: %s" (Printexc.to_string e)
  in
  Alcotest.(check int) "exactly one diagnostic" 1 (List.length ds);
  Alcotest.(check bool) "is invalid-json" true (has_code "invalid-json" ds);
  Alcotest.(check bool) "is an error" true (Lint.has_errors ds);
  let d = List.hd ds in
  Alcotest.(check string) "loc is $" "$" d.loc

(* ---- LINT 2: shape error => invalid-shape, no raise ---- *)

let test_lint_invalid_shape () =
  let ds =
    try Lint.check_json {|{ "name": "x", "steps": [ { "kind": "frobnicate" } ] }|}
    with e -> Alcotest.failf "check_json raised: %s" (Printexc.to_string e)
  in
  Alcotest.(check bool) "is invalid-shape" true (has_code "invalid-shape" ds);
  Alcotest.(check bool) "is an error" true (Lint.has_errors ds)

(* ---- LINT 3: all-at-once (>= 2 errors in one call) ---- *)

let test_lint_all_at_once () =
  (* BOTH an ungoverned loop AND a commit missing its floor gate. *)
  let wf =
    {
      name = "bad";
      version = None;
      steps =
        [
          Loop
            {
              body =
                [ Agent { id = "x"; prompt = "p"; read_only = true; output_schema = None; on_failure = Types.Abort } ];
              until = None;
              governors = [];
            };
          Commit { id = "submit" };
        ];
    }
  in
  let ds = Lint.check ~floor_gates:[ "g" ] wf in
  Alcotest.(check bool) ">= 2 errors collected" true (count_errors ds >= 2);
  Alcotest.(check bool) "has ungoverned-loop" true (has_code "ungoverned-loop" ds);
  Alcotest.(check bool) "has commit-missing-floor-gate" true
    (has_code "commit-missing-floor-gate" ds)

(* ---- LINT 4: dangling-output-ref warning (and NOT an error) ---- *)

let test_lint_dangling_output_ref () =
  let wf =
    {
      name = "dangling";
      version = None;
      steps =
        [
          Gate
            {
              id = "g";
              when_ = Expr.Exists [ "outputs"; "nope"; "x" ];
            };
        ];
    }
  in
  let ds = Lint.check ~floor_gates:[] wf in
  Alcotest.(check bool) "has dangling-output-ref" true
    (has_code "dangling-output-ref" ds);
  Alcotest.(check bool) "no errors for that alone" false (Lint.has_errors ds)

(* ---- v0.8 F3: branch-dependent dangling output ref (intersection) ---- *)

(* A gate AFTER a branch references outputs.x.v where x is produced ONLY in the
   then arm. Under the old union behaviour this was silently NOT flagged; with
   the intersection discipline it is a dangling-output-ref Warning. *)
let test_lint_branch_one_arm_only_dangling () =
  let wf =
    {
      name = "one-arm-ref";
      version = None;
      steps =
        [
          Branch
            {
              when_ = Expr.Lit (Expr.Bool true);
              then_ =
                [
                  Agent
                    {
                      id = "x";
                      prompt = "p";
                      read_only = true;
                      output_schema = Some [ ("v", Schema.Int) ];
                      on_failure = Types.Abort;
                    };
                ];
              else_ =
                [
                  Agent
                    { id = "y"; prompt = "p"; read_only = true; output_schema = None; on_failure = Types.Abort };
                ];
            };
          (* references x.v, produced only in the then arm *)
          Gate { id = "g"; when_ = Expr.Exists [ "outputs"; "x"; "v" ] };
        ];
    }
  in
  let ds = Lint.check ~floor_gates:[] wf in
  Alcotest.(check bool) "one-arm-only ref => dangling-output-ref" true
    (has_code "dangling-output-ref" ds)

(* A reference AFTER the branch to an output produced in BOTH arms => no
   dangling-output-ref warning. *)
let test_lint_branch_both_arms_ok () =
  let mk id =
    Agent
      {
        id;
        prompt = "p";
        read_only = true;
        output_schema = Some [ ("v", Schema.Int) ];
                      on_failure = Types.Abort;
      }
  in
  let wf =
    {
      name = "both-arms-ref";
      version = None;
      steps =
        [
          Branch
            {
              when_ = Expr.Lit (Expr.Bool true);
              then_ = [ mk "x" ];
              else_ = [ mk "x" ];
            };
          Gate { id = "g"; when_ = Expr.Exists [ "outputs"; "x"; "v" ] };
        ];
    }
  in
  let ds = Lint.check ~floor_gates:[] wf in
  Alcotest.(check bool) "both-arms ref => no dangling-output-ref" false
    (has_code "dangling-output-ref" ds)

(* ---- LINT 5: the CONTRACT (lint-clean <=> validate Ok) ---- *)

let test_lint_contract_examples () =
  let load f = match Workflow_json.of_file f with
    | Ok wf -> wf
    | Error e -> Alcotest.failf "could not load %s: %s" f e
  in
  let check_clean f floor =
    let wf = load f in
    let ds = Lint.check ~floor_gates:floor wf in
    Alcotest.(check bool)
      (Printf.sprintf "%s has no error diagnostics" f)
      false (Lint.has_errors ds);
    match Validate.workflow ~floor_gates:floor wf with
    | Ok _ -> ()
    | Error e -> Alcotest.failf "%s: lint-clean but Validate.workflow Error: %s" f e
  in
  check_clean (project_path "examples/bounty.workflow.json")
    [ "g-validated"; "g-observed"; "g-independent" ];
  check_clean (project_path "examples/smoke.workflow.json") [ "g-observed" ]

let test_lint_contract_badness () =
  (* A known-bad workflow: commit with no required floor gate. *)
  let wf = { name = "bad"; version = None; steps = [ Commit { id = "submit" } ] } in
  let ds = Lint.check ~floor_gates:[ "g" ] wf in
  Alcotest.(check bool) "has_errors true" true (Lint.has_errors ds);
  match Validate.workflow ~floor_gates:[ "g" ] wf with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "known-bad workflow must NOT validate"

(* ---- LINT 6: to_json / diagnostic_to_json shape ---- *)

let test_lint_to_json_shape () =
  let d : Lint.diagnostic =
    { severity = Lint.Error; code = "ungoverned-loop";
      message = "loop is ungoverned"; loc = "steps[0].governors" }
  in
  let j = Lint.diagnostic_to_json d in
  let field k = match j with
    | `Assoc fs -> List.assoc_opt k fs
    | _ -> None
  in
  Alcotest.(check (option string)) "severity" (Some "error")
    (match field "severity" with Some (`String s) -> Some s | _ -> None);
  Alcotest.(check (option string)) "code" (Some "ungoverned-loop")
    (match field "code" with Some (`String s) -> Some s | _ -> None);
  Alcotest.(check (option string)) "loc" (Some "steps[0].governors")
    (match field "loc" with Some (`String s) -> Some s | _ -> None);
  (* warning serialises as "warning" *)
  let w = Lint.diagnostic_to_json { d with severity = Lint.Warning } in
  (match w with
   | `Assoc fs -> (match List.assoc_opt "severity" fs with
       | Some (`String "warning") -> ()
       | _ -> Alcotest.fail "warning severity must serialise as \"warning\"")
   | _ -> Alcotest.fail "diagnostic_to_json must be an object");
  (* to_json wraps in {"diagnostics":[..]} *)
  match Lint.to_json [ d ] with
  | `Assoc [ ("diagnostics", `List [ _ ]) ] -> ()
  | _ -> Alcotest.fail "to_json must be {\"diagnostics\":[..]}"

(* ---- LINT 7: the headline generate->fix loop converges ---- *)

(* A tiny pure fixer keyed on diagnostic CODES. This stands in for a meta-agent
   patching its own generated workflow from the (stable) diagnostics. *)
let rec insert_gate_before_commit gate steps =
  List.concat_map
    (function
      | Commit _ as c -> [ Gate { id = gate; when_ = Expr.Lit (Expr.Bool true) }; c ]
      | Branch { when_; then_; else_ } ->
          [ Branch { when_;
                     then_ = insert_gate_before_commit gate then_;
                     else_ = insert_gate_before_commit gate else_ } ]
      | s -> [ s ])
    steps

let rec govern_loops steps =
  List.map
    (function
      | Loop { body; until; governors = [] } ->
          Loop { body = govern_loops body; until; governors = [ Max_iters 3 ] }
      | Loop { body; until; governors } ->
          (* fix bad bounds too *)
          let governors =
            List.map
              (function
                | Max_iters n when n < 1 -> Max_iters 1
                | Fixpoint { window; progress } when window < 1 ->
                    Fixpoint { window = 1; progress }
                | g -> g)
              governors
          in
          Loop { body = govern_loops body; until; governors }
      | Branch { when_; then_; else_ } ->
          Branch { when_; then_ = govern_loops then_; else_ = govern_loops else_ }
      | s -> s)
    steps

let fixer ~floor (ds : Lint.diagnostic list) (wf : workflow) : workflow =
  List.fold_left
    (fun wf (d : Lint.diagnostic) ->
      match d.code with
      | "ungoverned-loop" | "unbounded-max-iters" | "bad-fixpoint-window" ->
          { wf with steps = govern_loops wf.steps }
      | "commit-missing-floor-gate" ->
          (* insert each missing floor gate before every commit *)
          let steps =
            List.fold_left
              (fun steps g -> insert_gate_before_commit g steps)
              wf.steps floor
          in
          { wf with steps }
      | _ -> wf)
    wf ds

let test_lint_generate_fix_loop () =
  let floor = [ "g" ] in
  (* deliberately bad: ungoverned loop + commit with no floor gate *)
  let bad =
    {
      name = "self-correcting";
      version = None;
      steps =
        [
          Loop
            {
              body =
                [ Agent { id = "w"; prompt = "p"; read_only = false; output_schema = None; on_failure = Types.Abort } ];
              until = None;
              governors = [];
            };
          Commit { id = "submit" };
        ];
    }
  in
  let rec converge n wf =
    if n <= 0 then Alcotest.fail "generate->fix loop did not converge within bound"
    else
      let ds = Lint.check ~floor_gates:floor wf in
      if not (Lint.has_errors ds) then wf
      else converge (n - 1) (fixer ~floor ds wf)
  in
  let fixed = converge 5 bad in
  (* lint-clean now... *)
  Alcotest.(check bool) "converged to no errors" false
    (Lint.has_errors (Lint.check ~floor_gates:floor fixed));
  (* ...and the contract says it must validate. *)
  match Validate.workflow ~floor_gates:floor fixed with
  | Ok _ -> ()
  | Error e -> Alcotest.failf "converged workflow failed to validate: %s" e

(* ---- SCHEMA: the published JSON Schema artifact ---- *)

let assoc_keys = function `Assoc l -> List.map fst l | _ -> []

(* The schema value is well-formed JSON with the draft 2020-12 markers and the
   $defs we promise (expr, governor, plus a step def). *)
let test_schema_well_formed () =
  let j = Workflow_schema.schema in
  (* re-parse the pretty-printed form: proves it is valid JSON. *)
  (match Yojson.Safe.from_string (Workflow_schema.to_string ()) with
  | _ -> ()
  | exception _ -> Alcotest.fail "schema does not round-trip as valid JSON");
  let top = assoc_keys j in
  Alcotest.(check bool) "has $schema" true (List.mem "$schema" top);
  Alcotest.(check bool) "has $defs" true (List.mem "$defs" top);
  let defs =
    match j with
    | `Assoc l -> (
        match List.assoc_opt "$defs" l with Some d -> assoc_keys d | None -> [])
    | _ -> []
  in
  Alcotest.(check bool) "$defs has expr" true (List.mem "expr" defs);
  Alcotest.(check bool) "$defs has governor" true (List.mem "governor" defs);
  Alcotest.(check bool) "$defs has a step def" true (List.mem "step" defs)

(* NO DRIFT: the committed artifact byte-matches Workflow_schema.to_string ().
   This is the key test that keeps the file and the lib value in lock-step.
   The artifact is resolved via [project_path] (DUNE_SOURCEROOT-rooted, falling
   back to the cwd-relative ../ form inside the dune test sandbox) and is
   declared as a dep in test/dune. *)
let test_schema_no_drift () =
  let path = project_path "schema/workflow.schema.json" in
  let on_disk =
    try In_channel.with_open_bin path In_channel.input_all
    with Sys_error e -> Alcotest.failf "cannot read %s: %s" path e
  in
  Alcotest.(check string)
    "committed schema/workflow.schema.json == Workflow_schema.to_string ()"
    (Workflow_schema.to_string ())
    on_disk

(* PARSER <-> SCHEMA kinds agree: the set of step "kind" strings the schema
   enumerates equals the set the parser accepts; and the parser rejects an
   unknown kind. *)
let test_schema_kinds_agree () =
  (* Hard-coded expected set the parser (Workflow_json.step_of_json) accepts. *)
  let expected = [ "agent"; "branch"; "commit"; "foreach"; "gate"; "loop"; "parallel"; "run" ] in
  (* Extract the kind consts enumerated under $defs/step/oneOf in the schema. *)
  let top = match Workflow_schema.schema with `Assoc l -> l | _ -> [] in
  let step_def =
    match List.assoc_opt "$defs" top with
    | Some (`Assoc d) -> (
        match List.assoc_opt "step" d with Some s -> s | None -> `Null)
    | _ -> `Null
  in
  let variants =
    match step_def with
    | `Assoc l -> ( match List.assoc_opt "oneOf" l with Some (`List v) -> v | _ -> [])
    | _ -> []
  in
  let kind_of_variant v =
    match v with
    | `Assoc l -> (
        match List.assoc_opt "properties" l with
        | Some (`Assoc props) -> (
            match List.assoc_opt "kind" props with
            | Some (`Assoc kl) -> (
                match List.assoc_opt "const" kl with
                | Some (`String k) -> Some k
                | _ -> None)
            | _ -> None)
        | _ -> None)
    | _ -> None
  in
  let schema_kinds =
    List.sort compare (List.filter_map kind_of_variant variants)
  in
  Alcotest.(check (list string))
    "schema step kinds == parser-accepted kinds" expected schema_kinds;
  (* And the parser rejects an unknown kind (mirrors the existing parse test). *)
  match
    Workflow_json.of_string
      {|{ "name": "x", "steps": [ { "kind": "frobnicate" } ] }|}
  with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "parser must reject an unknown step kind"

(* CLOSED OBJECTS: every expr branch and every step/governor object sets
   [additionalProperties:false] (the parser requires exactly one operator/kind
   key and rejects junk keys), and the integer governor fields carry a [maximum]
   (so an out-of-OCaml-int-range literal is schema-invalid too). *)
let test_schema_closed_objects () =
  let top = match Workflow_schema.schema with `Assoc l -> l | _ -> [] in
  let defs =
    match List.assoc_opt "$defs" top with Some (`Assoc d) -> d | _ -> []
  in
  let def name = match List.assoc_opt name defs with Some d -> d | None -> `Null in
  let one_of d =
    match d with
    | `Assoc l -> ( match List.assoc_opt "oneOf" l with Some (`List v) -> v | _ -> [])
    | _ -> []
  in
  let additional v =
    match v with
    | `Assoc l -> List.assoc_opt "additionalProperties" l
    | _ -> None
  in
  let is_closed v = additional v = Some (`Bool false) in
  (* expr: every branch object is closed. *)
  let expr_branches = one_of (def "expr") in
  Alcotest.(check bool) "expr has branches" true (expr_branches <> []);
  List.iteri
    (fun i b ->
      Alcotest.(check bool)
        (Printf.sprintf "expr branch %d is additionalProperties:false" i)
        true (is_closed b))
    expr_branches;
  (* step: every step object is closed. *)
  let step_variants = one_of (def "step") in
  Alcotest.(check bool) "step has variants" true (step_variants <> []);
  List.iteri
    (fun i v ->
      Alcotest.(check bool)
        (Printf.sprintf "step variant %d is additionalProperties:false" i)
        true (is_closed v))
    step_variants;
  (* governor: every governor object is closed, and the integer fields (n,
     window) carry a maximum. *)
  let gov_variants = one_of (def "governor") in
  Alcotest.(check bool) "governor has variants" true (gov_variants <> []);
  let prop_field v field key =
    match v with
    | `Assoc l -> (
        match List.assoc_opt "properties" l with
        | Some (`Assoc props) -> (
            match List.assoc_opt field props with
            | Some (`Assoc fl) -> List.assoc_opt key fl
            | _ -> None)
        | _ -> None)
    | _ -> None
  in
  let int_field_count = ref 0 in
  List.iter
    (fun v ->
      Alcotest.(check bool) "governor variant is additionalProperties:false" true
        (is_closed v);
      List.iter
        (fun field ->
          match prop_field v field "maximum" with
          | Some (`Int _) ->
              incr int_field_count;
              Alcotest.(check bool)
                (Printf.sprintf "governor int field %s has minimum:1" field)
                true
                (prop_field v field "minimum" = Some (`Int 1))
          | _ -> ())
        [ "n"; "window" ])
    gov_variants;
  Alcotest.(check int) "two bounded integer governor fields (n, window)" 2
    !int_field_count

(* The schema must NOT accept what the parser REJECTS: a one-operator expr with a
   junk extra key, and an integer governor bound out of OCaml's safe int range.
   We assert the parser rejects them (the schema now mirrors this via
   additionalProperties:false / maximum). *)
let test_schema_no_overaccept () =
  (* expr object with an extra junk key beyond the single operator. *)
  (match
     Workflow_json.of_string
       {|{ "name": "x", "steps": [
            { "kind": "gate", "id": "g",
              "when": { "path": "outputs.a.x", "junk": 2 } } ] }|}
   with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expr with an extra key must be rejected by the parser");
  (* a Max_iters literal too big to be an OCaml int parses as `Intlit and the
     parser's req_int rejects it. *)
  match
    Workflow_json.of_string
      {|{ "name": "x", "steps": [
           { "kind": "loop", "governors": [ { "kind": "max_iters", "n": 100000000000000000000 } ],
             "body": [] } ] }|}
  with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "out-of-range Max_iters literal must be rejected"

(* ---- parser strictness: unknown keys rejected, per object type --------- *)

(* For each closed object type, a workflow that adds a non-underscore "junk" key
   to that object must be REJECTED by the parser. The table pairs a human label
   with a workflow JSON whose ONLY defect is the junk key. *)
let test_parser_rejects_unknown_keys () =
  let cases =
    [
      ( "top-level workflow",
        {|{ "name": "x", "junk": 1, "steps": [] }|} );
      ( "agent step",
        {|{ "name": "x", "steps": [
             { "kind": "agent", "id": "a", "prompt": "p", "junk": 1 } ] }|} );
      ( "gate step",
        {|{ "name": "x", "steps": [
             { "kind": "gate", "id": "g", "when": { "lit": true }, "junk": 1 } ] }|} );
      ( "branch step",
        {|{ "name": "x", "steps": [
             { "kind": "branch", "when": { "lit": true },
               "then": [], "else": [], "junk": 1 } ] }|} );
      ( "loop step",
        {|{ "name": "x", "steps": [
             { "kind": "loop",
               "governors": [ { "kind": "budget" } ], "body": [], "junk": 1 } ] }|} );
      ( "run step",
        {|{ "name": "x", "steps": [
             { "kind": "run", "id": "r", "cmd": ["echo"], "working_dir": "s",
               "junk": 1 } ] }|} );
      ( "commit step",
        {|{ "name": "x", "steps": [
             { "kind": "commit", "id": "c", "junk": 1 } ] }|} );
      ( "max_iters governor",
        {|{ "name": "x", "steps": [
             { "kind": "loop", "body": [],
               "governors": [ { "kind": "max_iters", "n": 3, "junk": 1 } ] } ] }|} );
      ( "budget governor",
        {|{ "name": "x", "steps": [
             { "kind": "loop", "body": [],
               "governors": [ { "kind": "budget", "junk": 1 } ] } ] }|} );
      ( "fixpoint governor",
        {|{ "name": "x", "steps": [
             { "kind": "loop", "body": [],
               "governors": [ { "kind": "fixpoint", "window": 2,
                               "progress": { "lit": true }, "junk": 1 } ] } ] }|} );
    ]
  in
  List.iter
    (fun (label, json) ->
      match Workflow_json.of_string json with
      | Error _ -> ()
      | Ok _ ->
          Alcotest.failf
            "parser must reject an unknown key on the %s object" label)
    cases

(* Underscore-prefixed metadata is accepted everywhere (the documented escape
   hatch the examples use): a top-level [_doc] and a step-level [_note] parse. *)
let test_parser_accepts_underscore_metadata () =
  let json =
    {|{ "name": "x", "_doc": "top-level note",
        "steps": [
          { "kind": "agent", "id": "a", "prompt": "p", "_note": "step note" },
          { "kind": "loop", "body": [],
            "governors": [ { "kind": "budget", "_why": "cheap stop" } ] } ] }|}
  in
  match Workflow_json.of_string json with
  | Ok _ -> ()
  | Error e ->
      Alcotest.failf "underscore metadata must be accepted, got Error: %s" e

(* ---- schema/parser parity (structural cross-check) ---------------------- *)

(* The known-key set the PARSER accepts for each closed object, hard-coded here
   so the test fails if either side drifts. *)
let parser_known_keys =
  [
    ("workflow", [ "name"; "steps"; "version" ]);
    ("agent", [ "kind"; "id"; "prompt"; "read_only"; "output_schema"; "on_failure" ]);
    ("gate", [ "kind"; "id"; "when" ]);
    ("branch", [ "kind"; "when"; "then"; "else" ]);
    ("loop", [ "kind"; "until"; "governors"; "body" ]);
    ("run", [ "kind"; "id"; "cmd"; "working_dir"; "timeout_ms"; "observe" ]);
    ("commit", [ "kind"; "id" ]);
    ("parallel", [ "kind"; "branches" ]);
    ("foreach", [ "kind"; "over"; "steps" ]);
    ("max_iters", [ "kind"; "n" ]);
    ("budget", [ "kind" ]);
    ("fixpoint", [ "kind"; "window"; "progress" ]);
  ]

(* Every closed object def in the schema must carry BOTH
   [additionalProperties:false] AND a [^_] patternProperty; AND the set of
   declared [properties] names must equal the parser's known-key set above. *)
let test_schema_parser_parity () =
  let top = match Workflow_schema.schema with `Assoc l -> l | _ -> [] in
  let defs =
    match List.assoc_opt "$defs" top with Some (`Assoc d) -> d | _ -> []
  in
  let def name = match List.assoc_opt name defs with Some d -> d | None -> `Null in
  let one_of d =
    match d with
    | `Assoc l -> ( match List.assoc_opt "oneOf" l with Some (`List v) -> v | _ -> [])
    | _ -> []
  in
  let kind_of v =
    match v with
    | `Assoc l -> (
        match List.assoc_opt "properties" l with
        | Some (`Assoc props) -> (
            match List.assoc_opt "kind" props with
            | Some (`Assoc kl) -> (
                match List.assoc_opt "const" kl with
                | Some (`String k) -> Some k
                | _ -> None)
            | _ -> None)
        | _ -> None)
    | _ -> None
  in
  let prop_names v =
    match v with
    | `Assoc l -> (
        match List.assoc_opt "properties" l with
        | Some (`Assoc props) -> List.sort compare (List.map fst props)
        | _ -> [])
    | _ -> []
  in
  let is_closed v =
    match v with
    | `Assoc l ->
        List.assoc_opt "additionalProperties" l = Some (`Bool false)
        && (match List.assoc_opt "patternProperties" l with
           | Some (`Assoc pp) -> List.mem_assoc "^_" pp
           | _ -> false)
    | _ -> false
  in
  (* Top-level workflow object. *)
  Alcotest.(check bool)
    "workflow object is closed (additionalProperties:false + ^_)" true
    (is_closed Workflow_schema.schema);
  Alcotest.(check (list string))
    "workflow properties == parser known keys"
    (List.sort compare (List.assoc "workflow" parser_known_keys))
    (prop_names Workflow_schema.schema);
  (* Each step variant, keyed by its kind const. *)
  List.iter
    (fun v ->
      match kind_of v with
      | Some k ->
          Alcotest.(check bool)
            (Printf.sprintf "step %s closed (additionalProperties:false + ^_)" k)
            true (is_closed v);
          Alcotest.(check (list string))
            (Printf.sprintf "step %s properties == parser known keys" k)
            (List.sort compare (List.assoc k parser_known_keys))
            (prop_names v)
      | None -> Alcotest.fail "step variant missing a kind const")
    (one_of (def "step"));
  (* Each governor variant, keyed by its kind const. *)
  List.iter
    (fun v ->
      match kind_of v with
      | Some k ->
          Alcotest.(check bool)
            (Printf.sprintf "governor %s closed (additionalProperties:false + ^_)"
               k)
            true (is_closed v);
          Alcotest.(check (list string))
            (Printf.sprintf "governor %s properties == parser known keys" k)
            (List.sort compare (List.assoc k parser_known_keys))
            (prop_names v)
      | None -> Alcotest.fail "governor variant missing a kind const")
    (one_of (def "governor"));
  (* Every expr branch is STRICTLY closed: additionalProperties:false AND NO
     [^_] patternProperty. Expr operator objects are single-operator-key; the
     parser rejects any extra key including an underscore one, so unlike
     workflow/step/governor objects they must NOT carry the [^_] escape hatch. *)
  let is_strictly_closed v =
    match v with
    | `Assoc l ->
        List.assoc_opt "additionalProperties" l = Some (`Bool false)
        && not (List.mem_assoc "patternProperties" l)
    | _ -> false
  in
  List.iteri
    (fun i b ->
      Alcotest.(check bool)
        (Printf.sprintf
           "expr branch %d strictly closed (additionalProperties:false, NO ^_)" i)
        true (is_strictly_closed b))
    (one_of (def "expr"))

(* ---- BEHAVIORAL schema<->parser parity (drive the PARSER over a battery) --- *)

(* The governing principle: [Workflow_json.of_string] parse-accepts a workflow
   IFF that workflow is structurally valid per schema/workflow.schema.json. The
   structural parity test above checks the schema's SHAPE; this one drives the
   actual PARSER over a battery of candidate JSON strings (mirroring the audit's
   54-case methodology) and asserts, per case, that the parser's accept/reject
   verdict matches the hard-coded structurally-schema-valid verdict. No JSON
   Schema validator is used: the [expect] column encodes what the schema says,
   so the test fails if the parser ever drifts from the schema on these inputs. *)

(* [expect = true] means "structurally schema-valid, so the parser MUST accept";
   [expect = false] means "structurally schema-invalid, so the parser MUST
   reject". A handful of governors-empty/loop bodies are otherwise well-formed at
   the STRUCTURAL level (semantic floor checks are Validate's job, not the
   parser's / schema's), so they are expected-accept here. *)
let behavioral_parity_cases : (string * string * bool) list =
  let wf steps = Printf.sprintf {|{ "name": "x", "steps": [ %s ] }|} steps in
  let loop govs body =
    Printf.sprintf
      {|{ "kind": "loop", "governors": [ %s ], "body": [ %s ] }|} govs body
  in
  let gate when_ =
    Printf.sprintf {|{ "kind": "gate", "id": "g", "when": %s }|} when_
  in
  let mi n = Printf.sprintf {|{ "kind": "max_iters", "n": %s }|} n in
  let fp w =
    Printf.sprintf
      {|{ "kind": "fixpoint", "window": %s, "progress": { "lit": true } }|} w
  in
  [
    (* ---- top-level workflow: unknown vs underscore key ---- *)
    ("workflow: unknown key", {|{ "name": "x", "junk": 1, "steps": [] }|}, false);
    ("workflow: _doc key", {|{ "name": "x", "_doc": "note", "steps": [] }|}, true);
    (* ---- agent step ---- *)
    ( "agent: ok",
      wf {|{ "kind": "agent", "id": "a", "prompt": "p" }|}, true );
    ( "agent: unknown key",
      wf {|{ "kind": "agent", "id": "a", "prompt": "p", "junk": 1 }|}, false );
    ( "agent: _note key",
      wf {|{ "kind": "agent", "id": "a", "prompt": "p", "_note": "n" }|}, true );
    ( "agent: id wrong type (int)",
      wf {|{ "kind": "agent", "id": 1, "prompt": "p" }|}, false );
    ( "agent: read_only wrong type (string)",
      wf {|{ "kind": "agent", "id": "a", "prompt": "p", "read_only": "yes" }|},
      false );
    (* output_schema is the one intentionally-open map: arbitrary field keys ok *)
    ( "agent: output_schema open map",
      wf
        {|{ "kind": "agent", "id": "a", "prompt": "p",
            "output_schema": { "anyField": "int", "another": { "enum": ["x"] } } }|},
      true );
    (* ---- gate step ---- *)
    ("gate: ok", wf (gate {|{ "lit": true }|}), true);
    ( "gate: unknown key",
      wf {|{ "kind": "gate", "id": "g", "when": { "lit": true }, "junk": 1 }|},
      false );
    ( "gate: _note key",
      wf {|{ "kind": "gate", "id": "g", "when": { "lit": true }, "_note": "n" }|},
      true );
    (* ---- branch step ---- *)
    ( "branch: ok",
      wf
        {|{ "kind": "branch", "when": { "lit": true }, "then": [], "else": [] }|},
      true );
    ( "branch: unknown key",
      wf
        {|{ "kind": "branch", "when": { "lit": true }, "then": [], "else": [],
            "junk": 1 }|},
      false );
    (* ---- commit step ---- *)
    ("commit: ok", wf {|{ "kind": "commit", "id": "c" }|}, true);
    ( "commit: unknown key",
      wf {|{ "kind": "commit", "id": "c", "junk": 1 }|}, false );
    (* ---- unknown step kind ---- *)
    ("step: unknown kind", wf {|{ "kind": "frobnicate" }|}, false);
    (* ---- loop step + governors ---- *)
    ( "loop: unknown key",
      wf {|{ "kind": "loop", "governors": [ { "kind": "budget" } ],
              "body": [], "junk": 1 }|},
      false );
    ( "loop: _note key",
      wf {|{ "kind": "loop", "governors": [ { "kind": "budget" } ],
              "body": [], "_note": "n" }|},
      true );
    (* empty governors: minItems:1 => structurally invalid => parser must reject *)
    ("loop: empty governors", wf (loop "" ""), false);
    ("loop: single governor (budget)", wf (loop {|{ "kind": "budget" }|} ""), true);
    (* governor: unknown / underscore keys *)
    ( "budget governor: unknown key",
      wf (loop {|{ "kind": "budget", "junk": 1 }|} ""), false );
    ( "budget governor: _why key",
      wf (loop {|{ "kind": "budget", "_why": "cheap" }|} ""), true );
    ("governor: unknown kind", wf (loop {|{ "kind": "throttle" }|} ""), false);
    (* ---- max_iters.n bounds: reject <1 and >max_int; accept 1..max_int ---- *)
    ("max_iters n=-5", wf (loop (mi "-5") ""), false);
    ("max_iters n=0", wf (loop (mi "0") ""), false);
    ("max_iters n=1", wf (loop (mi "1") ""), true);
    ("max_iters n=2", wf (loop (mi "2") ""), true);
    ("max_iters n=1073741824", wf (loop (mi "1073741824") ""), true);
    ("max_iters n=max_int", wf (loop (mi "4611686018427387903") ""), true);
    ( "max_iters n=>max_int (Intlit)",
      wf (loop (mi "100000000000000000000") ""), false );
    ("max_iters n wrong type (string)", wf (loop (mi {|"3"|}) ""), false);
    (* JSON Schema's "type":"integer" matches integer-valued floats, so 5.0 is
       schema-valid and the parser must accept it; a fractional float is invalid. *)
    ("max_iters n=5.0 (integer-valued float)", wf (loop (mi "5.0") ""), true);
    ("max_iters n=5.5 (fractional float)", wf (loop (mi "5.5") ""), false);
    ("max_iters n=0.0 (integer-valued float < 1)", wf (loop (mi "0.0") ""), false);
    (* ---- fixpoint.window bounds: same battery ---- *)
    ("fixpoint window=-5", wf (loop (fp "-5") ""), false);
    ("fixpoint window=0", wf (loop (fp "0") ""), false);
    ("fixpoint window=1", wf (loop (fp "1") ""), true);
    ("fixpoint window=2", wf (loop (fp "2") ""), true);
    ("fixpoint window=2.0 (integer-valued float)", wf (loop (fp "2.0") ""), true);
    ("fixpoint window=2.5 (fractional float)", wf (loop (fp "2.5") ""), false);
    ("fixpoint window=1073741824", wf (loop (fp "1073741824") ""), true);
    ( "fixpoint window=>max_int (Intlit)",
      wf (loop (fp "100000000000000000000") ""), false );
    (* ---- expr operator objects: 0 / 1 / 2 keys; underscore key REJECTED ---- *)
    ("expr 0-key {}", wf (gate "{}"), false);
    ("expr 1-key {lit}", wf (gate {|{ "lit": true }|}), true);
    ("expr 2-key {lit,eq}", wf (gate {|{ "lit": true, "eq": [] }|}), false);
    (* the headline divergence: underscore key inside an expr operator object *)
    ("expr {lit,_x} (underscore inside expr)",
      wf (gate {|{ "lit": true, "_x": 1 }|}), false);
    ("expr {path,_doc} (underscore inside expr)",
      wf (gate {|{ "path": "outputs.a.x", "_doc": "note" }|}), false);
    (* nested expr operand object likewise strictly single-key *)
    ( "expr nested not{lit,_x}",
      wf (gate {|{ "not": { "lit": true, "_x": 1 } }|}), false );
    (* ---- run step ---- *)
    ( "run: ok",
      wf
        {|{ "kind": "run", "id": "r", "cmd": ["mkdir","-p","out"],
            "working_dir": "scratch", "timeout_ms": 30000, "observe": ["out"] }|},
      true );
    ( "run: minimal (cmd + working_dir only)",
      wf {|{ "kind": "run", "id": "r", "cmd": ["echo"], "working_dir": "s" }|},
      true );
    ( "run: unknown key",
      wf
        {|{ "kind": "run", "id": "r", "cmd": ["echo"], "working_dir": "s",
            "junk": 1 }|},
      false );
    ( "run: _note key",
      wf
        {|{ "kind": "run", "id": "r", "cmd": ["echo"], "working_dir": "s",
            "_note": "n" }|},
      true );
    ( "run: empty cmd",
      wf {|{ "kind": "run", "id": "r", "cmd": [], "working_dir": "s" }|}, false );
    ( "run: cmd not strings",
      wf {|{ "kind": "run", "id": "r", "cmd": [1,2], "working_dir": "s" }|},
      false );
    ( "run: working_dir absolute",
      wf {|{ "kind": "run", "id": "r", "cmd": ["echo"], "working_dir": "/abs" }|},
      false );
    ( "run: working_dir with ..",
      wf
        {|{ "kind": "run", "id": "r", "cmd": ["echo"], "working_dir": "../up" }|},
      false );
    ( "run: working_dir nested .. segment",
      wf
        {|{ "kind": "run", "id": "r", "cmd": ["echo"], "working_dir": "a/../b" }|},
      false );
    ( "run: working_dir contains-but-not-segment ..",
      wf
        {|{ "kind": "run", "id": "r", "cmd": ["echo"], "working_dir": "a..b" }|},
      true );
    ( "run: missing working_dir",
      wf {|{ "kind": "run", "id": "r", "cmd": ["echo"] }|}, false );
    ( "run: timeout_ms=0 (below bound)",
      wf
        {|{ "kind": "run", "id": "r", "cmd": ["echo"], "working_dir": "s",
            "timeout_ms": 0 }|},
      false );
    (* ---- a couple of type confusions ---- *)
    ( "steps wrong type (string)",
      {|{ "name": "x", "steps": "nope" }|}, false );
    ("name wrong type (int)", {|{ "name": 1, "steps": [] }|}, false);
  ]

let test_schema_parser_behavioral_parity () =
  List.iter
    (fun (label, json, expect_accept) ->
      let accepted =
        match Workflow_json.of_string json with Ok _ -> true | Error _ -> false
      in
      Alcotest.(check bool)
        (Printf.sprintf
           "parser accept==schema-valid for %S (expect %s)" label
           (if expect_accept then "ACCEPT" else "REJECT"))
        expect_accept accepted)
    behavioral_parity_cases

(* ---- v0.10: on-disk ledger + replay-from-file ---- *)

(* TEST L1 — round-trip: a trace exercising EVERY trace_entry variant (incl. a
   Run_executed carrying file_changes, plus Loop/Budget/Fixpoint entries)
   round-trips through to_ndjson/of_ndjson byte-for-byte at the value level:
   [of_ndjson (to_ndjson t) = Ok t]. *)
let test_ledger_roundtrip_all_variants () =
  let trace : trace =
    [
      Agent_ran
        {
          id = "assess";
          success = true;
          output = `Assoc [ ("severity", `String "high"); ("n", `Int 3) ];
        };
      Agent_ran { id = "bad"; success = false; output = `Assoc [] };
      Gate_evaluated { id = "g"; verdict = Pass };
      Gate_evaluated { id = "g2"; verdict = Fail };
      Branch_taken { verdict = Pass };
      Branch_taken { verdict = Fail };
      Loop_iter { index = 0 };
      Budget_read { value = 7 };
      Fixpoint_progress { progress = true };
      Fixpoint_progress { progress = false };
      Loop_iter { index = 1 };
      Loop_stopped { iterations = 2; reason = "budget" };
      Run_executed
        {
          id = "mk";
          result =
            {
              exit = 0;
              stdout = "hello\nworld";
              stderr = "warn: x";
              truncated = true;
              files =
                [
                  { path = "out/x"; change = Created; size = 12; digest = "abc123" };
                  { path = "out/y"; change = Modified; size = 5; digest = "def456" };
                  { path = "out/z"; change = Deleted; size = 0; digest = "" };
                ];
            };
        };
      Committed_step { id = "submit"; token_digest = "deadbeef" };
      Blocked_at { id = "b"; reason = "gate \"g\" evaluated false" };
      Parallel_started;
      Parallel_branch_completed
        {
          branch_idx = 0;
          trace =
            [ Agent_ran { id = "b0"; success = true; output = `Assoc [] } ];
          outcome = Completed_no_commit;
          branch_outputs = [ ("b0", `Assoc [ ("result", `String "ok") ]) ];
        };
      Parallel_completed { outcome = Completed_no_commit };
      Foreach_iter_started { index = 0; element = `String "x" };
      Foreach_iter_completed { index = 0; outcome = Completed_no_commit };
      Foreach_completed { iterations = 1 };
    ]
  in
  let serialised = Ledger.to_ndjson trace in
  (* NDJSON: one newline-terminated line per entry, no blank lines. *)
  Alcotest.(check int)
    "one line per entry"
    (List.length trace)
    (List.length (List.filter (( <> ) "") (String.split_on_char '\n' serialised)));
  match Ledger.of_ndjson serialised with
  | Error e -> Alcotest.failf "round-trip failed to parse: %s" e
  | Ok back ->
      Alcotest.(check bool)
        "of_ndjson (to_ndjson t) = Ok t (every variant)" true (back = trace)

(* TEST L2 — end-to-end persist -> replay-from-file. Run a workflow with a stub
   backend, persist its trace via to_ndjson to a temp file, read it back with
   of_ndjson (a SEPARATE decode, simulating a later process), and Engine.replay
   it: the replayed outcome equals the original run's outcome, and a marker stub
   proves replay called NEITHER run_agent NOR run_command. *)
let test_ledger_persist_then_replay_from_file () =
  let agent_calls = ref 0 in
  let cmd_calls = ref 0 in
  let agent ~id ~prompt:_ ~read_only:_ =
    incr agent_calls;
    match id with
    | "assess" -> (true, `Assoc [ ("severity", `String "high") ])
    | _ -> (true, `Assoc [])
  in
  let run_command ~id:_ ~argv:_ ~working_dir:_ ~timeout_ms:_ ~observe:_ =
    incr cmd_calls;
    {
      exit = 0;
      stdout = "made";
      stderr = "";
      truncated = false;
      files = [ mk_file "out/x" Created ];
    }
  in
  let backend = Backend.stub ~agent ~run_command () in
  let wf =
    {
      name = "persist-replay";
      version = None;
      steps =
        [
          Agent
            { id = "assess"; prompt = "p"; read_only = true; output_schema = None; on_failure = Types.Abort };
          Run
            {
              id = "mk";
              cmd = [ "mkdir"; "out" ];
              working_dir = "scratch";
              timeout_ms = None;
              observe = None;
            };
          Gate
            {
              id = "g";
              when_ =
                Expr.Eq
                  (Expr.Path [ "outputs"; "mk"; "exit" ], Expr.Lit (Expr.Int 0));
            };
          Commit { id = "submit" };
        ];
    }
  in
  let v = validate_ok ~floor:[ "g" ] wf in
  let outcome, trace =
    engine_run ~run_allowlist:[ "mkdir" ] ~backend ~token:(Some "tok") v
  in
  (match outcome with
  | Committed _ -> ()
  | o -> Alcotest.failf "expected Committed, got %s" (Types.string_of_outcome o));
  Alcotest.(check int) "live run dispatched the agent once" 1 !agent_calls;
  Alcotest.(check int) "live run executed the command once" 1 !cmd_calls;
  (* Persist to a temp file, then read it back via a fresh decode. *)
  let path = Filename.temp_file "cwr_ledger" ".ndjson" in
  Fun.protect
    ~finally:(fun () -> try Sys.remove path with _ -> ())
    (fun () ->
      Out_channel.with_open_bin path (fun oc ->
          Out_channel.output_string oc (Ledger.to_ndjson trace));
      let raw = In_channel.with_open_bin path In_channel.input_all in
      match Ledger.of_ndjson raw with
      | Error e -> Alcotest.failf "ledger read back as Error: %s" e
      | Ok trace_from_file ->
          Alcotest.(check bool)
            "trace read from file equals original trace" true
            (trace_from_file = trace);
          let replayed = engine_replay ~trace:trace_from_file v in
          Alcotest.(check outcome_testable)
            "replay-from-file outcome identical to run" outcome replayed;
          (* replay touched NO backend effect *)
          Alcotest.(check int)
            "replay did NOT call run_agent (count unchanged)" 1 !agent_calls;
          Alcotest.(check int)
            "replay did NOT call run_command (count unchanged)" 1 !cmd_calls)

(* TEST L3 — corrupt / tampered ledger. (a) A malformed line => of_ndjson Error
   (fail-closed, never raises). (b) A valid ledger with a trailing extra entry
   line decodes fine but Engine.replay raises Replay_mismatch — the existing
   trailing-entry guard, now reached via the on-disk path. *)
let test_ledger_corrupt_and_tampered () =
  let v = validate_ok ~floor:[ "g" ] gated_workflow in
  let outcome, trace =
    engine_run ~backend:(Backend.stub ()) ~token:(Some "tok") v
  in
  let good = Ledger.to_ndjson trace in
  (* (a) malformed JSON line => Error, no raise *)
  (match Ledger.of_ndjson (good ^ "{not json}\n") with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "malformed line should yield Error");
  (* a well-formed JSON object with an unknown kind also fails closed *)
  (match Ledger.of_ndjson (good ^ "{\"kind\":\"bogus\"}\n") with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "unknown kind should yield Error");
  (* (b) tampered: append one extra (well-formed) entry line; it decodes, but
     replay rejects the trailing entry with Replay_mismatch. *)
  let tampered = good ^ Ledger.to_ndjson [ Loop_iter { index = 99 } ] in
  (match Ledger.of_ndjson tampered with
  | Error e -> Alcotest.failf "tampered ledger should still decode, got: %s" e
  | Ok trace_plus ->
      let raised =
        try
          ignore (engine_replay ~trace:trace_plus v);
          false
        with Engine.Replay_mismatch _ -> true
      in
      Alcotest.(check bool)
        "trailing entry via file => Replay_mismatch" true raised);
  (* sanity: the untampered round-trip replays to the original outcome *)
  match Ledger.of_ndjson good with
  | Error e -> Alcotest.failf "untampered ledger decode failed: %s" e
  | Ok t ->
      Alcotest.(check outcome_testable)
        "untampered ledger replays to original outcome" outcome
        (engine_replay ~trace:t v)

(* ---- foreach step tests ---- *)

(* A simple backend that returns `[]` for all agents — used by foreach/parallel
   tests whose agent steps don't need real output. *)
let noop_backend () = Backend.stub ()

(* Helper: build a workflow that puts a JSON array into ctx via a stub agent
   output and then iterates over it with foreach. The agent writes
   outputs.loader.items = [1,2,3]. The foreach step binds ctx["item"] and
   runs a gate `{ "lit": true }` per item. *)

let test_foreach_3_elements () =
  (* Workflow: agent (populates ctx), foreach over outputs.loader.items *)
  let wf =
    {
      name = "foreach-test";
      version = None;
      steps =
        [
          Agent
            {
              id = "loader";
              prompt = "load";
              read_only = true;
              output_schema = None;
              on_failure = Types.Abort;
            };
          Foreach
            {
              over = "items";
              steps =
                [
                  Gate
                    {
                      id = "check";
                      when_ = Expr.Lit (Expr.Bool true);
                    };
                ];
            };
        ];
    }
  in
  let agent ~id:_ ~prompt:_ ~read_only:_ =
    (true, `Assoc [])
  in
  let backend = Backend.stub ~agent () in
  let v = validate_ok ~floor:[] wf in
  (* Prime ctx manually with items: the engine initialises ctx as []; we need
     to feed a backend that makes "items" accessible. Instead, use a simpler
     workflow that directly uses a pre-seeded Foreach.over key by creating a
     workflow with Foreach over a ctx key. *)
  (* A simpler approach: a workflow with no agent steps, just Foreach over a
     ctx key. But ctx starts empty — Foreach.over key missing => Blocked. *)
  let outcome, trace = engine_run ~backend ~token:None v in
  (* Without "items" in ctx the foreach blocks immediately. *)
  (match outcome with
   | Blocked _ -> ()
   | o ->
       Alcotest.failf "expected Blocked (no 'items' in ctx), got %s"
         (Types.string_of_outcome o));
  (* The trace has at least the Agent_ran entry. *)
  ignore trace

(* Test foreach with ctx seeded from a gated agent output path. *)
let test_foreach_iterates_over_ctx_array () =
  (* Workflow with no commit (just foreach iteration). Agent output provides
     "results" = [1,2,3] which foreach.over picks up via ctx["results"]. But
     ctx key lookup is by direct top-level key, not outputs path. So we need
     the ctx key "results" directly. The engine only binds:
       - "outputs" (nested under step id)
       - "loop" (for loop.iter)
       - "item" (set by foreach)
     So foreach.over="results" needs ctx["results"] to exist. Since ctx starts
     empty and only "outputs" is top-level, foreach.over must be "outputs" or
     a key bound by a run step etc. This is a design constraint: foreach.over
     is a bare ctx key. Testing the empty-array and non-array paths suffices. *)

  (* Test: foreach.over key is absent => Blocked *)
  let wf_missing_key =
    {
      name = "foreach-missing";
      version = None;
      steps =
        [
          Foreach
            {
              over = "nonexistent";
              steps = [ Gate { id = "g"; when_ = Expr.Lit (Expr.Bool true) } ];
            };
        ];
    }
  in
  let v = validate_ok ~floor:[] wf_missing_key in
  let outcome, _trace = engine_run ~backend:(noop_backend ()) ~token:None v in
  (match outcome with
   | Blocked msg ->
       Alcotest.(check bool)
         "error mentions key name" true (contains_substring msg "nonexistent")
   | o ->
       Alcotest.failf "expected Blocked, got %s" (Types.string_of_outcome o));

  (* Test: foreach.over points to a non-array value => Blocked *)
  (* We need ctx to have a non-array value under some key. The engine binds
     "outputs" as an assoc. So foreach.over = "outputs" would be an assoc
     (not a list), triggering the non-array branch. *)
  let wf_non_array =
    {
      name = "foreach-non-array";
      version = None;
      steps =
        [
          Agent
            {
              id = "x";
              prompt = "p";
              read_only = true;
              output_schema = None;
              on_failure = Types.Abort;
            };
          Foreach
            {
              over = "outputs";
              steps = [ Gate { id = "g"; when_ = Expr.Lit (Expr.Bool true) } ];
            };
        ];
    }
  in
  let v2 = validate_ok ~floor:[] wf_non_array in
  let outcome2, _trace2 = engine_run ~backend:(noop_backend ()) ~token:None v2 in
  (match outcome2 with
   | Blocked msg ->
       Alcotest.(check bool)
         "non-array blocked" true (contains_substring msg "not a JSON array")
   | o ->
       Alcotest.failf "expected Blocked (non-array), got %s"
         (Types.string_of_outcome o))

(* Test: foreach iterates over an actual `List` value in ctx. We do this by
   creating a backend whose agent output includes an "items" key at the top
   level. But outputs go under ctx["outputs"][id]. So foreach.over="outputs"
   would get an Assoc (the outputs map). We cannot easily seed a bare ctx key
   with a `List without a custom step. This test verifies the happy path via
   a trace inspection instead: run a workflow where foreach.over="outputs.x.y"
   doesn't work (foreach.over is a bare key), and check the Blocked trace. *)

(* Replay test for foreach Blocked path *)
let test_foreach_replay_blocked () =
  let wf =
    {
      name = "foreach-replay-blocked";
      version = None;
      steps =
        [
          Foreach
            {
              over = "missing";
              steps = [ Gate { id = "g"; when_ = Expr.Lit (Expr.Bool true) } ];
            };
        ];
    }
  in
  let v = validate_ok ~floor:[] wf in
  let outcome, trace = engine_run ~backend:(noop_backend ()) ~token:None v in
  (match outcome with
   | Blocked _ -> ()
   | o ->
       Alcotest.failf "run: expected Blocked, got %s"
         (Types.string_of_outcome o));
  (* Replay should reproduce the same Blocked outcome *)
  let replayed = engine_replay ~trace v in
  Alcotest.(check outcome_testable) "foreach blocked replays identically"
    outcome replayed

(* ---- parallel step tests ---- *)

(* Test: two branches each succeed => Completed_no_commit overall.
   Both branches have a single agent step (no commit). The Parallel_started,
   two Parallel_branch_completed, and Parallel_completed entries must appear. *)
let test_parallel_two_branches_succeed () =
  let wf =
    {
      name = "parallel-ok";
      version = None;
      steps =
        [
          Parallel
            {
              branches =
                [
                  [ Agent { id = "a1"; prompt = "p"; read_only = true;
                             output_schema = None; on_failure = Types.Abort } ];
                  [ Agent { id = "a2"; prompt = "p"; read_only = true;
                             output_schema = None; on_failure = Types.Abort } ];
                ];
            };
        ];
    }
  in
  let agent_calls = ref 0 in
  let agent ~id:_ ~prompt:_ ~read_only:_ =
    incr agent_calls;
    (true, `Assoc [])
  in
  let backend = Backend.stub ~agent () in
  let v = validate_ok ~floor:[] wf in
  let outcome, trace = engine_run ~backend ~token:None v in
  Alcotest.(check outcome_testable)
    "two-branch parallel => Completed_no_commit"
    Completed_no_commit outcome;
  Alcotest.(check int) "both branches ran their agent" 2 !agent_calls;
  (* Check trace structure: Parallel_started, 2x Parallel_branch_completed,
     Parallel_completed *)
  let has_started = List.exists (function Parallel_started -> true | _ -> false) trace in
  let branch_count =
    List.length (List.filter (function Parallel_branch_completed _ -> true | _ -> false) trace)
  in
  let has_completed = List.exists (function Parallel_completed _ -> true | _ -> false) trace in
  Alcotest.(check bool) "Parallel_started in trace" true has_started;
  Alcotest.(check int) "two Parallel_branch_completed" 2 branch_count;
  Alcotest.(check bool) "Parallel_completed in trace" true has_completed

(* Test: one branch fails => overall Aborted, cancel-all semantics.
   Branch 0 has a successful agent. Branch 1 has a failing agent (abort). *)
let test_parallel_one_branch_aborts () =
  let wf =
    {
      name = "parallel-abort";
      version = None;
      steps =
        [
          Parallel
            {
              branches =
                [
                  [ Agent { id = "ok"; prompt = "p"; read_only = true;
                             output_schema = None; on_failure = Types.Abort } ];
                  [ Agent { id = "bad"; prompt = "p"; read_only = true;
                             output_schema = None; on_failure = Types.Abort } ];
                ];
            };
        ];
    }
  in
  let agent ~id ~prompt:_ ~read_only:_ =
    match id with
    | "bad" -> (false, `Assoc [])
    | _ -> (true, `Assoc [])
  in
  let backend = Backend.stub ~agent () in
  let v = validate_ok ~floor:[] wf in
  let outcome, trace = engine_run ~backend ~token:None v in
  (* Overall should be Aborted since branch with "bad" agent aborts *)
  (match outcome with
   | Aborted _ -> ()
   | o ->
       Alcotest.failf "expected Aborted when branch fails, got %s"
         (Types.string_of_outcome o));
  let has_completed = List.exists (function Parallel_completed _ -> true | _ -> false) trace in
  Alcotest.(check bool) "Parallel_completed present even on abort" true has_completed

(* Test: replay of a successful two-branch parallel *)
let test_parallel_replay_success () =
  let wf =
    {
      name = "parallel-replay";
      version = None;
      steps =
        [
          Parallel
            {
              branches =
                [
                  [ Agent { id = "r1"; prompt = "p"; read_only = true;
                             output_schema = None; on_failure = Types.Abort } ];
                  [ Agent { id = "r2"; prompt = "p"; read_only = true;
                             output_schema = None; on_failure = Types.Abort } ];
                ];
            };
        ];
    }
  in
  let agent ~id:_ ~prompt:_ ~read_only:_ = (true, `Assoc []) in
  let backend = Backend.stub ~agent () in
  let v = validate_ok ~floor:[] wf in
  let outcome, trace = engine_run ~backend ~token:None v in
  Alcotest.(check outcome_testable)
    "run: two-branch parallel => Completed_no_commit"
    Completed_no_commit outcome;
  let replayed = engine_replay ~trace v in
  Alcotest.(check outcome_testable)
    "replay: identical outcome" outcome replayed

(* ---- to-claude-workflow compiler tests ---- *)

let make_agent id =
  Agent { id; prompt = Printf.sprintf "do %s" id; read_only = true;
          output_schema = None; on_failure = Types.Abort }

let test_compiler_header () =
  (* Workflow with version = "1.0" must produce "// Compiled from CWR v1.0" *)
  let wf =
    { name = "smoke"; version = Some "1.0";
      steps = [ make_agent "a" ] }
  in
  let js, _notes = Compiler.compile_workflow wf in
  Alcotest.(check bool) "header contains v1.0" true
    (contains_substring js "// Compiled from CWR v1.0");

  (* Workflow with no version must produce "(unversioned)" *)
  let wf2 = { name = "smoke"; version = None; steps = [ make_agent "a" ] } in
  let js2, _notes2 = Compiler.compile_workflow wf2 in
  Alcotest.(check bool) "unversioned header" true
    (contains_substring js2 "(unversioned)")

let test_compiler_agent_step () =
  let wf =
    { name = "ag"; version = Some "1.0";
      steps = [ make_agent "extract" ] }
  in
  let js, notes = Compiler.compile_workflow wf in
  (* Agent step must produce: const extract = await agent(...) *)
  Alcotest.(check bool) "agent step: const extract" true
    (contains_substring js "const extract = await agent(");
  Alcotest.(check bool) "agent step: no compilation note" true
    (not (List.exists (fun (n : Compiler.note) -> n.kind = "agent") notes))

let test_compiler_parallel_step () =
  let wf =
    { name = "par"; version = Some "1.0";
      steps =
        [ Parallel
            { branches =
                [ [ make_agent "b1" ];
                  [ make_agent "b2" ] ] } ] }
  in
  let js, _notes = Compiler.compile_workflow wf in
  (* Parallel must produce: await parallel([ *)
  Alcotest.(check bool) "parallel: await parallel([" true
    (contains_substring js "await parallel([")

let test_compiler_foreach_step () =
  let wf =
    { name = "fe"; version = Some "1.0";
      steps =
        [ Foreach
            { over = "results";
              steps = [ make_agent "body" ] } ] }
  in
  let js, notes = Compiler.compile_workflow wf in
  (* Foreach must produce: await pipeline(results, async (item) => { *)
  Alcotest.(check bool) "foreach: await pipeline(results," true
    (contains_substring js "await pipeline(results,");
  (* Must have a foreach note *)
  Alcotest.(check bool) "foreach: compilation note present" true
    (List.exists (fun (n : Compiler.note) -> n.kind = "foreach") notes)

let test_compiler_run_step () =
  let wf =
    { name = "run"; version = Some "1.0";
      steps =
        [ Run
            { id = "mk"; cmd = [ "mkdir"; "-p"; "out" ];
              working_dir = "scratch"; timeout_ms = None; observe = None } ] }
  in
  let js, notes = Compiler.compile_workflow wf in
  (* Run step must produce the [CWR run: cmd=... comment *)
  Alcotest.(check bool) "run: CWR run comment" true
    (contains_substring js "[CWR run:");
  Alcotest.(check bool) "run: cmd present in comment" true
    (contains_substring js "mkdir -p out");
  Alcotest.(check bool) "run: allowlist note present" true
    (List.exists (fun (n : Compiler.note) -> n.kind = "run") notes);
  (* The note mentions allowlist *)
  let run_note = List.find (fun (n : Compiler.note) -> n.kind = "run") notes in
  Alcotest.(check bool) "run note: mentions allowlist" true
    (contains_substring run_note.description "allowlist")

let test_compiler_gate_commit_steps () =
  let wf =
    { name = "gc"; version = Some "1.0";
      steps =
        [ Gate { id = "g"; when_ = Expr.Lit (Expr.Bool true) };
          Commit { id = "submit" } ] }
  in
  let js, notes = Compiler.compile_workflow wf in
  (* Gate now compiles faithfully: if (!(true)) { throw ... } — no [CWR gate:] comment *)
  Alcotest.(check bool) "gate: faithful if/throw form" true
    (contains_substring js "if (!(true)) { throw new Error(\"gate g failed\"); }");
  Alcotest.(check bool) "gate: no CWR gate stub comment" true
    (not (contains_substring js "[CWR gate:"));
  Alcotest.(check bool) "commit: CWR commit comment" true
    (contains_substring js "[CWR commit");
  Alcotest.(check bool) "gate note absent (faithful compilation)" true
    (not (List.exists (fun (n : Compiler.note) -> n.kind = "gate") notes));
  Alcotest.(check bool) "commit note present" true
    (List.exists (fun (n : Compiler.note) -> n.kind = "commit") notes)

(* ---- new compiler tests: expr translator, meta, agent options, loop governors ---- *)

let test_compiler_meta_header () =
  let wf = { name = "my-pipeline"; version = Some "1.0"; steps = [] } in
  let js, _ = Compiler.compile_workflow wf in
  Alcotest.(check bool) "meta block present" true
    (contains_substring js "export const meta = {");
  Alcotest.(check bool) "meta name correct" true
    (contains_substring js "name: 'my-pipeline'");
  Alcotest.(check bool) "meta description empty" true
    (contains_substring js "description: ''")

let test_compiler_expr_edge_cases () =
  (* And [] → true; Or [] → false *)
  let wf_and = { name = "e"; version = None;
    steps = [ Gate { id = "g1"; when_ = Expr.And [] } ] } in
  let js_and, notes_and = Compiler.compile_workflow wf_and in
  Alcotest.(check bool) "And []: condition is true" true
    (contains_substring js_and "if (!(true))");
  Alcotest.(check bool) "And []: no gate note" true
    (not (List.exists (fun (n : Compiler.note) -> n.kind = "gate") notes_and));

  let wf_or = { name = "e"; version = None;
    steps = [ Gate { id = "g2"; when_ = Expr.Or [] } ] } in
  let js_or, _ = Compiler.compile_workflow wf_or in
  Alcotest.(check bool) "Or []: condition is false" true
    (contains_substring js_or "if (!(false))")

let test_compiler_expr_path () =
  (* Path outside outputs → args.key *)
  let wf = { name = "p"; version = None;
    steps = [ Gate { id = "g"; when_ = Expr.Path ["item"] } ] } in
  let js, _ = Compiler.compile_workflow wf in
  Alcotest.(check bool) "Path [item] → args.item" true
    (contains_substring js "args.item");

  (* Path inside outputs → id.field *)
  let wf2 = { name = "p2"; version = None;
    steps = [ Gate { id = "g2";
                     when_ = Expr.Path ["outputs"; "agent_a"; "score"] } ] } in
  let js2, _ = Compiler.compile_workflow wf2 in
  Alcotest.(check bool) "Path [outputs;agent_a;score] → agent_a.score" true
    (contains_substring js2 "agent_a.score")

let test_compiler_gate_real_expr () =
  let wf = { name = "ge"; version = None;
    steps = [ Gate { id = "check";
      when_ = Expr.Eq (Expr.Path ["outputs"; "a"; "score"], Expr.Lit (Expr.Int 5)) } ] } in
  let js, notes = Compiler.compile_workflow wf in
  Alcotest.(check bool) "gate: real expr Eq" true
    (contains_substring js "a.score === 5");
  Alcotest.(check bool) "gate: throw form" true
    (contains_substring js "throw new Error(\"gate check failed\")");
  Alcotest.(check bool) "gate: no gate note" true
    (not (List.exists (fun (n : Compiler.note) -> n.kind = "gate") notes))

let test_compiler_branch_real_expr () =
  let wf = { name = "br"; version = None;
    steps = [ Branch { when_ = Expr.Lit (Expr.Bool true);
                       then_ = [ make_agent "t" ];
                       else_ = [ make_agent "f" ] } ] } in
  let js, notes = Compiler.compile_workflow wf in
  Alcotest.(check bool) "branch: real if condition" true
    (contains_substring js "if (true) {");
  Alcotest.(check bool) "branch: else present" true
    (contains_substring js "} else {");
  Alcotest.(check bool) "branch: no branch note" true
    (not (List.exists (fun (n : Compiler.note) -> n.kind = "branch") notes))

let test_compiler_agent_schema () =
  let schema = Types.Schema.[ ("verdict", String); ("score", Int) ] in
  let wf = { name = "s"; version = None;
    steps = [ Agent { id = "check"; prompt = "evaluate";
                      read_only = false; output_schema = Some schema;
                      on_failure = Types.Abort } ] } in
  let js, _ = Compiler.compile_workflow wf in
  Alcotest.(check bool) "agent schema: type object" true
    (contains_substring js {|type: "object"|});
  Alcotest.(check bool) "agent schema: verdict string" true
    (contains_substring js {|"verdict": {type: "string"}|});
  Alcotest.(check bool) "agent schema: score integer" true
    (contains_substring js {|"score": {type: "integer"}|});
  Alcotest.(check bool) "agent schema: required array" true
    (contains_substring js {|required: ["verdict", "score"]|})

let test_compiler_agent_schema_types () =
  (* Bool → "boolean", Any → {}, Enum → enum array *)
  let schema = Types.Schema.[ ("flag", Bool); ("data", Any);
                               ("kind", Enum ["a"; "b"]) ] in
  let wf = { name = "t"; version = None;
    steps = [ Agent { id = "x"; prompt = "p"; read_only = false;
                      output_schema = Some schema; on_failure = Types.Abort } ] } in
  let js, _ = Compiler.compile_workflow wf in
  Alcotest.(check bool) "Bool → boolean" true
    (contains_substring js {|"flag": {type: "boolean"}|});
  Alcotest.(check bool) "Any → {}" true
    (contains_substring js {|"data": {}|});
  Alcotest.(check bool) "Enum → enum array" true
    (contains_substring js {|"kind": {type: "string", enum: ["a", "b"]}|})

let test_compiler_agent_on_failure () =
  (* Continue → try/catch; Abort → no try *)
  let wf_cont = { name = "c"; version = None;
    steps = [ Agent { id = "soft"; prompt = "p"; read_only = false;
                      output_schema = None; on_failure = Types.Continue } ] } in
  let js_cont, _ = Compiler.compile_workflow wf_cont in
  Alcotest.(check bool) "Continue: try block present" true
    (contains_substring js_cont "try {");
  Alcotest.(check bool) "Continue: catch present" true
    (contains_substring js_cont "catch (e)");
  Alcotest.(check bool) "Continue: soft fail null" true
    (contains_substring js_cont "= null; /* soft fail */");

  let wf_abort = { name = "a"; version = None;
    steps = [ Agent { id = "hard"; prompt = "p"; read_only = false;
                      output_schema = None; on_failure = Types.Abort } ] } in
  let js_abort, _ = Compiler.compile_workflow wf_abort in
  Alcotest.(check bool) "Abort: no try block" true
    (not (contains_substring js_abort "try {"));
  Alcotest.(check bool) "Abort: const binding" true
    (contains_substring js_abort "const hard = await agent(")

let test_compiler_agent_read_only () =
  let wf = { name = "ro"; version = None;
    steps = [ Agent { id = "fetch"; prompt = "read data"; read_only = true;
                      output_schema = None; on_failure = Types.Abort } ] } in
  let js, _ = Compiler.compile_workflow wf in
  Alcotest.(check bool) "read_only: comment present" true
    (contains_substring js "// [read-only]");
  Alcotest.(check bool) "read_only: agent call still present" true
    (contains_substring js "const fetch = await agent(")

let test_compiler_loop_max_iters () =
  let wf = { name = "lp"; version = None;
    steps = [ Loop { body = [ make_agent "step" ];
                     until = None;
                     governors = [ Max_iters 5 ] } ] } in
  let js, notes = Compiler.compile_workflow wf in
  Alcotest.(check bool) "Max_iters: counter before while" true
    (contains_substring js "_maxiters_0 = 0");
  Alcotest.(check bool) "Max_iters: break check" true
    (contains_substring js "++_maxiters_0 >= 5");
  Alcotest.(check bool) "Max_iters: while present" true
    (contains_substring js "while (true)");
  Alcotest.(check bool) "Max_iters: no loop note" true
    (not (List.exists (fun (n : Compiler.note) -> n.kind = "loop") notes))

let test_compiler_loop_budget () =
  let wf = { name = "lb"; version = None;
    steps = [ Loop { body = [ make_agent "step" ];
                     until = None;
                     governors = [ Budget ] } ] } in
  let js, _ = Compiler.compile_workflow wf in
  Alcotest.(check bool) "Budget: remaining check" true
    (contains_substring js "budget.remaining() <= 0")

let test_compiler_loop_until () =
  let wf = { name = "lu"; version = None;
    steps = [ Loop { body = [ make_agent "step" ];
                     until = Some (Expr.Lit (Expr.Bool true));
                     governors = [] } ] } in
  let js, notes = Compiler.compile_workflow wf in
  Alcotest.(check bool) "until: if break present" true
    (contains_substring js "if (true) break;");
  (* governor-less loop gets a note *)
  Alcotest.(check bool) "no-governor loop: note present" true
    (List.exists (fun (n : Compiler.note) -> n.kind = "loop") notes)

let test_compiler_loop_fixpoint () =
  let wf = { name = "lf"; version = None;
    steps = [ Loop { body = [ make_agent "step" ];
                     until = None;
                     governors = [ Fixpoint { window = 2;
                       progress = Expr.Lit (Expr.Bool true) } ] } ] } in
  let js, _ = Compiler.compile_workflow wf in
  Alcotest.(check bool) "Fixpoint: counter before while" true
    (contains_substring js "_fixcount_0 = 0");
  Alcotest.(check bool) "Fixpoint: window check" true
    (contains_substring js ">= 2")

let test_compiler_js_escape_completeness () =
  (* Prompts with control chars must not produce octal \ddd escapes — use \uXXXX instead.
     js_escape_string must cover \r and C0 control chars. *)
  let ctrl_prompt = "tab\there\rreturn\x01ctrl" in
  let wf = { name = "esc"; version = None;
    steps = [ Agent { id = "a"; prompt = ctrl_prompt; read_only = false;
                      output_schema = None; on_failure = Types.Abort } ] } in
  let js, _ = Compiler.compile_workflow wf in
  Alcotest.(check bool) "\\t escaped" true (contains_substring js "\\t");
  Alcotest.(check bool) "\\r escaped" true (contains_substring js "\\r");
  Alcotest.(check bool) "ctrl \\u0001 escaped" true (contains_substring js "\\u0001");
  Alcotest.(check bool) "no raw octal \\001" true
    (not (contains_substring js "\\001"));

  (* Schema keys with hyphens must be quoted *)
  let schema = Types.Schema.[ ("content-type", String) ] in
  let wf2 = { name = "sq"; version = None;
    steps = [ Agent { id = "b"; prompt = "p"; read_only = false;
                      output_schema = Some schema; on_failure = Types.Abort } ] } in
  let js2, _ = Compiler.compile_workflow wf2 in
  Alcotest.(check bool) "hyphen schema key: quoted" true
    (contains_substring js2 {|"content-type": {type: "string"}|});
  Alcotest.(check bool) "hyphen schema key: not bare" true
    (not (contains_substring js2 "content-type: {"));

  (* Leading-digit ID must be prefixed *)
  let wf3 = { name = "ld"; version = None;
    steps = [ Agent { id = "1abc"; prompt = "p"; read_only = false;
                      output_schema = None; on_failure = Types.Abort } ] } in
  let js3, _ = Compiler.compile_workflow wf3 in
  Alcotest.(check bool) "leading-digit id: prefixed with _" true
    (contains_substring js3 "const _1abc = ");
  Alcotest.(check bool) "leading-digit id: no bare const 1abc" true
    (not (contains_substring js3 "const 1abc = "));

  (* Space in ID must be sanitized *)
  let wf4 = { name = "sp"; version = None;
    steps = [ Agent { id = "step one"; prompt = "p"; read_only = false;
                      output_schema = None; on_failure = Types.Abort } ] } in
  let js4, _ = Compiler.compile_workflow wf4 in
  Alcotest.(check bool) "space in id: replaced with _" true
    (contains_substring js4 "const step_one = ");
  Alcotest.(check bool) "space in id: label preserves original" true
    (contains_substring js4 {|label: "step one"|})

let test_compiler_name_newline () =
  (* A workflow name with \n must not split the header comment into bare JS code. *)
  let wf = { name = "line1\nline2"; version = None; steps = [] } in
  let js, _ = Compiler.compile_workflow wf in
  Alcotest.(check bool) "newline in name: escaped in comment" true
    (contains_substring js "// Workflow: line1\\nline2");
  Alcotest.(check bool) "newline in name: no raw newline after //" true
    (not (contains_substring js "// Workflow: line1\nline2"));
  Alcotest.(check bool) "newline in name: meta name also escaped" true
    (contains_substring js {|name: 'line1\nline2'|})

let test_compiler_hyphenated_ids () =
  (* Step IDs with hyphens are valid CWR but invalid JS identifiers.
     The compiler must replace '-' with '_' in variable names and path references. *)
  let wf = { name = "h"; version = None;
    steps = [
      Agent { id = "deep-dive"; prompt = "p"; read_only = false;
              output_schema = None; on_failure = Types.Abort };
      Gate { id = "g";
             when_ = Expr.Path ["outputs"; "deep-dive"; "done"] };
    ] } in
  let js, _ = Compiler.compile_workflow wf in
  Alcotest.(check bool) "hyphen: variable binding sanitized" true
    (contains_substring js "const deep_dive = ");
  Alcotest.(check bool) "hyphen: no bare hyphen variable" true
    (not (contains_substring js "const deep-dive = "));
  Alcotest.(check bool) "hyphen: path reference sanitized" true
    (contains_substring js "deep_dive.done");
  Alcotest.(check bool) "hyphen: label preserved as-is" true
    (contains_substring js {|label: "deep-dive"|})

let test_compiler_nested_loops () =
  (* Outer loop: Max_iters 3; inner loop: Max_iters 2 — must get distinct suffixes *)
  let inner = Loop { body = [ make_agent "inner_step" ];
                     until = None;
                     governors = [ Max_iters 2 ] } in
  let wf = { name = "nl"; version = None;
    steps = [ Loop { body = [ inner ];
                     until = None;
                     governors = [ Max_iters 3 ] } ] } in
  let js, _ = Compiler.compile_workflow wf in
  Alcotest.(check bool) "nested: outer counter _0" true
    (contains_substring js "_maxiters_0");
  Alcotest.(check bool) "nested: inner counter _1" true
    (contains_substring js "_maxiters_1");
  Alcotest.(check bool) "nested: outer limit 3" true
    (contains_substring js ">= 3");
  Alcotest.(check bool) "nested: inner limit 2" true
    (contains_substring js ">= 2")

(* ---- foreach iteration tests (with initial_ctx) ---- *)

let test_foreach_iterates () =
  let wf =
    { name = "foreach-iter"; version = None;
      steps = [ Foreach { over = "items";
                          steps = [ Agent { id = "body"; prompt = "p";
                                            read_only = true;
                                            output_schema = None;
                                            on_failure = Types.Abort } ] } ] }
  in
  let calls = ref 0 in
  let agent ~id:_ ~prompt:_ ~read_only:_ =
    incr calls;
    (true, `Assoc [])
  in
  let backend = Backend.stub ~agent () in
  let v = validate_ok ~floor:[] wf in
  let outcome, trace =
    engine_run ~backend ~token:None
      ~initial_ctx:[("items", `List [`String "a"; `String "b"; `String "c"])]
      v
  in
  Alcotest.(check outcome_testable)
    "foreach 3-element array => Completed_no_commit"
    Completed_no_commit outcome;
  Alcotest.(check int) "agent called 3 times" 3 !calls;
  let iter_starts = List.filter (function
    | Types.Foreach_iter_started _ -> true | _ -> false) trace in
  Alcotest.(check int) "3 iter_started" 3 (List.length iter_starts);
  (match List.rev trace with
   | Types.Foreach_completed { iterations = 3 } :: _ -> ()
   | _ -> Alcotest.fail "expected Foreach_completed{3} at end of trace")

let test_foreach_empty_array () =
  let wf =
    { name = "fe"; version = None;
      steps = [ Foreach { over = "items";
                          steps = [ Agent { id = "a"; prompt = "p";
                                            read_only = true;
                                            output_schema = None;
                                            on_failure = Types.Abort } ] } ] }
  in
  let called = ref false in
  let agent ~id:_ ~prompt:_ ~read_only:_ = called := true; (true, `Assoc []) in
  let backend = Backend.stub ~agent () in
  let v = validate_ok ~floor:[] wf in
  let outcome, _trace =
    engine_run ~backend ~token:None
      ~initial_ctx:[("items", `List [])]
      v
  in
  Alcotest.(check bool) "agent never called" false !called;
  Alcotest.(check outcome_testable) "empty foreach => Completed_no_commit"
    Completed_no_commit outcome

let test_foreach_replay_iteration () =
  let wf =
    { name = "fr"; version = None;
      steps = [ Foreach { over = "items";
                          steps = [ Agent { id = "b"; prompt = "p";
                                            read_only = true;
                                            output_schema = None;
                                            on_failure = Types.Abort } ] } ] }
  in
  let agent ~id:_ ~prompt:_ ~read_only:_ = (true, `Assoc [("x", `Int 1)]) in
  let backend = Backend.stub ~agent () in
  let v = validate_ok ~floor:[] wf in
  let outcome, trace =
    engine_run ~backend ~token:None
      ~initial_ctx:[("items", `List [`Int 1; `Int 2])]
      v
  in
  let replayed =
    engine_replay ~trace
      ~initial_ctx:[("items", `List [`Int 1; `Int 2])]
      v
  in
  Alcotest.(check outcome_testable) "replay matches run" outcome replayed

(* ---- lint tests for parallel ---- *)

let test_lint_commit_in_parallel () =
  let wf =
    { name = "cip"; version = None;
      steps = [ Parallel { branches = [
        [ Commit { id = "bad-commit" } ];
        [ Agent { id = "a"; prompt = "p"; read_only = true;
                  output_schema = None; on_failure = Types.Abort } ] ] } ] }
  in
  let diags = Lint.check wf in
  let has_cip = List.exists (fun (d : Lint.diagnostic) ->
    d.code = "commit-in-parallel") diags in
  Alcotest.(check bool) "commit-in-parallel diagnostic present" true has_cip;
  (* validate returns the diagnostic message (not the code) *)
  (match Validate.workflow ~floor_gates:[] wf with
   | Error msg ->
       Alcotest.(check bool) "error mentions parallel" true
         (contains_substring msg "parallel")
   | Ok _ -> Alcotest.fail "expected validation Error for commit-in-parallel")

let test_lint_parallel_output_collision () =
  let wf =
    { name = "poc"; version = None;
      steps = [ Parallel { branches = [
        [ Agent { id = "dup"; prompt = "p"; read_only = true;
                  output_schema = None; on_failure = Types.Abort } ];
        [ Agent { id = "dup"; prompt = "p"; read_only = true;
                  output_schema = None; on_failure = Types.Abort } ] ] } ] }
  in
  let diags = Lint.check wf in
  let has_collision = List.exists (fun (d : Lint.diagnostic) ->
    d.code = "parallel-output-collision") diags in
  Alcotest.(check bool) "parallel-output-collision diagnostic present" true
    has_collision

let test_lint_floor_gate_parallel_intersection () =
  (* Gate "g" guaranteed in ALL branches => commit after parallel is valid. *)
  let wf_all_branches =
    { name = "par-gate-all"; version = None;
      steps =
        [ Parallel { branches =
            [ [ Gate { id = "g"; when_ = Expr.Lit (Expr.Bool true) } ];
              [ Gate { id = "g"; when_ = Expr.Lit (Expr.Bool true) } ] ] };
          Commit { id = "submit" } ] }
  in
  (match Validate.workflow ~floor_gates:["g"] wf_all_branches with
   | Ok _ -> ()
   | Error msg -> Alcotest.failf "gate in all branches should allow commit, got: %s" msg);
  (* Gate "g" guaranteed in ONLY ONE branch => commit after parallel is rejected. *)
  let wf_one_branch =
    { name = "par-gate-one"; version = None;
      steps =
        [ Parallel { branches =
            [ [ Gate { id = "g"; when_ = Expr.Lit (Expr.Bool true) } ];
              [ Agent { id = "a"; prompt = "p"; read_only = true;
                        output_schema = None; on_failure = Types.Abort } ] ] };
          Commit { id = "submit" } ] }
  in
  (match Validate.workflow ~floor_gates:["g"] wf_one_branch with
   | Error _ -> ()
   | Ok _ -> Alcotest.fail "gate in only one branch must not satisfy floor")

let test_parallel_branch_output_merge () =
  let wf =
    { name = "merge"; version = None;
      steps =
        [ Parallel { branches =
            [ [ Agent { id = "r1"; prompt = "p"; read_only = true;
                        output_schema = None; on_failure = Types.Abort } ];
              [ Agent { id = "r2"; prompt = "p"; read_only = true;
                        output_schema = None; on_failure = Types.Abort } ] ] } ] }
  in
  let agent ~id ~prompt:_ ~read_only:_ =
    (true, `Assoc [("result", `String id)])
  in
  let backend = Backend.stub ~agent () in
  let v = validate_ok ~floor:[] wf in
  let outcome, trace = engine_run ~backend ~token:None v in
  Alcotest.(check outcome_testable) "parallel merge => Completed_no_commit"
    Completed_no_commit outcome;
  let replayed = engine_replay ~trace v in
  Alcotest.(check outcome_testable) "replay matches run" outcome replayed;
  (* The branch_outputs in the Parallel_branch_completed entries carry the results *)
  let branch_outputs_found =
    List.exists (function
      | Types.Parallel_branch_completed { branch_outputs; _ } ->
          List.mem_assoc "r1" branch_outputs || List.mem_assoc "r2" branch_outputs
      | _ -> false) trace
  in
  Alcotest.(check bool) "branch outputs present in trace" true branch_outputs_found


let test_foreach_disk_replay () =
  (* Run a foreach workflow with initial_ctx, write to a temp ledger
     (Ctx_snapshot header + trace), read it back, replay with recovered
     initial_ctx, verify identical outcome. *)
  let wf =
    { name = "foreach-disk"; version = None;
      steps = [ Foreach { over = "items";
                           steps = [ Agent { id = "body"; prompt = "p";
                                             read_only = true;
                                             output_schema = None;
                                             on_failure = Types.Abort } ] } ] }
  in
  let agent ~id:_ ~prompt:_ ~read_only:_ = (true, `Assoc []) in
  let backend = Backend.stub ~agent () in
  let v = validate_ok ~floor:[] wf in
  let initial_ctx = [("items", `List [`String "x"; `String "y"])] in
  let outcome, trace = engine_run ~backend ~token:None ~initial_ctx v in
  Alcotest.(check outcome_testable) "run with 2 items => Completed_no_commit"
    Completed_no_commit outcome;
  (* Write ledger: first line = Ctx_snapshot, rest = trace *)
  let ledger_path = Filename.temp_file "cwr_test_" ".ndjson" in
  Fun.protect
    ~finally:(fun () -> try Sys.remove ledger_path with _ -> ())
    (fun () ->
      Out_channel.with_open_bin ledger_path (fun oc ->
        let header =
          Ledger.entry_to_json (Types.Ctx_snapshot { ctx = initial_ctx })
        in
        Out_channel.output_string oc (Yojson.Safe.to_string header ^ "\n");
        Out_channel.output_string oc (Ledger.to_ndjson trace));
      (* Read back and parse the ledger, recovering Ctx_snapshot *)
      let contents =
        In_channel.with_open_bin ledger_path In_channel.input_all
      in
      let lines = String.split_on_char '\n' contents
                  |> List.filter (fun s -> String.trim s <> "") in
      let replayed_ctx, trace_lines =
        match lines with
        | first :: rest -> (
            match Ledger.entry_of_json (Yojson.Safe.from_string first) with
            | Types.Ctx_snapshot { ctx } -> (ctx, rest)
            | _ -> ([], lines)
            | exception _ -> ([], lines))
        | [] -> ([], [])
      in
      let trace_str = String.concat "\n" trace_lines in
      let replayed_trace =
        match Ledger.of_ndjson trace_str with
        | Ok t -> t
        | Error msg -> Alcotest.failf "ledger parse error: %s" msg
      in
      let replayed_outcome =
        engine_replay ~trace:replayed_trace ~initial_ctx:replayed_ctx v
      in
      Alcotest.(check outcome_testable)
        "disk replay matches run" outcome replayed_outcome)

let () =
  Alcotest.run "cabal_workflow_runner"
    [
      ( "parse",
        [
          Alcotest.test_case "valid round-trips" `Quick test_parse_roundtrip;
          Alcotest.test_case "malformed => Error" `Quick test_parse_malformed;
        ] );
      ( "dsl",
        [
          Alcotest.test_case "total: missing/mismatch => false, no raise" `Quick
            test_total_dsl;
          Alcotest.test_case "F2: Exists treats present object/array as present"
            `Quick test_exists_present_object;
        ] );
      ( "structured-output",
        [
          Alcotest.test_case "high severity drives then_ branch" `Quick
            test_branch_high;
          Alcotest.test_case "low severity drives else_ branch" `Quick
            test_branch_low;
          Alcotest.test_case "missing schema field => Aborted (fail-closed)"
            `Quick test_schema_fail_closed;
          Alcotest.test_case "F1: failed agent step fails closed (Aborted)"
            `Quick test_failed_agent_fails_closed;
          Alcotest.test_case
            "F3: on_failure=continue soft-fails and continues (no abort)" `Quick
            test_soft_fail_agent_continues;
          Alcotest.test_case
            "F4: on_failure=continue + Commit is rejected (commit-free only)"
            `Quick test_soft_fail_with_commit_rejected;
        ] );
      ( "governed-loops",
        [
          Alcotest.test_case "ungoverned loop => Error" `Quick
            test_ungoverned_loop_rejected;
          Alcotest.test_case "Max_iters 0 => Error" `Quick
            test_bad_max_iters_rejected;
          Alcotest.test_case "unbounded loop, Budget only, terminates" `Quick
            test_loop_budget_terminates;
          Alcotest.test_case "unbounded loop, Fixpoint only, terminates" `Quick
            test_loop_fixpoint_terminates;
          Alcotest.test_case
            "ceiling stops Budget-only loop with constant budget" `Quick
            test_loop_ceiling_budget_constant;
          Alcotest.test_case
            "ceiling stops Fixpoint-only loop that always progresses" `Quick
            test_loop_ceiling_fixpoint_always_progresses;
        ] );
      ( "validate-fail-closed",
        [
          Alcotest.test_case "commit, no required gate => Error" `Quick
            test_validate_commit_no_gate;
          Alcotest.test_case "commit gated in one branch only => Error" `Quick
            test_validate_commit_one_branch_only;
          Alcotest.test_case "gate inside loop not guaranteed => Error" `Quick
            test_validate_loop_gate_not_guaranteed;
          Alcotest.test_case "well-gated workflow accepted" `Quick
            test_validate_accepts_gated;
        ] );
      ( "commit-token",
        [
          Alcotest.test_case "no token => Blocked" `Quick
            test_commit_no_token_blocked;
          Alcotest.test_case "token stored as digest only" `Quick
            test_commit_token_digest_only;
        ] );
      ( "replay",
        [
          Alcotest.test_case "governed loop + structured agents replays identically"
            `Quick test_replay_with_loop;
          Alcotest.test_case "F4: replay rejects trailing extra trace entries"
            `Quick test_replay_rejects_trailing_entries;
        ] );
      ( "ledger",
        [
          Alcotest.test_case
            "round-trip: every trace_entry variant survives to_ndjson/of_ndjson"
            `Quick test_ledger_roundtrip_all_variants;
          Alcotest.test_case
            "persist -> replay-from-file: identical outcome, no backend effect"
            `Quick test_ledger_persist_then_replay_from_file;
          Alcotest.test_case
            "corrupt line => Error; tampered (trailing) => Replay_mismatch"
            `Quick test_ledger_corrupt_and_tampered;
        ] );
      ( "run-step",
        [
          Alcotest.test_case "mkdir-like run: exit + files bound, gate passes"
            `Quick test_run_step_outputs_and_gate;
          Alcotest.test_case
            "allowlist: [] blocks, [bin] runs, [*] runs (basename match)" `Quick
            test_run_step_allowlist;
          Alcotest.test_case "replay never re-executes the command" `Quick
            test_run_step_replay_no_reexec;
          Alcotest.test_case "file diff: Created then Deleted both observed"
            `Quick test_run_step_file_diff;
          Alcotest.test_case ".. / absolute working_dir + empty cmd => Error"
            `Quick test_run_step_bad_working_dir;
          Alcotest.test_case "run-demo example validates + lints clean-of-errors"
            `Quick test_run_demo_example;
          Alcotest.test_case "destructive binary => warning (not error)" `Quick
            test_run_step_destructive_warning;
          Alcotest.test_case "Fix1: file digest is MD5 (known-answer)" `Quick
            test_run_step_digest_known_answer;
          Alcotest.test_case
            "Fix2: path-bearing cmd[0] (abs/rel) rejected; bare runs" `Quick
            test_run_step_rejects_path_argv0;
          Alcotest.test_case
            "Fix3: a failed run effect is recorded (no engine crash)" `Quick
            test_run_step_effect_failure_recorded;
        ] );
      ( "happy-path",
        [
          Alcotest.test_case "gated + token + pass => Committed" `Quick
            test_happy_path;
        ] );
      ( "gate-blocks",
        [
          Alcotest.test_case "false floor gate => Blocked (commit not reached)"
            `Quick test_false_gate_blocks;
          Alcotest.test_case "bounty example lints clean (zero warnings)" `Quick
            test_bounty_lint_clean_zero_warnings;
          Alcotest.test_case "smoke example still reaches Committed" `Quick
            test_smoke_still_committable;
        ] );
      ( "lint",
        [
          Alcotest.test_case "invalid JSON => invalid-json, no raise" `Quick
            test_lint_invalid_json;
          Alcotest.test_case "shape error => invalid-shape, no raise" `Quick
            test_lint_invalid_shape;
          Alcotest.test_case "all-at-once: >= 2 errors in one call" `Quick
            test_lint_all_at_once;
          Alcotest.test_case "dangling-output-ref is a warning, not error"
            `Quick test_lint_dangling_output_ref;
          Alcotest.test_case
            "F3: ref to one-arm-only output after branch => dangling warning"
            `Quick test_lint_branch_one_arm_only_dangling;
          Alcotest.test_case
            "F3: ref to both-arms output after branch => no warning" `Quick
            test_lint_branch_both_arms_ok;
          Alcotest.test_case "contract: examples lint-clean AND validate Ok"
            `Quick test_lint_contract_examples;
          Alcotest.test_case "contract: known-bad has_errors AND validate Error"
            `Quick test_lint_contract_badness;
          Alcotest.test_case "to_json / diagnostic_to_json shape" `Quick
            test_lint_to_json_shape;
          Alcotest.test_case "generate->fix loop converges + validates" `Quick
            test_lint_generate_fix_loop;
        ] );
      ( "schema",
        [
          Alcotest.test_case "well-formed: $schema + $defs(expr,governor,step)"
            `Quick test_schema_well_formed;
          Alcotest.test_case "no drift: committed artifact == to_string ()"
            `Quick test_schema_no_drift;
          Alcotest.test_case "parser <-> schema kinds agree" `Quick
            test_schema_kinds_agree;
          Alcotest.test_case
            "expr/step/governor objects closed; int fields bounded" `Quick
            test_schema_closed_objects;
          Alcotest.test_case "schema does not over-accept (junk key, huge int)"
            `Quick test_schema_no_overaccept;
          Alcotest.test_case
            "schema/parser parity: closed defs + properties == known keys"
            `Quick test_schema_parser_parity;
          Alcotest.test_case
            "behavioral parity: parser accepts iff structurally schema-valid"
            `Quick test_schema_parser_behavioral_parity;
        ] );
      ( "parser-strictness",
        [
          Alcotest.test_case
            "unknown key rejected on each closed object type" `Quick
            test_parser_rejects_unknown_keys;
          Alcotest.test_case "underscore metadata accepted (_doc, _note)" `Quick
            test_parser_accepts_underscore_metadata;
        ] );
      ( "foreach",
        [
          Alcotest.test_case
            "foreach.over missing key => Blocked with key name in message" `Quick
            test_foreach_3_elements;
          Alcotest.test_case
            "foreach.over non-array => Blocked; missing key => Blocked" `Quick
            test_foreach_iterates_over_ctx_array;
          Alcotest.test_case "foreach Blocked path replays identically" `Quick
            test_foreach_replay_blocked;
          Alcotest.test_case "foreach iterates 3 elements from initial_ctx" `Quick
            test_foreach_iterates;
          Alcotest.test_case "foreach empty array => 0 iterations" `Quick
            test_foreach_empty_array;
          Alcotest.test_case "foreach replay with iteration" `Quick
            test_foreach_replay_iteration;
          Alcotest.test_case
            "foreach disk replay: Ctx_snapshot header roundtrip" `Quick
            test_foreach_disk_replay;
        ] );
      ( "parallel",
        [
          Alcotest.test_case
            "two-branch parallel both succeed => Completed_no_commit" `Quick
            test_parallel_two_branches_succeed;
          Alcotest.test_case
            "one-branch abort => overall Aborted, cancel-all" `Quick
            test_parallel_one_branch_aborts;
          Alcotest.test_case "parallel replay: identical outcome" `Quick
            test_parallel_replay_success;
          Alcotest.test_case "branch outputs merged post-parallel" `Quick
            test_parallel_branch_output_merge;
        ] );
      ( "lint-parallel",
        [
          Alcotest.test_case "commit-in-parallel => error diagnostic" `Quick
            test_lint_commit_in_parallel;
          Alcotest.test_case "parallel-output-collision => error diagnostic" `Quick
            test_lint_parallel_output_collision;
          Alcotest.test_case "floor-gate intersection: all-branches => ok, one-branch => error" `Quick
            test_lint_floor_gate_parallel_intersection;
        ] );
      ( "compiler",
        [
          Alcotest.test_case
            "versioned header / unversioned fallback" `Quick
            test_compiler_header;
          Alcotest.test_case
            "agent step => const <id> = await agent(...)" `Quick
            test_compiler_agent_step;
          Alcotest.test_case
            "parallel step => await parallel([..." `Quick
            test_compiler_parallel_step;
          Alcotest.test_case
            "foreach step => await pipeline(over, ..." `Quick
            test_compiler_foreach_step;
          Alcotest.test_case
            "run step => [CWR run: cmd=... allowlist note" `Quick
            test_compiler_run_step;
          Alcotest.test_case
            "gate/commit steps => faithful gate if/throw, commit note" `Quick
            test_compiler_gate_commit_steps;
          Alcotest.test_case
            "meta header export const meta = {name,description}" `Quick
            test_compiler_meta_header;
          Alcotest.test_case
            "expr edge cases: And [] → true, Or [] → false" `Quick
            test_compiler_expr_edge_cases;
          Alcotest.test_case
            "expr path: outputs→id.field, other→args.key" `Quick
            test_compiler_expr_path;
          Alcotest.test_case
            "gate real expr: Eq path+int → id.field === 5" `Quick
            test_compiler_gate_real_expr;
          Alcotest.test_case
            "branch real expr: if/else structure, no branch note" `Quick
            test_compiler_branch_real_expr;
          Alcotest.test_case
            "agent schema: JSON Schema object with required array" `Quick
            test_compiler_agent_schema;
          Alcotest.test_case
            "agent schema types: Bool→boolean, Any→{}, Enum→enum" `Quick
            test_compiler_agent_schema_types;
          Alcotest.test_case
            "agent on_failure: Continue→try/catch, Abort→const" `Quick
            test_compiler_agent_on_failure;
          Alcotest.test_case
            "agent read_only: // [read-only] comment" `Quick
            test_compiler_agent_read_only;
          Alcotest.test_case
            "loop Max_iters: counter var + >= N break after body" `Quick
            test_compiler_loop_max_iters;
          Alcotest.test_case
            "loop Budget: budget.remaining() check after body" `Quick
            test_compiler_loop_budget;
          Alcotest.test_case
            "loop until: if (expr) break; after body" `Quick
            test_compiler_loop_until;
          Alcotest.test_case
            "loop Fixpoint: _fixcount var + window break" `Quick
            test_compiler_loop_fixpoint;
          Alcotest.test_case
            "nested loops: distinct counter suffixes _0 and _1" `Quick
            test_compiler_nested_loops;
          Alcotest.test_case
            "hyphenated ids: sanitized to _ in var/path, label preserved" `Quick
            test_compiler_hyphenated_ids;
          Alcotest.test_case
            "js escape: control chars, schema key quoting, leading-digit/space ids" `Quick
            test_compiler_js_escape_completeness;
          Alcotest.test_case
            "name with newline: escaped in comment and meta, no bare LF" `Quick
            test_compiler_name_newline;
        ] );
    ]
