open Cabal_workflow_runner
open Types

(* ---- helpers ---- *)

(* Resolve a project-relative path (e.g. "examples/smoke.workflow.json") robustly,
   so the test binary runs both under [dune test]'s sandbox AND standalone via
   [dune exec test/test_cwr.exe] from the repo root. Dune sets DUNE_SOURCEROOT to
   the project root for both [dune exec] and [dune test]; we join against it. The
   fallback is the legacy cwd-relative "../<rel>" form that resolves inside the
   [dune test] sandbox (cwd = test/). *)
let project_path rel =
  match Sys.getenv_opt "DUNE_SOURCEROOT" with
  | Some root -> Filename.concat root rel
  | None -> Filename.concat ".." rel

let validate_ok ~floor wf =
  match Validate.workflow ~floor_gates:floor wf with
  | Ok v -> v
  | Error e -> Alcotest.failf "expected valid workflow, got Error: %s" e

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

(* A workflow whose Commit is guaranteed-gated by "g" on every path. The gate
   condition is trivially true. *)
let gated_workflow =
  {
    name = "gated";
    steps =
      [
        Agent
          { id = "draft"; prompt = "do work"; read_only = false; output_schema = None };
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
    steps =
      [
        Agent { id = "a"; prompt = "assess"; read_only = true; output_schema = None };
        Branch
          {
            when_ =
              Expr.In
                ( Expr.Path [ "outputs"; "a"; "severity" ],
                  Expr.Lit (Expr.List [ Expr.String "high"; Expr.String "critical" ]) );
            then_ =
              [ Agent { id = "esc"; prompt = "escalate"; read_only = false; output_schema = None } ];
            else_ =
              [ Agent { id = "drop"; prompt = "drop"; read_only = false; output_schema = None } ];
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
  let _, trace = Engine.run ~backend ~token:None v in
  Alcotest.(check bool) "high => then_ (escalate ran)" true (ran_agent trace "esc");
  Alcotest.(check bool) "high => else_ NOT taken" false (ran_agent trace "drop")

let test_branch_low () =
  let backend = json_backend [ ("a", `Assoc [ ("severity", `String "low") ]) ] in
  let v = validate_ok ~floor:[] branch_wf in
  let _, trace = Engine.run ~backend ~token:None v in
  Alcotest.(check bool) "low => else_ (drop ran)" true (ran_agent trace "drop");
  Alcotest.(check bool) "low => then_ NOT taken" false (ran_agent trace "esc")

(* ---- TEST 2 (spec): schema fail-closed ---- *)

let test_schema_fail_closed () =
  let schema : Schema.t = [ ("severity", Schema.Enum [ "low"; "high" ]) ] in
  let wf =
    {
      name = "schema";
      steps =
        [
          Agent
            { id = "a"; prompt = "p"; read_only = true; output_schema = Some schema };
          (* would commit if it got here, but the agent output lacks "severity" *)
          Gate { id = "g"; when_ = Expr.Lit (Expr.Bool true) };
          Commit { id = "submit" };
        ];
    }
  in
  (* agent returns an object WITHOUT the required "severity" field *)
  let backend = json_backend [ ("a", `Assoc [ ("other", `String "x") ]) ] in
  let v = validate_ok ~floor:[ "g" ] wf in
  let outcome, _ = Engine.run ~backend ~token:(Some "tok") v in
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
      steps =
        [
          Agent { id = "a"; prompt = "p"; read_only = false; output_schema = None };
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
  let outcome, trace = Engine.run ~backend ~token:(Some "tok") v in
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
  let replayed = Engine.replay ~trace v in
  Alcotest.(check outcome_testable) "replay identical (Aborted)" outcome replayed

(* ---- TEST 6 (spec): ungoverned loop rejected ---- *)

let test_ungoverned_loop_rejected () =
  let wf =
    {
      name = "ungoverned";
      steps =
        [
          Loop
            {
              body =
                [ Agent { id = "x"; prompt = "p"; read_only = true; output_schema = None } ];
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
      steps =
        [
          Loop
            {
              body =
                [ Agent { id = "work"; prompt = "p"; read_only = false; output_schema = None } ];
              (* until never holds *)
              until = Some (Expr.Lit (Expr.Bool false));
              governors = [ Budget ];
            };
        ];
    }
  in
  let v = validate_ok ~floor:[] wf in
  let outcome, trace = Engine.run ~backend ~token:None v in
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
      steps =
        [
          Loop
            {
              body =
                [ Agent { id = "work"; prompt = "p"; read_only = false; output_schema = None } ];
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
  let outcome, trace = Engine.run ~backend ~token:None v in
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
      steps =
        [
          Loop
            {
              body =
                [ Agent { id = "work"; prompt = "p"; read_only = false; output_schema = None } ];
              until = Some (Expr.Lit (Expr.Bool false));
              governors = [ Budget ];
            };
        ];
    }
  in
  let v = validate_ok ~floor:[] wf in
  let outcome, trace = Engine.run ~max_loop_iters:5 ~backend ~token:None v in
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
  let replayed = Engine.replay ~max_loop_iters:5 ~trace v in
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
      steps =
        [
          Loop
            {
              body =
                [ Agent { id = "work"; prompt = "p"; read_only = false; output_schema = None } ];
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
  let outcome, trace = Engine.run ~max_loop_iters:4 ~backend ~token:None v in
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
      steps =
        [
          Agent { id = "assess"; prompt = "p"; read_only = true; output_schema = None };
          Loop
            {
              body =
                [ Agent { id = "work"; prompt = "q"; read_only = false; output_schema = None } ];
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
                [ Agent { id = "noop"; prompt = "r"; read_only = true; output_schema = None } ];
            };
        ];
    }
  in
  let v = validate_ok ~floor:[ "g" ] wf in
  let outcome, trace = Engine.run ~backend ~token:(Some "tok") v in
  (* replay re-feeds recorded outputs + budget readings, no backend *)
  let replayed = Engine.replay ~trace v in
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
  let outcome2, trace2 = Engine.run ~backend:backend2 ~token:(Some "tok") v in
  Alcotest.(check outcome_testable) "second run identical outcome" outcome outcome2;
  Alcotest.(check bool) "second run identical trace" true (trace = trace2)

(* ---- v0.8 F4: replay rejects trailing extra trace entries ---- *)

(* Take a real recorded trace; the unmodified trace replays fine, but the trace
   with one extra dummy entry appended must raise Replay_mismatch. *)
let test_replay_rejects_trailing_entries () =
  let v = validate_ok ~floor:[ "g" ] gated_workflow in
  let outcome, trace = Engine.run ~backend:(Backend.stub ()) ~token:(Some "tok") v in
  (* unmodified trace replays fine *)
  let replayed = Engine.replay ~trace v in
  Alcotest.(check outcome_testable) "unmodified trace replays fine" outcome
    replayed;
  (* append one extra dummy entry => Replay_mismatch *)
  let trace_plus = trace @ [ Loop_iter { index = 99 } ] in
  let raised =
    try
      ignore (Engine.replay ~trace:trace_plus v);
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
    Engine.run ~run_allowlist:[ "mkdir" ] ~backend ~token:None v
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
    Engine.run ~run_allowlist:[] ~backend:backend_count ~token:None v
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
    Engine.run ~run_allowlist:[ "rm" ] ~backend:backend2 ~token:None v
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
    Engine.run ~run_allowlist:[ "*" ] ~backend:backend3 ~token:None v
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
    Engine.run ~run_allowlist:[ "mkdir" ] ~backend ~token:None v
  in
  Alcotest.(check int) "live run executed the command exactly once" 1 !counter;
  (* replay re-feeds the recorded result, NEVER calling run_command again *)
  let replayed = Engine.replay ~trace v in
  Alcotest.(check int) "replay did NOT re-execute (counter unchanged)" 1 !counter;
  Alcotest.(check outcome_testable) "replay outcome identical" outcome replayed;
  (* trailing-entry rejection still holds for a run-bearing trace *)
  let trace_plus = trace @ [ Loop_iter { index = 99 } ] in
  let raised =
    try
      ignore (Engine.replay ~trace:trace_plus v);
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
    { name = "diff"; steps = [ mkrun "create"; mkrun "remove" ] }
  in
  let v = validate_ok ~floor:[] wf in
  let _outcome, trace =
    Engine.run ~run_allowlist:[ "touch" ] ~backend ~token:None v
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
  let _, trace = Engine.run ~run_allowlist:[ "touch" ] ~backend ~token:None v in
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
      Engine.run ~run_allowlist:[ "mkdir" ] ~backend ~token:None v
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
    Engine.run ~run_allowlist:[ "mkdir" ] ~backend:backend_ok ~token:None v_ok
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
    Engine.run ~run_allowlist:[ "definitely-not-a-real-binary-xyz" ] ~backend
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
  let replayed = Engine.replay ~trace v in
  Alcotest.(check outcome_testable) "replay identical" outcome replayed

(* ---- KEEP: fail-closed validation ---- *)

let test_validate_commit_no_gate () =
  let wf = { name = "ungated"; steps = [ Commit { id = "submit" } ] } in
  match Validate.workflow ~floor_gates:[ "g" ] wf with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "commit without floor gate must be rejected"

let test_validate_commit_one_branch_only () =
  let wf =
    {
      name = "one-branch";
      steps =
        [
          Branch
            {
              when_ = Expr.Lit (Expr.Bool true);
              then_ = [ Gate { id = "g"; when_ = Expr.Lit (Expr.Bool true) } ];
              else_ =
                [ Agent { id = "x"; prompt = "p"; read_only = true; output_schema = None } ];
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
  let outcome, _ = Engine.run ~backend:(Backend.stub ()) ~token:None v in
  match outcome with
  | Blocked _ -> ()
  | o -> Alcotest.failf "expected Blocked with no token, got %s" (Types.string_of_outcome o)

let test_commit_token_digest_only () =
  let v = validate_ok ~floor:[ "g" ] gated_workflow in
  let tok = "test-approval-token" in
  let outcome, trace = Engine.run ~backend:(Backend.stub ()) ~token:(Some tok) v in
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
           | Blocked_at { reason; _ } -> reason)
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
      steps =
        [
          Agent
            { id = "a"; prompt = "p"; read_only = true; output_schema = None };
          (* gate predicate is false: outputs.a.ok does not exist *)
          Gate { id = "g"; when_ = Expr.Exists [ "outputs"; "a"; "ok" ] };
          Commit { id = "submit" };
        ];
    }
  in
  let backend = json_backend [ ("a", `Assoc [ ("other", `String "x") ]) ] in
  let v = validate_ok ~floor:[ "g" ] wf in
  let outcome, trace = Engine.run ~backend ~token:(Some "tok") v in
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
  let replayed = Engine.replay ~trace v in
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
      let outcome, _ = Engine.run ~backend ~token:(Some "tok") v in
      match outcome with
      | Committed { id; _ } ->
          Alcotest.(check string) "smoke commits at submit" "submit" id
      | o ->
          Alcotest.failf "expected smoke to commit, got %s"
            (Types.string_of_outcome o))

(* ---- KEEP: happy path ---- *)

let test_happy_path () =
  let v = validate_ok ~floor:[ "g" ] gated_workflow in
  let outcome, _ = Engine.run ~backend:(Backend.stub ()) ~token:(Some "approve") v in
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
      steps =
        [
          Loop
            {
              body =
                [ Agent { id = "x"; prompt = "p"; read_only = true; output_schema = None } ];
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
                    };
                ];
              else_ =
                [
                  Agent
                    { id = "y"; prompt = "p"; read_only = true; output_schema = None };
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
      }
  in
  let wf =
    {
      name = "both-arms-ref";
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
  let wf = { name = "bad"; steps = [ Commit { id = "submit" } ] } in
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
      steps =
        [
          Loop
            {
              body =
                [ Agent { id = "w"; prompt = "p"; read_only = false; output_schema = None } ];
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
  let expected = [ "agent"; "branch"; "commit"; "gate"; "loop"; "run" ] in
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
    ("workflow", [ "name"; "steps" ]);
    ("agent", [ "kind"; "id"; "prompt"; "read_only"; "output_schema" ]);
    ("gate", [ "kind"; "id"; "when" ]);
    ("branch", [ "kind"; "when"; "then"; "else" ]);
    ("loop", [ "kind"; "until"; "governors"; "body" ]);
    ("run", [ "kind"; "id"; "cmd"; "working_dir"; "timeout_ms"; "observe" ]);
    ("commit", [ "kind"; "id" ]);
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
    ]
