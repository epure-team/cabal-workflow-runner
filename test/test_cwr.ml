open Cabal_workflow_runner
open Types

(* ---- helpers ---- *)

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
  let tok = "super-secret-approval" in
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
           | Blocked_at { reason; _ } -> reason)
         trace)
  in
  let contains hay needle =
    let nl = String.length needle and hl = String.length hay in
    let rec aux i = i + nl <= hl && (String.sub hay i nl = needle || aux (i + 1)) in
    nl = 0 || aux 0
  in
  Alcotest.(check bool) "raw token absent from trace" false (contains trace_text tok)

(* ---- KEEP: happy path ---- *)

let test_happy_path () =
  let v = validate_ok ~floor:[ "g" ] gated_workflow in
  let outcome, _ = Engine.run ~backend:(Backend.stub ()) ~token:(Some "approve") v in
  match outcome with
  | Committed { id; _ } -> Alcotest.(check string) "committed id" "submit" id
  | o -> Alcotest.failf "expected Committed, got %s" (Types.string_of_outcome o)

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
        ] );
      ( "structured-output",
        [
          Alcotest.test_case "high severity drives then_ branch" `Quick
            test_branch_high;
          Alcotest.test_case "low severity drives else_ branch" `Quick
            test_branch_low;
          Alcotest.test_case "missing schema field => Aborted (fail-closed)"
            `Quick test_schema_fail_closed;
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
        ] );
      ( "happy-path",
        [
          Alcotest.test_case "gated + token + pass => Committed" `Quick
            test_happy_path;
        ] );
    ]
