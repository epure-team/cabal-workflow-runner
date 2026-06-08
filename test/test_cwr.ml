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
  check_clean "../examples/bounty.workflow.json"
    [ "g-validated"; "g-observed"; "g-independent" ];
  check_clean "../examples/smoke.workflow.json" [ "g-observed" ]

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
          Alcotest.test_case "contract: examples lint-clean AND validate Ok"
            `Quick test_lint_contract_examples;
          Alcotest.test_case "contract: known-bad has_errors AND validate Error"
            `Quick test_lint_contract_badness;
          Alcotest.test_case "to_json / diagnostic_to_json shape" `Quick
            test_lint_to_json_shape;
          Alcotest.test_case "generate->fix loop converges + validates" `Quick
            test_lint_generate_fix_loop;
        ] );
    ]
