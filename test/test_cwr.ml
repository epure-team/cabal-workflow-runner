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

(* A workflow whose Commit is guaranteed-gated by "g" on every path. *)
let gated_workflow =
  {
    name = "gated";
    steps =
      [
        Agent { id = "draft"; prompt = "do work"; read_only = false };
        Gate { id = "g" };
        Commit { id = "submit" };
      ];
  }

(* ---- 1. parse ---- *)

let test_parse_roundtrip () =
  let json =
    {|{ "name": "demo",
        "steps": [
          { "kind": "agent", "id": "a", "prompt": "p", "read_only": true },
          { "kind": "gate", "id": "g" },
          { "kind": "branch", "on": "g",
            "then": [ { "kind": "commit", "id": "c" } ],
            "else": [ { "kind": "gate", "id": "h" } ] },
          { "kind": "loop", "max_iters": 2, "until": "g",
            "body": [ { "kind": "agent", "id": "b", "prompt": "q", "read_only": false } ] }
        ] }|}
  in
  match Workflow_json.of_string json with
  | Error e -> Alcotest.failf "valid JSON failed to parse: %s" e
  | Ok wf ->
      Alcotest.(check string) "name" "demo" wf.name;
      Alcotest.(check int) "step count" 4 (List.length wf.steps);
      (* round-trip: re-serialise and re-parse yields the same workflow *)
      let reparsed = Workflow_json.of_json (Workflow_json.to_json wf) in
      (match reparsed with
      | Ok wf2 -> Alcotest.(check bool) "roundtrip equal" true (wf = wf2)
      | Error e -> Alcotest.failf "round-trip parse failed: %s" e)

let test_parse_malformed () =
  (match Workflow_json.of_string "{ this is not json " with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "malformed JSON should yield Error");
  (* structurally malformed: unknown step kind *)
  match
    Workflow_json.of_string
      {|{ "name": "x", "steps": [ { "kind": "frobnicate" } ] }|}
  with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "unknown step kind should yield Error"

(* ---- 2. FAIL-CLOSED validation ---- *)

let test_validate_commit_no_gate () =
  let wf =
    { name = "ungated"; steps = [ Commit { id = "submit" } ] }
  in
  match Validate.workflow ~floor_gates:[ "g" ] wf with
  | Error _ -> () (* expected: commit with no required gate before it *)
  | Ok _ -> Alcotest.fail "commit without floor gate must be rejected"

let test_validate_commit_one_branch_only () =
  (* "g" is evaluated in the then-branch only; the commit lives after the
     branch, so on the else-path "g" was never guaranteed. *)
  let wf =
    {
      name = "one-branch";
      steps =
        [
          Branch
            {
              on = "decide";
              then_ = [ Gate { id = "g" } ];
              else_ = [ Agent { id = "x"; prompt = "p"; read_only = true } ];
            };
          Commit { id = "submit" };
        ];
    }
  in
  match Validate.workflow ~floor_gates:[ "g" ] wf with
  | Error _ -> () (* expected: g not guaranteed on every path *)
  | Ok _ -> Alcotest.fail "commit gated in only one branch must be rejected"

let test_validate_loop_zero () =
  let wf =
    {
      name = "unbounded";
      steps =
        [
          Loop
            {
              max_iters = 0;
              until = "g";
              body = [ Agent { id = "x"; prompt = "p"; read_only = true } ];
            };
        ];
    }
  in
  match Validate.workflow ~floor_gates:[] wf with
  | Error _ -> () (* expected: max_iters < 1 is unbounded *)
  | Ok _ -> Alcotest.fail "loop with max_iters:0 must be rejected"

let test_validate_loop_gate_not_guaranteed () =
  (* A gate inside a loop body does NOT count as guaranteed before a commit,
     because the loop may run zero iterations. *)
  let wf =
    {
      name = "loop-gate";
      steps =
        [
          Loop { max_iters = 3; until = "done"; body = [ Gate { id = "g" } ] };
          Commit { id = "submit" };
        ];
    }
  in
  match Validate.workflow ~floor_gates:[ "g" ] wf with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "gate inside loop body must not count as guaranteed"

let test_validate_accepts_gated () =
  let _ = validate_ok ~floor:[ "g" ] gated_workflow in
  ()

(* ---- 3. Commit needs the runtime token ---- *)

let test_commit_no_token_blocked () =
  let v = validate_ok ~floor:[ "g" ] gated_workflow in
  let outcome, _trace = Engine.run ~backend:(Backend.stub ()) ~token:None v in
  match outcome with
  | Blocked _ -> ()
  | o -> Alcotest.failf "expected Blocked with no token, got %s"
           (Types.string_of_outcome o)

let test_commit_token_digest_only () =
  let v = validate_ok ~floor:[ "g" ] gated_workflow in
  let tok = "super-secret-approval" in
  let outcome, trace =
    Engine.run ~backend:(Backend.stub ()) ~token:(Some tok) v
  in
  (match outcome with
  | Committed { token_digest; _ } ->
      Alcotest.(check string) "digest matches Engine.token_digest"
        (Engine.token_digest tok) token_digest;
      Alcotest.(check bool) "raw token not in digest" false
        (token_digest = tok)
  | o -> Alcotest.failf "expected Committed, got %s"
           (Types.string_of_outcome o));
  (* the raw token must not appear anywhere in the trace *)
  let trace_text =
    String.concat "|"
      (List.map
         (function
           | Committed_step { token_digest; _ } -> token_digest
           | Agent_ran { text; _ } -> text
           | Gate_evaluated { id; _ } -> id
           | Blocked_at { reason; _ } -> reason)
         trace)
  in
  let contains hay needle =
    let nl = String.length needle and hl = String.length hay in
    let rec aux i = i + nl <= hl && (String.sub hay i nl = needle || aux (i + 1)) in
    nl = 0 || aux 0
  in
  Alcotest.(check bool) "raw token absent from trace" false
    (contains trace_text tok)

(* ---- 4. Bounded loop terminates ---- *)

let counting_backend ~gate_verdict =
  let count = ref 0 in
  let agent ~id ~prompt:_ ~read_only:_ =
    incr count;
    (true, id)
  in
  let gate _ = gate_verdict in
  (Backend.stub ~gate ~agent (), count)

let test_loop_runs_max_when_never_pass () =
  (* until = Fail forever => loop runs body exactly max_iters times, then the
     workflow continues (no commit) => Completed_no_commit, never infinite. *)
  let backend, count = counting_backend ~gate_verdict:Fail in
  let wf =
    {
      name = "loopy";
      steps =
        [
          Loop
            {
              max_iters = 3;
              until = "done";
              body = [ Agent { id = "iter"; prompt = "p"; read_only = false } ];
            };
        ];
    }
  in
  let v = validate_ok ~floor:[] wf in
  let outcome, _ = Engine.run ~backend ~token:None v in
  Alcotest.(check int) "ran body exactly max_iters times" 3 !count;
  Alcotest.(check outcome_testable) "completed without commit"
    Completed_no_commit outcome

let test_loop_stops_early_when_pass () =
  (* until = Pass immediately => loop body runs zero times. *)
  let backend, count = counting_backend ~gate_verdict:Pass in
  let wf =
    {
      name = "loopy2";
      steps =
        [
          Loop
            {
              max_iters = 5;
              until = "done";
              body = [ Agent { id = "iter"; prompt = "p"; read_only = false } ];
            };
        ];
    }
  in
  let v = validate_ok ~floor:[] wf in
  let _ = Engine.run ~backend ~token:None v in
  Alcotest.(check int) "loop stopped early, body never ran" 0 !count

(* ---- 5. Deterministic replay ---- *)

let test_replay_identical () =
  (* mixed workflow with a branch and a loop, gated commit, token present *)
  let wf =
    {
      name = "mixed";
      steps =
        [
          Agent { id = "draft"; prompt = "p"; read_only = false };
          Loop
            {
              max_iters = 2;
              until = "ready";
              body = [ Agent { id = "fix"; prompt = "q"; read_only = false } ];
            };
          Gate { id = "g" };
          Branch
            {
              on = "g";
              then_ = [ Commit { id = "submit" } ];
              else_ = [ Agent { id = "noop"; prompt = "r"; read_only = true } ];
            };
        ];
    }
  in
  (* gate "g" Pass (take then -> commit), "ready" Fail (loop runs to cap) *)
  let gate = function "g" -> Pass | _ -> Fail in
  let backend = Backend.stub ~gate () in
  let v = validate_ok ~floor:[ "g" ] wf in
  let outcome, trace = Engine.run ~backend ~token:(Some "tok") v in
  let replayed = Engine.replay ~trace v in
  Alcotest.(check outcome_testable) "replay outcome identical" outcome replayed;
  (match outcome with
  | Committed _ -> ()
  | o -> Alcotest.failf "expected the run to commit, got %s"
           (Types.string_of_outcome o))

(* ---- 6. Happy path ---- *)

let test_happy_path () =
  let v = validate_ok ~floor:[ "g" ] gated_workflow in
  let outcome, _ =
    Engine.run ~backend:(Backend.stub ()) ~token:(Some "approve") v
  in
  match outcome with
  | Committed { id; _ } -> Alcotest.(check string) "committed id" "submit" id
  | o -> Alcotest.failf "expected Committed, got %s"
           (Types.string_of_outcome o)

let () =
  Alcotest.run "cabal_workflow_runner"
    [
      ( "parse",
        [
          Alcotest.test_case "valid round-trips" `Quick test_parse_roundtrip;
          Alcotest.test_case "malformed => Error" `Quick test_parse_malformed;
        ] );
      ( "validate-fail-closed",
        [
          Alcotest.test_case "commit, no required gate => Error" `Quick
            test_validate_commit_no_gate;
          Alcotest.test_case "commit gated in one branch only => Error" `Quick
            test_validate_commit_one_branch_only;
          Alcotest.test_case "loop max_iters:0 => Error" `Quick
            test_validate_loop_zero;
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
      ( "bounded-loop",
        [
          Alcotest.test_case "never-pass loop runs exactly max_iters" `Quick
            test_loop_runs_max_when_never_pass;
          Alcotest.test_case "early-pass loop stops immediately" `Quick
            test_loop_stops_early_when_pass;
        ] );
      ( "replay",
        [
          Alcotest.test_case "replay yields identical outcome" `Quick
            test_replay_identical;
        ] );
      ( "happy-path",
        [ Alcotest.test_case "gated + token + pass => Committed" `Quick
            test_happy_path ] );
    ]
