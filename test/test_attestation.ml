open Cabal_workflow_runner
open Types

let parse_exn raw =
  match Workflow_json.of_string raw with
  | Ok wf -> wf
  | Error e -> Alcotest.failf "parse failed: %s" e

let test_attest_step_parses_and_roundtrips () =
  let raw =
    {|{
      "name":"signed-export",
      "version":"1.0",
      "steps":[{
        "kind":"attest",
        "id":"publish-proof",
        "select":["outputs.prove", "campaign"],
        "replay_domain":"bounty-triage/v1",
        "output":"artifacts/proof.attestation.json"
      }]
    }|}
  in
  let wf = parse_exn raw in
  (match wf.steps with
  | [ Attest { id; select; replay_domain; output } ] ->
      Alcotest.(check string) "id" "publish-proof" id;
      Alcotest.(check (list string))
        "selection"
        [ "outputs.prove"; "campaign" ]
        select;
      Alcotest.(check string) "domain" "bounty-triage/v1" replay_domain;
      Alcotest.(check string) "output" "artifacts/proof.attestation.json" output
  | _ -> Alcotest.fail "expected one attest step");
  match Workflow_json.of_json (Workflow_json.to_json wf) with
  | Ok wf' -> Alcotest.(check bool) "roundtrip" true (wf = wf')
  | Error e -> Alcotest.failf "roundtrip parse failed: %s" e

let signer () =
  match
    Attestation.signer_of_seed (String.init 32 (fun i -> Char.chr (i + 1)))
  with
  | Ok s -> s
  | Error e -> Alcotest.failf "signer: %s" e

let workflow () =
  parse_exn
    {|{"name":"signed-export","version":"1.0","steps":[{"kind":"attest","id":"publish-proof","select":["outputs.prove"],"replay_domain":"bounty-triage/v1","output":"proof.json"}]}|}

let selected =
  [ ("outputs.prove", `Assoc [ ("ok", `Bool true); ("n", `Int 7) ]) ]

let make_envelope s =
  match Attestation.create ~signer:s ~workflow:(workflow ())
    ~step_id:"publish-proof" ~occurrence:0 ~output_path:"proof.json"
    ~replay_domain:"bounty-triage/v1" ~session_nonce:"run-2026-07-11-001"
    ~selected with
  | Ok envelope -> envelope
  | Error e -> Alcotest.failf "create envelope: %s" e

let verifier s =
  match Attestation.verifier_of_public_key (Attestation.public_key s) with
  | Ok v -> v
  | Error e -> Alcotest.failf "verifier: %s" e

let verify ?(wf = workflow ()) ?(step_id = "publish-proof")
    ?(domain = "bounty-triage/v1") ?(nonce = "run-2026-07-11-001")
    ?(values = selected) v envelope =
  Attestation.verify ~verifier:v ~workflow:wf ~step_id ~replay_domain:domain
    ~occurrence:0 ~output_path:"proof.json" ~session_nonce:nonce
    ~selected:values envelope

let test_ed25519_envelope_signing_and_binding () =
  let s = signer () in
  let v = verifier s in
  let envelope = make_envelope s in
  Alcotest.(check (result unit string))
    "valid signature" (Ok ()) (verify v envelope);
  Alcotest.(check bool)
    "key id is stable" true
    (String.length (Attestation.key_id s) > 20);
  let wrong_values = [ ("outputs.prove", `Assoc [ ("ok", `Bool false) ]) ] in
  Alcotest.(check bool)
    "output mutation fails" true
    (Result.is_error (verify ~values:wrong_values v envelope));
  Alcotest.(check bool)
    "wrong step fails" true
    (Result.is_error (verify ~step_id:"other" v envelope));
  Alcotest.(check bool)
    "wrong workflow fails" true
    (Result.is_error
       (verify ~wf:{ (workflow ()) with name = "other" } v envelope));
  Alcotest.(check bool)
    "wrong domain fails" true
    (Result.is_error (verify ~domain:"other/v1" v envelope));
  Alcotest.(check bool)
    "wrong session nonce fails" true
    (Result.is_error (verify ~nonce:"another-run" v envelope));
  let other =
    match Attestation.signer_of_seed (String.make 32 '\x55') with
    | Ok x -> x
    | Error e -> Alcotest.fail e
  in
  Alcotest.(check bool)
    "wrong pinned key fails" true
    (Result.is_error (verify (verifier other) envelope));
  match Attestation.verifier_of_identity (Attestation.public_identity s) with
  | Ok _ -> ()
  | Error e -> Alcotest.failf "public identity roundtrip: %s" e

let test_attest_parser_rejects_unsafe_inputs () =
  let step extra =
    Printf.sprintf
      {|{"name":"x","steps":[{"kind":"attest","id":"a","select":["outputs.x"],"replay_domain":"d","output":"ok.json",%s}]}|}
      extra
  in
  let invalid =
    [
      {|{"name":"x","steps":[{"kind":"attest","id":"a","select":["outputs.x"],"replay_domain":"d","output":"../x"}]}|};
      {|{"name":"x","steps":[{"kind":"attest","id":"a","select":["outputs.x"],"replay_domain":"d","output":"/x"}]}|};
      {|{"name":"x","steps":[{"kind":"attest","id":"a","select":["outputs.x"],"replay_domain":"d","output":"a/./x"}]}|};
      {|{"name":"x","steps":[{"kind":"attest","id":"a","select":["outputs.x"],"replay_domain":"d","output":"a//x"}]}|};
      {|{"name":"x","steps":[{"kind":"attest","id":"a","select":["outputs.x"],"replay_domain":"d","output":"a\\x"}]}|};
      {|{"name":"x","steps":[{"kind":"attest","id":"a","select":["outputs.x","outputs.x"],"replay_domain":"d","output":"x"}]}|};
      {|{"name":"x","steps":[{"kind":"attest","id":"a","select":["outputs.x"],"replay_domain":" ","output":"x"}]}|};
    ]
  in
  ignore step;
  List.iter
    (fun raw ->
      match Workflow_json.of_string raw with
      | Error _ -> ()
      | Ok _ -> Alcotest.failf "unsafe attest input accepted: %s" raw)
    invalid

let test_compiler_marks_attest_unrepresentable () =
  Alcotest.check_raises "compiler refuses authority loss"
    (Compiler.Compile_error
       "workflow contains Attest; Claude Workflow JS cannot preserve engine-held signing authority")
    (fun () -> ignore (Compiler.compile_workflow (workflow ())))

let validated ?(required = []) wf =
  match Validate.workflow ~required_attestations:required ~floor_gates:[] wf with
  | Ok v -> v
  | Error e -> Alcotest.failf "validation failed: %s" e

let with_temp_dir f =
  let p = Filename.temp_file "cwr-attest-" "" in
  Sys.remove p;
  Unix.mkdir p 0o700;
  Fun.protect
    ~finally:(fun () ->
      let rec remove path =
        match Unix.lstat path with
        | { Unix.st_kind = Unix.S_DIR; _ } ->
            Sys.readdir path
            |> Array.iter (fun n -> remove (Filename.concat path n));
            Unix.rmdir path
        | _ -> Unix.unlink path
        | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
      in
      remove p)
    (fun () -> f p)

let engine_run ?attestation_signer ?attestation_artifact_root
    ?attestation_session_nonce ?initial_ctx ~backend validated =
  Eio_main.run (fun _ ->
      Eio.Switch.run (fun sw ->
          Engine.run ?attestation_signer ?attestation_artifact_root
            ?attestation_session_nonce ?initial_ctx ~sw ~backend ~token:None validated))

let engine_replay ?initial_ctx ~attestation_verifier ~attestation_session_nonce ~trace
    validated =
  Eio_main.run (fun _ ->
      Eio.Switch.run (fun sw ->
          Engine.replay ?initial_ctx ~attestation_verifier ~attestation_session_nonce ~sw
            ~trace validated))

let runtime_workflow output =
  {
    name = "runtime-export";
    version = Some "1.0";
    steps =
      [
        Agent
          {
            id = "prove";
            prompt = "produce proof";
            read_only = true;
            output_schema = None;
            on_failure = Abort;
            protocol = None;
            brief = None;
            agent_type = None;
            model = None;
            input = None;
          };
        Attest
          {
            id = "export";
            select = [ "outputs.prove" ];
            replay_domain = "bounty-triage/v1";
            output;
          };
      ];
  }

let test_engine_exports_atomically_and_replay_authenticates () =
  with_temp_dir (fun root ->
      let s = signer () in
      let seen_prompt = ref "" in
      let agent ~id:_ ~prompt ~read_only:_ ~agent_type:_ ~model:_ =
        seen_prompt := prompt;
        (true, `Assoc [ ("finding", `String "F-1"); ("valid", `Bool true) ])
      in
      let backend = Backend.stub ~agent () in
      let wf = runtime_workflow "proof.json" in
      let vwf = validated ~required:["export"] wf in
      let outcome, trace =
        engine_run ~attestation_signer:s ~attestation_artifact_root:root
          ~attestation_session_nonce:"session-001" ~backend vwf
      in
      Alcotest.(check string)
        "completed" "Completed_no_commit"
        (Types.string_of_outcome outcome);
      Alcotest.(check string)
        "backend saw only workflow prompt" "produce proof" !seen_prompt;
      let exported = Filename.concat root "proof.json" in
      Alcotest.(check bool) "artifact exists" true (Sys.file_exists exported);
      let disk = Yojson.Safe.from_file exported in
      let envelope =
        match List.rev trace with
        | Attestation_exported { id = "export"; envelope } :: _ -> envelope
        | _ -> Alcotest.fail "missing attestation trace"
      in
      Alcotest.(check string)
        "disk equals trace"
        (Attestation.canonical_string envelope)
        (Attestation.canonical_string disk);
      let replayed =
        engine_replay ~attestation_verifier:(verifier s)
          ~attestation_session_nonce:"session-001" ~trace vwf
      in
      Alcotest.(check string)
        "authenticated replay" "Completed_no_commit"
        (Types.string_of_outcome replayed);
      let unrequired = validated wf in
      Alcotest.check_raises "programmatic replay requires structural pin"
        (Engine.Replay_mismatch
           "authenticated replay requires validated required_attestations")
        (fun () -> ignore (engine_replay ~attestation_verifier:(verifier s)
          ~attestation_session_nonce:"session-001" ~trace unrequired));
      Alcotest.check_raises "cross-session replay rejected"
        (Engine.Replay_mismatch
           "attestation verification failed: attestation binding mismatch")
        (fun () ->
          ignore
            (engine_replay ~attestation_verifier:(verifier s)
               ~attestation_session_nonce:"different-session" ~trace vwf));
      let tampered =
        List.map
          (function
            | Attestation_exported { id; envelope = `Assoc fields } ->
                Attestation_exported
                  {
                    id;
                    envelope =
                      `Assoc
                        (("key_id", `String "forged")
                        :: List.remove_assoc "key_id" fields);
                  }
            | entry -> entry)
          trace
      in
      Alcotest.check_raises "tampered ledger rejected"
        (Engine.Replay_mismatch
           "attestation verification failed: attestation binding mismatch")
        (fun () ->
          ignore
            (engine_replay ~attestation_verifier:(verifier s)
               ~attestation_session_nonce:"session-001" ~trace:tampered vwf));
      let unsafe_replay_cases : (string * Yojson.Safe.t) list =
        [ ("integer", `Int 9007199254740992); ("float", `Float 1.5);
          ("intlit", `Intlit "999999999999999999999999999") ] in
      List.iter (fun (label, unsafe) ->
        let unsafe_trace = List.map (function
          | Agent_ran { id = "prove"; success; _ } ->
              Agent_ran { id = "prove"; success;
                output = `Assoc [ ("unsafe", unsafe) ];
                request_receipt = None; result_receipt = None }
          | entry -> entry) trace in
        Alcotest.(check bool) ("unsafe " ^ label ^ " replay is controlled") true
          (match (try ignore (engine_replay ~attestation_verifier:(verifier s)
            ~attestation_session_nonce:"session-001" ~trace:unsafe_trace vwf);
            `Succeeded with Engine.Replay_mismatch _ -> `Mismatch
            | Invalid_argument _ -> `Crashed) with
          | `Mismatch -> true
          | _ -> false)) unsafe_replay_cases)

let test_engine_missing_key_and_symlink_fail_closed () =
  with_temp_dir (fun root ->
      let backend = Backend.stub () in
      let wf = runtime_workflow "proof.json" in
      let outcome, _ =
        engine_run ~attestation_artifact_root:root
          ~attestation_session_nonce:"session-001" ~backend (validated wf)
      in
      (match outcome with
      | Blocked reason ->
          Alcotest.(check bool)
            "missing signing config named" true
            (String.length reason > 0)
      | _ -> Alcotest.fail "missing signer must block");
      let outside = Filename.concat root "outside" in
      Unix.mkdir outside 0o700;
      Unix.symlink outside (Filename.concat root "link");
      let wf = runtime_workflow "link/proof.json" in
      let outcome, _ =
        engine_run ~attestation_signer:(signer ())
          ~attestation_artifact_root:root
          ~attestation_session_nonce:"session-001" ~backend (validated wf)
      in
      match outcome with
      | Blocked _ -> ()
      | _ -> Alcotest.fail "symlinked artifact parent must block")

let test_key_fd_is_exact_and_closed () =
  let seed = String.init 32 (fun i -> Char.chr (i + 1)) in
  let rd, wr = Unix.pipe ~cloexec:true () in
  ignore (Unix.write_substring wr seed 0 32);
  Unix.close wr;
  (match Attestation.signer_of_fd rd with
  | Ok _ -> ()
  | Error e -> Alcotest.failf "fd seed rejected: %s" e);
  (match Unix.read rd (Bytes.create 1) 0 1 with
  | _ -> Alcotest.fail "signer_of_fd must close the descriptor"
  | exception Unix.Unix_error (Unix.EBADF, _, _) -> ()
  | exception exn ->
      Alcotest.failf "unexpected descriptor result: %s" (Printexc.to_string exn));
  let rd, wr = Unix.pipe ~cloexec:true () in
  let overlong = seed ^ "x" in
  ignore (Unix.write_substring wr overlong 0 33);
  Unix.close wr;
  match Attestation.signer_of_fd rd with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "overlong seed must fail closed"

let test_canonical_json_rejects_ambiguous_values () =
  let invalid =
    [
      ("duplicate top-level", `Assoc [ ("x", `Int 1); ("x", `Int 2) ]);
      ("duplicate nested", `Assoc [ ("x", `Assoc [ ("y", `Bool true); ("y", `Bool false) ]) ]);
      ("float", `Float 1.0); ("non-finite", `Float nan);
      ("intlit", `Intlit "1");
    ]
  in
  List.iter (fun (label, value) ->
    Alcotest.(check bool) label true
      (Result.is_error (Attestation.validate_canonical_json value))) invalid;
  let duplicate_identity = match Attestation.public_identity (signer ()) with
    | `Assoc fields -> `Assoc (("key_id", `String "forged") :: fields)
    | _ -> assert false in
  Alcotest.(check bool) "duplicate identity" true
    (Result.is_error (Attestation.verifier_of_identity duplicate_identity));
  let duplicate_selected =
    [ ("outputs.prove", `Assoc [ ("ok", `Bool true); ("ok", `Bool false) ]) ] in
  Alcotest.(check bool) "duplicate selected object" true
    (Result.is_error (Attestation.validate_selected duplicate_selected));
  Alcotest.(check bool) "workflow duplicate rejected" true
    (Result.is_error (Workflow_json.of_string
      {|{"name":"first","name":"last","steps":[]}|}));
  Alcotest.(check bool) "ledger duplicate rejected" true
    (Result.is_error (Ledger.of_ndjson
      {|{"kind":"blocked_at","id":"a","id":"b","reason":"x"}|}));
  Alcotest.(check bool) "legacy workflow float retained" true
    (Result.is_ok (Workflow_json.of_string
      {|{"name":"x","steps":[{"kind":"loop","governors":[{"kind":"max_iters","n":1.0}],"body":[]}]}|}));
  Alcotest.(check bool) "attest-bearing workflow float rejected" true
    (Result.is_error (Workflow_json.of_string
      {|{"name":"x","steps":[{"kind":"attest","id":"a","select":["x"],"replay_domain":"d","output":"a.json","_n":1.0}]}|}))

let test_concurrent_target_conflict_fails_closed () =
  with_temp_dir (fun root ->
      let payload =
        `Assoc [ ("blob", `String (String.make (16 * 1024 * 1024) 'x')) ] in
      let domain = Domain.spawn (fun () ->
        let rec wait () =
          let names = Sys.readdir root in
          if Array.exists (fun n -> String.length n >= 11 &&
               String.sub n 0 11 = "proof.json.") names then
            Out_channel.with_open_bin (Filename.concat root "proof.json")
              (fun oc -> output_string oc "attacker")
          else (Domain.cpu_relax (); wait ())
        in wait ()) in
      let result = Attestation.write_atomic ~artifact_root:root
        ~relative_path:"proof.json" payload in
      Domain.join domain;
      Alcotest.(check bool) "conflicting target rejected" true
        (Result.is_error result);
      Alcotest.(check string) "attacker file preserved" "attacker"
        (In_channel.with_open_bin (Filename.concat root "proof.json")
           In_channel.input_all))

let test_concurrent_parent_swap_fails_closed () =
  with_temp_dir (fun root ->
      let parent = Filename.concat root "parent" in
      Unix.mkdir parent 0o700;
      let payload =
        `Assoc [ ("blob", `String (String.make (16 * 1024 * 1024) 'x')) ] in
      let domain = Domain.spawn (fun () ->
        let rec wait () =
          if Array.exists (fun n -> String.length n >= 11 &&
               String.sub n 0 11 = "proof.json.") (Sys.readdir parent) then (
            Unix.rename parent (Filename.concat root "moved");
            Unix.mkdir parent 0o700)
          else (Domain.cpu_relax (); wait ())
        in wait ()) in
      let result = Attestation.write_atomic ~artifact_root:root
        ~relative_path:"parent/proof.json" payload in
      Domain.join domain;
      Alcotest.(check bool) "swapped parent rejected" true
        (Result.is_error result);
      Alcotest.(check bool) "replacement parent untouched" false
        (Sys.file_exists (Filename.concat parent "proof.json")))

let test_symlink_in_artifact_root_ancestor_fails_closed () =
  with_temp_dir (fun base ->
      let real = Filename.concat base "real" in
      Unix.mkdir real 0o700;
      Unix.mkdir (Filename.concat real "root") 0o700;
      Unix.symlink real (Filename.concat base "link");
      let via_symlink = Filename.concat (Filename.concat base "link") "root" in
      Alcotest.(check bool) "ancestor symlink rejected" true
        (Result.is_error (Attestation.write_atomic ~artifact_root:via_symlink
          ~relative_path:"proof.json" (`Assoc [ ("ok", `Bool true) ]))))

let test_secure_fs_relative_starting_directory () =
  with_temp_dir (fun root ->
      let prior = Sys.getcwd () in
      Fun.protect ~finally:(fun () -> Unix.chdir prior) (fun () ->
        Unix.chdir root;
        Out_channel.with_open_bin "pin.json" (fun oc -> output_string oc "pin");
        Alcotest.(check (result string string)) "basename read" (Ok "pin")
          (Secure_fs.read_regular "pin.json");
        Alcotest.(check (result string string)) "dot-prefixed read" (Ok "pin")
          (Secure_fs.read_regular "./pin.json");
        Alcotest.(check bool) "root dot write" true
          (Result.is_ok (Attestation.write_atomic ~artifact_root:"."
            ~relative_path:"out.json" (`Assoc [ ("ok", `Bool true) ])))))

let test_attest_beneath_parallel_is_rejected () =
  let agent = Agent { id = "a"; prompt = "p"; read_only = true;
    output_schema = None; on_failure = Abort; protocol = None; brief = None;
    agent_type = None; model = None; input = None } in
  let attest = Attest { id = "export"; select = [ "campaign" ];
    replay_domain = "d"; output = "a.json" } in
  let wf = { name = "parallel-attest"; version = Some "1.0";
    steps = [ Parallel { branches = [ [ agent ];
      [ Branch { when_ = Expr.Lit (Expr.Bool true); then_ = [ attest ];
                 else_ = [] } ] ] } ] } in
  let ds = Lint.check ~floor_gates:[] wf in
  Alcotest.(check bool) "lint blocks embedded unsigned trace" true
    (List.exists (fun (d : Lint.diagnostic) ->
       d.severity = Lint.Error && d.code = "attest-in-parallel") ds);
  Alcotest.(check bool) "validation blocks live and replay" true
    (Result.is_error (Validate.workflow ~floor_gates:[] wf))

let test_post_rename_fsync_is_published_uncertain () =
  with_temp_dir (fun root ->
      Unix.putenv "CWR_TEST_FAIL_DIR_FSYNC" "1";
      let outcome = Fun.protect
        ~finally:(fun () -> Unix.putenv "CWR_TEST_FAIL_DIR_FSYNC" "")
        (fun () -> fst (engine_run ~attestation_signer:(signer ())
          ~attestation_artifact_root:root ~attestation_session_nonce:"s"
          ~backend:(Backend.stub ())
          ~initial_ctx:[ ("campaign", `String "C-1") ]
          (validated { (runtime_workflow "proof.json") with
            steps = [ Attest { id = "export"; select = [ "campaign" ];
              replay_domain = "d"; output = "proof.json" } ] }))) in
      Alcotest.(check bool) "explicit uncertain outcome" true
        (match outcome with Aborted reason ->
           String.length reason >= 19 && String.sub reason 0 19 = "published-uncertain"
         | _ -> false);
      Alcotest.(check bool) "artifact may already be published" true
        (Sys.file_exists (Filename.concat root "proof.json")))

let test_required_attestation_cannot_be_bypassed () =
  let empty = { name = "empty"; version = Some "1.0"; steps = [] } in
  Alcotest.(check bool) "empty workflow rejected" true
    (Result.is_error (Validate.workflow ~required_attestations:["export"]
      ~floor_gates:[] empty));
  let attest = Attest { id = "export"; select = [ "campaign" ];
    replay_domain = "d"; output = "a.json" } in
  let branched = { empty with steps = [ Branch {
    when_ = Expr.Lit (Expr.Bool true); then_ = [ attest ]; else_ = [] } ] } in
  Alcotest.(check bool) "branch-around rejected" true
    (Result.is_error (Validate.workflow ~required_attestations:["export"]
      ~floor_gates:[] branched));
  Alcotest.(check bool) "guaranteed attest accepted" true
    (Result.is_ok (Validate.workflow ~required_attestations:["export"]
      ~floor_gates:[] { empty with steps = [ attest ] }))

let test_distinct_attest_ids_and_outputs_are_unique () =
  let mk id output = Attest { id; select = [ "campaign" ];
    replay_domain = "d"; output } in
  let base = { name = "dups"; version = Some "1.0"; steps = [] } in
  let duplicate_id = { base with steps = [ mk "a" "a.json"; mk "a" "b.json" ] } in
  let duplicate_output = { base with steps = [ mk "a" "a.json"; mk "b" "a.json" ] } in
  Alcotest.(check bool) "duplicate id rejected" true
    (Result.is_error (Validate.workflow ~floor_gates:[] duplicate_id));
  Alcotest.(check bool) "duplicate output rejected" true
    (Result.is_error (Validate.workflow ~floor_gates:[] duplicate_output))

let test_loop_occurrences_are_distinct_and_replayable () =
  with_temp_dir (fun root ->
      let attest = Attest { id = "export"; select = [ "campaign" ];
        replay_domain = "d"; output = "proof-{occurrence}.json" } in
      let anchor = Attest { id = "anchor"; select = [ "campaign" ];
        replay_domain = "d"; output = "anchor.json" } in
      let wf = { name = "loop-occurrence"; version = Some "1.0";
        steps = [ anchor; Loop { body = [ attest ]; until = None;
          governors = [ Max_iters 2 ] } ] } in
      let validated = validated ~required:["anchor"] wf in
      let outcome, trace = engine_run ~attestation_signer:(signer ())
        ~attestation_artifact_root:root ~attestation_session_nonce:"s"
        ~initial_ctx:[ ("campaign", `String "C") ] ~backend:(Backend.stub ())
        validated in
      Alcotest.(check string) "two occurrences complete" "Completed_no_commit"
        (Types.string_of_outcome outcome);
      let envelopes = List.filter_map (function
        | Attestation_exported { id = "export"; envelope } -> Some envelope
        | _ -> None) trace in
      Alcotest.(check int) "two signed occurrences" 2 (List.length envelopes);
      Alcotest.(check bool) "occurrence bytes differ" true
        (List.nth envelopes 0 <> List.nth envelopes 1);
      Alcotest.(check bool) "occurrence zero immutable" true
        (Sys.file_exists (Filename.concat root "proof-0.json"));
      Alcotest.(check bool) "occurrence one immutable" true
        (Sys.file_exists (Filename.concat root "proof-1.json"));
      let replayed = engine_replay ~attestation_verifier:(verifier (signer ()))
        ~attestation_session_nonce:"s" ~trace
        ~initial_ctx:[ ("campaign", `String "C") ] validated in
      Alcotest.(check string) "loop replay" "Completed_no_commit"
        (Types.string_of_outcome replayed))

let test_repeatable_attest_requires_occurrence_template () =
  let attest = Attest { id = "export"; select = [ "campaign" ];
    replay_domain = "d"; output = "proof.json" } in
  let wf = { name = "bad-loop-output"; version = Some "1.0";
    steps = [ Loop { body = [ attest ]; until = None;
      governors = [ Max_iters 2 ] } ] } in
  let ds = Lint.check ~floor_gates:[] wf in
  Alcotest.(check bool) "repeatable static output rejected" true
    (List.exists (fun (d : Lint.diagnostic) ->
      d.severity = Lint.Error && d.code = "attest-occurrence-template-required") ds)

let test_attest_rejects_mutable_agent_producer () =
  let mutable_agent = Agent { id = "produce"; prompt = "p"; read_only = false;
    output_schema = None; on_failure = Abort; protocol = None; brief = None;
    agent_type = None; model = None; input = None } in
  let attest = Attest { id = "export"; select = [ "outputs.produce" ];
    replay_domain = "d"; output = "proof.json" } in
  let wf = { name = "mutable-producer"; version = Some "1";
    steps = [ mutable_agent; attest ] } in
  let ds = Lint.check ~floor_gates:[] wf in
  Alcotest.(check bool) "lint rejects mutable producer" true
    (List.exists (fun (d : Lint.diagnostic) -> d.severity = Lint.Error &&
      d.code = "attest-selects-mutable-agent") ds);
  Alcotest.(check bool) "validation rejects mutable producer" true
    (Result.is_error (Validate.workflow ~floor_gates:[] wf))

let test_restricted_safe_integer_and_utf8_order_vectors () =
  let bound = 9007199254740991 in
  Alcotest.(check bool) "positive JS-safe bound" true
    (Result.is_ok (Attestation.validate_canonical_json (`Int bound)));
  Alcotest.(check bool) "negative JS-safe bound" true
    (Result.is_ok (Attestation.validate_canonical_json (`Int (-bound))));
  Alcotest.(check bool) "positive bound plus one rejected" true
    (Result.is_error (Attestation.validate_canonical_json (`Int (bound + 1))));
  Alcotest.(check bool) "negative bound minus one rejected" true
    (Result.is_error (Attestation.validate_canonical_json (`Int (-bound - 1))));
  Alcotest.(check string) "UTF-8 byte ordering vector"
    {|{"":1,"𐀀":2}|}
    (Attestation.canonical_string
      (`Assoc [ ("𐀀", `Int 2); ("", `Int 1) ]))

let test_unsafe_selected_values_fail_as_controlled_outcomes () =
  let cases : (string * Yojson.Safe.t) list =
    [ ("unsafe integer", `Int 9007199254740992);
      ("float", `Float 1.5);
      ("intlit", `Intlit "999999999999999999999999999") ] in
  List.iter (fun (label, unsafe) ->
    with_temp_dir (fun root ->
      let wf = runtime_workflow "proof.json" in
      let validated = validated ~required:["export"] wf in
      let agent ~id:_ ~prompt:_ ~read_only:_ ~agent_type:_ ~model:_ =
        true, `Assoc [ ("unsafe", unsafe) ] in
      let outcome, trace = engine_run ~attestation_signer:(signer ())
        ~attestation_artifact_root:root ~attestation_session_nonce:"s"
        ~backend:(Backend.stub ~agent ()) validated in
      Alcotest.(check bool) (label ^ " blocks normally") true
        (match outcome with Blocked reason ->
           String.length reason > 0 | _ -> false);
      Alcotest.(check bool) (label ^ " records terminal trace") true
        (List.exists (function Blocked_at { id = "export"; _ } -> true
          | _ -> false) trace);
      Alcotest.(check bool) (label ^ " publishes nothing") false
        (Sys.file_exists (Filename.concat root "proof.json")))) cases;
  let bad_selected = [ ("outputs.prove", `Float 1.5) ] in
  Alcotest.(check bool) "direct creation is result error" true
    (Result.is_error (Attestation.create ~signer:(signer ())
      ~workflow:(workflow ()) ~step_id:"publish-proof" ~occurrence:0
      ~output_path:"proof.json" ~replay_domain:"d" ~session_nonce:"s"
      ~selected:bad_selected))

let () =
  Alcotest.run "cwr-attestation"
    [
      ( "workflow-format",
        [
          Alcotest.test_case "attest parses and roundtrips" `Quick
            test_attest_step_parses_and_roundtrips;
          Alcotest.test_case "unsafe paths and fields rejected" `Quick
            test_attest_parser_rejects_unsafe_inputs;
          Alcotest.test_case "compiler reports trust-boundary loss" `Quick
            test_compiler_marks_attest_unrepresentable;
        ] );
      ( "crypto",
        [
          Alcotest.test_case "Ed25519 signature and all bindings" `Quick
            test_ed25519_envelope_signing_and_binding;
        ] );
      ( "engine",
        [
          Alcotest.test_case "atomic export and authenticated replay" `Quick
            test_engine_exports_atomically_and_replay_authenticates;
          Alcotest.test_case "missing key and symlink fail closed" `Quick
            test_engine_missing_key_and_symlink_fail_closed;
        ] );
      ( "key-ingress",
        [
          Alcotest.test_case "fd is exact-length and closed" `Quick
            test_key_fd_is_exact_and_closed;
        ] );
      ( "canonical-json",
        [ Alcotest.test_case "ambiguous values rejected" `Quick
            test_canonical_json_rejects_ambiguous_values ] );
      ( "secure-filesystem",
        [ Alcotest.test_case "relative starting directory" `Quick
            test_secure_fs_relative_starting_directory;
          Alcotest.test_case "ancestor symlink rejected" `Quick
            test_symlink_in_artifact_root_ancestor_fails_closed;
          Alcotest.test_case "concurrent target conflict rejected" `Slow
            test_concurrent_target_conflict_fails_closed;
          Alcotest.test_case "concurrent parent swap rejected" `Slow
            test_concurrent_parent_swap_fails_closed;
          Alcotest.test_case "post-rename fsync is explicit uncertainty" `Quick
            test_post_rename_fsync_is_published_uncertain ] );
      ( "parallel-safety",
        [ Alcotest.test_case "attest beneath parallel rejected" `Quick
            test_attest_beneath_parallel_is_rejected ] );
      ( "authority-pinning",
        [ Alcotest.test_case "required attest cannot be bypassed" `Quick
            test_required_attestation_cannot_be_bypassed;
          Alcotest.test_case "distinct IDs and outputs are unique" `Quick
            test_distinct_attest_ids_and_outputs_are_unique;
          Alcotest.test_case "loop occurrences are signed and replayed" `Quick
            test_loop_occurrences_are_distinct_and_replayable;
          Alcotest.test_case "repeatable output requires occurrence template" `Quick
            test_repeatable_attest_requires_occurrence_template;
          Alcotest.test_case "attest rejects mutable agent producer" `Quick
            test_attest_rejects_mutable_agent_producer;
          Alcotest.test_case "safe integers and UTF-8 ordering vectors" `Quick
            test_restricted_safe_integer_and_utf8_order_vectors;
          Alcotest.test_case "unsafe selected values are controlled outcomes" `Quick
            test_unsafe_selected_values_fail_as_controlled_outcomes ] );
    ]
