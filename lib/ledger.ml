open Types

(* ------------------------------------------------------------------ *)
(* Serialisation: trace_entry -> Yojson, tagged by a "kind" field.     *)
(* ------------------------------------------------------------------ *)

let verdict_to_json = function Pass -> `String "pass" | Fail -> `String "fail"

let outcome_to_json = function
  | Committed { id; token_digest } ->
      `Assoc [ ("kind", `String "committed"); ("id", `String id);
               ("token_digest", `String token_digest) ]
  | Completed_no_commit ->
      `Assoc [ ("kind", `String "completed_no_commit") ]
  | Blocked reason ->
      `Assoc [ ("kind", `String "blocked"); ("reason", `String reason) ]
  | Aborted reason ->
      `Assoc [ ("kind", `String "aborted"); ("reason", `String reason) ]

let file_change_kind_to_json k = `String (string_of_file_change_kind k)

let file_change_to_json (fc : file_change) : Yojson.Safe.t =
  `Assoc
    [
      ("path", `String fc.path);
      ("change", file_change_kind_to_json fc.change);
      ("size", `Int fc.size);
      ("digest", `String fc.digest);
    ]

let run_result_to_json (r : run_result) : Yojson.Safe.t =
  `Assoc
    [
      ("exit", `Int r.exit);
      ("stdout", `String r.stdout);
      ("stderr", `String r.stderr);
      ("truncated", `Bool r.truncated);
      ("files", `List (List.map file_change_to_json r.files));
    ]

let rec entry_to_json (e : trace_entry) : Yojson.Safe.t =
  let tagged kind fields = `Assoc (("kind", `String kind) :: fields) in
  match e with
  | Agent_ran { id; success; output } ->
      tagged "agent_ran"
        [ ("id", `String id); ("success", `Bool success); ("output", output) ]
  | Gate_evaluated { id; verdict } ->
      tagged "gate_evaluated"
        [ ("id", `String id); ("verdict", verdict_to_json verdict) ]
  | Branch_taken { verdict } ->
      tagged "branch_taken" [ ("verdict", verdict_to_json verdict) ]
  | Loop_iter { index } -> tagged "loop_iter" [ ("index", `Int index) ]
  | Budget_read { value } -> tagged "budget_read" [ ("value", `Int value) ]
  | Fixpoint_progress { progress } ->
      tagged "fixpoint_progress" [ ("progress", `Bool progress) ]
  | Loop_stopped { iterations; reason } ->
      tagged "loop_stopped"
        [ ("iterations", `Int iterations); ("reason", `String reason) ]
  | Run_executed { id; result } ->
      tagged "run_executed"
        [ ("id", `String id); ("result", run_result_to_json result) ]
  | Committed_step { id; token_digest } ->
      tagged "committed_step"
        [ ("id", `String id); ("token_digest", `String token_digest) ]
  | Blocked_at { id; reason } ->
      tagged "blocked_at" [ ("id", `String id); ("reason", `String reason) ]
  | Parallel_started -> tagged "parallel_started" []
  | Parallel_branch_completed { branch_idx; trace; outcome; branch_outputs } ->
      tagged "parallel_branch_completed"
        [ ("branch_idx", `Int branch_idx);
          ("trace", `List (List.map entry_to_json trace));
          ("outcome", outcome_to_json outcome);
          ("branch_outputs", `Assoc branch_outputs) ]
  | Parallel_completed { outcome } ->
      tagged "parallel_completed" [ ("outcome", outcome_to_json outcome) ]
  | Foreach_iter_started { index; element } ->
      tagged "foreach_iter_started"
        [ ("index", `Int index); ("element", element) ]
  | Foreach_iter_completed { index; outcome } ->
      tagged "foreach_iter_completed"
        [ ("index", `Int index); ("outcome", outcome_to_json outcome) ]
  | Foreach_completed { iterations } ->
      tagged "foreach_completed" [ ("iterations", `Int iterations) ]
  | Shell_executed { id; results } ->
      tagged "shell_executed"
        [ ("id", `String id);
          ("results",
           `List (List.map (fun (cmd, code) ->
             `Assoc [ ("command", `String cmd); ("exit_code", `Int code) ])
             results)) ]
  | Evidence_evaluated { id; tier; passed } ->
      tagged "evidence_evaluated"
        [ ("id", `String id); ("tier", `String tier); ("passed", `Bool passed) ]
  | Ctx_snapshot { ctx } ->
      tagged "ctx_snapshot" [ ("ctx", `Assoc ctx) ]

(* [Yojson.Safe.to_string] emits no embedded newline for a single object, so one
   object per line is well-formed NDJSON. Each line is newline-terminated. *)
let to_ndjson (t : trace) : string =
  let buf = Buffer.create 256 in
  List.iter
    (fun e ->
      Buffer.add_string buf (Yojson.Safe.to_string (entry_to_json e));
      Buffer.add_char buf '\n')
    t;
  Buffer.contents buf

(* ------------------------------------------------------------------ *)
(* Parsing: fail-closed inverse. Internal exception at the boundary.   *)
(* ------------------------------------------------------------------ *)

exception Decode_error of string

let err fmt = Printf.ksprintf (fun s -> raise (Decode_error s)) fmt

let assoc_field key = function
  | `Assoc fields -> (
      match List.assoc_opt key fields with
      | Some v -> v
      | None -> err "missing field %S" key)
  | _ -> err "entry must be a JSON object"

let dec_string key json =
  match assoc_field key json with
  | `String s -> s
  | _ -> err "field %S must be a string" key

let dec_int key json =
  match assoc_field key json with
  | `Int n -> n
  | _ -> err "field %S must be an int" key

let dec_bool key json =
  match assoc_field key json with
  | `Bool b -> b
  | _ -> err "field %S must be a bool" key

let dec_verdict key json =
  match assoc_field key json with
  | `String "pass" -> Pass
  | `String "fail" -> Fail
  | _ -> err "field %S must be \"pass\" or \"fail\"" key

let dec_file_change_kind = function
  | `String "created" -> Created
  | `String "modified" -> Modified
  | `String "deleted" -> Deleted
  | _ -> err "file change kind must be created|modified|deleted"

let dec_file_change = function
  | `Assoc _ as j ->
      {
        path = dec_string "path" j;
        change = dec_file_change_kind (assoc_field "change" j);
        size = dec_int "size" j;
        digest = dec_string "digest" j;
      }
  | _ -> err "file entry must be a JSON object"

let dec_run_result json =
  let result_json = assoc_field "result" json in
  let files =
    match assoc_field "files" result_json with
    | `List l -> List.map dec_file_change l
    | _ -> err "field \"files\" must be a list"
  in
  {
    exit = dec_int "exit" result_json;
    stdout = dec_string "stdout" result_json;
    stderr = dec_string "stderr" result_json;
    truncated = dec_bool "truncated" result_json;
    files;
  }

let outcome_of_json json =
  match dec_string "kind" json with
  | "committed" ->
      Committed { id = dec_string "id" json;
                  token_digest = dec_string "token_digest" json }
  | "completed_no_commit" -> Completed_no_commit
  | "blocked" -> Blocked (dec_string "reason" json)
  | "aborted" -> Aborted (dec_string "reason" json)
  | other -> err "unknown outcome kind %S" other

let rec entry_of_json (json : Yojson.Safe.t) : trace_entry =
  match dec_string "kind" json with
  | "agent_ran" ->
      Agent_ran
        {
          id = dec_string "id" json;
          success = dec_bool "success" json;
          output = assoc_field "output" json;
        }
  | "gate_evaluated" ->
      Gate_evaluated
        { id = dec_string "id" json; verdict = dec_verdict "verdict" json }
  | "branch_taken" -> Branch_taken { verdict = dec_verdict "verdict" json }
  | "loop_iter" -> Loop_iter { index = dec_int "index" json }
  | "budget_read" -> Budget_read { value = dec_int "value" json }
  | "fixpoint_progress" ->
      Fixpoint_progress { progress = dec_bool "progress" json }
  | "loop_stopped" ->
      Loop_stopped
        {
          iterations = dec_int "iterations" json;
          reason = dec_string "reason" json;
        }
  | "run_executed" ->
      Run_executed { id = dec_string "id" json; result = dec_run_result json }
  | "committed_step" ->
      Committed_step
        {
          id = dec_string "id" json;
          token_digest = dec_string "token_digest" json;
        }
  | "blocked_at" ->
      Blocked_at { id = dec_string "id" json; reason = dec_string "reason" json }
  | "parallel_started" -> Parallel_started
  | "parallel_branch_completed" ->
      let branch_idx = dec_int "branch_idx" json in
      let trace =
        match assoc_field "trace" json with
        | `List l -> List.map entry_of_json l
        | _ -> err "field \"trace\" must be a list"
      in
      let outcome = outcome_of_json (assoc_field "outcome" json) in
      let branch_outputs =
        match assoc_field "branch_outputs" json with
        | `Assoc fields -> fields
        | _ -> err "field \"branch_outputs\" must be an object"
      in
      Parallel_branch_completed { branch_idx; trace; outcome; branch_outputs }
  | "parallel_completed" ->
      let outcome = outcome_of_json (assoc_field "outcome" json) in
      Parallel_completed { outcome }
  | "foreach_iter_started" ->
      Foreach_iter_started
        { index = dec_int "index" json; element = assoc_field "element" json }
  | "foreach_iter_completed" ->
      let outcome = outcome_of_json (assoc_field "outcome" json) in
      Foreach_iter_completed { index = dec_int "index" json; outcome }
  | "foreach_completed" ->
      Foreach_completed { iterations = dec_int "iterations" json }
  | "shell_executed" ->
      let id = dec_string "id" json in
      let results =
        match assoc_field "results" json with
        | `List l ->
            List.map (fun item ->
              (dec_string "command" item, dec_int "exit_code" item)) l
        | _ -> err "field \"results\" must be a list"
      in
      Shell_executed { id; results }
  | "evidence_evaluated" ->
      Evidence_evaluated
        { id = dec_string "id" json;
          tier = dec_string "tier" json;
          passed = dec_bool "passed" json }
  | "ctx_snapshot" ->
      let ctx = match assoc_field "ctx" json with
        | `Assoc fields -> fields
        | _ -> err "field \"ctx\" must be an object"
      in
      Ctx_snapshot { ctx }
  | other -> err "unknown entry kind %S" other

(* Split on '\n'; ignore blank lines (so a trailing newline is fine). Any line
   that is not parseable JSON, or whose object does not decode to a valid entry,
   fails the WHOLE parse (fail-closed). *)
let of_ndjson (s : string) : (trace, string) result =
  try
    let lines = String.split_on_char '\n' s in
    let entries =
      List.filter_map
        (fun line ->
          if String.trim line = "" then None
          else
            match Yojson.Safe.from_string line with
            | json -> Some (entry_of_json json)
            | exception (Yojson.Json_error msg) ->
                err "invalid JSON line: %s" msg)
        lines
    in
    Ok entries
  with
  | Decode_error msg -> Error msg
  | Yojson.Json_error msg -> Error (Printf.sprintf "invalid JSON: %s" msg)
