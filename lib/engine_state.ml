open Types

let token_digest tok =
  let domain = "cwr.approval-token/v2\000" in
  "sha256:" ^ Digestif.SHA256.(to_hex (digest_string (domain ^ tok)))

let token_is_wellformed = function
  | None -> false
  | Some t -> String.length (String.trim t) > 0

(* Execution state threaded through the walk. [rev_trace] accumulates in REVERSE
   order (most recent first) and is reversed at the end. [ctx] binds step ids to
   their recorded structured output (addressable as ["outputs.<id>..."]); the
   loop additionally binds ["loop"] to {"iter": <index>}. [terminal] is set when
   a Commit / Block / Abort ends the run. *)
type state = {
  rev_trace : trace_entry list;
  ctx : (string * Yojson.Safe.t) list;
  attest_counts : (string * int) list;
  terminal : outcome option;
}

let emit st entry = { st with rev_trace = entry :: st.rev_trace }

(* Bind/overwrite a key in ctx (most recent write wins; assoc lookup finds it). *)
let bind st key json =
  { st with ctx = (key, json) :: List.remove_assoc key st.ctx }

(* The expression context: agent outputs are nested under "outputs". We expose
   that to the DSL by keeping ctx keyed by "outputs" and "loop". The actual
   per-step output is merged into the single "outputs" object. *)
let ctx_for st = st.ctx

let finish st =
  let trace = List.rev st.rev_trace in
  let outcome =
    match st.terminal with Some o -> o | None -> Completed_no_commit
  in
  (outcome, trace)

(* Merge an agent's output object under outputs.<id>, preserving prior outputs. *)
let bind_output st id output =
  let prior =
    match List.assoc_opt "outputs" st.ctx with
    | Some (`Assoc fields) -> fields
    | _ -> []
  in
  let merged = `Assoc ((id, output) :: List.remove_assoc id prior) in
  bind st "outputs" merged

let bind_receipts st id request result =
  let prior =
    match List.assoc_opt "receipts" st.ctx with
    | Some (`Assoc fields) -> fields
    | _ -> []
  in
  let receipt = `Assoc [ ("request", request); ("result", result) ] in
  bind st "receipts"
    (`Assoc ((id, receipt) :: List.remove_assoc id prior))

let sha256 canonical =
  "sha256:" ^ Digestif.SHA256.(to_hex (digest_string canonical))

let canonical_digest json =
  Result.map (fun canonical -> (canonical, sha256 canonical))
    (Canonical_json.to_string json)

let bind_loop_iter st index = bind st "loop" (`Assoc [ ("iter", `Int index) ])

let run_input st paths =
  Result.bind (Attestation.select_context st.ctx paths) (fun selected ->
      Result.map
        (fun canonical ->
          let digest =
            "sha256:"
            ^ Digestif.SHA256.(to_hex (digest_string canonical))
          in
          (canonical, digest))
        (Canonical_json.to_string (`Assoc selected)))

let commit_lock_json path (identity : Secure_fs.lock_identity) =
  `Assoc [ ("path", `String path); ("held", `Bool true);
           ("device", `String identity.device);
           ("inode", `String identity.inode) ]

let reserved_commit_lock_path path =
  path = "cwr.commit_lock"
  || (String.length path > String.length "cwr.commit_lock"
      && String.sub path 0 (String.length "cwr.commit_lock" + 1)
         = "cwr.commit_lock.")

let commit_preflight_input st paths lock_identity =
  if List.exists reserved_commit_lock_path paths then
    Error "reserved engine path cwr.commit_lock cannot be selected"
  else Result.bind (Attestation.select_context st.ctx paths) (fun selected ->
      let selected = match lock_identity with
        | None -> selected
        | Some marker -> ("cwr.commit_lock", marker) :: selected in
      Result.map (fun canonical -> (canonical, sha256 canonical))
        (Canonical_json.to_string (`Assoc selected)))

let executable_json (identity : executable_identity) =
  `Assoc [ ("path", `String identity.path);
           ("digest", `String identity.digest) ]

let run_output_json ~input_digest ~parsed ~executable result =
  match json_of_run_result result with
  | `Assoc fields ->
      `Assoc
        (fields
        @ (match input_digest with
          | None -> []
          | Some digest -> [ ("input_digest", `String digest) ])
        @ (match executable with None -> []
          | Some identity -> [ ("executable", executable_json identity) ])
        @ match parsed with None -> [] | Some json -> [ ("parsed", json) ])
  | json -> json

let parse_run_stdout schema (result : run_result) =
  if result.truncated then Error "stdout was truncated"
  else match Yojson.Safe.from_string result.stdout with
  | exception Yojson.Json_error msg -> Error ("stdout is not JSON: " ^ msg)
  | (`Assoc _ as parsed) ->
      Result.bind (Canonical_json.to_string parsed) (fun canonical ->
          let normalized = Yojson.Safe.from_string canonical in
          Result.map
            (fun () -> normalized)
            (Result.map_error
               (fun field -> "stdout schema mismatch: " ^ field)
               (Schema.validate schema normalized)))
  | _ -> Error "stdout must be a JSON object"

let commit_preflight_receipt ~id ~input_digest ~result ~parsed ~lock_identity =
  Result.bind (canonical_digest parsed) (fun (_, output_digest) ->
      let receipt =
        `Assoc
          ([ ("step_id", `String id);
            ("input_digest", `String input_digest);
            ("process_exit", `Int result.exit);
            ("output_digest", `String output_digest);
            ("parsed", parsed) ]
          @ match lock_identity with None -> []
            | Some marker -> [ ("commit_lock", marker) ])
      in
      Result.map (fun (_, digest) -> (receipt, digest))
        (canonical_digest receipt))

(* ------------------------------------------------------------------ *)
(* Constants and gates shared by [run]'s and [replay]'s Run/Commit arms. *)
(* ------------------------------------------------------------------ *)

(* Unconditional hard iteration ceiling for every loop. A loop ALWAYS stops once
   it has executed this many iterations, regardless of governors / until / budget
   / agent behaviour — it is the termination GUARANTEE (Budget/Fixpoint/until are
   early-stop heuristics under it). Default chosen generously; tests pass a small
   value. The ceiling is a constant, so replay reproduces byte-identically. *)
let default_max_loop_iters = 10_000

(* A [Run] step executes only if the basename of its command's head is in the
   operator-supplied allowlist, OR the allowlist contains ["*"] (allow all). The
   default allowlist is [[]], so with no operator opt-in NO run step ever
   executes (fail-closed). The allowlist is a RUNTIME parameter, never read from
   the workflow file: a workflow cannot grant itself the right to run a command. *)
let run_permitted ~run_allowlist cmd =
  match cmd with
  | [] -> false (* validator rejects this; defensive. *)
  | head :: _ ->
      List.mem "*" run_allowlist
      || List.mem (Filename.basename head) run_allowlist

(* [cmd.(0)] must be a BARE command name resolved via PATH. A head containing a
   path separator ('/') — i.e. an absolute path ["/abs/x"], an explicit relative
   path ["./x"], or any ["a/b"] — is rejected: it bypasses the allowlist's
   [Filename.basename] match while executing an arbitrary binary. The bin runner
   execs bare names via PATH. *)
let path_bearing_head = function
  | head :: _ -> String.contains head '/'
  | [] -> false

