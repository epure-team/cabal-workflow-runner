open Types

type severity =
  | Error
  | Warning

type diagnostic = {
  severity : severity;
  code : string;
  message : string;
  loc : string;
}

let severity_to_string = function Error -> "error" | Warning -> "warning"

let diagnostic_to_json d : Yojson.Safe.t =
  `Assoc
    [
      ("severity", `String (severity_to_string d.severity));
      ("code", `String d.code);
      ("message", `String d.message);
      ("loc", `String d.loc);
    ]

let to_json (ds : diagnostic list) : Yojson.Safe.t =
  `Assoc [ ("diagnostics", `List (List.map diagnostic_to_json ds)) ]

let has_errors ds = List.exists (fun d -> d.severity = Error) ds

(* ---- expression reference extraction ----------------------------------- *)

(* Collect every dotted path referenced by an expression (from [Path] and
   [Exists] nodes), so we can statically check output references. *)
let rec expr_paths (e : Expr.t) : string list list =
  match e with
  | Expr.Path p -> [ p ]
  | Expr.Exists p -> [ p ]
  | Expr.Lit _ -> []
  | Expr.Not e -> expr_paths e
  | Expr.And es | Expr.Or es -> List.concat_map expr_paths es
  | Expr.Eq (a, b)
  | Expr.Ne (a, b)
  | Expr.Lt (a, b)
  | Expr.Le (a, b)
  | Expr.Gt (a, b)
  | Expr.Ge (a, b)
  | Expr.In (a, b) ->
      expr_paths a @ expr_paths b

(* ---- floor / structural analysis (Error diagnostics) -------------------- *)

module String_set = Set.Make (String)

(* We thread:
   - [guaranteed]: gates guaranteed-evaluated on every path to here (for the
     floor check);
   - a mutable accumulator [acc] of diagnostics.
   The walk mirrors [Validate]'s former logic but COLLECTS all violations
   rather than stopping at the first. It returns the (possibly augmented)
   guaranteed set after the sequence. *)

let loc_index prefix i = Printf.sprintf "%s[%d]" prefix i

(* ---- reachability helpers (used by floor analysis and top-level check) ---- *)

let rec any_commit steps =
  List.exists
    (function
      | Commit _ -> true
      | Branch { then_; else_; _ } -> any_commit then_ || any_commit else_
      | Loop { body; _ } -> any_commit body
      | Parallel { branches } -> List.exists any_commit branches
      | Foreach { steps = body; _ } -> any_commit body
      | Agent _ | Gate _ | Run _ | Shell _ | Evidence _ -> false)
    steps

(* Check if a single step (at any depth) contains a Commit. Used for
   commit-in-parallel detection. *)
and any_commit_step step = any_commit [ step ]

(* Collect all agent ids reachable inside a step list (recursively). Used for
   parallel-output-collision detection. *)
and collect_agent_ids_step step : string list =
  match step with
  | Agent { id; _ } -> [ id ]
  | Branch { then_; else_; _ } ->
      collect_agent_ids then_ @ collect_agent_ids else_
  | Loop { body; _ } -> collect_agent_ids body
  | Parallel { branches } -> List.concat_map collect_agent_ids branches
  | Foreach { steps = body; _ } -> collect_agent_ids body
  | Gate _ | Run _ | Commit _ | Shell _ | Evidence _ -> []

and collect_agent_ids steps =
  List.concat_map collect_agent_ids_step steps

let rec floor_steps ~floor ~loc_prefix ~guaranteed acc steps =
  let _, g =
    List.fold_left
      (fun (i, guaranteed) step ->
        let loc = loc_index loc_prefix i in
        let guaranteed' = floor_step ~floor ~loc ~guaranteed acc step in
        (i + 1, guaranteed'))
      (0, guaranteed) steps
  in
  g

and floor_step ~floor ~loc ~guaranteed acc step =
  match step with
  | Agent _ -> guaranteed
  | Run _ -> guaranteed
  | Shell _ -> guaranteed
  | Evidence _ -> guaranteed
  | Gate { id; when_ = _ } -> String_set.add id guaranteed
  | Commit { id } ->
      let missing =
        List.filter (fun g -> not (String_set.mem g guaranteed)) floor
      in
      if missing <> [] then
        acc :=
          {
            severity = Error;
            code = "commit-missing-floor-gate";
            message =
              Printf.sprintf
                "Commit %S is reachable without floor gate(s) %s guaranteed on \
                 every preceding path"
                id
                (String.concat ", " (List.map (Printf.sprintf "%S") missing));
            loc;
          }
          :: !acc;
      guaranteed
  | Branch { when_ = _; then_; else_ } ->
      let g_then =
        floor_steps ~floor ~loc_prefix:(loc ^ ".then") ~guaranteed acc then_
      in
      let g_else =
        floor_steps ~floor ~loc_prefix:(loc ^ ".else") ~guaranteed acc else_
      in
      (* only gates guaranteed in BOTH branches remain guaranteed. *)
      String_set.inter g_then g_else
  | Loop { governors; until = _; body } ->
      (if governors = [] then
         acc :=
           {
             severity = Error;
             code = "ungoverned-loop";
             message = "loop is ungoverned";
             loc = loc ^ ".governors";
           }
           :: !acc
       else
         List.iteri
           (fun gi gov ->
             match gov with
             | Max_iters n when n < 1 ->
                 acc :=
                   {
                     severity = Error;
                     code = "unbounded-max-iters";
                     message =
                       Printf.sprintf
                         "Max_iters governor must be >= 1 (got %d)" n;
                     loc = Printf.sprintf "%s.governors[%d]" loc gi;
                   }
                   :: !acc
             | Fixpoint { window; _ } when window < 1 ->
                 acc :=
                   {
                     severity = Error;
                     code = "bad-fixpoint-window";
                     message =
                       Printf.sprintf
                         "Fixpoint governor window must be >= 1 (got %d)" window;
                     loc = Printf.sprintf "%s.governors[%d]" loc gi;
                   }
                   :: !acc
             | _ -> ())
           governors);
      (* check the body for its own violations, but do NOT propagate body gates
         as guaranteed: a loop may execute zero iterations. *)
      ignore
        (floor_steps ~floor ~loc_prefix:(loc ^ ".body") ~guaranteed acc body);
      guaranteed
  | Parallel { branches } ->
      (* commit-in-parallel: any Commit reachable inside any branch is an Error.
         We collect which commits are already flagged to suppress soft-fail-with-commit
         for those same commits. *)
      List.iteri
        (fun m branch ->
          List.iteri
            (fun k step ->
              let branch_loc = Printf.sprintf "%s.branches[%d][%d]" loc m k in
              ignore (floor_step ~floor ~loc:branch_loc ~guaranteed acc step);
              if any_commit_step step then
                acc :=
                  {
                    severity = Error;
                    code = "commit-in-parallel";
                    message =
                      Printf.sprintf
                        "Commit is reachable inside a parallel branch at %s; \
                         commits are not permitted inside parallel branches"
                        branch_loc;
                    loc = branch_loc;
                  }
                  :: !acc)
            branch)
        branches;
      (* parallel-output-collision: collect agent ids from each branch, find duplicates *)
      let branch_ids = List.map (fun branch -> collect_agent_ids branch) branches in
      let all_ids = List.concat branch_ids in
      let seen = Hashtbl.create 16 in
      List.iteri
        (fun m ids ->
          List.iter
            (fun id ->
              match Hashtbl.find_opt seen id with
              | Some prev_m when prev_m <> m ->
                  acc :=
                    {
                      severity = Error;
                      code = "parallel-output-collision";
                      message =
                        Printf.sprintf
                          "agent id %S appears in multiple parallel branches \
                           (branches %d and %d); outputs would collide"
                          id prev_m m;
                      loc;
                    }
                    :: !acc
              | None -> Hashtbl.add seen id m
              | Some _ -> ())
            ids)
        branch_ids;
      ignore all_ids;
      (* Floor-gate analysis: intersection of guaranteed sets across all branches.
         A gate is guaranteed after the parallel step only if it is guaranteed
         in EVERY branch. *)
      let branch_guaranteed_sets =
        List.mapi
          (fun m branch ->
            floor_steps ~floor ~loc_prefix:(Printf.sprintf "%s.branches[%d]" loc m)
              ~guaranteed acc branch)
          branches
      in
      (match branch_guaranteed_sets with
      | [] -> guaranteed
      | first :: rest ->
          List.fold_left String_set.inter first rest)
  | Foreach { over = _; steps = body } ->
      (* Foreach follows loop semantics: body gates are NOT added to the
         guaranteed set (the array may be empty, body may never execute). *)
      ignore
        (floor_steps ~floor ~loc_prefix:(loc ^ ".steps") ~guaranteed acc body);
      guaranteed

(* ---- output-reference analysis (Warning diagnostics) -------------------- *)

(* Walk steps in order tracking, on the current path, which [outputs.<id>] have
   been produced and (if declared) the set of schema field names for each. For
   every expression in a gate/branch/loop we flag [outputs.<id>.<field>...]
   references that cannot be produced on that path:
   - [dangling-output-ref]: no prior agent step produced [<id>], OR the step's
     declared [output_schema] (if any) has no such [<field>].
   - [missing-output-schema]: a referenced [<id>] exists but declares no schema
     (so its output can't be validated). One per offending (referenced) id.

   [produced]: id -> field-name list option (None = no declared schema). *)

type produced = (string * string list option) list

let schema_fields (s : Schema.t) : string list = List.map fst s

(* Check one expression's references against [produced]; push warnings into
   [acc]. [warned_missing] collects ids we've already flagged with
   missing-output-schema (one per id). *)
let check_expr_refs ~loc ~(produced : produced) ~warned_missing acc (e : Expr.t)
    =
  List.iter
    (fun path ->
      match path with
      | "outputs" :: id :: rest -> (
          match List.assoc_opt id produced with
          | None ->
              acc :=
                {
                  severity = Warning;
                  code = "dangling-output-ref";
                  message =
                    Printf.sprintf
                      "reference to outputs.%s.* but no prior agent step %S \
                       produces it on this path"
                      id id;
                  loc;
                }
                :: !acc
          | Some None ->
              (* id exists but declares no output_schema; its output can't be
                 validated. Flag once per id. *)
              if not (List.mem id !warned_missing) then begin
                warned_missing := id :: !warned_missing;
                acc :=
                  {
                    severity = Warning;
                    code = "missing-output-schema";
                    message =
                      Printf.sprintf
                        "agent step %S is referenced via outputs.%s but \
                         declares no output_schema (its output can't be \
                         validated)"
                        id id;
                    loc;
                  }
                  :: !acc
              end
          | Some (Some fields) -> (
              match rest with
              | field :: _ when not (List.mem field fields) ->
                  acc :=
                    {
                      severity = Warning;
                      code = "dangling-output-ref";
                      message =
                        Printf.sprintf
                          "reference to outputs.%s.%s but step %S's \
                           output_schema declares no such field"
                          id field id;
                      loc;
                    }
                    :: !acc
              | _ -> ()))
      | _ -> ())
    (expr_paths e)

let rec refs_steps ~loc_prefix ~(produced : produced) ~warned_missing acc steps
    : produced =
  let _, p =
    List.fold_left
      (fun (i, produced) step ->
        let loc = loc_index loc_prefix i in
        let produced' =
          refs_step ~loc ~produced ~warned_missing acc step
        in
        (i + 1, produced'))
      (0, produced) steps
  in
  p

and refs_step ~loc ~produced ~warned_missing acc step : produced =
  match step with
  | Shell _ -> produced
  | Evidence _ -> produced
  | Agent { id; output_schema; _ } ->
      let fields = Option.map schema_fields output_schema in
      (id, fields) :: List.remove_assoc id produced
  | Run { id; _ } ->
      (* A Run step binds outputs.<id> with a FIXED shape (see
         [Types.json_of_run_result]); register those field names so references
         like outputs.<id>.exit are not flagged dangling. *)
      let fields = Some [ "exit"; "stdout"; "stderr"; "truncated"; "files" ] in
      (id, fields) :: List.remove_assoc id produced
  | Gate { when_; _ } ->
      check_expr_refs ~loc ~produced ~warned_missing acc when_;
      produced
  | Branch { when_; then_; else_ } ->
      check_expr_refs ~loc ~produced ~warned_missing acc when_;
      (* Each branch sees [produced] so far. At runtime exactly ONE arm is taken,
         so after the branch an output is GUARANTEED available only if BOTH arms
         produce it — the produced set is the INTERSECTION of the two arms'
         produced sets (mirroring the floor's branch=intersection discipline).
         A reference after the branch to an output produced in only one arm is
         therefore correctly flagged as dangling. Within an arm, that arm's own
         outputs remain available (intra-arm behaviour is unchanged). Field-level
         merge: an [id] survives only if present in both arms; its fields are the
         intersection when both declare [Some], else [None] if either is [None]. *)
      let p_then =
        refs_steps ~loc_prefix:(loc ^ ".then") ~produced ~warned_missing acc
          then_
      in
      let p_else =
        refs_steps ~loc_prefix:(loc ^ ".else") ~produced ~warned_missing acc
          else_
      in
      let merge_fields a b =
        match (a, b) with
        | Some fa, Some fb -> Some (List.filter (fun f -> List.mem f fb) fa)
        | _ -> None
      in
      List.filter_map
        (fun (id, fields_then) ->
          match List.assoc_opt id p_else with
          | Some fields_else -> Some (id, merge_fields fields_then fields_else)
          | None -> None)
        p_then
  | Loop { body; until; governors } ->
      (* The body runs BEFORE [until] / [fixpoint] are evaluated each iteration,
         and the engine keeps body outputs bound in the run context afterward.
         So we first compute the body's produced set, then check the loop's stop
         expressions against [produced + body], and propagate the body outputs
         forward to later steps. *)
      let p_body =
        refs_steps ~loc_prefix:(loc ^ ".body") ~produced ~warned_missing acc
          body
      in
      (match until with
      | Some e -> check_expr_refs ~loc ~produced:p_body ~warned_missing acc e
      | None -> ());
      List.iteri
        (fun gi gov ->
          match gov with
          | Fixpoint { progress; _ } ->
              check_expr_refs
                ~loc:(Printf.sprintf "%s.governors[%d]" loc gi)
                ~produced:p_body ~warned_missing acc progress
          | _ -> ())
        governors;
      p_body
  | Commit _ -> produced
  | Parallel { branches } ->
      (* Each branch sees [produced] so far. After parallel, an output is
         available only if ALL branches produce it — intersection semantics,
         mirroring Branch. Within a branch, that branch's outputs are available. *)
      let branch_produced_sets =
        List.mapi
          (fun m branch ->
            refs_steps
              ~loc_prefix:(Printf.sprintf "%s.branches[%d]" loc m)
              ~produced ~warned_missing acc branch)
          branches
      in
      (match branch_produced_sets with
      | [] -> produced
      | first :: rest ->
          let merge_fields a b =
            match (a, b) with
            | Some fa, Some fb -> Some (List.filter (fun f -> List.mem f fb) fa)
            | _ -> None
          in
          List.fold_left
            (fun acc_p p ->
              List.filter_map
                (fun (id, fields_a) ->
                  match List.assoc_opt id p with
                  | Some fields_b -> Some (id, merge_fields fields_a fields_b)
                  | None -> None)
                acc_p)
            first rest)
  | Foreach { over = _; steps = body } ->
      (* The body runs once per element; outputs produced inside the body are
         accessible after foreach (unlike loop, foreach always has at least 0
         iterations but the outputs accumulate). We follow loop semantics here
         and propagate body outputs forward, since typical use is: foreach body
         produces something, then steps after foreach read it. *)
      refs_steps ~loc_prefix:(loc ^ ".steps") ~produced ~warned_missing acc body

(* ---- unreachable-after-commit + no-commit (Warnings) -------------------- *)

(* Any agent step (anywhere in the tree) declaring [on_failure = Continue]. A
   soft-failing agent is incompatible with a Commit: the commit-floor invariant
   tracks only gate IDs, not whether a floor gate's PREDICATE consumes the failed
   agent's output, so a trivially-true floor gate would let a commit fire despite
   the soft-failed agent. We therefore forbid the combination (Error below),
   keeping soft-fail to commit-free continuous loops. *)
let rec any_continue_agent steps =
  List.exists
    (function
      | Agent { on_failure = Types.Continue; _ } -> true
      | Branch { then_; else_; _ } ->
          any_continue_agent then_ || any_continue_agent else_
      | Loop { body; _ } -> any_continue_agent body
      | Parallel { branches } -> List.exists any_continue_agent branches
      | Foreach { steps = body; _ } -> any_continue_agent body
      | Agent _ | Gate _ | Run _ | Commit _ | Shell _ | Evidence _ -> false)
    steps

(* Flag steps that follow a Commit at the same level (a commit ends the run). *)
let rec unreachable_steps ~loc_prefix acc steps =
  let _, _seen_commit =
    List.fold_left
      (fun (i, seen) step ->
        let loc = loc_index loc_prefix i in
        if seen then begin
          acc :=
            {
              severity = Warning;
              code = "unreachable-after-commit";
              message =
                "step follows a Commit at the same level and is unreachable (a \
                 commit ends the run)";
              loc;
            }
            :: !acc;
          (i + 1, seen)
        end
        else begin
          (* recurse into branch/loop/parallel/foreach bodies for their own intra-level commits *)
          (match step with
          | Branch { then_; else_; _ } ->
              unreachable_steps ~loc_prefix:(loc ^ ".then") acc then_;
              unreachable_steps ~loc_prefix:(loc ^ ".else") acc else_
          | Loop { body; _ } -> unreachable_steps ~loc_prefix:(loc ^ ".body") acc body
          | Parallel { branches } ->
              List.iteri
                (fun m branch ->
                  unreachable_steps
                    ~loc_prefix:(Printf.sprintf "%s.branches[%d]" loc m) acc branch)
                branches
          | Foreach { steps = body; _ } ->
              unreachable_steps ~loc_prefix:(loc ^ ".steps") acc body
          | _ -> ());
          let seen' = match step with Commit _ -> true | _ -> false in
          (i + 1, seen')
        end)
      (0, false) steps
  in
  ()

(* ---- run-step diagnostics (Warnings) ------------------------------------ *)

(* Known destructive binaries. Matched by [Filename.basename] of the command
   head, or — for the shell fork-bomb idiom — as a prefix of the head string.
   This is BEST-EFFORT advisory only (a louder warning so a generator/operator
   notices), NOT a security control: the allowlist (operator opt-in at runtime)
   is the trust boundary. The list is deliberately small and documented. *)
let destructive_bins = [ "rm"; "rmdir"; "mv"; "dd"; "mkfs"; "shred"; "truncate" ]

let is_destructive cmd =
  match cmd with
  | [] -> false
  | head :: _ ->
      let base = Filename.basename head in
      List.mem base destructive_bins
      (* the classic ":(){ :|:& };:" fork bomb starts with ":(){" *)
      || (String.length head >= 4 && String.sub head 0 4 = ":(){")

(* Emit, for every Run step: an informational warning that it executes commands
   ([run-step-executes-commands]), and additionally a louder warning when the
   command head is a known-destructive binary ([run-step-destructive-command]).
   Both are WARNINGS — they never set [has_errors]. *)
let rec run_steps ~loc_prefix acc steps =
  List.iteri
    (fun i step ->
      let loc = loc_index loc_prefix i in
      match step with
      | Run { id; cmd; _ } ->
          acc :=
            {
              severity = Warning;
              code = "run-step-executes-commands";
              message =
                Printf.sprintf
                  "run step %S executes a shell command (%s); it runs only if \
                   the operator's runtime allowlist permits its binary"
                  id
                  (String.concat " " cmd);
              loc;
            }
            :: !acc;
          if is_destructive cmd then
            acc :=
              {
                severity = Warning;
                code = "run-step-destructive-command";
                message =
                  Printf.sprintf
                    "run step %S invokes a potentially DESTRUCTIVE command \
                     (%s); best-effort advisory, not a security control — the \
                     runtime allowlist is the trust boundary"
                    id (List.nth cmd 0);
                loc;
              }
              :: !acc
      | Branch { then_; else_; _ } ->
          run_steps ~loc_prefix:(loc ^ ".then") acc then_;
          run_steps ~loc_prefix:(loc ^ ".else") acc else_
      | Loop { body; _ } -> run_steps ~loc_prefix:(loc ^ ".body") acc body
      | Parallel { branches } ->
          List.iteri
            (fun m branch ->
              run_steps
                ~loc_prefix:(Printf.sprintf "%s.branches[%d]" loc m) acc branch)
            branches
      | Foreach { steps = body; _ } ->
          run_steps ~loc_prefix:(loc ^ ".steps") acc body
      | _ -> ())
    steps

(* ---- top-level check ---------------------------------------------------- *)

let check ?(floor_gates = []) (wf : Types.workflow) : diagnostic list =
  let acc = ref [] in
  (* Errors: floor + structural. *)
  ignore
    (floor_steps ~floor:floor_gates ~loc_prefix:"steps"
       ~guaranteed:String_set.empty acc wf.steps);
  (* Warnings: output references. *)
  let warned_missing = ref [] in
  ignore
    (refs_steps ~loc_prefix:"steps" ~produced:[] ~warned_missing acc wf.steps);
  (* Warnings: unreachable-after-commit. *)
  unreachable_steps ~loc_prefix:"steps" acc wf.steps;
  (* Warnings: run steps execute commands (+ destructive-command notice). *)
  run_steps ~loc_prefix:"steps" acc wf.steps;
  (* Error: a soft-failing agent (on_failure=continue) in a workflow that can
     Commit. The commit-floor invariant guarantees a Commit is gate-ID-preceded
     but NOT that those gates' predicates consume the soft-failed agent's output —
     so a trivially-true floor gate would let a commit fire despite the failure.
     Forbid the combination: on_failure=continue is for COMMIT-FREE continuous
     loops only. (This is what makes "continue does not weaken the commit floor"
     a true, enforced invariant rather than an assumption about gate authoring.) *)
  if any_commit wf.steps && any_continue_agent wf.steps then
    acc :=
      {
        severity = Error;
        code = "soft-fail-with-commit";
        message =
          "an agent step has on_failure=\"continue\" in a workflow that contains \
           a Commit; soft-fail is permitted only in commit-free workflows (a \
           soft-failed agent could otherwise reach a commit past a trivially-true \
           floor gate)";
        loc = "steps";
      }
      :: !acc;
  (* Warning: no commit at all. *)
  if not (any_commit wf.steps) then
    acc :=
      {
        severity = Warning;
        code = "no-commit";
        message = "workflow contains no Commit step (maybe intended)";
        loc = "steps";
      }
      :: !acc;
  (* Errors first, then warnings; within each, document/source order. *)
  let ds = List.rev !acc in
  let errors = List.filter (fun d -> d.severity = Error) ds in
  let warnings = List.filter (fun d -> d.severity = Warning) ds in
  errors @ warnings

let check_json ?(floor_gates = []) (raw : string) : diagnostic list =
  match Yojson.Safe.from_string raw with
  | exception Yojson.Json_error msg ->
      [
        {
          severity = Error;
          code = "invalid-json";
          message = "input is not valid JSON: " ^ msg;
          loc = "$";
        };
      ]
  | exception _ ->
      [
        {
          severity = Error;
          code = "invalid-json";
          message = "input is not valid JSON";
          loc = "$";
        };
      ]
  | json -> (
      match Workflow_json.of_json json with
      | Error msg ->
          [
            {
              severity = Error;
              code = "invalid-shape";
              message = "JSON is not a valid workflow: " ^ msg;
              loc = "$";
            };
          ]
      | Ok wf -> check ~floor_gates wf)
