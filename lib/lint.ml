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
  | Agent { id; output_schema; _ } ->
      let fields = Option.map schema_fields output_schema in
      (id, fields) :: List.remove_assoc id produced
  | Gate { when_; _ } ->
      check_expr_refs ~loc ~produced ~warned_missing acc when_;
      produced
  | Branch { when_; then_; else_ } ->
      check_expr_refs ~loc ~produced ~warned_missing acc when_;
      (* Each branch sees [produced] so far. The engine never removes bindings
         from the run context, so outputs produced in EITHER taken branch are
         visible to later steps; we union both branches' produced sets (a
         reference is dangling only if neither branch could produce it). *)
      let p_then =
        refs_steps ~loc_prefix:(loc ^ ".then") ~produced ~warned_missing acc
          then_
      in
      let p_else =
        refs_steps ~loc_prefix:(loc ^ ".else") ~produced ~warned_missing acc
          else_
      in
      List.fold_left
        (fun acc_p (id, fields) ->
          if List.mem_assoc id acc_p then acc_p else (id, fields) :: acc_p)
        p_then p_else
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

(* ---- unreachable-after-commit + no-commit (Warnings) -------------------- *)

let rec any_commit steps =
  List.exists
    (function
      | Commit _ -> true
      | Branch { then_; else_; _ } -> any_commit then_ || any_commit else_
      | Loop { body; _ } -> any_commit body
      | _ -> false)
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
          (* recurse into branch/loop bodies for their own intra-level commits *)
          (match step with
          | Branch { then_; else_; _ } ->
              unreachable_steps ~loc_prefix:(loc ^ ".then") acc then_;
              unreachable_steps ~loc_prefix:(loc ^ ".else") acc else_
          | Loop { body; _ } -> unreachable_steps ~loc_prefix:(loc ^ ".body") acc body
          | _ -> ());
          let seen' = match step with Commit _ -> true | _ -> false in
          (i + 1, seen')
        end)
      (0, false) steps
  in
  ()

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
