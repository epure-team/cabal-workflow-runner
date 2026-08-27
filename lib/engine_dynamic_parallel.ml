open Types

(* ------------------------------------------------------------------ *)
(* Dynamic_parallel: resolving [over] and instantiating one branch per   *)
(* runtime array element.                                                *)
(* ------------------------------------------------------------------ *)

(* Hard engine ceiling, independent of ANY caller-side clamp elsewhere in the
   stack: a Dynamic_parallel step never dispatches more than this many
   concurrent branches, full stop. *)
let dynamic_parallel_max_branches = 32

(* Resolve [over] (a flat ctx key, exactly like Foreach's) to the list of
   unique, non-empty runtime branch keys, or a clear fail-closed reason.
   Fails BEFORE any branch is dispatched: malformed/non-array/non-string
   element/duplicate/over-ceiling all reject here, never partway through
   forking. *)
let resolve_dynamic_parallel_over ctx over =
  match List.assoc_opt over ctx with
  | None ->
      Error (Printf.sprintf "dynamic_parallel.over=%S not found in ctx" over)
  | Some (`List elements) ->
      let rec collect acc = function
        | [] -> Ok (List.rev acc)
        | `String s :: rest ->
            if String.trim s = "" then
              Error (Printf.sprintf
                "dynamic_parallel.over=%S contains an empty-string element" over)
            else if String.contains s '/' then
              Error (Printf.sprintf
                "dynamic_parallel.over=%S contains a key with an unsafe '/' \
                 character (%S)" over s)
            else collect (s :: acc) rest
        | _ :: _ ->
            Error (Printf.sprintf
              "dynamic_parallel.over=%S contains a non-string element" over)
      in
      Result.bind (collect [] elements) (fun keys ->
        let uniq = List.sort_uniq String.compare keys in
        if List.length uniq <> List.length keys then
          Error (Printf.sprintf
            "dynamic_parallel.over=%S contains duplicate elements" over)
        else if List.length keys > dynamic_parallel_max_branches then
          Error (Printf.sprintf
            "dynamic_parallel.over=%S resolves to %d branches, exceeding the \
             %d-branch engine ceiling"
            over (List.length keys) dynamic_parallel_max_branches)
        else Ok keys)
  | Some other ->
      Error (Printf.sprintf
        "dynamic_parallel.over=%S is not a JSON array (got %s)" over
        (Yojson.Safe.to_string other))

(* Every id a Dynamic_parallel template declares as its OWN producer
   (Agent/Run/Commit/Shell/Evidence/Attest/Dynamic_parallel/Gate/Spawn ids),
   collected ONCE per template. Used to tell "this selector/expr path refers
   to a sibling INSIDE this template" (must be rewritten per-branch) from
   "this selector refers to ctx bound before Dynamic_parallel ran" (must NOT
   be rewritten). *)
let rec dynamic_parallel_template_ids steps =
  List.concat_map
    (function
      | Agent { id; _ } | Run { id; _ } | Commit { id; _ }
      | Shell { id; _ } | Evidence { id; _ } | Attest { id; _ }
      | Dynamic_parallel { id; _ } | Gate { id; _ } -> [ id ]
      | Branch { then_; else_; _ } ->
          dynamic_parallel_template_ids then_
          @ dynamic_parallel_template_ids else_
      | Loop { body; _ } -> dynamic_parallel_template_ids body
      | Parallel { branches } ->
          List.concat_map dynamic_parallel_template_ids branches
      | Foreach { steps; _ } -> dynamic_parallel_template_ids steps
      | Spawn { id; children } ->
          id
          :: List.concat_map
               (fun (c : spawn_child) ->
                 c.id :: dynamic_parallel_template_ids c.steps)
               children)
    steps

(* Rewrite a dotted "outputs.<id>..." / "receipts.<id>..." selector so it
   points at the branch-local (suffixed) id, iff [pid] is one of THIS
   template's own producer ids. Any other selector (referring to ctx bound
   before Dynamic_parallel ran) is left untouched. *)
let dynamic_parallel_rewrite_selector ~template_ids ~suffix path =
  match String.split_on_char '.' path with
  | (("outputs" | "receipts") as ns) :: pid :: rest when List.mem pid template_ids ->
      String.concat "." (ns :: (pid ^ suffix) :: rest)
  | _ -> path

let rec dynamic_parallel_rewrite_expr ~template_ids ~suffix (e : Expr.t) : Expr.t =
  let rw = dynamic_parallel_rewrite_expr ~template_ids ~suffix in
  let rw_path segs =
    match segs with
    | (("outputs" | "receipts") as ns) :: pid :: rest when List.mem pid template_ids ->
        ns :: (pid ^ suffix) :: rest
    | segs -> segs
  in
  match e with
  | Expr.Path p -> Expr.Path (rw_path p)
  | Expr.Exists p -> Expr.Exists (rw_path p)
  | Expr.Lit _ -> e
  | Expr.Not e -> Expr.Not (rw e)
  | Expr.And es -> Expr.And (List.map rw es)
  | Expr.Or es -> Expr.Or (List.map rw es)
  | Expr.Eq (a, b) -> Expr.Eq (rw a, rw b)
  | Expr.Ne (a, b) -> Expr.Ne (rw a, rw b)
  | Expr.Lt (a, b) -> Expr.Lt (rw a, rw b)
  | Expr.Le (a, b) -> Expr.Le (rw a, rw b)
  | Expr.Gt (a, b) -> Expr.Gt (rw a, rw b)
  | Expr.Ge (a, b) -> Expr.Ge (rw a, rw b)
  | Expr.In (a, b) -> Expr.In (rw a, rw b)

(* Instantiate one Dynamic_parallel branch's copy of the template: every step
   id gets suffixed with "#<key>" (so ctx/ledger keys can never collide across
   siblings — this is what makes a branch's Attest step_id genuinely
   branch-unique, with ZERO changes to attestation.ml), every in-template
   selector/expr path referencing a sibling producer id is rewritten to match,
   and every Attest output path is namespaced by branch index (so two
   siblings materializing the SAME output template — e.g. both using only
   "{occurrence}" — can never race on the same file). *)
(* Namespace an Attest output path by branch index, WITHOUT introducing any
   new directory component: [write_atomic]/[Secure_fs] never creates missing
   parent directories, so a directory-prefix scheme would fail closed the
   moment a template's own declared directory didn't already exist. Renaming
   the file's stem instead ("proof.json" -> "proof#0.json") stays inside the
   template's own (already-required-to-exist) directory. *)
let dynamic_parallel_namespace_output ~branch_idx output =
  let dir = Filename.dirname output in
  let base = Filename.basename output in
  let ext = Filename.extension base in
  let stem = Filename.remove_extension base in
  let namespaced = Printf.sprintf "%s#%d%s" stem branch_idx ext in
  if dir = "." then namespaced else Filename.concat dir namespaced

let rec dynamic_parallel_rewrite_step ~template_ids ~suffix ~branch_idx
    (step : step) : step =
  let rw = dynamic_parallel_rewrite_step ~template_ids ~suffix ~branch_idx in
  let rws = List.map rw in
  let sfx id = id ^ suffix in
  let rwsel = dynamic_parallel_rewrite_selector ~template_ids ~suffix in
  let rwselopt = Option.map (List.map rwsel) in
  let rwexpr = dynamic_parallel_rewrite_expr ~template_ids ~suffix in
  match step with
  | Agent a -> Agent { a with id = sfx a.id; input = rwselopt a.input }
  | Gate g -> Gate { id = sfx g.id; when_ = rwexpr g.when_ }
  | Branch b -> Branch { when_ = rwexpr b.when_; then_ = rws b.then_; else_ = rws b.else_ }
  | Loop l ->
      Loop
        {
          body = rws l.body;
          until = Option.map rwexpr l.until;
          governors =
            List.map
              (function
                | Fixpoint { window; progress } ->
                    Fixpoint { window; progress = rwexpr progress }
                | g -> g)
              l.governors;
        }
  | Run r -> Run { r with id = sfx r.id; input = rwselopt r.input }
  | Commit c ->
      Commit
        {
          id = sfx c.id;
          preflight =
            Option.map
              (fun (p : commit_preflight) -> { p with input = List.map rwsel p.input })
              c.preflight;
        }
  | Parallel p -> Parallel { branches = List.map rws p.branches }
  | Foreach f -> Foreach { over = f.over; steps = rws f.steps }
  | Dynamic_parallel dp ->
      Dynamic_parallel { id = sfx dp.id; over = dp.over; steps = rws dp.steps }
  | Spawn sp ->
      Spawn
        {
          id = sfx sp.id;
          children =
            List.map
              (fun (c : spawn_child) -> { id = sfx c.id; steps = rws c.steps })
              sp.children;
        }
  | Shell sh -> Shell { sh with id = sfx sh.id }
  | Evidence e -> Evidence { e with id = sfx e.id }
  | Attest a ->
      Attest
        {
          id = sfx a.id;
          select = List.map rwsel a.select;
          replay_domain = a.replay_domain;
          output = dynamic_parallel_namespace_output ~branch_idx a.output;
        }

let dynamic_parallel_branch_steps ~template_ids ~key ~branch_idx steps =
  let suffix = "#" ^ key in
  List.map
    (dynamic_parallel_rewrite_step ~template_ids ~suffix ~branch_idx)
    steps

