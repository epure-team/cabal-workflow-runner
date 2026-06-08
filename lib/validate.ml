open Types

module String_set = Set.Make (String)

module Validated = struct
  type t = { wf : workflow; floor : string list }

  let workflow t = t.wf
  let floor_gates t = t.floor
end

(** Conservative static analysis. We walk a [step list] threading the set of
    gates *guaranteed* to have been evaluated on every path reaching the current
    position. The walk returns the (possibly augmented) guaranteed set after the
    sequence, or [Error] as soon as an unsafe step is found.

    - [Gate g] adds [g] to the guaranteed set.
    - [Branch] adds only gates guaranteed in BOTH branches (set intersection),
      since at runtime only one branch is taken.
    - [Loop] body gates are NOT added (the loop may run zero iterations); the
      body is still recursively checked for its own rule violations.
    - [Commit] requires [floor_gates] subset of the guaranteed set here.
    - [Agent] does not change the guaranteed set. *)

let rec check_steps ~floor ~guaranteed steps =
  match steps with
  | [] -> Ok guaranteed
  | step :: rest -> (
      match check_step ~floor ~guaranteed step with
      | Error _ as e -> e
      | Ok guaranteed' -> check_steps ~floor ~guaranteed:guaranteed' rest)

and check_step ~floor ~guaranteed step =
  match step with
  | Agent _ -> Ok guaranteed
  | Gate { id; when_ = _ } -> Ok (String_set.add id guaranteed)
  | Commit { id } ->
      let missing =
        List.filter (fun g -> not (String_set.mem g guaranteed)) floor
      in
      if missing = [] then Ok guaranteed
      else
        Error
          (Printf.sprintf
             "Commit %S is reachable without floor gate(s) %s guaranteed on \
              every preceding path"
             id
             (String.concat ", " (List.map (Printf.sprintf "%S") missing)))
  | Branch { when_ = _; then_; else_ } -> (
      match
        (check_steps ~floor ~guaranteed then_, check_steps ~floor ~guaranteed else_)
      with
      | Error _ as e, _ -> e
      | _, (Error _ as e) -> e
      | Ok g_then, Ok g_else ->
          (* Only gates guaranteed in BOTH branches remain guaranteed. *)
          Ok (String_set.inter g_then g_else))
  | Loop { governors; until = _; body } -> (
      (* A loop must declare >= 1 governor (the termination guarantee). It may
         legitimately have NO Max_iters — unbounded but governed. What is
         forbidden is an EMPTY governors list. Each governor is itself
         well-formedness-checked. *)
      match check_governors governors with
      | Error _ as e -> e
      | Ok () ->
          (* Check the body for violations, but do NOT propagate body gates as
             guaranteed: a loop may execute zero iterations. *)
          Result.map (fun _ -> guaranteed)
            (check_steps ~floor ~guaranteed body))

and check_governors governors =
  if governors = [] then Error "loop is ungoverned"
  else
    let bad =
      List.find_map
        (function
          | Max_iters n when n < 1 ->
              Some (Printf.sprintf "Max_iters governor must be >= 1 (got %d)" n)
          | Fixpoint { window; _ } when window < 1 ->
              Some
                (Printf.sprintf "Fixpoint governor window must be >= 1 (got %d)"
                   window)
          | _ -> None)
        governors
    in
    match bad with Some msg -> Error msg | None -> Ok ()

let workflow ~floor_gates wf =
  match
    check_steps ~floor:floor_gates ~guaranteed:String_set.empty wf.steps
  with
  | Error _ as e -> e
  | Ok _ -> Ok { Validated.wf; floor = floor_gates }
