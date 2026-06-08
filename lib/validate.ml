open Types

module String_set = Set.Make (String)

module Validated = struct
  type t = { wf : workflow; floor : gate_id list }

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
  | Gate { id } -> Ok (String_set.add id guaranteed)
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
  | Branch { on = _; then_; else_ } -> (
      match
        (check_steps ~floor ~guaranteed then_, check_steps ~floor ~guaranteed else_)
      with
      | Error _ as e, _ -> e
      | _, (Error _ as e) -> e
      | Ok g_then, Ok g_else ->
          (* Only gates guaranteed in BOTH branches remain guaranteed. *)
          Ok (String_set.inter g_then g_else))
  | Loop { max_iters; until = _; body } ->
      if max_iters < 1 then
        Error
          (Printf.sprintf "Loop is unbounded: max_iters=%d (must be >= 1)"
             max_iters)
      else
        (* Check the body for violations, but do NOT propagate body gates as
           guaranteed: a loop may execute zero iterations. *)
        Result.map (fun _ -> guaranteed)
          (check_steps ~floor ~guaranteed body)

let workflow ~floor_gates wf =
  match
    check_steps ~floor:floor_gates ~guaranteed:String_set.empty wf.steps
  with
  | Error _ as e -> e
  | Ok _ -> Ok { Validated.wf; floor = floor_gates }
