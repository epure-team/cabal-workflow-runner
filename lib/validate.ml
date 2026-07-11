open Types

module Validated = struct
  type t = { wf : workflow; floor : string list; required : string list }

  let workflow t = t.wf
  let floor_gates t = t.floor
  let required_attestations t = t.required
end

(** {!workflow} is defined in terms of {!Lint.check}: the linter is the single
    source of truth for the safety floor, so the gate and the linter cannot
    drift. The conservative static analysis (guaranteed-gate threading, branch
    intersection, loop-body gates not counting, governor well-formedness) lives
    in {!Lint}. Here we simply reject iff [Lint.check] produced any
    error-severity diagnostic, rendering the error message from those errors. *)

module S = Set.Make(String)

let required_attestation_errors required steps =
  let required = S.of_list required in
  let missing guaranteed = S.diff required guaranteed |> S.elements in
  let rec sequence guaranteed errors = function
    | [] -> guaranteed, errors
    | Attest { id; _ } :: rest -> sequence (S.add id guaranteed) errors rest
    | Commit { id; _ } :: _ ->
        let absent = missing guaranteed in
        guaranteed,
        (if absent = [] then errors else
           (Printf.sprintf "Commit %S is reachable without required attestation(s) %s"
              id (String.concat ", " absent)) :: errors)
    | Branch { then_; else_; _ } :: rest ->
        let gt, errors = sequence guaranteed errors then_ in
        let ge, errors = sequence guaranteed errors else_ in
        sequence (S.inter gt ge) errors rest
    | Loop { body; _ } :: rest ->
        let _, errors = sequence guaranteed errors body in
        sequence guaranteed errors rest
    | Foreach { steps; _ } :: rest ->
        let _, errors = sequence guaranteed errors steps in
        sequence guaranteed errors rest
    | Parallel { branches } :: rest ->
        let branch_sets, errors = List.fold_left (fun (sets, errors) branch ->
          let g, errors = sequence guaranteed errors branch in g :: sets, errors)
          ([], errors) branches in
        let g = match branch_sets with [] -> guaranteed
          | first :: tail -> List.fold_left S.inter first tail in
        sequence g errors rest
    | _ :: rest -> sequence guaranteed errors rest
  in
  let guaranteed, errors = sequence S.empty [] steps in
  let absent = missing guaranteed in
  if absent = [] then errors else
    ("workflow can complete without required attestation(s) " ^
     String.concat ", " absent) :: errors

let workflow ?(required_attestations = []) ~floor_gates wf =
  let ds = Lint.check ~floor_gates wf in
  let errors = List.filter (fun (d : Lint.diagnostic) -> d.severity = Error) ds in
  let attest_errors = required_attestation_errors required_attestations wf.steps in
  match errors, attest_errors with
  | [], [] -> Ok { Validated.wf; floor = floor_gates;
                    required = required_attestations }
  | _ ->
      Error
        (String.concat "; "
           (List.map (fun (d : Lint.diagnostic) -> d.message) errors @ attest_errors))
