open Types

module Validated = struct
  type t = { wf : workflow; floor : string list }

  let workflow t = t.wf
  let floor_gates t = t.floor
end

(** {!workflow} is defined in terms of {!Lint.check}: the linter is the single
    source of truth for the safety floor, so the gate and the linter cannot
    drift. The conservative static analysis (guaranteed-gate threading, branch
    intersection, loop-body gates not counting, governor well-formedness) lives
    in {!Lint}. Here we simply reject iff [Lint.check] produced any
    error-severity diagnostic, rendering the error message from those errors. *)

let workflow ~floor_gates wf =
  let ds = Lint.check ~floor_gates wf in
  let errors = List.filter (fun (d : Lint.diagnostic) -> d.severity = Error) ds in
  match errors with
  | [] -> Ok { Validated.wf; floor = floor_gates }
  | _ ->
      Error
        (String.concat "; "
           (List.map (fun (d : Lint.diagnostic) -> d.message) errors))
