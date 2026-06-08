(* cabal-backed backend. This is the ONLY place that depends on cabal: the
   [cabal_workflow_runner] library (engine/validate/types) depends on yojson
   only. Agent steps dispatch through cabal's registry; if no backend is
   available we fail closed. *)

open Cabal

(* Evaluate a gate's verdict. cabal has no gate primitive, so gate semantics are
   embedder-defined. The MVP CLI treats every gate as [Pass]: the safety floor
   does not rely on gate verdicts being honest — it relies on the validator
   guaranteeing the floor gates are *evaluated* on every path to a commit and on
   the runtime token. A real embedder would replace this with a check against
   observed evidence / CI results. *)
let eval_gate (_ : Cabal_workflow_runner.Types.gate_id) =
  Cabal_workflow_runner.Types.Pass

(* Build a backend record bound to a live eio environment + switch. Dispatches
   agent work to the first available cabal backend; fails closed if none. *)
let make ~sw ~env ~working_dir : Cabal_workflow_runner.Backend.t =
  let run_agent ~id ~prompt ~read_only =
    match Registry.first_available ~sw ~env with
    | None -> (false, Printf.sprintf "no cabal backend available (step %s)" id)
    | Some backend ->
        let spec =
          Backend_types.make_task_spec ~prompt ~working_dir ~read_only ()
        in
        let request = { Backend_types.spec; ctxt = id } in
        let response =
          Agentic_backend.run_task_with_ctxt ~sw ~env backend request
        in
        let result = response.Backend_types.result in
        let success =
          match result.Backend_types.status with
          | Backend_types.Success -> true
          | _ -> false
        in
        (success, result.Backend_types.agent_text)
  in
  { Cabal_workflow_runner.Backend.run_agent; eval_gate }
