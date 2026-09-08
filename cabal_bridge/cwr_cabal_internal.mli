(** Package-private implementation and deterministic mapping test surface. *)

type bootstrap
type custom_backend

val bootstrap_hardened : unit -> (bootstrap, string) result

val register_custom_backend :
  bootstrap:bootstrap ->
  descriptor:Cabal.Backend_registry.descriptor ->
  backend:Cabal.Agentic_backend.t ->
  (custom_backend, string) result

val create :
  bootstrap:bootstrap ->
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  limits:Cabal.Task_preflight.limits ->
  backend_id:string ->
  working_dir:string ->
  ?custom_backend:custom_backend ->
  ?default_model:string ->
  unit ->
  (Cabal_workflow_runner.Runtime.t, string) result

val map_event :
  no_invocation:bool ->
  Cabal.Task_event.t ->
  (Cabal_workflow_runner.Workflow_event.t, string) result

val map_trace :
  ?no_invocation:bool ->
  Cabal.Backend_completer.event_trace ->
  (Cabal_workflow_runner.Workflow_event.trace, string) result

val map_ok :
  descriptor:Cabal.Backend_registry.descriptor ->
  Cabal.Backend_completer.rich_completion_response ->
  ( Cabal_workflow_runner.Agent_execution.response,
    Cabal_workflow_runner.Agent_execution.error )
  result

val map_error :
  descriptor:Cabal.Backend_registry.descriptor ->
  Cabal.Backend_completer.rich_completion_error ->
  Cabal_workflow_runner.Agent_execution.error

module Private : sig
  (** Deterministic replacement-race seam. The hook runs after CWR request
      checks and immediately before the guarded Cabal completer call. *)
  val create_with_selection_hook :
    bootstrap:bootstrap ->
    sw:Eio.Switch.t ->
    env:Eio_unix.Stdenv.base ->
    limits:Cabal.Task_preflight.limits ->
    backend_id:string ->
    working_dir:string ->
    ?custom_backend:custom_backend ->
    ?default_model:string ->
    after_selection:(unit -> unit) ->
    unit ->
    (Cabal_workflow_runner.Runtime.t, string) result
end
