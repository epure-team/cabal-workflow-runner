(** Hardened Cabal bridge for CWR's host-neutral rich execution contract.

    This is a separate installable library. The core [cabal_workflow_runner]
    library remains Cabal-free; applications opt into this module by linking
    [cabal_workflow_runner.cabal_bridge]. All task execution goes through
    [Cabal.Backend_completer.make_rich] and therefore through Cabal's central
    registry, capability/input preflight, deadline, schema-enforcement, event,
    and cleanup owners. *)

type custom_backend
(** Opaque authorization for one explicitly registered custom backend. It is
    returned only by {!register_custom_backend}; it cannot authorize another
    id or survive replacement of the validated registry entry. *)

val bootstrap_hardened : unit -> (unit, string) result
(** [bootstrap_hardened ()] invokes
    [Runtime_bootstrap.register_runtime Hardened_builtins] exactly once for the
    process startup sequence. The Cabal registry must be empty. It never loads
    user, project, or global YAML and does not probe models. A second call or a
    pre-existing registry is rejected with Cabal's sanitized conflict message.

    Applications must call this before {!create} or
    {!register_custom_backend}, while startup is still single-domain. *)

val register_custom_backend :
  descriptor:Cabal.Backend_registry.descriptor ->
  backend:Cabal.Agentic_backend.t ->
  (custom_backend, string) result
(** [register_custom_backend ~descriptor ~backend] is the explicit
    post-bootstrap extension point for deterministic host/test backends. It
    first verifies that every hardened built-in binding is still intact, then
    delegates atomically to [Runtime_bootstrap.register_custom]. On success the
    returned token is required to select that custom backend with {!create}.
    Per-request routing cannot use the token to select a different custom id. *)

val create :
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  limits:Cabal.Task_preflight.limits ->
  backend_id:string ->
  working_dir:string ->
  ?custom_backend:custom_backend ->
  ?default_model:string ->
  unit ->
  (Cabal_workflow_runner.Runtime.t, string) result
(** [create ~sw ~env ~limits ~backend_id ~working_dir ()] constructs a CWR rich
    runtime bound to an explicit hardened backend id. There is no first-available
    fallback. [limits] is mandatory caller policy; this bridge defines no Cabal
    media defaults. [default_model], when supplied, is used only when a request
    has no model hint.

    Every completion requires explicit CWR read-only intent and maps one request
    through [Backend_completer.make_completion_request]. The request model,
    read-only flag, and selected backend are constructor inputs to
    [Backend_completer.make_rich]. A request routing hint may select another
    intact hardened built-in; it cannot silently select raw, extensible YAML, or
    another custom backend. The explicitly selected custom backend is accepted
    only with its matching [custom_backend] token.

    CWR [image/png] and [image/jpeg] attachments map exactly to Cabal media
    types, in request order. Other MIME types, oversized integer metadata, and
    domain-restricted web policies are rejected before invoking the rich
    completer because the pinned Cabal completion DTO cannot represent them.
    Unrestricted web levels, finite timeout, maximum turns, schema, and resume
    session map directly. Diagnostics contain no attachment path or digest.

    Cabal token counts map to non-negative [int64] fields. A finite non-negative
    USD float maps to integer micro-USD with [ceil (usd * 1_000_000)]; positive
    representational overflow saturates at [Int64.max_int], matching CWR's
    aggregate saturation. Negative and non-finite monetary telemetry is treated
    as an execution-contract failure. The same conversion is used for attempts
    and cumulative usage events.

    Structured output never receives an injected session id. A standard
    object/array [report.raw_json] is preferred unless a strict object/array
    parse of normalized final text proves it inconsistent; otherwise only that
    strict parse is used. Fences, prose extraction, bracket scanning, scalars,
    and non-standard Yojson values are never accepted as structured JSON.

    The returned runtime retains complete attempt, status, schema-error,
    delivery, elapsed, session, usage/cost, cleanup, continuation, and bounded
    normalized event evidence in CWR Batch-1 types. Raw protocol lines, stdout,
    stderr, process ids, tool arguments, chain-of-thought, and attachment paths
    are never projected. Cabal's final fallback result metadata can be delivered
    immediately after its transport attempt-finished notification; the bridge
    places those metadata payloads inside the normalized attempt envelope before
    constructing the validated CWR trace, without dropping them. *)
