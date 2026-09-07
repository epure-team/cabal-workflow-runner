(** Hardened Cabal bridge for CWR's host-neutral rich execution contract.

    This is a separate installable library. The core [cabal_workflow_runner]
    library remains Cabal-free; applications opt into this module by linking
    [cabal_workflow_runner.cabal_bridge]. All task execution goes through
    [Cabal.Backend_completer.make_rich] and therefore through Cabal's central
    registry, capability/input preflight, deadline, schema-enforcement, event,
    and cleanup owners. *)

type bootstrap
(** Opaque identity snapshot for the exact validated runtime entries installed
    by one successful hardened bootstrap. It is reusable by concurrent
    {!create} calls but cannot be recreated after registry clearing or
    replacement in the same process. *)

type custom_backend
(** Opaque authorization for one explicitly registered custom backend. It is
    returned only by {!register_custom_backend}; it cannot authorize another
    id or survive replacement of the validated registry entry. *)

val bootstrap_hardened : unit -> (bootstrap, string) result
(** [bootstrap_hardened ()] invokes
    [Runtime_bootstrap.register_runtime Hardened_builtins] at most once
    successfully for the process. The Cabal registry must be empty. It never
    loads user, project, or global YAML and does not probe models. A successful
    call captures the physical identity and immutable metadata of every
    installed entry and backend. Later bootstrap calls are rejected even if
    test-only code clears Cabal's registry; a failed call that installed
    nothing leaves the one-shot claim available for a corrected startup call.

    Applications must call this before {!create} or
    {!register_custom_backend}, while startup is still single-domain. *)

val register_custom_backend :
  bootstrap:bootstrap ->
  descriptor:Cabal.Backend_registry.descriptor ->
  backend:Cabal.Agentic_backend.t ->
  (custom_backend, string) result
(** [register_custom_backend ~bootstrap ~descriptor ~backend] is the explicit
    post-bootstrap extension point for deterministic host/test backends. It
    first verifies that every hardened built-in binding is still intact, then
    delegates atomically to [Runtime_bootstrap.register_custom]. On success the
    returned token is required to select that custom backend with {!create}.
    Per-request routing cannot use the token to select a different custom id. *)

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
(** [create ~bootstrap ~sw ~env ~limits ~backend_id ~working_dir ()] constructs
    a CWR rich
    runtime bound to an explicit hardened backend id. There is no first-available
    fallback. [limits] is mandatory caller policy; this bridge defines no Cabal
    media defaults. [default_model], when supplied, is used only when a request
    has no model hint.

    Every completion requires explicit CWR read-only intent and maps one request
    through [Backend_completer.make_completion_request]. The request model,
    read-only flag, and selected backend are constructor inputs to
    [Backend_completer.make_rich]. A request routing hint may select another
    intact hardened built-in; it cannot silently select raw, extensible YAML, or
    another custom backend. Every call rechecks the current registry entry
    against the bootstrap's captured metadata and physical entry/backend
    identities, so an equal-looking validated replacement is rejected. The
    explicitly selected custom backend is accepted only with its matching
    bootstrap-bound [custom_backend] token and unchanged physical entry.

    CWR [image/png] and [image/jpeg] attachments map exactly to Cabal media
    types, in request order. Other MIME types, oversized integer metadata, and
    domain-restricted web policies are rejected before invoking the rich
    completer because the pinned Cabal completion DTO cannot represent them.
    Unrestricted web levels, finite timeout, maximum turns, schema, and resume
    session map directly. Advertising maximum-turn support means that the bridge
    accepts and forwards the bound; it is not independent evidence that every
    backend CLI enforces it. Diagnostics contain no attachment path or digest.

    Cabal token counts map to non-negative [int64] fields. A finite non-negative
    USD float maps to integer micro-USD with [ceil (usd * 1_000_000)]; positive
    representational overflow saturates at [Int64.max_int], matching CWR's
    aggregate saturation. Negative and non-finite monetary telemetry fails safe
    conversion. When the source trace itself remains valid, the resulting
    telemetry-mapping error retains it exactly. The same conversion is used for
    attempts and cumulative usage events.

    Structured output never receives an injected session id. A standard
    object/array [report.raw_json] is preferred unless a strict object/array
    parse of normalized final text proves it inconsistent; two valid but
    different sources fail closed rather than selecting one. Otherwise only the
    strict text parse is used. Fences, prose extraction, bracket scanning,
    scalars, and non-standard Yojson values are never accepted as structured
    JSON.

    The returned runtime retains complete attempt, status, schema-error,
    delivery, elapsed, session, usage/cost, cleanup, continuation, and bounded
    normalized event evidence in CWR Batch-1 types. Raw protocol lines, stdout,
    stderr, process ids, tool arguments, chain-of-thought, and attachment paths
    are never projected. Cabal may deliver final [Session_id] and [Token_usage]
    metadata immediately after its transport [Attempt_finished] notification.
    [Workflow_event] permits only those two same-attempt final observations in
    that position. The bridge preserves their original sequence, attempt, and
    elapsed envelopes; it never rotates or reassociates them. *)
