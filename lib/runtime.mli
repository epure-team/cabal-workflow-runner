(** Opaque one-call runtime seam for rich agent execution.

    The runtime is additive and is not wired into {!Engine}. It lets a host
    supply a Cabal-backed or other implementation later without introducing a
    Cabal dependency in the library. *)

type capabilities
(** Opaque advisory runtime capability metadata. Callers must still handle a
    structured dispatch rejection because capabilities can drift at runtime. *)

val max_media_mime_types : int
(** Maximum number of exact media MIME types in a capability value. *)

val make_capabilities :
  ?native_json_schema:bool ->
  ?session_resume:bool ->
  ?media_mime_types:string list ->
  ?maximum_web:Agent_execution.web_level ->
  ?restricted_web_domains:bool ->
  ?read_only:bool ->
  ?max_turns:bool ->
  ?hard_timeout:bool ->
  ?routing:bool ->
  ?model_selection:bool ->
  unit ->
  (capabilities, string) result
(** Construct validated capability metadata. Media support is disabled when
    [media_mime_types] is empty; supplied MIME types are canonicalized, bounded,
    and must be unique. [restricted_web_domains] may be true only when
    [maximum_web] enables web access. Every boolean defaults to [false], the
    MIME list defaults to empty, and maximum web defaults to
    {!Agent_execution.Web_disabled}. *)

val native_json_schema : capabilities -> bool
(** Whether JSON Schema can be enforced natively by this runtime. *)

val session_resume : capabilities -> bool
(** Whether an existing backend session can be resumed. *)

val attachments : capabilities -> bool
(** Whether any media attachment encoding is supported. Derived from
    {!media_mime_types}; it is not an independent claim. *)

val media_mime_types : capabilities -> string list
(** Ordered canonical MIME types whose attachment transports are supported. *)

val maximum_web : capabilities -> Agent_execution.web_level
(** Maximum supported backend-native web level. *)

val restricted_web_domains : capabilities -> bool
(** Whether a non-disabled web policy can enforce its domain allowlist. *)

val read_only : capabilities -> bool
(** Whether read-only intent is supported. *)

val max_turns : capabilities -> bool
(** Whether a maximum backend turn count is supported. *)

val hard_timeout : capabilities -> bool
(** Whether the mandatory request timeout is enforced as a hard runtime bound.
*)

val routing : capabilities -> bool
(** Whether host-neutral routing hints are supported. *)

val model_selection : capabilities -> bool
(** Whether per-request model-selection hints are supported. *)

type t
(** Opaque rich execution runtime. *)

val make :
  ?identity:string ->
  ?capabilities:capabilities ->
  complete:
    (Agent_execution.request ->
    (Agent_execution.response, Agent_execution.error) result) ->
  unit ->
  (t, string) result
(** [make ~complete ()] wraps exactly one completion function. [identity] is an
    optional portable identifier; [capabilities] defaults to all unsupported.
    Construction performs no backend work. *)

val identity : t -> string option
(** Optional validated runtime identity. *)

val capabilities : t -> capabilities
(** Advisory capability metadata supplied at construction. *)

val complete :
  t ->
  Agent_execution.request ->
  (Agent_execution.response, Agent_execution.error) result
(** Invoke the wrapped completion function exactly once. *)

val of_legacy_backend :
  ?now:(unit -> float) ->
  ?attested_read_only:bool ->
  ?attested_routing:bool ->
  ?attested_model_selection:bool ->
  Backend.t ->
  t
(** Adapt the unchanged legacy {!Backend.t} agent function to a rich runtime.

    The adapter combines the separate prompts in a documented system/user
    envelope, forwards id/read-only/routing/model, and calls [run_agent] once.
    Read-only intent must be explicit: both [Some true] and [Some false] are
    forwarded exactly, while [None] is rejected before dispatch rather than
    being silently weakened to write-enabled access. Its synthetic response
    contains exactly one initial attempt, unknown usage/cost, no event trace,
    [Cleanup_not_required], and no session id. It never scans or mutates model
    JSON to infer or inject a session id.

    Legacy [bool = false] becomes an execution failure retaining that attempt.
    Schema, resume, attachment, web, and max-turn requests are each rejected as
    [Unsupported_request] before dispatch because {!Backend.t} cannot carry
    them. The legacy interface also cannot enforce the request timeout; its
    [hard_timeout] capability is therefore false. Legacy callback arguments do
    not prove that the implementation honors their semantics, so read-only,
    routing, and model-selection capabilities default to false. A caller that
    has independently verified its concrete backend may set the corresponding
    [attested_*] flags. [now] is an injectable elapsed-time seam and defaults to
    [Unix.gettimeofday]. *)
