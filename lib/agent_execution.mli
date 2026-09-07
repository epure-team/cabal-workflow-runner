(** Additive host-neutral rich agent execution contract.

    These DTOs are independent of the deterministic workflow interpreter and of
    Cabal. They are intended for a later host bridge. Construction is opaque and
    validated so optional fields can be extended without breaking callers. No
    constructor performs filesystem or backend I/O. *)

type attachment
(** Opaque validated workspace-relative attachment reference. It contains
    metadata only, never file bytes. *)

val make_attachment :
  id:string ->
  path:string ->
  mime_type:string ->
  sha256:string ->
  size_bytes:int64 ->
  unit ->
  (attachment, string) result
(** [make_attachment ~id ~path ~mime_type ~sha256 ~size_bytes ()] validates an
    attachment reference without opening it. [id] uses the portable identifier
    alphabet, [path] is normalized workspace-relative syntax, [mime_type] is a
    syntactically valid type/subtype and is lowercased, [sha256] is exactly 64
    lowercase hexadecimal characters, and [size_bytes] is non-negative.
    Rejection diagnostics never quote the path or digest. *)

val attachment_id : attachment -> string
(** Attachment identifier. *)

val attachment_path : attachment -> string
(** Workspace-relative attachment path. This in-process accessor is the only
    rich execution API that exposes the path; safe JSON projections omit it. *)

val attachment_mime_type : attachment -> string
(** Canonical lowercase MIME type. *)

val attachment_sha256 : attachment -> string
(** Canonical lowercase SHA-256 hex. Safe JSON projections omit it. *)

val attachment_size_bytes : attachment -> int64
(** Declared non-negative attachment byte size. *)

val canonical_mime_type : string -> string option
(** Return the canonical lowercase [type/subtype] form of a syntactically valid
    MIME type, or [None]. *)

(** Hierarchical backend-native web access level. This policy is an execution
    request, not a network sandbox. *)
type web_level = Web_disabled | Web_search | Web_search_and_fetch

type web_policy
(** Opaque web policy. A non-disabled level may optionally be restricted to a
    non-empty ordered list of canonical lowercase DNS domains. *)

val web_disabled : web_policy
(** Unrestricted disabled policy, used as the request default. *)

val web_search : web_policy
(** Unrestricted search-only policy. *)

val web_search_and_fetch : web_policy
(** Unrestricted search-and-fetch policy. *)

val max_restricted_domains : int
(** Maximum number of domains accepted in one restricted web policy. *)

val make_restricted_web_policy :
  level:web_level -> domains:string list -> unit -> (web_policy, string) result
(** [make_restricted_web_policy ~level ~domains ()] validates a
    domain-restricted policy. [level] must not be [Web_disabled]; domains must
    be non-empty, canonical lowercase DNS names, duplicate-free, and contain at
    most {!max_restricted_domains} entries. Diagnostics never quote rejected
    domains. *)

val web_level : web_policy -> web_level
(** Requested hierarchical access level. *)

val restricted_domains : web_policy -> string list option
(** [None] for an unrestricted policy, or the ordered domain allowlist. *)

type request
(** Opaque validated rich agent request. *)

val max_json_depth : int
(** Maximum nesting depth accepted for schema and structured-output JSON. *)

val max_json_nodes : int
(** Maximum number of JSON values accepted for one schema or structured output.
*)

val max_json_bytes : int
(** Maximum serialized byte length of one schema or structured output. *)

val max_public_text_bytes : int
(** Maximum UTF-8 byte length of one attempt's public final text. *)

val max_attempts : int
(** Maximum number of complete attempts retained in one response. *)

val attempt_timing_tolerance_s : float
(** One-millisecond one-sided tolerance when comparing an attempt duration with
    its enclosing retained start-to-finish event interval. *)

val max_response_projection_bytes : int
(** Upper byte bound guaranteed for a serialized safe response projection.
    Response construction performs exact compact-encoding size preflight without
    serializing the projection. *)

val max_error_projection_bytes : int
(** Upper byte bound guaranteed for a serialized safe error projection. *)

val make_request :
  id:string ->
  system_prompt:string ->
  user_prompt:string ->
  timeout_s:float ->
  ?json_schema:Yojson.Safe.t ->
  ?resume_session:string ->
  ?attachments:attachment list ->
  ?web_policy:web_policy ->
  ?max_turns:int ->
  ?routing:string ->
  ?model:string ->
  ?read_only:bool ->
  unit ->
  (request, string) result
(** [make_request ~id ~system_prompt ~user_prompt ~timeout_s ()] constructs a
    request. [timeout_s] is mandatory, finite, and strictly positive.

    Defaults are explicit: no schema, no resume session, no attachments,
    {!web_disabled}, no maximum turns, no routing or model hint, and unspecified
    read-only intent. A supplied maximum turn count must be positive. Attachment
    identifiers must be unique. Schema JSON is constrained by {!max_json_depth},
    {!max_json_nodes}, and {!max_json_bytes}. Construction validates only DTO
    syntax and does no filesystem, capability, or backend work. *)

val id : request -> string
(** Stable request identifier. *)

val system_prompt : request -> string
(** System instructions, kept separate from {!user_prompt}. *)

val user_prompt : request -> string
(** User turn, kept separate from {!system_prompt}. *)

val json_schema : request -> Yojson.Safe.t option
(** Optional standard JSON Schema object or boolean value. *)

val resume_session : request -> string option
(** Optional validated backend session identifier to resume. *)

val attachments : request -> attachment list
(** Ordered attachment references. *)

val web_policy : request -> web_policy
(** Requested web policy. *)

val timeout_s : request -> float
(** Mandatory finite positive whole-request timeout in seconds. *)

val max_turns : request -> int option
(** Optional positive backend turn limit. *)

val routing : request -> string option
(** Optional host-neutral routing hint. *)

val model : request -> string option
(** Optional backend model-selection hint. *)

val read_only : request -> bool option
(** Optional read-only intent. [None] means the host did not state a policy; it
    is not silently converted to [false]. *)

(** Attachment handling intended for one attempt. *)
type attachment_delivery = Upload_attachments | Reuse_session_attachments

type delivery_intent
(** Opaque path-free per-attempt delivery intent. *)

val make_delivery_intent :
  attachment_count:int ->
  attachment_delivery:attachment_delivery ->
  web_policy:web_policy ->
  unit ->
  (delivery_intent, string) result
(** [make_delivery_intent] records only an attachment count, delivery mode, and
    web policy. It never stores attachment paths, digests, or bytes. The count
    must be non-negative. *)

val delivery_attachment_count : delivery_intent -> int
(** Number of referenced attachments for this attempt. *)

val delivery_attachment_mode : delivery_intent -> attachment_delivery
(** Whether the attempt uploads references or reuses session media. *)

val delivery_web_policy : delivery_intent -> web_policy
(** Web intent for this attempt. *)

(** Attempt kind shared with normalized workflow events. *)
type attempt_kind = Workflow_event.attempt_kind =
  | Initial_attempt
  | Fresh_attempt
  | Resumed_attempt

(** Structured attempt status. A failed status retains its normalized diagnostic
    in memory; safe persistence projections retain only the [failed] class. *)
type status = Success | Failed of string | Timed_out | Cancelled

type attempt
(** Opaque complete normalized telemetry for one actually invoked backend call.
*)

val make_attempt :
  number:int ->
  kind:attempt_kind ->
  status:status ->
  elapsed_s:float ->
  delivery:delivery_intent ->
  ?schema_error:string ->
  ?session_id:string ->
  ?usage:Execution_metrics.usage ->
  ?cost:Execution_metrics.cost ->
  ?text:string ->
  ?structured_json:Yojson.Safe.t ->
  unit ->
  (attempt, string) result
(** [make_attempt] validates one attempt. Numbers are one-based; elapsed time is
    finite and non-negative. [schema_error] is accepted only for a transport
    [Success] that was rejected by schema validation. Text line endings are
    normalized to LF. Optional usage/cost/session values remain optional rather
    than being zero-filled. Text and structured JSON are bounded by the public
    limits above; JSON must be a standard finite JSON value. *)

val attempt_number : attempt -> int
(** One-based invocation number. *)

val attempt_kind : attempt -> attempt_kind
(** Initial, fresh, or resumed invocation kind. *)

val attempt_status : attempt -> status
(** Structured result status. *)

val attempt_text : attempt -> string
(** Normalized public assistant final text for this attempt. *)

val attempt_structured_json : attempt -> Yojson.Safe.t option
(** Optional public structured output for this attempt. *)

val attempt_schema_error : attempt -> string option
(** Optional validator diagnostic for a successful transport response. *)

val attempt_delivery : attempt -> delivery_intent
(** Path-free input delivery intent. *)

val attempt_elapsed_s : attempt -> float
(** Finite non-negative elapsed seconds for the backend call. *)

val attempt_session_id : attempt -> string option
(** Optional session identifier reported by the attempt. *)

val attempt_usage : attempt -> Execution_metrics.usage option
(** Optional token usage; a present all-unknown record remains present. *)

val attempt_cost : attempt -> Execution_metrics.cost option
(** Optional micro-USD cost; a present unknown record remains present. *)

(** Sanitized prepared-input cleanup outcome. *)
type cleanup_status =
  | Cleanup_not_required
  | Cleanup_succeeded
  | Cleanup_failed

type response
(** Opaque normalized response for an execution that invoked at least one
    backend attempt. *)

val make_response :
  attempts:attempt list ->
  status:status ->
  total_elapsed_s:float ->
  cleanup_status:cleanup_status ->
  ?event_trace:Workflow_event.trace ->
  unit ->
  (response, string) result
(** [make_response ~attempts ~status ~total_elapsed_s ~cleanup_status ()]
    validates and constructs a response. Attempts must be in exact invocation
    order: number 1 is [Initial_attempt], subsequent numbers are contiguous and
    are [Fresh_attempt] or [Resumed_attempt]. Total elapsed time is finite and
    non-negative, and cannot be shorter than the sum of sequential attempts. At
    most {!max_attempts} attempts are accepted. [status] must match the final
    attempt, except that a transport-successful final attempt carrying
    [schema_error] requires an overall [Failed _] status.

    Final text/JSON come from the last attempt. Final session is the last
    reported session across all attempts. Usage and cost are independently
    aggregated with saturating integer arithmetic. A supplied trace is
    cross-validated against attempt kinds, outcomes, durations, sessions,
    metrics, total elapsed time, final attempt number, and overall status. An
    attempt duration may be shorter than its event start-to-finish envelope;
    only a duration exceeding that interval by more than
    {!attempt_timing_tolerance_s} is rejected.

    Usage events are cumulative snapshots. Every known retained dimension sets
    a lower bound that the attempt aggregate must meet; a missing aggregate
    dimension cannot erase that evidence. Exact equality is required only for
    dimensions present in the final retained observation when no positive
    unlocated omission count, later sequence gap, or later usage-truncation
    marker makes finality unknown. A dimension absent from the final observation
    remains lower-bound-only. Retained known snapshots must also be
    non-decreasing. [event_trace] defaults to [None], preserving the distinction
    between no collected trace and a trace with omissions. *)

val attempts : response -> attempt list
(** Ordered complete attempt list. *)

val final_status : response -> status
(** Overall normalized status. This differs from the final transport attempt
    only when schema validation rejects a transport-successful result. *)

val final_text : response -> string
(** Normalized final assistant text from the final attempt. *)

val final_structured_json : response -> Yojson.Safe.t option
(** Optional final structured JSON from the final attempt. *)

val total_elapsed_s : response -> float
(** Whole-operation elapsed seconds, not a sum of attempt durations. *)

val total_usage : response -> Execution_metrics.usage option
(** Field-wise saturating aggregate, or [None] when no attempt reported usage.
*)

val total_cost : response -> Execution_metrics.cost option
(** Saturating micro-USD aggregate, or [None] when no attempt reported cost. *)

val final_session_id : response -> string option
(** Last reported session identifier across attempts. *)

val cleanup_status : response -> cleanup_status
(** Sanitized cleanup outcome. *)

val event_trace : response -> Workflow_event.trace option
(** Optional bounded post-completion event trace. *)

(** Stable host-neutral dispatch-layer cause categories. {!Dispatch_failure}
    uses them only when no backend invocation occurred;
    {!Post_execution_dispatch_failed} uses them when completed execution exists.
*)
type dispatch_failure_kind =
  | Invalid_request
  | Backend_unavailable
  | Unsupported_request
  | Capability_mismatch
  | Preflight_failed
  | Deadline_before_dispatch
  | Internal_dispatch_failure

(** Stable categories for failures after at least one backend invocation. *)
type execution_failure_kind =
  | Native_schema_rejection
  | Schema_retry_failed
  | Backend_execution_failed
  | Execution_contract_failed

type error
(** Opaque rich execution error. *)

(** Exhaustive in-process view distinguishing a no-execution dispatch failure,
    a dispatch-layer failure after completed execution exists, and an execution
    failure. Both post-execution forms retain normalized attempt telemetry. *)
type error_view =
  | Dispatch_failure of { kind : dispatch_failure_kind; message : string }
  | Post_execution_dispatch_failed of {
      cause : dispatch_failure_kind;
      message : string;
      response : response;
    }
  | Execution_failure of {
      kind : execution_failure_kind;
      message : string;
      response : response;
    }

val make_dispatch_error :
  kind:dispatch_failure_kind -> message:string -> unit -> (error, string) result
(** Construct a pre-execution failure. The non-empty UTF-8 message is normalized
    for in-process diagnostics but omitted from safe JSON persistence. *)

val redacted_dispatch_error : dispatch_failure_kind -> error
(** Construct a total, message-free dispatch error for adapter fallback paths
    that have no safe diagnostic. The in-process message is the fixed string
    [details unavailable]. *)

val make_post_execution_dispatch_error :
  cause:dispatch_failure_kind ->
  message:string ->
  response:response ->
  unit ->
  (error, string) result
(** Construct a dispatch-layer failure that occurred after at least one backend
    attempt completed. Any coherent non-empty {!response} is accepted, including
    success, backend failure, timeout, cancellation, and schema rejection; the
    constructor never rewrites its status or fabricates telemetry. [cause] is a
    fixed host-neutral category. The non-empty UTF-8 diagnostic remains
    in-process and is omitted from safe JSON persistence. *)

val make_execution_error :
  kind:execution_failure_kind ->
  message:string ->
  response:response ->
  unit ->
  (error, string) result
(** Construct a post-invocation failure retaining its full normalized response.
    The failure kind must agree with the response. Native/backend/contract
    failures retain a failed attempt. [Schema_retry_failed] requires exactly two
    attempts: an initial schema-rejected transport success followed by a fresh
    or resumed corrective attempt. That corrective attempt may itself be
    schema-rejected after transport success, fail at transport/backend level,
    time out, or be cancelled; the outer response status must match that final
    condition. The non-empty UTF-8 message is omitted from safe JSON
    persistence. *)

val error_view : error -> error_view
(** Inspect the error classification and retained in-process diagnostic. *)

val response_to_yojson : response -> Yojson.Safe.t
(** Stable redacted JSON persistence projection with schema version
    [cwr.agent-execution.response/v1]. Public assistant output is retained;
    prompts, attachment paths/digests/bytes, diagnostics, stdout/stderr, argv,
    and private backend payloads are unrepresentable or omitted. *)

val error_to_yojson : error -> Yojson.Safe.t
(** Stable redacted JSON persistence projection with schema version
    [cwr.agent-execution.error/v1]. Diagnostics are omitted. Post-execution
    dispatch failures retain only their fixed cause plus the safe response;
    execution failures likewise embed the safe response and retain attempts. *)
