(** Additive host-neutral rich agent execution contract.

    These DTOs are independent of the deterministic workflow interpreter and of
    Cabal. They are consumed by separately linked host bridges. Construction is
    opaque and validated so optional fields can be extended without breaking
    callers. No constructor performs filesystem or backend I/O. *)

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

val max_incomplete_execution_projection_bytes : int
(** Upper byte bound guaranteed for one serialized safe incomplete-execution
    projection, including its complete outer normalized trace. *)

val max_error_projection_bytes : int
(** Upper byte bound guaranteed for a serialized safe error projection,
    including a separately retained outer event trace. *)

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

(** Evidence about an invoked-but-uncommitted corrective continuation.
    [Invocation_may_have_started] is conservative when retained lifecycle events
    do not prove entry into the continuation and a boundary-local omission/gap
    makes that evidence unknowable. Earlier omissions within the completed
    attempt do not qualify. [Invocation_started] requires explicit retained
    lifecycle or observation evidence for the continuation; the exact start
    event may be omitted when later activity proves invocation. *)
type continuation_invocation =
  | Invocation_may_have_started
  | Invocation_started

type incomplete_continuation
(** Opaque identity of at most one invoked-but-uncommitted continuation. It
    deliberately has no result, elapsed duration, output, session, or aggregate
    fields. *)

val make_incomplete_continuation :
  number:int ->
  kind:attempt_kind ->
  invocation:continuation_invocation ->
  unit ->
  (incomplete_continuation, string) result
(** Construct a potential or known continuation. Its number must be greater
    than one and its kind must be [Fresh_attempt] or [Resumed_attempt]. Exact
    contiguity and prior retry context are checked by
    {!make_incomplete_execution}. *)

val continuation_number : incomplete_continuation -> int
(** One-based number of the uncommitted continuation. *)

val continuation_kind : incomplete_continuation -> attempt_kind
(** Fresh or resumed continuation kind. *)

val continuation_invocation :
  incomplete_continuation -> continuation_invocation
(** Whether invocation may have begun or is known to have begun. *)

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
    non-decreasing. A retained post-finish text fallback must equal the matching
    attempt text unless a positive text-truncation marker immediately follows it
    in the source sequence and makes it a prefix lower bound; the completed
    result still retains the full normalized text. Such an attempt-N fallback
    marker is result-delivery evidence and cannot establish an omitted
    continuation N+1. [event_trace] defaults to [None], preserving the
    distinction between no collected trace and a trace with omissions. *)

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

type incomplete_execution
(** Opaque interrupted execution with one or more complete attempts and at most
    one invoked-but-uncommitted continuation. No attempt or backend result is
    synthesized for the continuation. *)

val make_incomplete_execution :
  completed_attempts:attempt list ->
  outer_status:status ->
  total_elapsed_s:float ->
  cleanup_status:cleanup_status ->
  ?continuation:incomplete_continuation ->
  outer_event_trace:Workflow_event.trace ->
  unit ->
  (incomplete_execution, string) result
(** Construct interrupted execution evidence. [completed_attempts] must be
    non-empty, contiguous, coherent completed results. [outer_status] must be
    [Failed _], [Timed_out], or [Cancelled] and must exactly match the terminal
    of [outer_event_trace].

    A continuation, when present, must be numbered immediately after the last
    completed attempt, be fresh or resumed, and follow retained schema/retry
    context. The outer trace may contain lifecycle evidence only through that
    one continuation, and its terminal must be attributed to the continuation
    number rather than the last completed attempt. A retained continuation
    finish must be failed, timed out, or cancelled consistently with
    [outer_status]; success and evidence for a second continuation are rejected.
    [Invocation_started] requires explicit retained continuation lifecycle or
    observation evidence. [Invocation_may_have_started] requires no such
    evidence plus a positive truncation marker or sequence gap within the N to
    N+1 boundary that can account for its absence. When the N retry transition
    is retained, that interval begins after the transition (and any later
    retained N evidence). Otherwise it begins after the last retained lifecycle
    or observation event for completed N. A prefix gap or earlier N truncation
    that ends before a dense retained transition-to-terminal suffix does not
    qualify; an unlocated trace omission count alone does not qualify either.
    Truncation can account for a continuation only when its marker belongs to
    N+1; a bounded final-text fallback marker on completed attempt N cannot. A
    prior [schema_error] establishes retry context but does not by itself
    establish that a continuation began.

    Aggregate usage/cost and final session are derived exclusively from
    [completed_attempts]. Usage/cost observations for the incomplete
    continuation remain in the bounded outer trace and are additionally exposed
    as separate lower bounds; they are never folded into completed aggregates.
    Ordinary {!make_response} trace/status fusion remains unchanged. *)

val incomplete_completed_attempts : incomplete_execution -> attempt list
(** Exact ordered list of committed complete attempts. *)

val incomplete_outer_status : incomplete_execution -> status
(** Failed, timed-out, or cancelled outer operation status. *)

val incomplete_total_elapsed_s : incomplete_execution -> float
(** Finite non-negative outer operation elapsed time. *)

val incomplete_completed_usage :
  incomplete_execution -> Execution_metrics.usage option
(** Saturating aggregate based only on completed attempt results. *)

val incomplete_completed_cost :
  incomplete_execution -> Execution_metrics.cost option
(** Saturating cost aggregate based only on completed attempt results. *)

val incomplete_final_session_id : incomplete_execution -> string option
(** Last session identifier among completed attempts only. *)

val incomplete_cleanup_status : incomplete_execution -> cleanup_status
(** Sanitized outer cleanup outcome. *)

val incomplete_continuation :
  incomplete_execution -> incomplete_continuation option
(** Optional single invoked-but-uncommitted continuation identity. *)

val incomplete_continuation_usage_lower_bound :
  incomplete_execution -> Execution_metrics.usage option
(** Field-wise lower bound derived only from bounded usage observations for the
    incomplete continuation. It is not part of completed usage. *)

val incomplete_continuation_cost_lower_bound :
  incomplete_execution -> Execution_metrics.cost option
(** Lower bound derived only from bounded cost observations for the incomplete
    continuation. It is not part of completed cost. *)

val incomplete_outer_event_trace :
  incomplete_execution -> Workflow_event.trace
(** Complete bounded normalized outer trace, including retained continuation
    lifecycle/observation evidence and omission metadata. *)

val incomplete_execution_to_yojson : incomplete_execution -> Yojson.Safe.t
(** Stable redacted projection with schema version
    [cwr.agent-execution.incomplete/v1]. *)

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

(** Stable categories for failures after at least one backend invocation.
    [Native_backend_failure_with_schema] means only that a native-schema backend
    failed while a schema was in force; it does not attribute the failure to
    schema rejection. *)
type execution_failure_kind =
  | Native_backend_failure_with_schema
  | Schema_retry_failed
  | Backend_execution_failed
  | Execution_contract_failed

type error
(** Opaque rich execution error. *)

(** Exhaustive in-process view distinguishing a proven no-invocation dispatch
    failure, an outcome with no completed attempt and indeterminate invocation
    progress, an interrupted execution with completed progress, a dispatch-layer
    failure strictly after a completed execution, an execution failure, and a
    boundary telemetry-mapping failure. Completed-progress forms retain
    normalized attempts; mapping failures retain the exact safe source trace
    even when its terminal status cannot fit another error constructor. *)
type error_view =
  | Dispatch_failure of {
      kind : dispatch_failure_kind;
      message : string;
      event_trace : Workflow_event.trace option;
    }
  | No_completed_attempt of {
      status : status;
      invocation_may_have_started : bool;
      message : string;
      event_trace : Workflow_event.trace option;
    }
  | Incomplete_execution of {
      message : string;
      execution : incomplete_execution;
    }
  | Post_execution_dispatch_failed of {
      cause : dispatch_failure_kind;
      message : string;
      response : response;
      outer_event_trace : Workflow_event.trace;
    }
  | Execution_failure of {
      kind : execution_failure_kind;
      message : string;
      response : response;
    }
  | Telemetry_mapping_failure of {
      message : string;
      event_trace : Workflow_event.trace;
    }

val make_dispatch_error :
  kind:dispatch_failure_kind ->
  message:string ->
  ?event_trace:Workflow_event.trace ->
  unit ->
  (error, string) result
(** Construct a failure known to precede backend invocation. A supplied trace
    must contain no nonzero attempt evidence and must terminate as failed. The
    non-empty UTF-8 message is normalized for in-process diagnostics but omitted
    from safe JSON persistence. *)

val redacted_dispatch_error : dispatch_failure_kind -> error
(** Construct a total, message-free dispatch error for adapter fallback paths
    that have no safe diagnostic. The in-process message is the fixed string
    [details unavailable]. *)

val make_no_completed_attempt_error :
  status:status ->
  invocation_may_have_started:bool ->
  message:string ->
  ?event_trace:Workflow_event.trace ->
  unit ->
  (error, string) result
(** Construct an outcome for which no complete backend result exists. [status]
    must be [Failed _], [Timed_out], or [Cancelled].
    [invocation_may_have_started] explicitly distinguishes uncertain/in-flight
    progress from a caller that knows dispatch did not begin; pure registry,
    capability, and preflight failures should normally use
    {!make_dispatch_error}. A supplied trace must terminate with [status]; when
    invocation is definitely absent, it must contain no nonzero attempt
    evidence. No attempt is synthesized. Diagnostics are omitted from safe JSON.
*)

val make_incomplete_execution_error :
  message:string ->
  execution:incomplete_execution ->
  unit ->
  (error, string) result
(** Wrap validated partial execution evidence as its distinct error shape. The
    normalized non-empty diagnostic remains in process and is omitted from safe
    persistence. *)

val make_post_execution_dispatch_error :
  cause:dispatch_failure_kind ->
  message:string ->
  response:response ->
  outer_event_trace:Workflow_event.trace ->
  unit ->
  (error, string) result
(** Construct a dispatch-layer failure that occurred after at least one backend
    attempt completed. Any coherent non-empty {!response} is accepted, including
    success, backend failure, timeout, cancellation, and schema rejection; the
    constructor never rewrites its status or fabricates telemetry. [cause] is a
    fixed host-neutral category. [outer_event_trace] is retained separately from
    the completed response, must terminate as failed, and is cross-checked
    against response attempts without requiring its elapsed time or terminal to
    equal the nested completed execution. Thus a successful nested transport can
    coexist with a failed outer cleanup/dispatch terminal without weakening
    ordinary {!make_response} trace fusion. The non-empty UTF-8 diagnostic
    remains in-process and is omitted from safe JSON persistence. *)

val make_execution_error :
  kind:execution_failure_kind ->
  message:string ->
  response:response ->
  unit ->
  (error, string) result
(** Construct a post-invocation failure retaining its full normalized response.
    The failure kind must agree with the response. Native backend failures with
    a schema in force, generic backend failures, and contract failures retain a
    failed attempt; the native category does not claim schema causality.
    [Schema_retry_failed] requires exactly two
    attempts: an initial schema-rejected transport success followed by a fresh
    or resumed corrective attempt. That corrective attempt may itself be
    schema-rejected after transport success, fail at transport/backend level,
    time out, or be cancelled; the outer response status must match that final
    condition. The non-empty UTF-8 message is omitted from safe JSON
    persistence. *)

val make_telemetry_mapping_error :
  message:string ->
  event_trace:Workflow_event.trace ->
  unit ->
  (error, string) result
(** Construct an adapter-boundary failure for source telemetry that cannot be
    represented by a richer execution/error constructor. The already validated
    trace is retained byte-for-byte regardless of its terminal status or attempt
    evidence; no result, attempt, status, or event is fabricated. The non-empty
    UTF-8 diagnostic remains in process and is omitted from safe persistence. *)

val error_view : error -> error_view
(** Inspect the error classification and retained in-process diagnostic. *)

val response_to_yojson : response -> Yojson.Safe.t
(** Stable redacted JSON persistence projection with schema version
    [cwr.agent-execution.response/v1]. Public assistant output is retained;
    prompts, attachment paths/digests/bytes, diagnostics, stdout/stderr, argv,
    and private backend payloads are unrepresentable or omitted. *)

val error_to_yojson : error -> Yojson.Safe.t
(** Stable redacted JSON persistence projection with schema version
    [cwr.agent-execution.error/v1]. Diagnostics are omitted. No-completed-attempt
    outcomes retain only status, invocation uncertainty, and an optional safe
    trace. Incomplete execution embeds exact completed attempts, outer status,
    separate incomplete-observation lower bounds, and the safe outer trace, but
    no synthetic continuation result. Post-execution dispatch failures retain
    their fixed cause, safe response, and separate safe outer trace; execution
    failures likewise embed the safe response and retain attempts. Telemetry
    mapping failures persist only their exact safe event trace. *)
