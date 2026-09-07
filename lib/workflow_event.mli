(** Bounded, normalized, host-neutral agent-completion lifecycle traces.

    This vocabulary is intentionally unable to carry raw protocol lines,
    prompts, command arguments, attachment metadata, stdout/stderr, tool
    arguments, chain-of-thought, or private backend JSON. A bridge that observes
    an unknown future backend event must use {!Opaque_backend_observation}.

    Despite the module name, {!trace} is not the deterministic engine's
    {!Types.trace}: it is optional backend-execution telemetry nested in an
    {!Agent_execution.response}, is not persisted by {!Ledger}, and is not
    consumed by {!Engine.replay}. Batch 1 stores only completed traces; it does
    not provide live streaming. *)

(** Stable kind of an actually invoked backend attempt. *)
type attempt_kind = Initial_attempt | Fresh_attempt | Resumed_attempt

(** Transport-level result of one attempt. *)
type attempt_outcome =
  | Attempt_succeeded
  | Attempt_failed
  | Attempt_timed_out
  | Attempt_cancelled

(** Kind of transition to a corrective attempt. *)
type retry_kind = Fresh_retry | Resume_retry

(** Redacted reason category for a retry. No backend text is retained. *)
type retry_reason =
  | Schema_validation
  | Resume_rejected
  | Transport_retry
  | Other_redacted

(** Process completion projected without raw status text. [Exited code] accepts
    every non-negative host integer; the host-neutral contract deliberately does
    not impose a Unix-specific 0--255 upper bound. *)
type process_exit = Exited of int | Signaled | Unknown

type tool
(** Opaque validated public tool identity. Arguments and backend payloads are
    not representable. *)

val make_tool : ?id:string -> name:string -> unit -> (tool, string) result
(** Construct a tool identity. The optional id and required name use the bounded
    portable identifier alphabet. *)

val tool_id : tool -> string option
(** Optional public tool-call identifier. *)

val tool_name : tool -> string
(** Public tool name. *)

type omission_counts
(** Omission summary copied from a bounded event delivery layer. All values must
    already be safely saturated by the producer. *)

val make_omission_counts :
  ?text_events:int64 ->
  ?text_bytes:int64 ->
  ?usage_events:int64 ->
  ?session_events:int64 ->
  ?tool_events:int64 ->
  ?control_events:int64 ->
  unit ->
  (omission_counts, string) result
(** Construct non-negative omission counts. Every omitted field defaults to
    zero. *)

val omitted_text_events : omission_counts -> int64
(** Number of omitted text events. *)

val omitted_text_bytes : omission_counts -> int64
(** Number of omitted public text bytes. *)

val omitted_usage_events : omission_counts -> int64
(** Number of omitted usage observations. *)

val omitted_session_events : omission_counts -> int64
(** Number of omitted session observations. *)

val omitted_tool_events : omission_counts -> int64
(** Number of omitted tool observations. *)

val omitted_control_events : omission_counts -> int64
(** Number of omitted lifecycle/control observations. *)

(** Terminal result of the whole agent execution. Failure diagnostics live in
    {!Agent_execution}; event traces retain only the stable outcome class. *)
type terminal = Succeeded | Failed | Timed_out | Cancelled

(** Safe normalized event vocabulary. [Agent_text_delta] is public assistant
    output and is subject to {!max_text_bytes}. [Usage_observed] values are
    cumulative snapshots within one attempt, not deltas: repeated known token or
    cost dimensions must be non-decreasing. [Opaque_backend_observation]
    deliberately has no payload. *)
type payload =
  | Task_started
  | Backend_selected of string
  | Preflight_started
  | Preflight_completed
  | Version_probe_started
  | Version_probe_completed
  | Availability_check_started
  | Availability_check_completed
  | Attempt_started of attempt_kind
  | Attempt_finished of attempt_outcome
  | Retry_transition of { kind : retry_kind; reason : retry_reason }
  | Process_started
  | Process_termination_requested
  | Process_kill_escalated
  | Process_exited of process_exit
  | Session_id of string
  | Agent_text_delta of string
  | Tool_started of tool
  | Tool_finished of { id : string option; name : string option }
  | Usage_observed of {
      usage : Execution_metrics.usage option;
      cost : Execution_metrics.cost option;
    }
  | Delivery_truncated of omission_counts
  | Opaque_backend_observation
  | Terminal of terminal

type t
(** One validated event. Sequence numbers are positive and attempt numbers are
    non-negative; attempt zero is used for pre-dispatch lifecycle events. *)

val max_events : int
(** Maximum number of retained events in one trace, including its terminal. *)

val max_text_bytes : int
(** Maximum UTF-8 byte length of one {!Agent_text_delta}. *)

val max_trace_projection_bytes : int
(** Maximum byte length guaranteed for a serialized safe trace projection. *)

val make :
  seq:int64 -> attempt:int -> elapsed_s:float -> payload -> (t, string) result
(** [make ~seq ~attempt ~elapsed_s payload] validates one safe event.
    [elapsed_s] must be finite and non-negative. Identifiers are restricted to a
    bounded portable alphabet, omission counts must be non-negative, and text
    must be valid UTF-8 and within {!max_text_bytes}. Diagnostics never quote
    rejected values. *)

val seq : t -> int64
(** Event sequence number. *)

val attempt : t -> int
(** One-based backend attempt number, or zero before any attempt starts. *)

val elapsed_s : t -> float
(** Finite non-negative monotonic elapsed seconds from task start. *)

val payload : t -> payload
(** Normalized event payload. *)

type trace
(** Opaque validated completed trace. A present trace always contains exactly
    one terminal event and that terminal is last. Retained lifecycle events form
    a valid subsequence of a complete execution lifecycle. *)

val make_trace : ?omitted_count:int64 -> t list -> (trace, string) result
(** [make_trace ?omitted_count events] validates a completed trace. Events must
    be non-empty, contain at most {!max_events} entries, have strictly
    increasing sequence numbers, non-decreasing attempt numbers and elapsed
    times, and have exactly one terminal as the final entry. [omitted_count]
    defaults to zero and must be non-negative. Gaps in sequence numbers and
    omitted lifecycle prefixes are allowed: an absent start/completion/process
    event is treated as unknown, not as proof it did not occur. Visible
    contradictions are rejected, including repeated starts/finishes, completion
    before a later start, attempt activity after finish/exit/retry, decreasing
    cumulative usage observations, negative process exit codes, process
    termination out of order, a retry kind inconsistent with the next retained
    attempt start, and pre-dispatch events carrying nonzero attempt numbers. The
    retained trace must also fit {!max_trace_projection_bytes}; its exact JSON
    encoding size, including escaping, is accounted without serializing it. *)

val events : trace -> t list
(** Retained events in chronological order. *)

val omitted_count : trace -> int64
(** Number of events omitted by collection after delivery. *)

val trace_to_yojson : trace -> Yojson.Safe.t
(** Stable redacted JSON persistence projection. The root carries schema version
    [cwr.workflow-event-trace/v1]. The projection contains only the typed fields
    above; it cannot contain an opaque backend payload. *)
