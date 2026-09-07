(** Validated usage and monetary metrics for rich agent execution.

    Counts and money are deliberately represented with [int64], never floats.
    Monetary values are integer micro-US-dollars (USD 10^-6). Optional fields
    preserve the distinction between an unavailable metric and a reported zero.
*)

type usage
(** Opaque token-usage telemetry. Each field is independently optional. *)

val make_usage :
  ?input_tokens:int64 ->
  ?output_tokens:int64 ->
  ?cache_creation_tokens:int64 ->
  ?cache_read_tokens:int64 ->
  unit ->
  (usage, string) result
(** [make_usage ()] constructs usage telemetry. Every supplied count must be
    non-negative. An all-unknown value is valid and remains distinguishable from
    an absent [usage option]. *)

val input_tokens : usage -> int64 option
(** Reported input-token count, or [None] when unknown. *)

val output_tokens : usage -> int64 option
(** Reported output-token count, or [None] when unknown. *)

val cache_creation_tokens : usage -> int64 option
(** Reported cache-creation token count, or [None] when unknown. *)

val cache_read_tokens : usage -> int64 option
(** Reported cache-read token count, or [None] when unknown. *)

val aggregate_usages : usage option list -> usage option
(** [aggregate_usages values] sums each known field independently and saturates
    at [Int64.max_int] rather than wrapping. It returns [None] only when every
    input is [None]; a present all-unknown usage remains present. *)

type cost
(** Opaque monetary telemetry. The optional amount is integer micro-USD. *)

val make_cost : ?usd_micros:int64 -> unit -> (cost, string) result
(** [make_cost ?usd_micros ()] constructs monetary telemetry. A supplied amount
    must be non-negative. Omitting it records a present but unknown cost. *)

val usd_micros : cost -> int64 option
(** Reported cost in micro-USD, or [None] when unknown. *)

val aggregate_costs : cost option list -> cost option
(** [aggregate_costs values] sums known micro-USD amounts with saturation at
    [Int64.max_int]. It returns [None] only when every input is [None]. *)
