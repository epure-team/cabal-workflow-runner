(** Optional host-owned provider observations. This stream is separate from the
    engine ledger and cannot influence replay or grant execution authority. *)
type t
type call
type usage = {
  input_tokens : int option;
  output_tokens : int option;
  cache_read_tokens : int option;
  cache_write_tokens : int option;
}
exception Sink_error
val open_sink : path:string -> run_id:string -> (t, string) result

(** Persist and fsync start before returning. Calls get unique persisted
    occurrence IDs across appends/restarts. A write/identity failure raises
    [Sink_error], so the caller must not dispatch. *)
val start : t -> step:string -> backend:string option -> model:string option -> call

(** Persist a sanitized final observation before returning. No prose/raw output
    is accepted. Missing usage remains null; failure usage is marked partial.
    Invalid numeric or identifier values are omitted. Raises [Sink_error]. *)
val finish : t -> ?backend:string -> call -> outcome:string -> exit_code:int option ->
  session_id:string option -> duration_ms:float option -> usage:usage option -> unit

val close : t -> (unit, string) result
