type usage = {
  input_tokens : int option; output_tokens : int option;
  cache_read_tokens : int option; cache_write_tokens : int option;
}
type t = { handle : Secure_fs.ledger_handle; run_id : string; mutable seq : int; mutable failed : bool }
type call = { id : string; step : string option; backend : string option;
              model : string option; started : float; seq : int; mutable finished : bool }
exception Sink_error

let safe_identifier s =
  String.length s > 0 && String.length s <= 128
  && not (List.exists (fun prefix -> String.starts_with ~prefix s)
            ["sk-"; "lin_api_"; "glpat-"; "ghp_"; "gho_"; "ghu_"; "ghs_"; "ghr_"; "Bearer"; "eyJ"])
  && String.for_all (function 'a'..'z' | 'A'..'Z' | '0'..'9' | '-' | '_' | '.' | ':' | '/' -> true | _ -> false) s
let safe = function Some s when safe_identifier s -> Some s | _ -> None
let string = function Some s -> `String s | None -> `Null
let count = function Some n when n >= 0 -> `Int n | _ -> `Null
let duration = function Some n when Float.is_finite n && n >= 0. -> `Float n | _ -> `Null

let open_sink ~path ~run_id =
  if not (safe_identifier run_id) then Error "invalid telemetry run identifier"
  else match Secure_fs.ledger_open_append path with
  | Error _ -> Error "cannot open private telemetry sink"
  | Ok handle ->
      let result =
        match Secure_fs.read_regular path with
        | Error _ -> Error "cannot read telemetry sink"
        | Ok raw when String.length raw > 16 * 1024 * 1024 -> Error "telemetry sink exceeds 16 MiB"
        | Ok raw when raw <> "" && raw.[String.length raw - 1] <> '\n' ->
            Error "telemetry sink has an unterminated event"
        | Ok raw ->
            try
              let seq = String.split_on_char '\n' raw |> List.filter ((<>) "")
                |> List.fold_left (fun n line ->
                    let j = Yojson.Safe.from_string line in
                    let open Yojson.Safe.Util in
                    if member "run_id" j = `String run_id then
                      max n (member "call_seq" j |> to_int) else n) 0 in
              Ok {handle; run_id; seq; failed=false}
            with _ -> Error "telemetry sink contains incomplete or invalid events"
      in
      (match result with Error _ -> ignore (Secure_fs.ledger_close handle) | Ok _ -> ());
      result

let write t fields =
  let fail () = t.failed <- true; raise Sink_error in
  if t.failed then fail ();
  let valid = match Secure_fs.ledger_identity_matches t.handle with Ok true -> true | _ -> false in
  if not valid then fail ();
  let bytes = Yojson.Safe.to_string (`Assoc fields) ^ "\n" in
  match Secure_fs.ledger_write t.handle ~phase:"telemetry" bytes with
  | Error _ -> fail ()
  | Ok () ->
      (match Secure_fs.ledger_flush t.handle ~phase:"telemetry" with
       | Ok () -> () | Error _ -> fail ())

let common t call typ suffix now = [
  "schema_version", `Int 1; "type", `String typ;
  "event_id", `String (call.id ^ suffix); "run_id", `String t.run_id;
  "call_id", `String call.id; "call_seq", `Int call.seq;
  "step", string call.step; "backend", string call.backend;
  "provider", `Null;
  "requested_model", string call.model; "observed_model", `Null;
  "parent_call_id", `Null; "recorded_at", `Float now;
  "origin", `String "cwr-host" ]

let start (t : t) ~step ~backend ~model =
  t.seq <- t.seq + 1;
  let call = {id = Printf.sprintf "%s:call:%d" t.run_id t.seq;
    step = safe (Some step); backend = safe backend; model = safe model;
    started = Unix.gettimeofday (); seq = t.seq; finished=false} in
  write t (common t call "call.started" ":start" call.started @ ["started_at", `Float call.started]);
  call

let finish t ?backend call ~outcome ~exit_code ~session_id ~duration_ms ~usage =
  if call.finished then (t.failed <- true; raise Sink_error);
  let outcome = if List.mem outcome ["ok"; "failed"; "timeout"; "cancelled"; "refused"] then outcome else "unknown" in
  let u = Option.value usage ~default:{input_tokens=None;output_tokens=None;cache_read_tokens=None;cache_write_tokens=None} in
  let basis = match usage with
    | None -> "unknown"
    | Some _ when outcome = "ok" && count u.input_tokens <> `Null && count u.output_tokens <> `Null -> "observed"
    | Some _ -> "partial" in
  let now = Unix.gettimeofday () in
  let final_call = match backend with None -> call | Some backend -> {call with backend=safe (Some backend)} in
  write t (common t final_call "call.finished" ":finish" now @ [
    "outcome", `String outcome; "exit_code", (match exit_code with Some n -> `Int n | None -> `Null);
    "session_id", string (safe session_id);
    "usage", `Assoc ["input_tokens", count u.input_tokens;"output_tokens", count u.output_tokens;
      "cache_read_tokens", count u.cache_read_tokens;"cache_write_tokens", count u.cache_write_tokens;
      "usage_basis", `String basis; "input_semantics", `String "unknown";
      "scope", `String "invocation"; "aggregation", `String "direct"];
    "cost", `Assoc ["cost_usd", `Null; "basis", `String "unknown"];
    "timing", `Assoc ["started_at", `Float call.started; "ended_at", `Float now;
      "duration_ms", duration duration_ms; "duration_source", `String "cabal-elapsed-wall-clock";
      "timing_basis", `String (if duration duration_ms = `Null then "unknown" else "measured")]]);
  call.finished <- true

let close t =
  let closed = Secure_fs.ledger_close t.handle in
  if t.failed then Error "telemetry sink failed" else closed
