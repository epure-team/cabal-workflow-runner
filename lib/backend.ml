type t = {
  run_agent :
    id:string -> prompt:string -> read_only:bool -> bool * Yojson.Safe.t;
  budget : unit -> int;
  run_command :
    id:string ->
    argv:string list ->
    working_dir:string ->
    timeout_ms:int option ->
    observe:string list option ->
    Types.run_result;
}

(* By default every agent succeeds, returning an empty JSON object. *)
let default_agent ~id:_ ~prompt:_ ~read_only:_ = (true, `Assoc [])

(* By default the budget is effectively unbounded (a large constant). Tests that
   want [Budget] to force termination supply a decrementing stub. *)
let default_budget () = max_int

(* By default a [Run] step is a no-op success: exit 0, empty output, no files.
   The real process execution lives in [bin/]; tests inject a deterministic
   [run_command]. The lib never spawns a process. *)
let default_run_command ~id:_ ~argv:_ ~working_dir:_ ~timeout_ms:_ ~observe:_ :
    Types.run_result =
  { Types.exit = 0; stdout = ""; stderr = ""; truncated = false; files = [] }

let stub ?(agent = default_agent) ?(budget = default_budget)
    ?(run_command = default_run_command) () =
  { run_agent = agent; budget; run_command }
