type t = {
  run_agent :
    id:string ->
    prompt:string ->
    read_only:bool ->
    agent_type:string option ->
    model:string option ->
    bool * Yojson.Safe.t;
  budget : unit -> int;
  run_command :
    id:string ->
    argv:string list ->
    working_dir:string ->
    timeout_ms:int option ->
    observe:string list option ->
    Types.run_result;
  run_shell_command : string -> int;
      (** Run a shell command string via [sh -c]; return its exit code.
          The lib never spawns a process; this is injected by [bin/]. *)
}

(* By default every agent succeeds, returning an empty JSON object. *)
let default_agent ~id:_ ~prompt:_ ~read_only:_ ~agent_type:_ ~model:_ = (true, `Assoc [])

(* By default the budget is effectively unbounded (a large constant). Tests that
   want [Budget] to force termination supply a decrementing stub. *)
let default_budget () = max_int

(* By default a [Run] step is a no-op success: exit 0, empty output, no files.
   The real process execution lives in [bin/]; tests inject a deterministic
   [run_command]. The lib never spawns a process. *)
let default_run_command ~id:_ ~argv:_ ~working_dir:_ ~timeout_ms:_ ~observe:_ :
    Types.run_result =
  { Types.exit = 0; stdout = ""; stderr = ""; truncated = false; files = [] }

(* By default a [Shell] step command exits 0 (success). Tests that want to
   exercise failure inject a stub that returns non-zero. *)
let default_run_shell_command _cmd = 0

let stub ?(agent = default_agent) ?(budget = default_budget)
    ?(run_command = default_run_command)
    ?(run_shell_command = default_run_shell_command) () =
  { run_agent = agent; budget; run_command; run_shell_command }
