type t = {
  run_agent :
    id:string -> prompt:string -> read_only:bool -> bool * Yojson.Safe.t;
  budget : unit -> int;
}

(* By default every agent succeeds, returning an empty JSON object. *)
let default_agent ~id:_ ~prompt:_ ~read_only:_ = (true, `Assoc [])

(* By default the budget is effectively unbounded (a large constant). Tests that
   want [Budget] to force termination supply a decrementing stub. *)
let default_budget () = max_int

let stub ?(agent = default_agent) ?(budget = default_budget) () =
  { run_agent = agent; budget }
