open Types

type t = {
  run_agent : id:string -> prompt:string -> read_only:bool -> bool * string;
  eval_gate : gate_id -> gate_verdict;
}

let default_gate (_ : gate_id) = Pass

let default_agent ~id ~prompt:_ ~read_only:_ = (true, id)

let stub ?(gate = default_gate) ?(agent = default_agent) () =
  { run_agent = agent; eval_gate = gate }
