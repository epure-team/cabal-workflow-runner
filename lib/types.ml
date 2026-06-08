type gate_id = string

type step =
  | Agent of { id : string; prompt : string; read_only : bool }
  | Gate of { id : gate_id }
  | Branch of { on : gate_id; then_ : step list; else_ : step list }
  | Loop of { max_iters : int; until : gate_id; body : step list }
  | Commit of { id : string }

type workflow = { name : string; steps : step list }

type gate_verdict =
  | Pass
  | Fail

type outcome =
  | Committed of { id : string; token_digest : string }
  | Completed_no_commit
  | Blocked of string
  | Aborted of string

type trace_entry =
  | Agent_ran of { id : string; success : bool; text : string }
  | Gate_evaluated of { id : gate_id; verdict : gate_verdict }
  | Committed_step of { id : string; token_digest : string }
  | Blocked_at of { id : string; reason : string }

type trace = trace_entry list

let verdict_to_string = function Pass -> "pass" | Fail -> "fail"

let string_of_outcome = function
  | Committed { id; token_digest } ->
      Printf.sprintf "Committed(%s, digest=%s)" id token_digest
  | Completed_no_commit -> "Completed_no_commit"
  | Blocked reason -> Printf.sprintf "Blocked(%s)" reason
  | Aborted reason -> Printf.sprintf "Aborted(%s)" reason
