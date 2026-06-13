module Schema = struct
  type ty =
    | String
    | Int
    | Number
    | Bool
    | Enum of string list
    | Any

  type t = (string * ty) list

  let string_of_ty = function
    | String -> "string"
    | Int -> "int"
    | Number -> "number"
    | Bool -> "bool"
    | Enum opts -> "enum[" ^ String.concat "," opts ^ "]"
    | Any -> "any"

  (* Fail-closed: every required field must be present AND type-correct.
     Returns [Error field] naming the first offending field. *)
  let validate (schema : t) (json : Yojson.Safe.t) : (unit, string) result =
    let field_value key =
      match json with
      | `Assoc fields -> List.assoc_opt key fields
      | _ -> None
    in
    let ok_ty ty v =
      match (ty, v) with
      | String, `String _ -> true
      | Int, `Int _ -> true
      | Number, (`Int _ | `Float _) -> true
      | Bool, `Bool _ -> true
      | Enum opts, `String s -> List.mem s opts
      | Any, _ -> true
      | _ -> false
    in
    let rec go = function
      | [] -> Ok ()
      | (field, ty) :: rest -> (
          match field_value field with
          | None -> Error field
          | Some v -> if ok_ty ty v then go rest else Error field)
    in
    go schema
end

type on_failure = Abort | Continue

type step =
  | Agent of {
      id : string;
      prompt : string;
      read_only : bool;
      output_schema : Schema.t option;
      on_failure : on_failure;
    }
  | Gate of { id : string; when_ : Expr.t }
  | Branch of { when_ : Expr.t; then_ : step list; else_ : step list }
  | Loop of {
      body : step list;
      until : Expr.t option;
      governors : governor list;
    }
  | Run of {
      id : string;
      cmd : string list;
      working_dir : string;
      timeout_ms : int option;
      observe : string list option;
    }
  | Commit of { id : string }
  | Parallel of { branches : step list list }
  | Foreach of { over : string; steps : step list }

and governor =
  | Max_iters of int
  | Budget
  | Fixpoint of { window : int; progress : Expr.t }

type workflow = { name : string; steps : step list; version : string option }

type gate_verdict =
  | Pass
  | Fail

type outcome =
  | Committed of { id : string; token_digest : string }
  | Completed_no_commit
  | Blocked of string
  | Aborted of string

(* A single observed filesystem change produced by a [Run] step. *)
type file_change_kind =
  | Created
  | Modified
  | Deleted

type file_change = {
  path : string;  (** relative to the run step's [working_dir]. *)
  change : file_change_kind;
  size : int;  (** post-change size in bytes (0 for [Deleted]). *)
  digest : string;
      (** post-change MD5 content digest ("" for [Deleted]). For
          change-detection / observability only — NOT a cryptographic integrity
          guarantee. *)
}

(* The structured result of executing a [Run] step's command. Stdout/stderr are
   size-capped by the runner; [truncated] flags that a cap was hit. *)
type run_result = {
  exit : int;
  stdout : string;
  stderr : string;
  truncated : bool;
  files : file_change list;
}

let string_of_file_change_kind = function
  | Created -> "created"
  | Modified -> "modified"
  | Deleted -> "deleted"

(* Bind a [run_result] into the run context as JSON under [outputs.<id>] so the
   DSL can read [outputs.<id>.exit], [exists(outputs.<id>.files)], etc. *)
let json_of_file_change (fc : file_change) : Yojson.Safe.t =
  `Assoc
    [
      ("path", `String fc.path);
      ("change", `String (string_of_file_change_kind fc.change));
      ("size", `Int fc.size);
      ("digest", `String fc.digest);
    ]

let json_of_run_result (r : run_result) : Yojson.Safe.t =
  `Assoc
    [
      ("exit", `Int r.exit);
      ("stdout", `String r.stdout);
      ("stderr", `String r.stderr);
      ("truncated", `Bool r.truncated);
      ("files", `List (List.map json_of_file_change r.files));
    ]

(* Everything a stop / branch decision READS is recorded, in order, so replay is
   byte-identical and the (data-driven, possibly unbounded) loop bound is purely
   a function of recorded inputs. *)
type trace_entry =
  | Agent_ran of { id : string; success : bool; output : Yojson.Safe.t }
  | Gate_evaluated of { id : string; verdict : gate_verdict }
  | Branch_taken of { verdict : gate_verdict }
  | Loop_iter of { index : int }  (** start of loop iteration [index] (0-based). *)
  | Budget_read of { value : int }  (** a [backend.budget ()] reading. *)
  | Fixpoint_progress of { progress : bool }  (** a Fixpoint progress verdict. *)
  | Loop_stopped of { iterations : int; reason : string }
  | Run_executed of { id : string; result : run_result }
      (** A [Run] step's command executed once; the full result is recorded so
          replay re-binds it WITHOUT re-executing the command. *)
  | Committed_step of { id : string; token_digest : string }
  | Blocked_at of { id : string; reason : string }
  | Parallel_started
  | Parallel_branch_completed of {
      branch_idx : int;
      trace : trace_entry list;
      outcome : outcome;
      branch_outputs : (string * Yojson.Safe.t) list;
          (** Snapshot of ctx["outputs"] at branch completion; used by replay
              to reconstruct host ctx without re-walking the sub-trace. *)
    }
  | Parallel_completed of { outcome : outcome }
  | Foreach_iter_started of { index : int; element : Yojson.Safe.t }
  | Foreach_iter_completed of { index : int; outcome : outcome }
  | Foreach_completed of { iterations : int }
  | Ctx_snapshot of { ctx : (string * Yojson.Safe.t) list }
      (** Ledger-layer header recording the initial_ctx that was passed to
          [Engine.run]. Written as the FIRST line of an on-disk ledger so that
          [replay --ledger file] can reconstruct the same initial context.
          NOT emitted by the engine and NOT fed to [Engine.replay] as a trace
          entry — it is stripped by [cmd_replay] before parsing the trace. *)

type trace = trace_entry list

let verdict_to_string = function Pass -> "pass" | Fail -> "fail"

let string_of_outcome = function
  | Committed { id; token_digest } ->
      Printf.sprintf "Committed(%s, digest=%s)" id token_digest
  | Completed_no_commit -> "Completed_no_commit"
  | Blocked reason -> Printf.sprintf "Blocked(%s)" reason
  | Aborted reason -> Printf.sprintf "Aborted(%s)" reason
