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

type step =
  | Agent of {
      id : string;
      prompt : string;
      read_only : bool;
      output_schema : Schema.t option;
    }
  | Gate of { id : string; when_ : Expr.t }
  | Branch of { when_ : Expr.t; then_ : step list; else_ : step list }
  | Loop of {
      body : step list;
      until : Expr.t option;
      governors : governor list;
    }
  | Commit of { id : string }

and governor =
  | Max_iters of int
  | Budget
  | Fixpoint of { window : int; progress : Expr.t }

type workflow = { name : string; steps : step list }

type gate_verdict =
  | Pass
  | Fail

type outcome =
  | Committed of { id : string; token_digest : string }
  | Completed_no_commit
  | Blocked of string
  | Aborted of string

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
