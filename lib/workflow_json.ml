open Types

(* A tiny fail-closed parser. We use exceptions internally and convert to a
   result at the boundary, so any structural defect yields [Error reason]. *)

exception Parse_error of string

let err fmt = Printf.ksprintf (fun s -> raise (Parse_error s)) fmt

let member_opt key = function
  | `Assoc fields -> List.assoc_opt key fields
  | _ -> None

let req_string key json =
  match member_opt key json with
  | Some (`String s) -> s
  | Some _ -> err "field %S must be a string" key
  | None -> err "missing required field %S" key

let opt_bool key default json =
  match member_opt key json with
  | Some (`Bool b) -> b
  | Some _ -> err "field %S must be a boolean" key
  | None -> default

let req_int key json =
  match member_opt key json with
  | Some (`Int n) -> n
  | Some _ -> err "field %S must be an integer" key
  | None -> err "missing required field %S" key

let req_list key json =
  match member_opt key json with
  | Some (`List l) -> l
  | Some _ -> err "field %S must be a list" key
  | None -> err "missing required field %S" key

let rec step_of_json json =
  let kind = req_string "kind" json in
  match kind with
  | "agent" ->
      Agent
        {
          id = req_string "id" json;
          prompt = req_string "prompt" json;
          read_only = opt_bool "read_only" false json;
        }
  | "gate" -> Gate { id = req_string "id" json }
  | "branch" ->
      Branch
        {
          on = req_string "on" json;
          then_ = List.map step_of_json (req_list "then" json);
          else_ = List.map step_of_json (req_list "else" json);
        }
  | "loop" ->
      Loop
        {
          max_iters = req_int "max_iters" json;
          until = req_string "until" json;
          body = List.map step_of_json (req_list "body" json);
        }
  | "commit" -> Commit { id = req_string "id" json }
  | other -> err "unknown step kind %S" other

let of_json json =
  try
    let name = req_string "name" json in
    let steps = List.map step_of_json (req_list "steps" json) in
    Ok { name; steps }
  with
  | Parse_error msg -> Error msg

let of_string s =
  match Yojson.Safe.from_string s with
  | json -> of_json json
  | exception Yojson.Json_error msg -> Error ("malformed JSON: " ^ msg)

let of_file path =
  match Yojson.Safe.from_file path with
  | json -> of_json json
  | exception Yojson.Json_error msg -> Error ("malformed JSON: " ^ msg)
  | exception Sys_error msg -> Error ("cannot read file: " ^ msg)

let rec step_to_json = function
  | Agent { id; prompt; read_only } ->
      `Assoc
        [
          ("kind", `String "agent");
          ("id", `String id);
          ("prompt", `String prompt);
          ("read_only", `Bool read_only);
        ]
  | Gate { id } -> `Assoc [ ("kind", `String "gate"); ("id", `String id) ]
  | Branch { on; then_; else_ } ->
      `Assoc
        [
          ("kind", `String "branch");
          ("on", `String on);
          ("then", `List (List.map step_to_json then_));
          ("else", `List (List.map step_to_json else_));
        ]
  | Loop { max_iters; until; body } ->
      `Assoc
        [
          ("kind", `String "loop");
          ("max_iters", `Int max_iters);
          ("until", `String until);
          ("body", `List (List.map step_to_json body));
        ]
  | Commit { id } -> `Assoc [ ("kind", `String "commit"); ("id", `String id) ]

let to_json { name; steps } =
  `Assoc
    [ ("name", `String name); ("steps", `List (List.map step_to_json steps)) ]
