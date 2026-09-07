let valid_utf8 s = String.is_valid_utf_8 s
let max_depth = 128
let max_nodes = 100_000
let max_canonical_bytes = 64 * 1024 * 1024

let finite value =
  match classify_float value with
  | FP_normal | FP_subnormal | FP_zero -> true
  | FP_infinite | FP_nan -> false

let valid_integer_literal literal =
  let length = String.length literal in
  let first_digit = if length > 0 && literal.[0] = '-' then 1 else 0 in
  first_digit < length
  && (length - first_digit = 1 || literal.[first_digit] <> '0')
  &&
  let valid = ref true in
  let index = ref first_digit in
  while !valid && !index < length do
    (match literal.[!index] with '0' .. '9' -> () | _ -> valid := false);
    incr index
  done;
  !valid

let float_needs_period value =
  let needs_period = ref true in
  let index = ref 0 in
  while !needs_period && !index < String.length value do
    (match value.[!index] with
    | '0' .. '9' | '-' -> ()
    | _ -> needs_period := false);
    incr index
  done;
  !needs_period

(* Mirrors Yojson.Safe's compact finite-float writer without allocating a JSON
   buffer. Boundary tests compare the result with the installed writer. *)
let float_encoded_length value =
  let short = Printf.sprintf "%.16g" value in
  let encoded =
    if float_of_string short = value then short
    else Printf.sprintf "%.17g" value
  in
  String.length encoded + if float_needs_period encoded then 2 else 0

let max_diagnostic_path_bytes = 256

let diagnostic_key key =
  if
    key <> ""
    && String.length key <= 64
    && String.for_all
         (function
           | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' -> true
           | _ -> false)
         key
  then key
  else "<key>"

let append_diagnostic_path path component =
  let separator = if component.[0] = '[' then "" else "." in
  let addition = separator ^ component in
  if String.length path + String.length addition <= max_diagnostic_path_bytes
  then path ^ addition
  else path

let diagnostic path message = Error (path ^ ": " ^ message)

let validate_and_measure ~restricted ~max_depth ~max_nodes ~max_bytes json =
  if max_depth <= 0 || max_nodes <= 0 then Error "invalid JSON resource limit"
  else if max_bytes < 0 then Error "invalid JSON byte limit"
  else
    let bytes = ref 0 in
    let add_bytes amount =
      if amount > max_bytes - !bytes then false
      else (
        bytes := !bytes + amount;
        true)
    in
    let add_string_bytes value =
      (* This is Yojson.Safe's compact string escaping table. *)
      if not (add_bytes 2) then false
      else
        let within_limit = ref true in
        let index = ref 0 in
        while !within_limit && !index < String.length value do
          let encoded_bytes =
            match value.[!index] with
            | '"' | '\\' | '\b' | '\012' | '\n' | '\r' | '\t' -> 2
            | '\x00' .. '\x1F' | '\x7F' -> 6
            | _ -> 1
          in
          within_limit := add_bytes encoded_bytes;
          incr index
        done;
        !within_limit
    in
    let stack = ref [ ("$", 1, json) ] in
    let queued = ref 1 in
    let seen = ref 0 in
    let enqueue path depth child =
      if !seen + !queued >= max_nodes then Error "$: JSON node limit exceeded"
      else (
        stack := (path, depth, child) :: !stack;
        incr queued;
        Ok ())
    in
    let rec enqueue_list path depth index = function
      | [] -> Ok ()
      | child :: rest ->
          if index > 0 && not (add_bytes 1) then
            Error "$: JSON byte limit exceeded"
          else
            let child_path =
              append_diagnostic_path path (Printf.sprintf "[%d]" index)
            in
            Result.bind (enqueue child_path depth child) (fun () ->
                enqueue_list path depth (index + 1) rest)
    in
    let enqueue_fields path depth fields =
      let keys = Hashtbl.create 16 in
      let rec loop index = function
        | [] -> Ok ()
        | (key, child) :: rest ->
            if not (valid_utf8 key) then Error "$: invalid UTF-8 object key"
            else if Hashtbl.mem keys key then Error "$: duplicate object key"
            else if index > 0 && not (add_bytes 1) then
              Error "$: JSON byte limit exceeded"
            else (
              Hashtbl.add keys key ();
              if not (add_string_bytes key && add_bytes 1) then
                Error "$: JSON byte limit exceeded"
              else
                let child_path =
                  append_diagnostic_path path (diagnostic_key key)
                in
                Result.bind (enqueue child_path depth child) (fun () ->
                    loop (index + 1) rest))
      in
      loop 0 fields
    in
    let rec loop () =
      match !stack with
      | [] -> Ok !bytes
      | (path, depth, value) :: rest ->
          stack := rest;
          decr queued;
          if depth > max_depth then Error "$: JSON nesting limit exceeded"
          else (
            incr seen;
            match value with
            | `Null ->
                if add_bytes 4 then loop ()
                else Error "$: JSON byte limit exceeded"
            | `Bool value ->
                if add_bytes (if value then 4 else 5) then loop ()
                else Error "$: JSON byte limit exceeded"
            | `Int value ->
                if
                  restricted
                  && (value < -9007199254740991 || value > 9007199254740991)
                then
                  diagnostic path "integer exceeds the cross-runtime safe range"
                else if add_bytes (String.length (string_of_int value)) then
                  loop ()
                else Error "$: JSON byte limit exceeded"
            | `String value ->
                if not (valid_utf8 value) then
                  diagnostic path "invalid UTF-8 string"
                else if add_string_bytes value then loop ()
                else Error "$: JSON byte limit exceeded"
            | `List values ->
                if not (add_bytes 2) then Error "$: JSON byte limit exceeded"
                else if values <> [] && depth >= max_depth then
                  Error "$: JSON nesting limit exceeded"
                else
                  Result.bind
                    (enqueue_list path (depth + 1) 0 values)
                    (fun () -> loop ())
            | `Assoc fields ->
                if not (add_bytes 2) then Error "$: JSON byte limit exceeded"
                else if fields <> [] && depth >= max_depth then
                  Error "$: JSON nesting limit exceeded"
                else
                  Result.bind
                    (enqueue_fields path (depth + 1) fields)
                    (fun () -> loop ())
            | `Float _ when restricted ->
                diagnostic path "floats are not canonical; use an integer"
            | `Intlit _ when restricted ->
                diagnostic path
                  "integer literals outside native range are not canonical"
            | `Float value ->
                if not (finite value) then
                  diagnostic path "non-finite JSON number"
                else if add_bytes (float_encoded_length value) then loop ()
                else Error "$: JSON byte limit exceeded"
            | `Intlit literal ->
                if not (valid_integer_literal literal) then
                  diagnostic path "invalid JSON integer literal"
                else if add_bytes (String.length literal) then loop ()
                else Error "$: JSON byte limit exceeded"
            | `Tuple _ | `Variant _ -> diagnostic path "non-standard JSON value")
    in
    loop ()

let validate_with_limits ~restricted ~max_depth ~max_nodes json =
  Result.map
    (fun _ -> ())
    (validate_and_measure ~restricted ~max_depth ~max_nodes ~max_bytes:max_int
       json)

let validate_with ~restricted json =
  validate_with_limits ~restricted ~max_depth ~max_nodes json

let validate json = validate_with ~restricted:true json
let validate_no_duplicates json = validate_with ~restricted:false json

let validate_standard ~max_depth ~max_nodes ~max_bytes json =
  Result.map
    (fun _ -> ())
    (validate_and_measure ~restricted:false ~max_depth ~max_nodes ~max_bytes
       json)

let rec normalize = function
  | `Assoc fields ->
      `Assoc
        (fields
        |> List.rev_map (fun (k, v) -> (k, normalize v))
        |> List.rev
        |> List.sort (fun (a, _) (b, _) -> String.compare a b))
  | `List values -> `List (values |> List.rev_map normalize |> List.rev)
  | (`Tuple _ | `Variant _) as value -> value
  | value -> value

let to_string json =
  match
    validate_and_measure ~restricted:true ~max_depth ~max_nodes
      ~max_bytes:max_canonical_bytes json
  with
  | Error "$: JSON byte limit exceeded" ->
      Error "$: canonical JSON byte limit exceeded"
  | Error _ as error -> error
  | Ok encoded_size ->
      Ok (Yojson.Safe.to_string ~len:encoded_size (normalize json))
