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

let validate_with_limits ~restricted ~max_depth ~max_nodes json =
  if max_depth <= 0 || max_nodes <= 0 then Error "invalid JSON resource limit"
  else
    let rec loop seen = function
      | [] -> Ok ()
      | _ when seen >= max_nodes -> Error "$: JSON node limit exceeded"
      | (path, depth, _) :: _ when depth > max_depth ->
          Error (path ^ ": JSON nesting limit exceeded")
      | (path, depth, value) :: rest -> (
          let continue children =
            loop (seen + 1) (List.rev_append (List.rev children) rest)
          in
          match value with
          | `Null | `Bool _ -> continue []
          | `Int n
            when restricted && (n < -9007199254740991 || n > 9007199254740991)
            ->
              Error (path ^ ": integer exceeds the cross-runtime safe range")
          | `Int _ -> continue []
          | `String s ->
              if valid_utf8 s then continue []
              else Error (path ^ ": invalid UTF-8 string")
          | `List values ->
              let _, children =
                List.fold_left
                  (fun (index, children) child ->
                    ( index + 1,
                      (Printf.sprintf "%s[%d]" path index, depth + 1, child)
                      :: children ))
                  (0, []) values
              in
              continue (List.rev children)
          | `Assoc fields ->
              let keys = Hashtbl.create (List.length fields) in
              let rec collect children = function
                | [] -> continue (List.rev children)
                | (key, _) :: _ when not (valid_utf8 key) ->
                    Error (path ^ ": invalid UTF-8 object key")
                | (key, _) :: _ when Hashtbl.mem keys key ->
                    Error (path ^ ": duplicate object key")
                | (key, child) :: fields ->
                    Hashtbl.add keys key ();
                    collect
                      ((path ^ "." ^ key, depth + 1, child) :: children)
                      fields
              in
              collect [] fields
          | `Float _ when restricted ->
              Error (path ^ ": floats are not canonical; use an integer")
          | `Intlit _ when restricted ->
              Error
                (path
               ^ ": integer literals outside native range are not canonical")
          | `Float value ->
              if finite value then continue []
              else Error (path ^ ": non-finite JSON number")
          | `Intlit literal ->
              if valid_integer_literal literal then continue []
              else Error (path ^ ": invalid JSON integer literal")
          | `Tuple _ | `Variant _ -> Error (path ^ ": non-standard JSON value"))
    in
    loop 0 [ ("$", 1, json) ]

let validate_with ~restricted json =
  validate_with_limits ~restricted ~max_depth ~max_nodes json

let validate json = validate_with ~restricted:true json
let validate_no_duplicates json = validate_with ~restricted:false json

let validate_standard ~max_depth ~max_nodes ~max_bytes json =
  if max_bytes < 0 then Error "invalid JSON byte limit"
  else
    Result.bind
      (validate_with_limits ~restricted:false ~max_depth ~max_nodes json)
      (fun () ->
        let serialized = Yojson.Safe.to_string json in
        if String.length serialized > max_bytes then
          Error "$: JSON byte limit exceeded"
        else Ok ())

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
  Result.bind (validate json) (fun () ->
      let serialized = Yojson.Safe.to_string (normalize json) in
      if String.length serialized > max_canonical_bytes then
        Error "$: canonical JSON byte limit exceeded"
      else Ok serialized)
