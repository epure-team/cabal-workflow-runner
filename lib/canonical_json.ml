let valid_utf8 s = String.is_valid_utf_8 s

let validate_with ~restricted json =
  let rec go path = function
    | `Null | `Bool _ -> Ok ()
    | `Int n when restricted &&
        (n < -9007199254740991 || n > 9007199254740991) ->
        Error (path ^ ": integer exceeds the cross-runtime safe range")
    | `Int _ -> Ok ()
    | `String s ->
        if valid_utf8 s then Ok () else Error (path ^ ": invalid UTF-8 string")
    | `List values ->
        let rec all i = function
          | [] -> Ok ()
          | value :: rest ->
              Result.bind (go (Printf.sprintf "%s[%d]" path i) value)
                (fun () -> all (i + 1) rest)
        in
        all 0 values
    | `Assoc fields ->
        let keys = List.map fst fields in
        if List.exists (fun key -> not (valid_utf8 key)) keys then
          Error (path ^ ": invalid UTF-8 object key")
        else if List.length keys <> List.length (List.sort_uniq String.compare keys) then
          Error (path ^ ": duplicate object key")
        else
          let rec all = function
            | [] -> Ok ()
            | (key, value) :: rest ->
                Result.bind (go (path ^ "." ^ key) value) (fun () -> all rest)
          in
          all fields
    | `Float _ when restricted -> Error (path ^ ": floats are not canonical; use an integer")
    | `Intlit _ when restricted -> Error (path ^ ": integer literals outside native range are not canonical")
    | `Float _ | `Intlit _ -> Ok ()
  in
  go "$" json

let validate json = validate_with ~restricted:true json
let validate_no_duplicates json = validate_with ~restricted:false json

let rec normalize = function
  | `Assoc fields ->
      `Assoc (fields |> List.map (fun (k, v) -> k, normalize v)
              |> List.sort (fun (a, _) (b, _) -> String.compare a b))
  | `List values -> `List (List.map normalize values)
  | value -> value

let to_string json =
  Result.map (fun () -> Yojson.Safe.to_string (normalize json)) (validate json)
