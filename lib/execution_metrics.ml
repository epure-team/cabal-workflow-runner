type usage = {
  input_tokens : int64 option;
  output_tokens : int64 option;
  cache_creation_tokens : int64 option;
  cache_read_tokens : int64 option;
}

let validate_nonnegative name = function
  | Some value when Int64.compare value 0L < 0 ->
      Error (name ^ " must be non-negative")
  | _ -> Ok ()

let make_usage ?input_tokens ?output_tokens ?cache_creation_tokens
    ?cache_read_tokens () =
  Result.bind (validate_nonnegative "input_tokens" input_tokens) (fun () ->
      Result.bind (validate_nonnegative "output_tokens" output_tokens)
        (fun () ->
          Result.bind
            (validate_nonnegative "cache_creation_tokens" cache_creation_tokens)
            (fun () ->
              Result.map
                (fun () ->
                  {
                    input_tokens;
                    output_tokens;
                    cache_creation_tokens;
                    cache_read_tokens;
                  })
                (validate_nonnegative "cache_read_tokens" cache_read_tokens))))

let input_tokens usage = usage.input_tokens
let output_tokens usage = usage.output_tokens
let cache_creation_tokens usage = usage.cache_creation_tokens
let cache_read_tokens usage = usage.cache_read_tokens

let saturating_add left right =
  if Int64.compare left (Int64.sub Int64.max_int right) > 0 then Int64.max_int
  else Int64.add left right

let aggregate_field get values =
  let rec loop total = function
    | [] -> total
    | value :: rest -> (
        match (get value, total) with
        | None, total -> loop total rest
        | Some count, None -> loop (Some count) rest
        | Some count, Some accumulated ->
            loop (Some (saturating_add accumulated count)) rest)
  in
  loop None values

let aggregate_usages values =
  let present = List.filter_map Fun.id values in
  match present with
  | [] -> None
  | _ ->
      Some
        {
          input_tokens = aggregate_field input_tokens present;
          output_tokens = aggregate_field output_tokens present;
          cache_creation_tokens = aggregate_field cache_creation_tokens present;
          cache_read_tokens = aggregate_field cache_read_tokens present;
        }

type cost = { usd_micros : int64 option }

let make_cost ?usd_micros () =
  Result.map
    (fun () -> { usd_micros })
    (validate_nonnegative "usd_micros" usd_micros)

let usd_micros cost = cost.usd_micros

let aggregate_costs values =
  let present = List.filter_map Fun.id values in
  match present with
  | [] -> None
  | _ -> Some { usd_micros = aggregate_field usd_micros present }
