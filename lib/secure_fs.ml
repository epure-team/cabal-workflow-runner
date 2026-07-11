external read_regular_raw : string -> string = "cwr_secure_read_regular"
external write_atomic_raw : string -> string -> string -> int =
  "cwr_secure_write_atomic"

let protect f =
  try Ok (f ()) with Failure msg -> Error msg | Unix.Unix_error (e, fn, p) ->
    Error (Printf.sprintf "%s(%s): %s" fn p (Unix.error_message e))

let read_regular path = protect (fun () -> read_regular_raw path)

let write_atomic_noreplace ~root ~relative ~content =
  Result.map (function 0 -> `Written | 1 -> `Published_uncertain
    | n -> failwith (Printf.sprintf "invalid secure-write result %d" n))
    (protect (fun () -> write_atomic_raw root relative content))
