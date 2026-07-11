external read_regular_raw : string -> string = "cwr_secure_read_regular"
external read_unaliased_regular_raw : string -> string =
  "cwr_secure_read_unaliased_regular"
external write_atomic_raw : string -> string -> string -> int =
  "cwr_secure_write_atomic"
type lock_identity = { device : string; inode : string }

external lock_acquire_raw : string -> string -> int * string * string =
  "cwr_secure_lock_acquire"
external lock_release_raw : int -> unit = "cwr_secure_lock_release"
external lock_identity_matches_raw : string -> string -> string -> string -> bool =
  "cwr_secure_lock_identity_matches"

let protect f =
  try Ok (f ()) with Failure msg -> Error msg | Unix.Unix_error (e, fn, p) ->
    Error (Printf.sprintf "%s(%s): %s" fn p (Unix.error_message e))

let read_regular path = protect (fun () -> read_regular_raw path)
let read_unaliased_regular path =
  protect (fun () -> read_unaliased_regular_raw path)

let write_atomic_noreplace ~root ~relative ~content =
  Result.map (function 0 -> `Written | 1 -> `Published_uncertain
    | n -> failwith (Printf.sprintf "invalid secure-write result %d" n))
    (protect (fun () -> write_atomic_raw root relative content))

let with_exclusive_lock ~root ~relative f =
  match protect (fun () -> lock_acquire_raw root relative) with
  | Error _ as error -> error
  | Ok (fd, device, inode) ->
      Fun.protect
        ~finally:(fun () -> lock_release_raw fd)
        (fun () -> Ok (f { device; inode }))

let lock_identity_matches ~root ~relative identity =
  protect (fun () -> lock_identity_matches_raw root relative
    identity.device identity.inode)
