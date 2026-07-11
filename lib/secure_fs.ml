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

type ledger_handle = { fd : int; path : string; device : string; inode : string }
external ledger_open_raw : string -> int * string * string = "cwr_secure_ledger_open"
external ledger_write_raw : int -> string -> string -> unit = "cwr_secure_ledger_write"
external ledger_flush_raw : int -> string -> unit = "cwr_secure_ledger_flush"
external ledger_identity_matches_raw : string -> string -> string -> bool = "cwr_secure_ledger_identity_matches"
external ledger_close_raw : int -> unit = "cwr_secure_ledger_close"

let ledger_open path = Result.map (fun (fd, device, inode) -> { fd; path; device; inode })
  (protect (fun () -> ledger_open_raw path))
let ledger_write handle ~phase content = protect (fun () -> ledger_write_raw handle.fd content phase)
let ledger_flush handle ~phase = protect (fun () -> ledger_flush_raw handle.fd phase)
let ledger_identity_matches handle = protect (fun () ->
  ledger_identity_matches_raw handle.path handle.device handle.inode)
let ledger_close handle = protect (fun () -> ledger_close_raw handle.fd)
