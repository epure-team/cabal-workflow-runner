val read_regular : string -> (string, string) result
val read_unaliased_regular : string -> (string, string) result
(* Reads exact bytes through an O_NOFOLLOW descriptor and rejects files with
   more than one hard link. Intended for executable/content identity pins. *)
val write_atomic_noreplace : root:string -> relative:string -> content:string ->
  ([ `Written | `Published_uncertain ], string) result
type lock_identity = { device : string; inode : string }

(** Securely open/create a regular, non-symlink file beneath [root], acquire an
    exclusive whole-file BSD advisory lock without waiting, call the callback
    while holding it, and release it afterward. A busy lock, path traversal, or
    non-regular target fails closed. *)
val with_exclusive_lock : root:string -> relative:string ->
  (lock_identity -> 'a) ->
  ('a, string) result
val lock_identity_matches : root:string -> relative:string -> lock_identity ->
  (bool, string) result
