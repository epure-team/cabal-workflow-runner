val max_depth : int
(** Maximum accepted JSON nesting depth, including the root value. *)

val max_nodes : int
(** Maximum number of values visited by canonical JSON validation. *)

val max_canonical_bytes : int
(** Maximum byte length emitted by {!to_string}. *)

val validate : Yojson.Safe.t -> (unit, string) result
(** Validate the restricted canonical profile with bounded iterative traversal.
*)

val validate_no_duplicates : Yojson.Safe.t -> (unit, string) result
(** Validate finite standard Yojson without duplicate keys or extension values,
    using the same traversal bounds as {!validate}. *)

val validate_standard :
  max_depth:int ->
  max_nodes:int ->
  max_bytes:int ->
  Yojson.Safe.t ->
  (unit, string) result
(** Validate finite standard JSON using caller-supplied positive depth/node
    bounds and a non-negative serialized-byte bound. The traversal is iterative.
*)

val to_string : Yojson.Safe.t -> (string, string) result
(** Serialize the restricted profile with recursively sorted object keys,
    failing when the canonical result exceeds {!max_canonical_bytes}. *)
