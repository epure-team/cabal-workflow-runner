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
    bounds and a non-negative compact-encoded-byte bound. Iterative saturating
    accounting includes string/key escaping, separators, and delimiters without
    first serializing the value. Resource-limit diagnostics are fixed;
    value-error paths are capped and include only short portable object keys,
    never an unbounded attacker-controlled key. *)

val to_string : Yojson.Safe.t -> (string, string) result
(** Serialize the restricted profile with recursively sorted object keys. Exact
    compact-encoded size is checked iteratively before normalization or output
    allocation; the serializer receives that bounded size as its buffer
    capacity. *)
