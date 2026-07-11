(** Ed25519-authenticated engine output envelopes. Private key material is
    represented by the abstract [signer] type and never enters {!Backend.t}. *)

type signer
type verifier

val signer_of_seed : string -> (signer, string) result
(** Decode an exact 32-byte Ed25519 seed. *)

val signer_of_fd : Unix.file_descr -> (signer, string) result
(** Read exactly one 32-byte seed, require EOF, and close the descriptor on
    every path. *)

val verifier_of_public_key : string -> (verifier, string) result
(** Decode an exact 32-byte Ed25519 public key. *)

val public_key : signer -> string
val key_id : signer -> string
val workflow_digest : Types.workflow -> string
val public_identity : signer -> Yojson.Safe.t
val verifier_of_identity : Yojson.Safe.t -> (verifier, string) result
val validate_canonical_json : Yojson.Safe.t -> (unit, string) result
val validate_selected : (string * Yojson.Safe.t) list -> (unit, string) result

val create :
  signer:signer ->
  workflow:Types.workflow ->
  step_id:string ->
  occurrence:int ->
  output_path:string ->
  replay_domain:string ->
  session_nonce:string ->
  selected:(string * Yojson.Safe.t) list ->
  (Yojson.Safe.t, string) result
(** Create a canonical signed envelope. *)

val verify :
  verifier:verifier ->
  workflow:Types.workflow ->
  step_id:string ->
  occurrence:int ->
  output_path:string ->
  replay_domain:string ->
  session_nonce:string ->
  selected:(string * Yojson.Safe.t) list ->
  Yojson.Safe.t ->
  (unit, string) result
(** Verify both the signature and every expected binding, fail-closed. *)

val canonical_string : Yojson.Safe.t -> string
(** Deterministic RFC-8259 JSON encoding with recursively sorted object keys. *)

val select_context :
  (string * Yojson.Safe.t) list ->
  string list ->
  ((string * Yojson.Safe.t) list, string) result
(** Resolve dotted context paths exactly, preserving each path as the map key.
*)

val materialize_output_path : template:string -> occurrence:int -> string

type write_error = Failed of string | Published_uncertain of string

val write_atomic :
  artifact_root:string ->
  relative_path:string ->
  Yojson.Safe.t ->
  (unit, write_error) result
(** Atomically write an envelope beneath [artifact_root], rejecting symlinks,
    non-directories, and unsafe path components. *)
