module Ed25519 = Mirage_crypto_ec.Ed25519

type write_error = Failed of string | Published_uncertain of string

type signer = {
  private_key : Ed25519.priv;
  public_key : string;
  key_id : string;
}

type verifier = {
  public_key_value : Ed25519.pub;
  public_key : string;
  key_id : string;
}

let sha256_hex s = Digestif.SHA256.(to_hex (digest_string s))
let key_id_of_public_key key = "sha256:" ^ sha256_hex key

let signer_of_seed seed =
  if String.length seed <> 32 then Error "Ed25519 seed must be exactly 32 bytes"
  else
    match Ed25519.priv_of_octets seed with
    | Error _ -> Error "invalid Ed25519 seed"
    | Ok private_key ->
        let public_key =
          Ed25519.pub_of_priv private_key |> Ed25519.pub_to_octets
        in
        Ok { private_key; public_key; key_id = key_id_of_public_key public_key }

let signer_of_fd fd =
  Fun.protect
    ~finally:(fun () -> try Unix.close fd with Unix.Unix_error _ -> ())
    (fun () ->
      let bytes = Bytes.create 33 in
      let rec read offset =
        if offset = 33 then offset
        else
          match Unix.read fd bytes offset (33 - offset) with
          | 0 -> offset
          | n -> read (offset + n)
      in
      match read 0 with
      | 32 -> signer_of_seed (Bytes.sub_string bytes 0 32)
      | n ->
          Error
            (Printf.sprintf
               "attestation key fd must contain exactly 32 bytes (read %d)" n))

let verifier_of_public_key public_key =
  match Ed25519.pub_of_octets public_key with
  | Error _ -> Error "invalid Ed25519 public key"
  | Ok public_key_value ->
      Ok
        {
          public_key_value;
          public_key;
          key_id = key_id_of_public_key public_key;
        }

let public_key (s : signer) = s.public_key
let key_id (s : signer) = s.key_id

let public_identity (s : signer) =
  `Assoc
    [
      ("algorithm", `String "Ed25519");
      ("key_id", `String s.key_id);
      ("public_key", `String (Base64.encode_exn s.public_key));
      ("schema_version", `String "cwr.attestation-key.v1");
    ]

let verifier_of_identity json =
  Result.bind (Canonical_json.validate json) (fun () -> match json with
  | `Assoc fields -> (
      match
        ( List.assoc_opt "algorithm" fields,
          List.assoc_opt "key_id" fields,
          List.assoc_opt "public_key" fields,
          List.assoc_opt "schema_version" fields )
      with
      | ( Some (`String "Ed25519"),
          Some (`String claimed_id),
          Some (`String encoded),
          Some (`String "cwr.attestation-key.v1") ) -> (
          match Base64.decode encoded with
          | Error (`Msg _) -> Error "invalid base64 public key"
          | Ok raw -> (
              match verifier_of_public_key raw with
              | Error e -> Error e
              | Ok verifier ->
                  if verifier.key_id <> claimed_id then
                    Error "public key id does not match public key"
                  else Ok verifier))
      | _ -> Error "invalid attestation public-key identity")
  | _ -> Error "attestation public-key identity must be an object")

let validate_canonical_json = Canonical_json.validate

let canonical_string json =
  match Canonical_json.to_string json with
  | Ok encoded -> encoded
  | Error e -> invalid_arg e

let validate_selected selected =
  let keys = List.map fst selected in
  if List.length keys <> List.length (List.sort_uniq String.compare keys) then
    Error "duplicate selected path"
  else Canonical_json.validate (`Assoc selected)

let select_context ctx paths =
  let valid_ctx = Canonical_json.validate_no_duplicates (`Assoc ctx) in
  let rec descend json = function
    | [] -> Some json
    | key :: rest -> (
        match json with
        | `Assoc fields ->
            Option.bind (List.assoc_opt key fields) (fun v -> descend v rest)
        | _ -> None)
  in
  let lookup path =
    match String.split_on_char '.' path with
    | [] -> None
    | root :: rest ->
        Option.bind (List.assoc_opt root ctx) (fun v -> descend v rest)
  in
  let rec collect acc = function
    | [] -> Ok (List.rev acc)
    | path :: rest -> (
        match lookup path with
        | Some value -> collect ((path, value) :: acc) rest
        | None ->
            Error (Printf.sprintf "selected context path %S is absent" path))
  in
  Result.bind valid_ctx (fun () -> collect [] paths)

let workflow_digest workflow =
  "sha256:" ^ (Workflow_json.to_json workflow |> canonical_string |> sha256_hex)

let workflow_binding workflow =
  `Assoc
    ([
       ("digest", `String (workflow_digest workflow));
       ("name", `String workflow.Types.name);
     ]
    @
    match workflow.Types.version with
    | None -> [ ("version", `Null) ]
    | Some version -> [ ("version", `String version) ])

let materialize_output_path ~template ~occurrence =
  let needle = "{occurrence}" in
  let replacement = string_of_int occurrence in
  let find_from start =
    let rec search i =
      if i + String.length needle > String.length template then None
      else if String.sub template i (String.length needle) = needle then Some i
      else search (i + 1)
    in search start
  in
  let rec loop start acc =
    match find_from start with
    | None -> acc ^ String.sub template start (String.length template - start)
    | Some at ->
        loop (at + String.length needle)
          (acc ^ String.sub template start (at - start) ^ replacement)
  in
  loop 0 ""

let payload ~workflow ~step_id ~occurrence ~output_path ~replay_domain
    ~session_nonce ~selected =
  `Assoc
    [
      ("replay_domain", `String replay_domain);
      ("occurrence", `Int occurrence);
      ("output_path", `String output_path);
      ( "selected",
        `Assoc (List.sort (fun (a, _) (b, _) -> String.compare a b) selected) );
      ("session_nonce", `String session_nonce);
      ("step_id", `String step_id);
      ("workflow", workflow_binding workflow);
    ]

let unsigned ~public_key ~key_id payload =
  `Assoc
    [
      ("algorithm", `String "Ed25519");
      ("key_id", `String key_id);
      ("payload", payload);
      ("public_key", `String (Base64.encode_exn public_key));
      ("schema_version", `String "cwr.attestation.v1");
    ]

let create ~(signer : signer) ~workflow ~step_id ~occurrence ~output_path
    ~replay_domain ~session_nonce ~selected =
  if occurrence < 0 then Error "attestation occurrence must be non-negative"
  else Result.bind (validate_selected selected) (fun () ->
    let payload =
      payload ~workflow ~step_id ~occurrence ~output_path ~replay_domain
        ~session_nonce ~selected
    in
    let unsigned =
      unsigned ~public_key:signer.public_key ~key_id:signer.key_id payload
    in
    Result.bind (Canonical_json.to_string unsigned) (fun canonical ->
      let signature = canonical |> Ed25519.sign ~key:signer.private_key
        |> Base64.encode_exn in
      match unsigned with
      | `Assoc fields -> Ok (`Assoc (("signature", `String signature) :: fields))
      | _ -> Error "internal unsigned envelope is not an object"))

let verify ~(verifier : verifier) ~workflow ~step_id ~occurrence ~output_path
    ~replay_domain ~session_nonce ~selected envelope =
  if occurrence < 0 then Error "attestation occurrence must be non-negative" else
  let inputs_valid = Result.bind (validate_selected selected) (fun () ->
    Canonical_json.validate envelope) in
  Result.bind inputs_valid (fun () ->
  let expected_payload =
    payload ~workflow ~step_id ~occurrence ~output_path ~replay_domain
      ~session_nonce ~selected
  in
  let expected_unsigned =
    unsigned ~public_key:verifier.public_key ~key_id:verifier.key_id
      expected_payload
  in
  match envelope with
  | `Assoc fields -> (
      match List.assoc_opt "signature" fields with
      | Some (`String encoded_signature) -> (
          let actual_unsigned = `Assoc (List.remove_assoc "signature" fields) in
          if
            canonical_string actual_unsigned
            <> canonical_string expected_unsigned
          then Error "attestation binding mismatch"
          else
            match Base64.decode encoded_signature with
            | Error (`Msg _) -> Error "invalid base64 signature"
            | Ok signature ->
                if
                  Ed25519.verify ~key:verifier.public_key_value signature
                    ~msg:(canonical_string expected_unsigned)
                then Ok ()
                else Error "invalid Ed25519 signature")
      | _ -> Error "attestation signature missing or invalid")
  | _ -> Error "attestation envelope must be an object")

let write_atomic ~artifact_root ~relative_path envelope =
  let content = canonical_string envelope ^ "\n" in
  match Secure_fs.write_atomic_noreplace ~root:artifact_root
          ~relative:relative_path ~content with
  | Ok `Written -> Ok ()
  | Ok `Published_uncertain ->
      Error (Published_uncertain "artifact renamed but directory fsync failed")
  | Error e -> Error (Failed e)
