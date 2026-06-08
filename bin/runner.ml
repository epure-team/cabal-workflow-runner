(* Process execution + filesystem snapshot for the [Run] step. This is the ONLY
   place (besides backend_cabal) that performs an effect for a run step; the
   [cabal_workflow_runner] library stays yojson-only and takes the run effect as
   the injected [Backend.run_command]. We implement it via
   [Cabal.Backend_process.run_process] (no shell) plus a before/after directory
   snapshot (path -> digest + size) diffed into a [Types.file_change list].

   The per-file [digest] is an MD5 content digest computed with OCaml's stdlib
   [Digest] module. It is a fingerprint for CHANGE DETECTION / OBSERVABILITY in
   the file diff — it is NOT a cryptographic integrity guarantee.

   Honest scope note: [working_dir] bounds the cwd and the snapshot scope, but it
   does NOT sandbox the command from touching absolute paths in its args. The
   operator allowlist (Engine.run ~run_allowlist) is the trust control; full
   isolation (container/chroot) is out of scope. *)

open Cabal_workflow_runner

(* Cap captured stdout/stderr at 64 KiB; set [truncated] when a cap is hit. *)
let output_cap = 64 * 1024

(* Documented exit codes the runner SYNTHESISES when the effect cannot produce a
   real exit code, so the outcome is still RECORDED + replayable (never a crash):
   - 124 timeout (already produced from [Backend_types.Timeout] below);
   - 127 spawn failure (command not found / Eio process error);
   - 125 output overflow (captured output exceeded the internal buffer limit). *)
let exit_spawn_failure = 127
let exit_output_overflow = 125

let cap_string s =
  if String.length s > output_cap then (String.sub s 0 output_cap, true)
  else (s, false)

(* ---- directory snapshot -------------------------------------------------- *)

(* A snapshot maps a relative path -> (size, digest) for every regular file
   reachable under the snapshot roots. [roots] are relative to [base]; the
   default root is "." (the whole working_dir). The digest is MD5 ([Digest]),
   used purely as a content fingerprint for the change diff. *)
let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> In_channel.input_all ic)

(* The digest sentinel recorded for a directory entry (directories have no
   content; we still record them so a created/removed dir is observable). *)
let dir_digest = "directory"

let rec walk ~base rel (acc : (string, int * string) Hashtbl.t) =
  let abs = Filename.concat base rel in
  match (Unix.lstat abs).Unix.st_kind with
  | Unix.S_REG ->
      let content = try read_file abs with _ -> "" in
      Hashtbl.replace acc rel
        (String.length content, Digest.to_hex (Digest.string content))
  | Unix.S_DIR ->
      (* record the directory itself (so an empty created dir is observable),
         except the snapshot-root "." which is always present. *)
      if rel <> "." then Hashtbl.replace acc rel (0, dir_digest);
      let entries = try Sys.readdir abs with _ -> [||] in
      Array.iter
        (fun name ->
          let child = if rel = "." then name else Filename.concat rel name in
          walk ~base child acc)
        entries
  | _ -> () (* symlinks / specials: ignore *)
  | exception _ -> ()

let snapshot ~base roots : (string, int * string) Hashtbl.t =
  let acc = Hashtbl.create 64 in
  List.iter (fun root -> walk ~base root acc) roots;
  acc

(* Diff two snapshots into a [Types.file_change list], sorted by path for
   determinism. *)
let diff (before : (string, int * string) Hashtbl.t)
    (after : (string, int * string) Hashtbl.t) : Types.file_change list =
  let changes = ref [] in
  Hashtbl.iter
    (fun path (size, dg) ->
      match Hashtbl.find_opt before path with
      | None ->
          changes :=
            { Types.path; change = Types.Created; size; digest = dg } :: !changes
      | Some (bsize, bdg) when bsize <> size || bdg <> dg ->
          changes :=
            { Types.path; change = Types.Modified; size; digest = dg }
            :: !changes
      | Some _ -> ())
    after;
  Hashtbl.iter
    (fun path _ ->
      if not (Hashtbl.mem after path) then
        changes :=
          { Types.path; change = Types.Deleted; size = 0; digest = "" }
          :: !changes)
    before;
  List.sort (fun a b -> compare a.Types.path b.Types.path) !changes

(* ---- the injected run_command -------------------------------------------- *)

(* [make ~sw ~env ~base] builds a [run_command] that resolves [working_dir]
   relative to [base] (the CLI cwd), snapshots before/after, runs the command
   without a shell, and returns the observed [run_result]. Default timeout 60s
   when [timeout_ms] is absent. *)
let make ~sw ~env ~base :
    id:string ->
    argv:string list ->
    working_dir:string ->
    timeout_ms:int option ->
    observe:string list option ->
    Types.run_result =
 fun ~id:_ ~argv ~working_dir ~timeout_ms ~observe ->
  let wd = Filename.concat base working_dir in
  (* Ensure the working_dir exists so a command can write into it. *)
  (try Unix.mkdir wd 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) | _ -> ());
  let roots = match observe with Some l when l <> [] -> l | _ -> [ "." ] in
  let before = snapshot ~base:wd roots in
  let timeout_seconds =
    match timeout_ms with Some ms -> float_of_int ms /. 1000.0 | None -> 60.0
  in
  (* The whole run effect is wrapped: a spawn failure (ENOENT), the internal
     [Eio.Buf_read] [Buffer_limit_exceeded] (output beyond the process backend's
     128 MiB cap), or any other exception MUST NOT propagate as an uncaught
     exception (which would abort the engine with no recorded outcome — an
     attacker-authored engine-kill). Instead we ALWAYS return a well-formed
     [run_result] with a documented synthetic exit code, so the run is RECORDED
     and replayable, and the contract "exactly one recorded result" holds.

     Note on bounding: the captured output is buffered by
     [Cabal.Backend_process.run_process] (max 128 MiB) and then capped here to
     64 KiB with [truncated]. Unbounded output (e.g. [yes]) that exceeds the
     backend's buffer limit raises [Eio.Buf_read.Buffer_limit_exceeded], which we
     catch below and report as a truncated overflow result (exit
     [exit_output_overflow]) rather than OOM/crash. *)
  let files_now () = diff before (snapshot ~base:wd roots) in
  try
    let pr =
      Cabal.Backend_process.run_process ~sw ~env ~cmd:argv ~working_dir:wd
        ~timeout_seconds ()
    in
    let files = files_now () in
    let stdout, t1 = cap_string pr.Cabal.Backend_process.stdout in
    let stderr, t2 = cap_string pr.Cabal.Backend_process.stderr in
    let exit_code =
      match pr.Cabal.Backend_process.status with
      | Cabal.Backend_types.Timeout -> 124 (* conventional timeout exit code *)
      | _ -> pr.Cabal.Backend_process.exit_code
    in
    { Types.exit = exit_code; stdout; stderr; truncated = t1 || t2; files }
  with
  | Eio.Buf_read.Buffer_limit_exceeded ->
      (* Output overflowed the capture buffer: record a truncated result. *)
      {
        Types.exit = exit_output_overflow;
        stdout = "";
        stderr = "run: captured output exceeded the buffer limit";
        truncated = true;
        files = (try files_now () with _ -> []);
      }
  | exn ->
      (* Spawn failure (command not found) or any other effect error: record a
         well-formed non-zero result carrying a short message, never crash. *)
      {
        Types.exit = exit_spawn_failure;
        stdout = "";
        stderr =
          Printf.sprintf "run: command could not be executed: %s"
            (Printexc.to_string exn);
        truncated = false;
        files = (try files_now () with _ -> []);
      }
