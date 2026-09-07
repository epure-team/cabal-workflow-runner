let test_ordinary_shell_exception_maps_to_127 () =
  Alcotest.(check int) "ordinary exception" 127
    (Backend_cabal.protect_shell_command (fun () -> raise Exit))

let test_fatal_shell_exceptions_propagate () =
  let check label exception_value predicate =
    match Backend_cabal.protect_shell_command (fun () -> raise exception_value) with
    | _ -> Alcotest.fail (label ^ " was converted to an exit code")
    | exception error when predicate error -> ()
    | exception _ -> Alcotest.fail (label ^ " changed exception class")
  in
  check "out of memory" Out_of_memory
    (function Out_of_memory -> true | _ -> false);
  check "stack overflow" Stack_overflow
    (function Stack_overflow -> true | _ -> false);
  check "break" Sys.Break (function Sys.Break -> true | _ -> false)

let test_eio_cancellation_propagates () =
  Eio_posix.run @@ fun _env ->
  let cancelled = ref false in
  (try
     Eio.Cancel.sub (fun cancellation ->
         Eio.Cancel.cancel cancellation Exit;
         ignore
           (Backend_cabal.protect_shell_command (fun () ->
                Eio.Fiber.yield ();
                0)))
   with Eio.Cancel.Cancelled _ -> cancelled := true);
  Alcotest.(check bool) "cancellation propagated" true !cancelled

let test_live_agent_type_is_fixed_to_bound_backend () =
  let check label expected input =
    Alcotest.(check (result (option string) string)) label expected
      (Backend_cabal.live_agent_routing ~backend_id:"codex" input)
  in
  check "omitted agent type" (Ok None) None;
  check "blank agent type remains omitted" (Ok None) (Some "  ");
  check "equal agent type" (Ok (Some "codex")) (Some "codex");
  check "trimmed equal agent type" (Ok (Some "codex")) (Some " codex ");
  match
    Backend_cabal.live_agent_routing ~backend_id:"codex" (Some "reviewer")
  with
  | Error message ->
      Alcotest.(check bool) "mismatch diagnostic is fixed" true
        (message = "agent_type does not match the bound live backend")
  | Ok _ -> Alcotest.fail "cross-backend agent_type was accepted"

let () =
  Alcotest.run "CWR Cabal legacy shell adapter"
    [
      ( "exceptions",
        [
          Alcotest.test_case "ordinary exception becomes 127" `Quick
            test_ordinary_shell_exception_maps_to_127;
          Alcotest.test_case "fatal exceptions propagate" `Quick
            test_fatal_shell_exceptions_propagate;
          Alcotest.test_case "Eio cancellation propagates" `Quick
            test_eio_cancellation_propagates;
          Alcotest.test_case "live agent type is fixed" `Quick
            test_live_agent_type_is_fixed_to_bound_backend;
        ] );
    ]
