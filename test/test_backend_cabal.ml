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
        ] );
    ]
