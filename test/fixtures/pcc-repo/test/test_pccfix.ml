let () =
  Alcotest.run "pccfix"
    [ ( "ui",
        [ Alcotest.test_case "handle 1 = 2" `Quick (fun () ->
              Alcotest.(check int) "handle" 2 (Pccfix.Ui.handle 1)) ] );
      ( "db",
        [ Alcotest.test_case "write 3 = 3" `Quick (fun () ->
              Alcotest.(check int) "write" 3 (Pccfix.Db.write 3)) ] ) ]
