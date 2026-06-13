(* Thin re-export shim: the actual compiler lives in lib/compiler.ml
   (Cabal_workflow_runner.Compiler) so it can be tested from the test suite.
   This module re-exports the public surface for use within bin/. *)

include Cabal_workflow_runner.Compiler
