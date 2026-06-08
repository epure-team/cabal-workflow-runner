(lang dune 3.13)
(name cabal_workflow_runner)
(generate_opam_files true)

(package
 (name cabal_workflow_runner)
 (synopsis "Deterministic workflow engine on cabal")
 (description
  "A deterministic interpreter for declarative agentic workflows (sequence, bounded loop, branch, gate, agent, terminal commit). Structural control flow runs deterministically in OCaml; agent steps dispatch through cabal. A fail-closed validator enforces a safety floor as an engine invariant (commit requires a runtime human-approval token plus floor gates guaranteed on every path; loops are bounded), and recorded traces replay byte-identically.")
 (depends
  (ocaml (>= 5.3.0))
  (dune (>= 3.13))
  cabal
  yojson
  (eio (>= 1.0))
  (eio_main (>= 1.0))
  cmdliner
  (alcotest :with-test))
 (license MIT)
 (authors "Epure Team")
 (maintainers "Epure Team")
 (homepage "https://github.com/epure-team/cabal-workflow-runner")
 (source (github epure-team/cabal-workflow-runner))
 (bug_reports "https://github.com/epure-team/cabal-workflow-runner/issues")
 (tags ("agentic" "workflow" "deterministic" "engine" "cabal")))
