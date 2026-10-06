# Claude read-only host policy

Parent approved this narrow upstream feature on 6 October 2026 to support a
bounded read-only invocation without local consumer CLI shims. Existing Cabal
task_spec and CWR Backend/Engine protocols must remain unchanged; no new library
dependency. Trusted CLI controls, not prompts/workflow output, select tools and
per-invocation native budget/turn ceilings.

Acceptance: all three flags together, unique nonempty subset of Read/Glob/Grep,
finite positive USD ceiling, positive integer turn ceiling. Omission preserves
legacy behavior. Mutable or non-Claude adapters refuse before model dispatch;
no fallback. Claude exact toolset excludes shell/write/network/sub-agent tools.
Empty strict MCP, native safe/restricted mode and no permission bypass are
required. OAuth remains usable; no credentials/config/transcripts are exported.
Unsupported native options fail closed. This does not grant engine Run authority
or replace an OS sandbox/campaign budget. No live failure/auth/quota injection.

Tests: `dune exec test/test_host_policy_cli.exe -- _build/default/bin/main.exe`
and full `dune runtest`, under the CWR switch with staged Cabal OCAMLPATH.
Synthetic CLI fixtures assert exact native argv and no dispatch for invalid,
partial, mutable or unsupported backend policy. Source/output schemas remain
unchanged. Parent independent review is required before shipping; no merge.
