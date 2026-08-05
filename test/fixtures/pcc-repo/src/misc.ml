(* Pre-existing, deliberately UNTOUCHED decision-lint finding (duplicate conjunct). Needed so
   arch-impact --fail-on-new-findings can reach verdict:pass rather than verdict:refused — an
   index whose decisions table is empty always refuses (see JALON3-rapport.md, Phase 0). None of
   the integration test's scenarios edit this file, so this finding is never "new". *)
let quirky (a : bool) (b : bool) : int = if a && b && a then 1 else 2
