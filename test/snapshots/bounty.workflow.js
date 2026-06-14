// Compiled from CWR (unversioned)
// Workflow: bounty-pipeline

export const meta = { name: 'bounty-pipeline', description: '' };

// [read-only]
const recon = await agent("Survey the target and enumerate candidate findings.", {label: "recon", schema: {type: "object", properties: {"candidates": {type: "integer"}}, required: ["candidates"]}});
let _maxiters_0 = 0;
let _fixcount_0 = 0;
while (true) {
  // [read-only]
  const deep_dive = await agent("Investigate the next candidate finding in depth.", {label: "deep-dive", schema: {type: "object", properties: {"exhausted": {type: "boolean"}, "progressed": {type: "boolean"}}, required: ["exhausted", "progressed"]}});
  const draft_poc = await agent("Draft a proof-of-concept for the current candidate.", {label: "draft-poc", schema: {type: "object", properties: {"ok": {type: "boolean"}}, required: ["ok"]}});
  if ((deep_dive.exhausted === true)) break;
  if (++_maxiters_0 >= 5) break;
  if (budget.remaining() <= 0) break;
  if (!(deep_dive.progressed)) { if (++_fixcount_0 >= 2) break; } else { _fixcount_0 = 0; }
}
const writeup = await agent("Write the report for the strongest finding.", {label: "writeup", schema: {type: "object", properties: {"severity": {type: "string", enum: ["low", "medium", "high", "critical"]}}, required: ["severity"]}});
// [read-only]
const final_review = await agent("Independent reviewer signs off on the finding.", {label: "final-review", schema: {type: "object", properties: {"verdict": {type: "string", enum: ["approved", "rejected"]}}, required: ["verdict"]}});
if (!((writeup.severity !== null && writeup.severity !== undefined))) { throw new Error("gate g-validated failed"); }
if (!((draft_poc.ok !== null && draft_poc.ok !== undefined))) { throw new Error("gate g-observed failed"); }
if (!((final_review.verdict === "approved"))) { throw new Error("gate g-independent failed"); }
if ((["high", "critical"]).includes(writeup.severity)) {
  // [CWR commit — token approval mechanism not preserved]
  await agent("request human approval", {label: "commit_submit-bounty"});
} else {
  // [read-only]
  const abort_note = await agent("Record why the finding did not clear triage.", {label: "abort-note"});
}
