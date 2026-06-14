// Compiled from CWR (unversioned)
// Workflow: smoke

export const meta = { name: 'smoke', description: '' };

// [read-only]
const classify = await agent("Output ONLY this JSON object and nothing else (no prose, no markdown fences): {\"severity\":\"high\",\"observed\":true}", {label: "classify", schema: {type: "object", properties: {"severity": {type: "string", enum: ["low", "medium", "high", "critical"]}, "observed": {type: "boolean"}}, required: ["severity", "observed"]}});
if (!((classify.observed === true))) { throw new Error("gate g-observed failed"); }
let _fixcount_0 = 0;
let _maxiters_0 = 0;
while (true) {
  // [read-only]
  const tick = await agent("Output ONLY this JSON object and nothing else (no prose, no markdown fences): {\"done\":false,\"progressed\":false}", {label: "tick", schema: {type: "object", properties: {"done": {type: "boolean"}, "progressed": {type: "boolean"}}, required: ["done", "progressed"]}});
  if ((tick.done === true)) break;
  if (!(tick.progressed)) { if (++_fixcount_0 >= 2) break; } else { _fixcount_0 = 0; }
  if (++_maxiters_0 >= 4) break;
}
if ((["high", "critical"]).includes(classify.severity)) {
  // [read-only]
  const review = await agent("Output ONLY this JSON object and nothing else (no prose, no markdown fences): {\"ok\":true}", {label: "review", schema: {type: "object", properties: {"ok": {type: "boolean"}}, required: ["ok"]}});
  // [CWR commit — token approval mechanism not preserved]
  await agent("request human approval", {label: "commit_submit"});
} else {
  // [read-only]
  const note = await agent("Output ONLY this JSON object and nothing else (no prose, no markdown fences): {\"noted\":true}", {label: "note", schema: {type: "object", properties: {"noted": {type: "boolean"}}, required: ["noted"]}});
}
