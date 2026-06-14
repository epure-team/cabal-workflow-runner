// Compiled from CWR (unversioned)
// Workflow: run-demo

export const meta = { name: 'run-demo', description: '' };

// [CWR run: cmd="mkdir -p out" working_dir="scratch" — replay safety and allowlist not preserved]
await agent("run: mkdir -p out", {label: "mk"});
if (!((mk.exit === 0))) { throw new Error("gate g-ran-ok failed"); }
