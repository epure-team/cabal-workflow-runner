// Guards the three properties the monitor's README claims and nothing pinned:
// it is read-only, it stays inside its roots, and it survives hostile input.
// Each case here was written against a reproduction, not against the source.
import { test, before, after } from "node:test";
import assert from "node:assert/strict";
import { spawn, type ChildProcess } from "node:child_process";
import { mkdtemp, writeFile, mkdir } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { connect } from "node:net";

const port = 8873;
const base = `http://127.0.0.1:${port}`;
let server: ChildProcess;

before(async () => {
  const root = await mkdtemp(join(tmpdir(), "cwr-monitor-"));
  const ledgers = join(root, "ledgers");
  await mkdir(ledgers);
  await writeFile(
    join(ledgers, "demo.ndjson"),
    JSON.stringify({ kind: "ctx_snapshot", note: "Bearer sk_livesecrettokenvalue000" }) + "\n",
  );
  server = spawn(
    process.execPath,
    ["--experimental-strip-types", new URL("../src/server.ts", import.meta.url).pathname],
    { env: { ...process.env, PORT: String(port), CWR_LEDGER_ROOT: ledgers }, stdio: "ignore" },
  );
  for (let i = 0; i < 100; i++) {
    try { await fetch(`${base}/v1/ledgers`); return; } catch { await new Promise((r) => setTimeout(r, 50)); }
  }
  throw new Error("monitor did not start");
});

after(() => server?.kill());

// A raw socket, because `fetch` will not let us send a malformed Host.
const rawRequest = (payload: string) =>
  new Promise<void>((resolve) => {
    const socket = connect(port, "127.0.0.1", () => socket.end(payload));
    socket.on("close", () => resolve());
    socket.on("error", () => resolve());
  });

test("an empty Host header does not kill the process", async () => {
  await rawRequest("GET /v1/ledgers HTTP/1.1\r\nHost: \r\n\r\n");
  await new Promise((r) => setTimeout(r, 200));
  // Before the fix this threw ECONNREFUSED: `new URL` rejected outside the
  // handler's try, and Node exits on an unhandled rejection.
  const response = await fetch(`${base}/v1/ledgers`);
  assert.equal(response.status, 200);
});

test("only GET is served", async () => {
  for (const method of ["POST", "PUT", "DELETE", "PATCH"]) {
    const response = await fetch(`${base}/v1/ledgers`, { method });
    assert.equal(response.status, 405, `${method} must be refused`);
    assert.equal((await response.json()).error, "read-only");
  }
});

test("static serving stays inside public/", async () => {
  // Pre-encoded so the path survives to the handler instead of being
  // normalised away by fetch's own URL parsing.
  const response = await fetch(`${base}/..%2f..%2fpackage.json`);
  assert.equal(response.status, 404);
});

test("a ledger id cannot escape the ledger root", async () => {
  const response = await fetch(`${base}/v1/ledgers/${encodeURIComponent("../../package")}/entries`);
  assert.equal(response.status, 400);
});

test("secrets in ledger content are redacted before they are served", async () => {
  const response = await fetch(`${base}/v1/ledgers/demo/entries`);
  assert.equal(response.status, 200);
  const body = await response.text();
  assert.ok(!body.includes("sk_livesecrettokenvalue000"), "raw secret must not reach the client");
  assert.ok(body.includes("redacted"), "redaction must be visible, not a silent drop");
});
