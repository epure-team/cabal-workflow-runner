import { createServer, type ServerResponse } from "node:http";
import { readFile, readdir, realpath, stat } from "node:fs/promises";
import { dirname, extname, join, relative, resolve } from "node:path";
import { fileURLToPath } from "node:url";

type JsonRecord = Record<string, unknown>;
type ParsedEntry = { offset: number; value: unknown };
type LedgerSummary = {
  total: number;
  bytes: number;
  updatedAt: string;
  kinds: Record<string, number>;
  latestKind: string | null;
  attention: number;
};

const port = Number(process.env.PORT ?? 8787);
const ledgerRoot = resolve(process.env.CWR_LEDGER_ROOT ?? join(process.cwd(), "ledgers"));
const publicRoot = resolve(dirname(fileURLToPath(import.meta.url)), "../public");
const safeId = (id: string) => /^[A-Za-z0-9][A-Za-z0-9._-]{0,127}$/.test(id) ? id : null;
const isRecord = (value: unknown): value is JsonRecord =>
  typeof value === "object" && value !== null && !Array.isArray(value);

function json(res: ServerResponse, status: number, value: unknown): void {
  res.writeHead(status, {
    "content-type": "application/json; charset=utf-8",
    "cache-control": "no-store",
  });
  res.end(JSON.stringify(value));
}

function parseLines(content: string): ParsedEntry[] {
  return content.split("\n").filter(Boolean).map((line, offset) => {
    try {
      return { offset, value: JSON.parse(line) as unknown };
    } catch {
      return { offset, value: { raw_untrusted_line: line.slice(0, 8192) } };
    }
  });
}

function kindOf(value: unknown): string {
  return isRecord(value) && typeof value.kind === "string" ? value.kind : "unknown";
}

function needsAttention(value: unknown): boolean {
  if (!isRecord(value)) return true;
  if (value.success === false || value.passed === false || value.verdict === "fail") return true;
  const outcome = isRecord(value.outcome) ? value.outcome : null;
  return outcome?.kind === "blocked" || outcome?.kind === "aborted"
    || value.kind === "blocked_at" || "raw_untrusted_line" in value;
}

function summarize(entries: ParsedEntry[], bytes: number, updatedAt: string): LedgerSummary {
  const kinds: Record<string, number> = {};
  let attention = 0;
  for (const entry of entries) {
    const kind = kindOf(entry.value);
    kinds[kind] = (kinds[kind] ?? 0) + 1;
    if (needsAttention(entry.value)) attention += 1;
  }
  return {
    total: entries.length,
    bytes,
    updatedAt,
    kinds,
    latestKind: entries.length ? kindOf(entries.at(-1)?.value) : null,
    attention,
  };
}

async function resolveLedger(id: string): Promise<string> {
  const target = resolve(ledgerRoot, `${id}.ndjson`);
  if (relative(ledgerRoot, target).startsWith("..")) throw new Error("path");
  const root = await realpath(ledgerRoot);
  const actual = await realpath(target);
  if (!actual.startsWith(`${root}/`) || !(await stat(actual)).isFile()) throw new Error("path");
  return actual;
}

async function readLedger(id: string): Promise<{ entries: ParsedEntry[]; summary: LedgerSummary }> {
  const actual = await resolveLedger(id);
  const [content, metadata] = await Promise.all([readFile(actual, "utf8"), stat(actual)]);
  const entries = parseLines(content);
  return { entries, summary: summarize(entries, metadata.size, metadata.mtime.toISOString()) };
}

async function ledgers() {
  try {
    const names = await readdir(ledgerRoot);
    const items = await Promise.all(names
      .filter((name) => extname(name) === ".ndjson" && safeId(name.slice(0, -7)))
      .map(async (name) => {
        const id = name.slice(0, -7);
        const ledger = await readLedger(id);
        return { id, raw_untrusted: true, summary: ledger.summary };
      }));
    return items.sort((a, b) => b.summary.updatedAt.localeCompare(a.summary.updatedAt));
  } catch {
    return [];
  }
}

const mime = (path: string) => path.endsWith(".js")
  ? "text/javascript"
  : path.endsWith(".css") ? "text/css" : "text/html";

createServer(async (req, res) => {
  const url = new URL(req.url ?? "/", `http://${req.headers.host}`);
  if (req.method !== "GET") return json(res, 405, { error: "read-only" });
  try {
    if (url.pathname === "/v1/ledgers") return json(res, 200, { items: await ledgers() });
    const match = url.pathname.match(/^\/v1\/ledgers\/([^/]+)\/entries$/);
    if (match) {
      const id = safeId(match[1]);
      const offset = Number(url.searchParams.get("offset") ?? 0);
      const limit = Number(url.searchParams.get("limit") ?? 100);
      if (!id || !Number.isInteger(offset) || offset < 0 || !Number.isInteger(limit)
          || limit < 1 || limit > 200) {
        return json(res, 400, { error: "invalid request" });
      }
      const ledger = await readLedger(id);
      return json(res, 200, {
        ledgerId: id,
        raw_untrusted: true,
        summary: ledger.summary,
        entries: ledger.entries.slice(offset, offset + limit),
      });
    }
    const path = url.pathname === "/" ? "index.html" : url.pathname.slice(1);
    const target = resolve(publicRoot, path);
    if (relative(publicRoot, target).startsWith("..")) return json(res, 404, { error: "not found" });
    const content = await readFile(target);
    res.writeHead(200, { "content-type": `${mime(target)}; charset=utf-8`, "cache-control": "no-store" });
    res.end(content);
  } catch {
    if (!res.headersSent) json(res, 404, { error: "ledger not found or not selected" });
    else res.end();
  }
}).listen(port, "127.0.0.1", () => {
  console.log(`CWR monitor on 127.0.0.1:${port}; root=${ledgerRoot}`);
});
