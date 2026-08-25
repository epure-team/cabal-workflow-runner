import { createServer, type ServerResponse } from "node:http";
import { open, readFile, readdir, realpath, stat } from "node:fs/promises";
import { dirname, extname, join, relative, resolve } from "node:path";
import { fileURLToPath } from "node:url";

type JsonRecord = Record<string, unknown>;
type ParsedEntry = { offset: number; value: unknown };
type LedgerSummary = {
  total: number; bytes: number; updatedAt: string; kinds: Record<string, number>;
  latestKind: string | null; attention: number;
};
type PiTool = { id: string | null; name: string; arguments?: unknown; error?: boolean };
type PiEvent = {
  offset: number; timestamp: string | null; kind: "session" | "session_info" | "model_change" | "message";
  role?: string; text?: string; model?: string; tools?: PiTool[]; raw: unknown;
};
type PiSession = {
  id: string; sessionId: string | null; name: string | null; cwd: string | null;
  createdAt: string; updatedAt: string; model: string | null; models: string[];
  status: "complete" | "tool_pending" | "incomplete"; messages: number; tools: number;
  bytes: number; truncated: boolean; invalidLines: number; linkedLedgers: string[];
};

const port = Number(process.env.PORT ?? 8787);
const ledgerRoot = resolve(process.env.CWR_LEDGER_ROOT ?? join(process.cwd(), "ledgers"));
const piSessionRoot = resolve(process.env.PI_SESSION_ROOT ?? join(process.cwd(), "pi-sessions"));
const piMaxBytes = boundedNumber(process.env.PI_SESSION_MAX_BYTES, 8 * 1024 * 1024, 64 * 1024, 64 * 1024 * 1024);
const piMaxLines = boundedNumber(process.env.PI_SESSION_MAX_LINES, 10_000, 100, 100_000);
const piMaxFiles = boundedNumber(process.env.PI_SESSION_MAX_FILES, 500, 1, 5_000);
const publicRoot = resolve(dirname(fileURLToPath(import.meta.url)), "../public");
const safeId = (id: string) => /^[A-Za-z0-9][A-Za-z0-9._-]{0,127}$/.test(id) ? id : null;
const isRecord = (value: unknown): value is JsonRecord => typeof value === "object" && value !== null && !Array.isArray(value);

function boundedNumber(raw: string | undefined, fallback: number, min: number, max: number): number {
  const value = Number(raw ?? fallback);
  return Number.isInteger(value) && value >= min && value <= max ? value : fallback;
}

function json(res: ServerResponse, status: number, value: unknown): void {
  res.writeHead(status, { "content-type": "application/json; charset=utf-8", "cache-control": "no-store" });
  res.end(JSON.stringify(value));
}

function parseLines(content: string, maxLines = Number.POSITIVE_INFINITY): ParsedEntry[] {
  return content.split("\n").filter(Boolean).slice(0, maxLines).map((line, offset) => {
    try { return { offset, value: JSON.parse(line) as unknown }; }
    catch { return { offset, value: { raw_untrusted_line: "<invalid JSON omitted>" } }; }
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
  return { total: entries.length, bytes, updatedAt, kinds, latestKind: entries.length ? kindOf(entries.at(-1)?.value) : null, attention };
}

async function resolveDirectFile(rootPath: string, filename: string): Promise<string> {
  const target = resolve(rootPath, filename);
  if (relative(rootPath, target).startsWith("..")) throw new Error("path");
  const root = await realpath(rootPath);
  const actual = await realpath(target);
  if (!actual.startsWith(`${root}/`) || !(await stat(actual)).isFile()) throw new Error("path");
  return actual;
}

async function readLedger(id: string): Promise<{ entries: ParsedEntry[]; summary: LedgerSummary }> {
  const actual = await resolveDirectFile(ledgerRoot, `${id}.ndjson`);
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
  } catch { return []; }
}

const sensitiveKey = /^(authorization|cookie|set-cookie|password|passwd|secret|api[_-]?key|access[_-]?token|refresh[_-]?token|private[_-]?key)$/i;
const secretValue = /(-----BEGIN (?:RSA |EC |OPENSSH )?PRIVATE KEY-----[\s\S]*?-----END (?:RSA |EC |OPENSSH )?PRIVATE KEY-----)|(\bBearer\s+)[A-Za-z0-9._~+\/-]+=*|\b(?:sk|hf)_[A-Za-z0-9_-]{16,}|\b(password|passwd|secret|api[_-]?key|access[_-]?token|refresh[_-]?token)\s*[:=]\s*([^\s,;]+)/gi;

function redactString(value: string): string {
  const limited = value.length > 16_384 ? `${value.slice(0, 16_384)}… <truncated>` : value;
  return limited.replace(secretValue, (match, privateKey: string | undefined, bearer: string | undefined, key: string | undefined) => {
    if (privateKey) return "<redacted private key>";
    if (bearer) return `${bearer}<redacted>`;
    return key ? `${key}=<redacted>` : "<redacted>";
  });
}

function redact(value: unknown, depth = 0): unknown {
  if (typeof value === "string") return redactString(value);
  if (value === null || typeof value !== "object") return value;
  if (depth >= 10) return "<depth limit>";
  if (Array.isArray(value)) return value.slice(0, 500).map((item) => redact(item, depth + 1));
  const output: JsonRecord = {};
  for (const [key, child] of Object.entries(value as JsonRecord).slice(0, 500)) {
    output[key] = sensitiveKey.test(key) ? "<redacted>" : redact(child, depth + 1);
  }
  return output;
}

async function readLimited(path: string): Promise<{ content: string; truncated: boolean }> {
  const handle = await open(path, "r");
  try {
    const buffer = Buffer.alloc(piMaxBytes + 1);
    const { bytesRead } = await handle.read(buffer, 0, buffer.length, 0);
    const truncated = bytesRead > piMaxBytes;
    let content = buffer.subarray(0, Math.min(bytesRead, piMaxBytes)).toString("utf8");
    if (truncated) content = content.slice(0, Math.max(0, content.lastIndexOf("\n")));
    return { content, truncated };
  } finally { await handle.close(); }
}

function stringField(record: JsonRecord, key: string): string | null {
  return typeof record[key] === "string" ? record[key] as string : null;
}

function messageText(message: JsonRecord): string | undefined {
  if (typeof message.content === "string") return redactString(message.content);
  if (!Array.isArray(message.content)) return undefined;
  const value = message.content.filter(isRecord)
    .filter((part) => part.type === "text" && typeof part.text === "string")
    .map((part) => redactString(part.text as string)).join("\n");
  return value || undefined;
}

function messageTools(message: JsonRecord): PiTool[] {
  if (message.role === "toolResult") {
    return [{
      id: typeof message.toolCallId === "string" ? message.toolCallId : null,
      name: typeof message.toolName === "string" ? message.toolName : "tool",
      error: message.isError === true,
    }];
  }
  if (!Array.isArray(message.content)) return [];
  const tools: PiTool[] = [];
  for (const part of message.content.filter(isRecord)) {
    if (part.type === "toolCall" && typeof part.name === "string") {
      tools.push({ id: typeof part.id === "string" ? part.id : null, name: part.name, arguments: redact(part.arguments) });
    }
  }
  return tools;
}

function piEvents(entries: ParsedEntry[]): PiEvent[] {
  const events: PiEvent[] = [];
  for (const entry of entries) {
    if (!isRecord(entry.value) || typeof entry.value.type !== "string") continue;
    const value = entry.value;
    const timestamp = stringField(value, "timestamp");
    if (value.type === "session") events.push({ offset: entry.offset, timestamp, kind: "session", raw: redact(value) });
    else if (value.type === "session_info") events.push({ offset: entry.offset, timestamp, kind: "session_info", text: stringField(value, "name") ?? undefined, raw: redact(value) });
    else if (value.type === "model_change") {
      const provider = stringField(value, "provider");
      const modelId = stringField(value, "modelId");
      events.push({ offset: entry.offset, timestamp, kind: "model_change", model: [provider, modelId].filter(Boolean).join("/") || undefined, raw: redact(value) });
    } else if (value.type === "message" && isRecord(value.message)) {
      const tools = messageTools(value.message);
      events.push({
        offset: entry.offset, timestamp, kind: "message", role: stringField(value.message, "role") ?? "unknown",
        text: messageText(value.message), tools: tools.length ? tools : undefined,
        model: stringField(value.message, "model") ?? undefined, raw: redact(value),
      });
    }
  }
  return events;
}

function containsSessionId(value: unknown, sessionId: string, depth = 0): boolean {
  if (depth > 12 || value === null || typeof value !== "object") return false;
  if (Array.isArray(value)) return value.some((item) => containsSessionId(item, sessionId, depth + 1));
  const record = value as JsonRecord;
  if (record.session_id === sessionId) return true;
  return Object.values(record).some((item) => containsSessionId(item, sessionId, depth + 1));
}

async function linkedLedgers(sessionId: string | null): Promise<string[]> {
  if (!sessionId) return [];
  const matches: string[] = [];
  for (const item of await ledgers()) {
    try {
      const ledger = await readLedger(item.id);
      if (ledger.entries.some((entry) => containsSessionId(entry.value, sessionId))) matches.push(item.id);
    } catch { /* A ledger may disappear between listing and reading. */ }
  }
  return matches;
}

async function readPiSession(id: string): Promise<{ summary: PiSession; events: PiEvent[] }> {
  const actual = await resolveDirectFile(piSessionRoot, `${id}.jsonl`);
  const metadata = await stat(actual);
  const limited = await readLimited(actual);
  const sourceLines = limited.content.split("\n").filter(Boolean);
  const entries = parseLines(limited.content, piMaxLines);
  const events = piEvents(entries);
  const records = entries.map((entry) => entry.value).filter(isRecord);
  const header = records.find((entry) => entry.type === "session");
  const info = records.find((entry) => entry.type === "session_info");
  const sessionId = header ? stringField(header, "id") : null;
  const models = [...new Set(events.filter((event) => event.model).map((event) => event.model!))];
  const messageRecords = records.filter((entry) => entry.type === "message" && isRecord(entry.message));
  const lastMessage = messageRecords.at(-1)?.message;
  const role = isRecord(lastMessage) ? stringField(lastMessage, "role") : null;
  const stopReason = isRecord(lastMessage) ? stringField(lastMessage, "stopReason") : null;
  const status = role === "assistant" && stopReason === "stop" ? "complete"
    : role === "assistant" && stopReason === "toolUse" ? "tool_pending" : "incomplete";
  const timestamps = events.map((event) => event.timestamp).filter((value): value is string => value !== null);
  const summary: PiSession = {
    id, sessionId, name: info ? stringField(info, "name") : null, cwd: header ? stringField(header, "cwd") : null,
    createdAt: (header ? stringField(header, "timestamp") : null) ?? timestamps[0] ?? metadata.birthtime.toISOString(),
    updatedAt: timestamps.at(-1) ?? metadata.mtime.toISOString(), model: models.at(-1) ?? null, models,
    status, messages: messageRecords.length, tools: events.reduce((total, event) => total + (event.tools?.length ?? 0), 0),
    bytes: metadata.size, truncated: limited.truncated || sourceLines.length > piMaxLines,
    invalidLines: entries.filter((entry) => isRecord(entry.value) && "raw_untrusted_line" in entry.value).length,
    linkedLedgers: await linkedLedgers(sessionId),
  };
  return { summary, events };
}

async function piSessions(): Promise<PiSession[]> {
  try {
    const names = (await readdir(piSessionRoot)).filter((name) => extname(name) === ".jsonl" && safeId(name.slice(0, -6))).slice(0, piMaxFiles);
    const items = await Promise.all(names.map(async (name) => readPiSession(name.slice(0, -6)).then((session) => session.summary)));
    return items.sort((a, b) => b.updatedAt.localeCompare(a.updatedAt));
  } catch { return []; }
}

async function linkedPiSessions(entries: ParsedEntry[]): Promise<Array<{ id: string; sessionId: string; name: string | null }>> {
  const explicitIds = new Set<string>();
  const collect = (value: unknown, depth = 0): void => {
    if (depth > 12 || value === null || typeof value !== "object") return;
    if (Array.isArray(value)) return value.forEach((item) => collect(item, depth + 1));
    const record = value as JsonRecord;
    if (typeof record.session_id === "string") explicitIds.add(record.session_id);
    Object.values(record).forEach((item) => collect(item, depth + 1));
  };
  entries.forEach((entry) => collect(entry.value));
  if (!explicitIds.size) return [];
  return (await piSessions()).filter((session) => session.sessionId && explicitIds.has(session.sessionId))
    .map((session) => ({ id: session.id, sessionId: session.sessionId!, name: session.name }));
}

const mime = (path: string) => path.endsWith(".js") ? "text/javascript" : path.endsWith(".css") ? "text/css" : "text/html";

createServer(async (req, res) => {
  const url = new URL(req.url ?? "/", `http://${req.headers.host}`);
  if (req.method !== "GET") return json(res, 405, { error: "read-only" });
  try {
    if (url.pathname === "/v1/ledgers") return json(res, 200, { items: await ledgers() });
    if (url.pathname === "/v1/pi-sessions") return json(res, 200, { raw_untrusted: true, items: await piSessions() });
    const piMatch = url.pathname.match(/^\/v1\/pi-sessions\/([^/]+)$/);
    if (piMatch) {
      const id = safeId(decodeURIComponent(piMatch[1]));
      if (!id) return json(res, 400, { error: "invalid request" });
      return json(res, 200, { raw_untrusted: true, ...await readPiSession(id) });
    }
    const match = url.pathname.match(/^\/v1\/ledgers\/([^/]+)\/entries$/);
    if (match) {
      const id = safeId(decodeURIComponent(match[1]));
      const offset = Number(url.searchParams.get("offset") ?? 0);
      const limit = Number(url.searchParams.get("limit") ?? 100);
      if (!id || !Number.isInteger(offset) || offset < 0 || !Number.isInteger(limit) || limit < 1 || limit > 200) {
        return json(res, 400, { error: "invalid request" });
      }
      const ledger = await readLedger(id);
      return json(res, 200, { ledgerId: id, raw_untrusted: true, summary: ledger.summary,
        linkedPiSessions: await linkedPiSessions(ledger.entries), entries: ledger.entries.slice(offset, offset + limit) });
    }
    const path = url.pathname === "/" ? "index.html" : url.pathname.slice(1);
    const target = resolve(publicRoot, path);
    if (relative(publicRoot, target).startsWith("..")) return json(res, 404, { error: "not found" });
    const content = await readFile(target);
    res.writeHead(200, { "content-type": `${mime(target)}; charset=utf-8`, "cache-control": "no-store" });
    res.end(content);
  } catch {
    if (!res.headersSent) json(res, 404, { error: "source not found or not selected" });
    else res.end();
  }
}).listen(port, "127.0.0.1", () => {
  console.log(`CWR monitor on 127.0.0.1:${port}; ledgers=${ledgerRoot}; pi-sessions=${piSessionRoot}`);
});
