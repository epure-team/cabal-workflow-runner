import { createServer } from "node:http";
import { readFile, readdir, realpath, stat } from "node:fs/promises";
import { dirname, extname, join, relative, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const port = Number(process.env.PORT ?? 8787);
const ledgerRoot = resolve(process.env.CWR_LEDGER_ROOT ?? join(process.cwd(), "ledgers"));
const publicRoot = resolve(dirname(fileURLToPath(import.meta.url)), "../public");
const json = (res: any, status: number, value: unknown) => { res.writeHead(status, {"content-type":"application/json; charset=utf-8", "cache-control":"no-store"}); res.end(JSON.stringify(value)); };
const safeId = (id: string) => /^[A-Za-z0-9][A-Za-z0-9._-]{0,127}$/.test(id) ? id : null;
async function ledgers() { try { const names = await readdir(ledgerRoot); return (await Promise.all(names.filter(n => extname(n) === ".ndjson" && safeId(n.slice(0,-7))).map(async n => ({id:n.slice(0,-7), raw_untrusted:true})))); } catch { return []; } }
async function entries(id: string, offset: number, limit: number) { const name = `${id}.ndjson`, target = resolve(ledgerRoot, name); if (relative(ledgerRoot, target).startsWith("..")) throw new Error("path"); const root = await realpath(ledgerRoot); const actual = await realpath(target); if (!actual.startsWith(root + "/")) throw new Error("path"); if (!(await stat(actual)).isFile()) throw new Error("path"); const lines = (await readFile(actual,"utf8")).split("\n").filter(Boolean); return lines.slice(offset, offset + limit).map((line, index) => { try { return {offset:offset+index, value:JSON.parse(line)}; } catch { return {offset:offset+index, value:{raw_untrusted_line:line.slice(0,8192)}}; }}); }
const mime = (path:string) => path.endsWith(".js") ? "text/javascript" : path.endsWith(".css") ? "text/css" : "text/html";
createServer(async (req,res) => { const url = new URL(req.url ?? "/", `http://${req.headers.host}`); if (req.method !== "GET") return json(res,405,{error:"read-only"}); try {
  if (url.pathname === "/v1/ledgers") return json(res,200,{items:await ledgers()});
  const match = url.pathname.match(/^\/v1\/ledgers\/([^/]+)\/entries$/); if (match) { const id=safeId(match[1]); const offset=Number(url.searchParams.get("offset")??0), limit=Number(url.searchParams.get("limit")??50); if(!id||!Number.isInteger(offset)||offset<0||!Number.isInteger(limit)||limit<1||limit>200) return json(res,400,{error:"invalid request"}); return json(res,200,{ledgerId:id,raw_untrusted:true,entries:await entries(id,offset,limit)}); }
  const path = url.pathname === "/" ? "index.html" : url.pathname.slice(1); const target=resolve(publicRoot,path); if(relative(publicRoot,target).startsWith("..")) return json(res,404,{error:"not found"}); const content=await readFile(target); res.writeHead(200,{"content-type":mime(target)+"; charset=utf-8","cache-control":"no-store"}); res.end(content);
 } catch { if (!res.headersSent) json(res,404,{error:"ledger not found or not selected"}); else res.end(); }}).listen(port,"127.0.0.1",()=>console.log(`CWR monitor on 127.0.0.1:${port}; root=${ledgerRoot}`));
