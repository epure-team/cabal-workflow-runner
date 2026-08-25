const app = document.querySelector("#app");
const isRecord = (value) => typeof value === "object" && value !== null && !Array.isArray(value);
const text = (record, key) => typeof record[key] === "string" ? record[key] : null;
const number = (record, key) => typeof record[key] === "number" ? record[key] : null;
const h = (value) => String(value ?? "").replace(/[&<>"']/g, (char) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", "\"": "&quot;", "'": "&#39;" })[char]);
const json = (value) => JSON.stringify(value, null, 2);
const compact = (value, max = 150) => {
    const raw = typeof value === "string" ? value : JSON.stringify(value);
    if (!raw)
        return "—";
    const oneLine = raw.replace(/\s+/g, " ").trim();
    return oneLine.length > max ? `${oneLine.slice(0, max)}…` : oneLine;
};
const kindOf = (value) => isRecord(value) && typeof value.kind === "string" ? value.kind : "unknown";
let current = "";
let offset = 0;
let limit = 100;
async function get(path) {
    const response = await fetch(path, { headers: { accept: "application/json" } });
    if (!response.ok)
        throw new Error(response.status === 404 ? "Ledger introuvable ou momentanément indisponible" : `Erreur HTTP ${response.status}`);
    return response.json();
}
function banner() {
    return `<aside class="trust" role="note"><span class="trust-mark">!</span><div><b>raw_untrusted</b><span>Données brutes non vérifiées. Aucune intégrité, attestation ou possibilité de replay n’est confirmée par cette vue.</span></div></aside>`;
}
function formatDate(value) {
    const date = new Date(value);
    return Number.isNaN(date.valueOf()) ? "date inconnue" : new Intl.DateTimeFormat("fr-FR", { dateStyle: "medium", timeStyle: "medium" }).format(date);
}
function formatBytes(bytes) {
    return bytes < 1024 ? `${bytes} o` : `${(bytes / 1024).toFixed(bytes < 10240 ? 1 : 0)} Ko`;
}
function outcome(value) {
    if (!isRecord(value))
        return null;
    switch (value.kind) {
        case "committed": return { label: "Commit effectué", tone: "success" };
        case "completed_no_commit": return { label: "Terminé sans commit", tone: "success" };
        case "blocked": return { label: "Bloqué", tone: "danger" };
        case "aborted": return { label: "Interrompu", tone: "danger" };
        default: return null;
    }
}
function eventTone(value) {
    if (!isRecord(value))
        return "warning";
    if (value.success === false || value.passed === false || value.verdict === "fail" || value.kind === "blocked_at" || "raw_untrusted_line" in value)
        return "danger";
    const result = isRecord(value.result) ? value.result : null;
    if (typeof result?.exit === "number" && result.exit !== 0)
        return "danger";
    const terminal = outcome(value.outcome);
    if (terminal)
        return terminal.tone;
    if (value.success === true || value.passed === true || value.verdict === "pass" || String(value.kind).endsWith("_completed"))
        return "success";
    if (String(value.kind).includes("started") || value.kind === "agent_ran")
        return "info";
    return "neutral";
}
const labels = {
    agent_ran: "Agent", gate_evaluated: "Gate", branch_taken: "Branche", loop_iter: "Itération", budget_read: "Budget",
    fixpoint_progress: "Progression", loop_stopped: "Boucle terminée", run_executed: "Commande",
    commit_preflight_executed: "Préflight", committed_step: "Commit", blocked_at: "Blocage",
    parallel_started: "Parallèle démarré", parallel_branch_completed: "Branche parallèle", parallel_completed: "Parallèle terminé",
    foreach_iter_started: "Élément démarré", foreach_iter_completed: "Élément terminé", foreach_completed: "Foreach terminé",
    ctx_snapshot: "Contexte initial", approval_supplied: "Approbation fournie", shell_executed: "Shell",
    evidence_evaluated: "Preuve évaluée", attestation_exported: "Attestation", spawn_started: "Délégation démarrée",
    spawn_child_completed: "Sous-agent terminé", spawn_completed: "Délégation terminée", unknown: "Entrée brute",
};
function eventIdentity(value) {
    return text(value, "child_id") ?? text(value, "id") ?? (number(value, "branch_idx") !== null ? `branche ${number(value, "branch_idx")}` : "");
}
function eventSummary(value) {
    if (!isRecord(value))
        return compact(value);
    const terminal = outcome(value.outcome);
    if (terminal)
        return terminal.label;
    if (typeof value.success === "boolean")
        return value.success ? "Exécution réussie" : "Exécution en échec";
    if (typeof value.passed === "boolean")
        return value.passed ? "Preuve validée" : "Preuve refusée";
    if (typeof value.verdict === "string")
        return value.verdict === "pass" ? "Condition validée" : "Condition refusée";
    if (isRecord(value.result)) {
        const exit = number(value.result, "exit");
        const stdout = text(value.result, "stdout");
        return `Code ${exit ?? "?"}${stdout ? ` · ${compact(stdout, 110)}` : ""}`;
    }
    if (Array.isArray(value.results)) {
        const failures = value.results.filter((item) => isRecord(item) && item.exit_code !== 0).length;
        return `${value.results.length} commande${value.results.length > 1 ? "s" : ""}${failures ? ` · ${failures} en échec` : " · toutes réussies"}`;
    }
    if (value.kind === "spawn_started")
        return "Exécution séquentielle des enfants";
    if (value.kind === "spawn_completed")
        return `${number(value, "children") ?? 0} enfant(s) traité(s)`;
    if (value.kind === "ctx_snapshot" && isRecord(value.ctx))
        return `${Object.keys(value.ctx).length} clé(s) de contexte`;
    if (typeof value.reason === "string")
        return value.reason;
    if ("output" in value)
        return compact(value.output);
    return compact(value);
}
function eventBody(value) {
    if (!isRecord(value))
        return "";
    if (Array.isArray(value.results)) {
        return `<div class="command-list">${value.results.map((item) => {
            const row = isRecord(item) ? item : {};
            const code = number(row, "exit_code");
            return `<div><code>${h(text(row, "command") ?? "commande inconnue")}</code><span class="exit ${code === 0 ? "ok" : "bad"}">exit ${h(code ?? "?")}</span></div>`;
        }).join("")}</div>`;
    }
    if (isRecord(value.result)) {
        const stdout = text(value.result, "stdout");
        const stderr = text(value.result, "stderr");
        const files = Array.isArray(value.result.files) ? value.result.files : [];
        return `<div class="result-preview">${stdout ? `<pre><span>stdout</span>${h(compact(stdout, 360))}</pre>` : ""}${stderr ? `<pre class="stderr"><span>stderr</span>${h(compact(stderr, 240))}</pre>` : ""}${files.length ? `<p>${files.length} fichier(s) modifié(s)</p>` : ""}</div>`;
    }
    const payload = value.kind === "agent_ran" ? value.output : value.kind === "spawn_child_completed" ? value.ctx : value.kind === "ctx_snapshot" ? value.ctx : null;
    return payload === null || payload === undefined ? "" : `<div class="payload-preview"><span>${value.kind === "agent_ran" ? "sortie" : "contexte"}</span><code>${h(compact(payload, 300))}</code></div>`;
}
function spawnDepths(entries) {
    const depths = new Map();
    let depth = 0;
    for (const entry of entries) {
        const kind = kindOf(entry.value);
        if (kind === "spawn_completed")
            depth = Math.max(0, depth - 1);
        depths.set(entry.offset, depth);
        if (kind === "spawn_started")
            depth += 1;
    }
    return depths;
}
function eventCard(entry, depth) {
    const value = isRecord(entry.value) ? entry.value : {};
    const kind = kindOf(entry.value);
    const tone = eventTone(entry.value);
    const identity = eventIdentity(value);
    return `<article class="event tone-${tone}" style="--depth:${Math.min(depth, 3)}"><div class="rail"><span></span></div><div class="event-main"><div class="event-head"><div><small>#${entry.offset}</small><strong>${h(labels[kind] ?? kind)}</strong>${identity ? `<code>${h(identity)}</code>` : ""}</div><span class="status ${tone}">${tone === "danger" ? "attention" : tone === "success" ? "ok" : tone === "info" ? "actif" : "info"}</span></div><p>${h(eventSummary(entry.value))}</p>${eventBody(entry.value)}<button class="inspect" data-offset="${entry.offset}">Voir le JSON</button></div></article>`;
}
function terminalStatus(summary) {
    if (!summary.total)
        return { label: "Vide", tone: "neutral", detail: "Aucun événement enregistré" };
    if (summary.attention)
        return { label: "Attention", tone: "danger", detail: `${summary.attention} événement(s) à vérifier` };
    if (summary.latestKind?.endsWith("_completed") || summary.latestKind === "committed_step")
        return { label: "Terminé", tone: "success", detail: labels[summary.latestKind] ?? summary.latestKind };
    return { label: "En cours ou incomplet", tone: "info", detail: labels[summary.latestKind ?? "unknown"] };
}
function stats(summary) {
    const state = terminalStatus(summary);
    const agents = summary.kinds.agent_ran ?? 0;
    const commands = (summary.kinds.shell_executed ?? 0) + (summary.kinds.run_executed ?? 0);
    const delegations = summary.kinds.spawn_child_completed ?? 0;
    return `<section class="stats" aria-label="Synthèse du run"><div class="stat primary"><span>État observé</span><strong class="text-${state.tone}">${h(state.label)}</strong><small>${h(state.detail)}</small></div><div class="stat"><span>Événements</span><strong>${summary.total}</strong><small>${formatBytes(summary.bytes)}</small></div><div class="stat"><span>Agents</span><strong>${agents}</strong><small>${delegations} sous-agent(s)</small></div><div class="stat"><span>Commandes</span><strong>${commands}</strong><small>${summary.kinds.gate_evaluated ?? 0} gate(s)</small></div></section>`;
}
async function home() {
    app.innerHTML = `<main class="loading"><p>Lecture des ledgers…</p></main>`;
    try {
        const data = await get("/v1/ledgers");
        app.innerHTML = `<main><header class="hero"><div><p class="eyebrow">CWR / MONITOR</p><h1>Exécutions</h1><p>Suivi factuel des pipelines · lecture seule</p></div><span class="read-only">GET ONLY</span></header>${banner()}<section class="panel ledger-panel"><div class="panel-head"><div><h2>Ledgers disponibles</h2><p>${data.items.length} source(s) locale(s)</p></div><input id="q" type="search" placeholder="Rechercher…" aria-label="Rechercher un ledger"></div><div id="list" class="ledger-list">${data.items.length ? data.items.map((item) => {
            const state = terminalStatus(item.summary);
            return `<button class="ledger" data-id="${h(item.id)}"><span class="ledger-state ${state.tone}"></span><span class="ledger-name"><code>${h(item.id)}</code><small>${formatDate(item.summary.updatedAt)}</small></span><span class="ledger-metrics"><b>${item.summary.total}</b><small>événements</small></span><span class="status ${state.tone}">${h(state.label)}</span><span class="arrow">→</span></button>`;
        }).join("") : `<div class="empty"><span>◇</span><h2>Aucun ledger</h2><p>Le répertoire surveillé ne contient encore aucune trace NDJSON.</p></div>`}</div></section></main>`;
        document.querySelectorAll(".ledger").forEach((button) => { button.onclick = () => { current = button.dataset.id; offset = 0; void view(); }; });
        document.querySelector("#q")?.addEventListener("input", (event) => {
            const query = event.target.value.toLowerCase();
            document.querySelectorAll(".ledger").forEach((row) => { row.hidden = !row.dataset.id.toLowerCase().includes(query); });
        });
    }
    catch (error) {
        renderError("Impossible de charger les ledgers", error, home);
    }
}
async function view() {
    app.innerHTML = `<main class="loading"><p>Lecture de ${h(current)}…</p></main>`;
    try {
        const data = await get(`/v1/ledgers/${encodeURIComponent(current)}/entries?offset=${offset}&limit=${limit}`);
        const depths = spawnDepths(data.entries);
        const end = Math.min(offset + data.entries.length, data.summary.total);
        app.innerHTML = `<main><nav><button class="back">← Tous les ledgers</button><button class="refresh">Actualiser</button></nav><header class="run-head"><div><p class="eyebrow">PIPELINE · LECTURE SEULE</p><h1><code>${h(current)}</code></h1><p>Mis à jour ${formatDate(data.summary.updatedAt)}</p></div><span class="source-chip">NDJSON local</span></header>${banner()}${stats(data.summary)}<section class="workspace"><div class="timeline-panel"><div class="section-head"><div><h2>Timeline</h2><p>Événements ${data.summary.total ? offset + 1 : 0}–${end} sur ${data.summary.total}</p></div></div><div class="timeline">${data.entries.length ? data.entries.map((entry) => eventCard(entry, depths.get(entry.offset) ?? 0)).join("") : `<div class="empty"><span>◇</span><h2>Aucune donnée ici</h2><p>Ce ledger est vide ou cette page dépasse sa dernière entrée.</p></div>`}</div><footer><button id="prev" ${offset === 0 ? "disabled" : ""}>← Précédent</button><label>Page <select id="limit">${[25, 50, 100, 200].map((size) => `<option ${size === limit ? "selected" : ""}>${size}</option>`).join("")}</select></label><button id="next" ${end >= data.summary.total ? "disabled" : ""}>Suivant →</button></footer></div><aside class="inspector" id="inspector"><div class="inspector-empty"><span>{ }</span><h2>Détail brut</h2><p>Sélectionnez « Voir le JSON » sur un événement.</p></div></aside></section></main>`;
        document.querySelector(".back").onclick = () => void home();
        document.querySelector(".refresh").onclick = () => void view();
        document.querySelectorAll(".inspect").forEach((button) => {
            button.onclick = () => {
                const entry = data.entries.find((candidate) => candidate.offset === Number(button.dataset.offset));
                if (!entry)
                    return;
                document.querySelectorAll(".event.selected").forEach((row) => row.classList.remove("selected"));
                button.closest(".event")?.classList.add("selected");
                document.querySelector("#inspector").innerHTML = `<div class="inspector-head"><div><small>ENTRÉE #${entry.offset}</small><h2>${h(labels[kindOf(entry.value)] ?? kindOf(entry.value))}</h2></div><button class="close" aria-label="Fermer">×</button></div><pre>${h(json(entry.value))}</pre>`;
                document.querySelector(".close").onclick = () => { document.querySelector("#inspector").innerHTML = `<div class="inspector-empty"><span>{ }</span><h2>Détail brut</h2><p>Sélectionnez « Voir le JSON » sur un événement.</p></div>`; button.closest(".event")?.classList.remove("selected"); };
            };
        });
        document.querySelector("#prev").onclick = () => { offset = Math.max(0, offset - limit); void view(); };
        document.querySelector("#next").onclick = () => { offset += limit; void view(); };
        document.querySelector("#limit").onchange = (event) => { limit = Number(event.target.value); offset = 0; void view(); };
    }
    catch (error) {
        renderError("Consultation indisponible", error, view);
    }
}
function renderError(title, error, retry) {
    app.innerHTML = `<main><header><p class="eyebrow">CWR / MONITOR</p><h1>${h(title)}</h1></header>${banner()}<section class="error"><span>!</span><div><h2>La source n’a pas répondu</h2><p>${h(error instanceof Error ? error.message : String(error))}</p><button class="retry">Réessayer</button>${current ? `<button class="back">Tous les ledgers</button>` : ""}</div></section></main>`;
    document.querySelector(".retry").onclick = () => void retry();
    document.querySelector(".back")?.addEventListener("click", () => void home());
}
void home();
export {};
