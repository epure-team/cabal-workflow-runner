const app = document.querySelector("#app");
const esc = (x) => JSON.stringify(x, null, 2);
let current = "", offset = 0, limit = 50;
async function get(path) { const r = await fetch(path); if (!r.ok)
    throw new Error(r.status === 404 ? "Ledger introuvable ou non sélectionné" : `Erreur ${r.status}`); return r.json(); }
function banner() { return `<div class="trust"><b>raw_untrusted</b><span>Données brutes non vérifiées — aucune intégrité, attestation ou possibilité de replay n’est confirmée.</span></div>`; }
async function home() { const data = await get("/v1/ledgers"); app.innerHTML = `<main><header><p class="eyebrow">CWR / MONITOR</p><h1>Ledgers</h1><p>Console de preuve · lecture seule</p></header>${banner()}<section class="panel"><input id="q" placeholder="Rechercher un ledger" aria-label="Rechercher un ledger"><div id="list">${data.items.length ? data.items.map(x => `<button class="ledger" data-id="${x.id}"><code>${x.id}</code><span>Consulter →</span></button>`).join("") : "<p>Aucun ledger sélectionné pour cet opérateur.</p>"}</div></section></main>`; document.querySelectorAll(".ledger").forEach(b => b.onclick = () => { current = b.dataset.id; offset = 0; view(); }); document.querySelector("#q").oninput = e => { const q = e.target.value.toLowerCase(); document.querySelectorAll(".ledger").forEach(x => x.hidden = !x.dataset.id.toLowerCase().includes(q)); }; }
async function view() { try {
    const d = await get(`/v1/ledgers/${encodeURIComponent(current)}/entries?offset=${offset}&limit=${limit}`);
    app.innerHTML = `<main><button class="back">← Ledgers</button><header><p class="eyebrow">LECTURE SEULE</p><h1><code>${current}</code></h1><p>Entrées ${offset + 1}–${offset + d.entries.length}</p></header>${banner()}<section class="grid"><div class="timeline">${d.entries.length ? d.entries.map(e => `<button class="entry" data-i="${e.offset}"><small>#${e.offset}</small><b>${typeof e.value === "object" && e.value && "kind" in e.value ? String(e.value.kind) : "JSON"}</b><span>${esc(e.value).slice(0, 72)}</span></button>`).join("") : "<p>Aucune entrée à cet offset.</p>"}<footer><button id="prev" ${offset === 0 ? "disabled" : ""}>Précédent</button><label>Par page <select id="limit">${[25, 50, 100, 200].map(n => `<option ${n === limit ? "selected" : ""}>${n}</option>`).join("")}</select></label><button id="next" ${d.entries.length < limit ? "disabled" : ""}>Suivant</button></footer></div><pre id="inspect">Sélectionnez une entrée pour lire son JSON.</pre></section></main>`;
    document.querySelector(".back").addEventListener("click", home);
    document.querySelectorAll(".entry").forEach((b, i) => b.onclick = () => { document.querySelector("#inspect").textContent = esc(d.entries[i].value); });
    document.querySelector("#prev").onclick = () => { offset = Math.max(0, offset - limit); view(); };
    document.querySelector("#next").onclick = () => { offset += limit; view(); };
    document.querySelector("#limit").onchange = e => { limit = Number(e.target.value); offset = 0; view(); };
}
catch (e) {
    app.innerHTML = `<main><h1>Consultation indisponible</h1>${banner()}<p>${String(e.message)}</p><button class="back">← Ledgers</button></main>`;
    document.querySelector(".back").addEventListener("click", home);
} }
home().catch(e => app.innerHTML = `<main><h1>Erreur réseau</h1><p>${String(e)}</p></main>`);
export {};
