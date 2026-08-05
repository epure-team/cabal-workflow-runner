# Jalon 3 — rapport : rendre `proof-carrying-change` réellement exécutable

Ce rapport couvre les deux dépôts : `epure-team/arch-index` (branche `claude/pcc-binaries-jalon3`)
et `epure-team/cabal-workflow-runner` (branche `claude/pcc-integration-jalon3`, ce dépôt).

**Résumé en une phrase : le chemin heureux est atteignable.** Les trois binaires `pcc-*` sont
livrés, `IT1` (chemin heureux → `Committed`), `IT2` (violation d'architecture réelle → `Blocked`
sur `g-rules-pass`) et `IT3` (régression réelle → `Blocked` sur `submit`/preflight) passent tous
les trois contre le **vrai** workflow, avec de **vrais** sous-processus (`arch-impact`,
`arch-rules`, `pcc-index`, `pcc-dossier`, `pcc-preflight`) et des agents simulés qui modifient
réellement le disque.

---

## 1. Phase 0 — quel index atteint `contract_ok:true`

**Commande exacte et valeur observée :**

```sh
# Producteur OCaml (CMT path)
./_build/default/bin/arch_callgraph_ocaml/arch_callgraph_ocaml.exe \
  --build-dir=<projet>/_build/default --db-path=<db> --schema-path=architecture-schema.sql
./arch-impact <db> --diff HEAD --format json
# => {"contract_ok": true, "verdict": "pass", ...}   (sur un projet OCaml/dune de contrôle)
```

```sh
# Producteur Go (NDJSON path)
arch-callgraph-go <module>/... | ./arch-load <db>
./arch-impact <db> --diff HEAD --format json
# => {"contract_ok": true, ...}   mais avec --fail-on-new-findings : verdict "refused" TOUJOURS
```

**Les deux producteurs atteignent `contract_ok:true`** sur un projet de contrôle (⊤-marquage
présent dans les deux cas — `callgraph_contract=v1`). Mais une **découverte de second ordre**
détermine le choix du langage de fixture :

- `arch-impact --fail-on-new-findings` ne peut renvoyer `verdict:"pass"` (plutôt que `"refused"`)
  que si l'index porte une analyse de décisions (`findings.computed:true`), c'est-à-dire une table
  `decisions` **non vide** (`Arch_db.nonempty` — `arch_impact.ml:262`, `arch_db.ml:312-317`). Un
  index dont la table `decisions` est vide — ce qui est le cas de **tout** index produit par
  `arch_callgraph_ocaml` ou `arch-callgraph-go` seuls — refuse **toujours** (`exit 3`,
  `verdict:"refused"`), jamais `"pass"`.
- Le seul outil qui peuple cette table à partir de vrai code est `poc/decision-lint`
  (`decision_lint --db <db>`), et **il n'existe que pour OCaml** (frontend Parsetree/Typedtree) —
  aucun équivalent Go.
- Conséquence : sur un projet Go, `g-computed` (qui lit `verdict`, pas `computed` — round-1 review
  du workflow) **bloque systématiquement**, quel que soit l'état réel du code, avant même que
  `g-rules-pass` ait une chance de s'exprimer. Le chemin heureux (`IT1`) est donc **irréalisable
  en Go** avec le pipeline actuel, et `IT2` (démontrer que `g-rules-pass` bloque *spécifiquement*,
  pas `g-computed` en amont) l'est tout autant.
- Sur OCaml, `decision-lint --db <db>` sur un fichier portant un vrai finding
  (`if a && b && a then 1 else 2`, motif du propre fixture de `decision-lint`) fait passer l'index
  de `verdict:"refused"` à `verdict:"pass"` + `contract_ok:true`, à condition que ce finding reste
  **hors du diff** de chaque itération (sinon il compterait comme `new_findings`).

**Décision : fixture OCaml.** C'est le seul chemin qui permette de démontrer à la fois `IT1`
(chemin heureux) et `IT2` (blocage nommément sur `g-rules-pass`, pas `g-computed`). `pcc-index`
appelle donc `arch_callgraph_ocaml` **et** `decision-lint --db` à chaque (ré)indexation.

Répond à la question ouverte §9.1 du brief : **OCaml**, empiriquement, pas par défaut.

## 2. Réponses aux [VÉRIFIE]

| # | Point à vérifier | Réponse |
|---|---|---|
| 1 | Commandes/arguments exacts du workflow | Lus dans `examples/proof-carrying-change.workflow.json` — `pcc-index --db .pcc/index.db` (×3 : `index`/`reindex`/`reindex-final`), `arch-impact <db> --diff HEAD --format json --fail-on-new-findings`, `arch-rules <db> arch-rules.txt --format json --on-unknown fail --on-possible fail --on-vacuous fail --on-not-computed fail`, `pcc-dossier --db .pcc/index.db --repo . --diff HEAD --rules arch-rules.txt --out .pcc/dossier.md`, `pcc-preflight` (comme `preflight` du `Commit "submit"`). |
| 2 | Contrat `pcc-*` (docs/proof-carrying-change.md) | Table "The `pcc-*` convention" lue intégralement ; contrat honoré tel quel (voir §3). Une divergence mineure notée et résolue en §5 ci-dessous (pas un `[STOP-ET-RAPPORTE]` — voir justification). |
| 3 | Motif `Backend.stub`/`run_command`/`run_agent` de `test_pcc.ml` | Lu en entier. Repris pour `floor_gates`, `pcc_allowlist`, `has_blocked_at`, le motif `engine_run` — dupliqué (pas partagé par un module commun) dans `test_pcc_integration.ml` pour ne jamais toucher `test_pcc.ml`. |
| 4 | Contrat JSON `arch-impact`/`arch-rules` (`docs/change-impact.md`, `docs/fitness-functions.md`) | Lus. `pcc-dossier` les **consomme** (`--format md`, mêmes flags que le workflow), ne les réinvente pas. |
| 5 | Étape "Self-index smoke test" de la CI arch-index | `.github/workflows/ci.yml:47-60` : `arch_callgraph_ocaml.exe --build-dir=_build/default/lib/arch_index --db-path=... --schema-path=architecture-schema.sql`. `pcc-index` reprend exactement ce pipeline (`dune build` puis l'exécutable direct-vers-DB), généralisé au dépôt courant. |
| 6 | `selftest-impact.sh`/`selftest-rules.sh` | Lus : confirment le contrat JSON strict (int/bool/string/null, jamais de float) et la commande `arch-query <db> stats`. `pcc-index` utilise une requête SQL directe (`sqlite3 <db> "SELECT count(*) FROM functions;"`) plutôt que de parser la sortie multi-lignes de `arch-query stats --format json` (qui imprime plusieurs objets JSON, un par section — pas un契 objet unique), en accord avec le §4.1 du brief ("ou une requête SQL directe"). |
| 7 (§3) | Commande exacte d'installation des deps cwr | CI cwr : `opam pin add -n cabal https://github.com/epure-team/cabal.git` puis `opam install . --deps-only --with-test`. Reproduite telle quelle dans le job `pcc-integration` et pour le setup local. |
| 8 (§5.1) | Où vit le vrai `run_command` de production | `bin/backend_cabal.ml:241-242` appelait `Runner.make`/`Runner.make_pinned`, définis dans `bin/runner.ml` (process réel + snapshot avant/après, via `Cabal.Backend_process.run_process`). Extrait dans sa propre bibliothèque `cwr_runner` (voir §4 "Garde-fous") pour que le test d'intégration réutilise la **même** fonction, pas une réimplémentation. |
| 9 (§5.1) | `working_dir` des steps | `"."` dans le workflow. `Cwr_runner.Runner.make ~base` résout `working_dir` par rapport à `~base` — le test passe `~base:fixture_dir` (une **copie** fraîche, jamais le template `test/fixtures/pcc-repo/` lui-même). |
| 10 (§5.2) | "why `--diff HEAD`" | `docs/proof-carrying-change.md`, section dédiée : `git diff HEAD` (un seul ref, pas `A..B`) diffe l'arbre de travail contre le dernier commit, donc voit les modifications non commitées de l'agent. Le fixture committe un état de base puis laisse chaque "author" simulé modifier les fichiers **sans commit**. |

## 3. Livré vs critères §6

- [x] Phase 0 documentée en tête du rapport.
- [x] `scripts/pcc/pcc-index`, `pcc-dossier`, `pcc-preflight` livrés dans arch-index, exécutables,
      honorant les contrats §4 et `docs/proof-carrying-change.md`.
- [x] `selftest-pcc.sh` vert et câblé en CI arch-index ; chaque assertion mutation-checkée (voir
      §4 "Preuve rouge-puis-vert").
- [x] Test d'intégration dans cwr : **IT1, IT2 et IT3 tous verts** (IT1 chemin heureux atteint,
      pas seulement IT2/IT3). `IT4` (non-⊤-marquage) **non livré** — non requis : le brief ne le
      demande que "si Phase 0 = pas de ⊤-marquage" (§5.3), et Phase 0 a montré le ⊤-marquage
      atteignable. Le blocage sur `g-sound` reste indirectement couvert par la discipline
      existante de `test_pcc.ml` (T4, avec un `Backend.stub`).
- [x] Chaque scénario asserte le gate/step bloquant nommément (`has_blocked_at ~id:"..."`).
- [x] Le test lance de vraies commandes (`pcc-index`/`pcc-dossier`/`pcc-preflight`/`arch-impact`/
      `arch-rules` en sous-processus, via `Cwr_runner.Runner.make` — le même chemin que `cwr run`
      en production) et des agents simulés qui modifient réellement le disque (`author_edit`
      écrit dans le fichier `.ml` du fixture avant de renvoyer sa sortie structurée). Aucun stub
      de sortie d'outil.
- [x] Job CI d'intégration câblé (`pcc-integration`), skip bruyant (`::warning`) si
      `epure-team/arch-index` n'est pas joignable — jamais silencieux ; de plus, le binaire de
      test lui-même échoue bruyamment (exit 3, message `PCC-INTEGRATION-SKIP`) si les outils
      n'étaient pas réellement sur le `PATH` malgré un job qui prétend les avoir installés.
- [x] `dune test` vert des deux côtés ; aucun test existant affaibli ; aucun `--format json`, exit
      code, ni selftest existant modifié. Le nouveau test d'intégration n'est **pas** dans l'alias
      `@runtest` par défaut (voir §4) : un `dune test` ordinaire, sans arch-index, reste vert
      exactement comme avant ce Jalon.
- [x] Deux branches poussées (`claude/pcc-binaries-jalon3` sur arch-index,
      `claude/pcc-integration-jalon3` sur cwr), aucune PR ouverte.

## 4. Scénarios

Chaque run ci-dessous est un run **réel** (`opam exec -- dune build @pcc-integration`, avec
`arch-index` construit et son root + `scripts/pcc/` sur `PATH`), pas une relecture de log.

| # | Mise en place | Issue obtenue | Gate/step bloquant |
|---|---|---|---|
| **IT1** | author simulé ajoute `let handle2 (x:int):int = x+2` à `src/ui.ml` (n'introduit aucune violation, ne touche pas le finding pré-existant de `src/misc.ml`, ne casse aucun test) ; fixer déclare `done` dès l'itération 1 ; reviewer approuve ; token fourni | **`Committed`** | — (ledger réel : `Ledger.to_ndjson`/`of_ndjson` round-trip, `Engine.replay` reproduit la même issue terminale) |
| **IT2** | author simulé réécrit `src/ui.ml` en `Db.write (x+1)` — violation réelle de `arch-rules.txt` ("ui must not reach db") ; fixer ne corrige jamais (`progressed:true, done:false` à chaque itération, comme `T2` de `test_pcc.ml`) | **`Blocked`** | **`g-rules-pass`**, nommément — ni `g-computed` ni `g-sound` (contract_ok reste `true` tout du long) |
| **IT3** | author simulé change `handle` pour renvoyer `x+999`, cassant l'assertion `handle 1 = 2` de `test/test_pccfix.ml` (aucune violation d'architecture, aucun nouveau finding decision-lint) ; fixer déclare `done` dès l'itération 1 ; reviewer approuve | **`Blocked`** | **`submit`** (preflight = vrai `dune build && dune test`) — `g-independent` (donc tous les gates tooled) était passé en amont |
| IT4 | *(non applicable)* | — | Phase 0 a montré le ⊤-marquage atteignable ; voir §3 |

### Preuve rouge-puis-vert (chaque assertion vue échouer avant d'être acceptée)

- **`pcc-index`** : mutation "toujours `contract_ok:false`" → `selftest-pcc.sh` rouge
  (`expected contract_ok:true ... got contract_ok: false`) → restauré → vert.
- **`pcc-dossier`** : mutation "avaler la sortie du sous-outil" (`echo "(suppressed)"` au lieu du
  vrai contenu) → `selftest-pcc.sh` rouge (`must actually surface the real arch-rules FAIL
  verdict`) → restauré → vert.
- **`pcc-preflight`** : mutation "toujours `ok:true`" → `selftest-pcc.sh` rouge (`must report
  ok:false ... got {"ok": true, ...}`) → restauré → vert.
- **`IT1`** : author remplacé par `author_rule_violation` → rouge (`expected Committed, got
  Blocked(gate "g-rules-pass" evaluated false)`) → restauré → vert.
- **`IT2`** : author remplacé par `author_clean_change` → rouge (`expected Blocked, got
  Committed(submit, ...)`) → restauré → vert.
- **`IT3`** : author remplacé par `author_clean_change` → rouge (`expected Blocked, got
  Committed(submit, ...)`) → restauré → vert.

## 5. Limites & découvertes

- **Pas de `[STOP-ET-RAPPORTE]` rencontré au sens strict** — le seul point de friction notable
  (ci-dessous) était résoluble sans changer le contrat ni recréer une divergence `contract_ok`.
- **Divergence mineure brief/doc sur `pcc-dossier`, résolue.** Le §4.2 du brief détaille "capture
  le JSON de chaque outil avec les flags exacts, puis rends un markdown clair" (implique un
  rendu markdown maison). `docs/proof-carrying-change.md` ("The `pcc-*` convention") décrit
  l'implémentation comme "runs arch-impact ... --format md and arch-rules ... --format md ... and
  concatenates them". `pcc-dossier` suit la doc (source de vérité désignée par le brief lui-même,
  §1 point 2) : il appelle les DEUX outils avec les mêmes flags EXACTS que le workflow (dont
  `--fail-on-new-findings` et les quatre `--on-* fail`) mais en `--format md` plutôt qu'en JSON
  ré-interprété à la main — ce qui évite de dupliquer la logique de rendu des outils et garantit
  que le dossier ne peut jamais diverger de leur propre rendu. Le contrat observable (fichier
  markdown non vide, exit 0 quoi qu'il arrive) est identique dans les deux lectures.
- **Le producteur Go n'a pas d'équivalent `decision-lint`.** Documenté en Phase 0 : ce n'est pas
  un bug de ce Jalon, mais une limite réelle du pipeline actuel qui mérite d'être remontée comme
  chantier séparé si un jour un fixture Go est souhaité pour ce workflow (répond à la question
  ouverte §9.3 : je livre le constat, pas une extension du ⊤-marquage — cf. la recommandation par
  défaut du brief).
- **`Runner` extrait en bibliothèque (`cwr_runner`).** `bin/runner.ml` (le vrai `run_command` de
  production) n'était accessible qu'à l'intérieur de l'exécutable `bin/main.exe`, pas à un
  exécutable de test. Extrait dans sa propre bibliothèque dune (`bin/dune`), sans changer une
  seule ligne de logique — seul `backend_cabal.ml` a été mis à jour pour référencer
  `Cwr_runner.Runner.make` au lieu de `Runner.make`. `lib/engine.ml`/`lib/validate.ml` et le
  workflow JSON mergé n'ont **pas** été touchés (garde-fou §7 respecté).
- **`Agent`'s `brief`/`protocol` ne sont PAS relatifs à `working_dir`.** Contrairement à
  `run_command`, ces lectures de fichier (`lib/engine.ml`) sont relatives au `cwd` du **process**
  au moment de l'appel, pas à un `~base`. Le test doit donc `Sys.chdir` vers le fixture avant
  `Engine.run` (et restaurer après) — sinon `author`'s `brief: ".pcc/task.md"` échoue avec
  `Aborted(agent "author": cannot read brief ...)`. Vu rouge avant d'être compris (voir historique
  de session), documenté ici pour la prochaine personne qui écrit ce genre de test.
- **Idempotence vérifiée empiriquement.** Le workflow appelle `pcc-index --db .pcc/index.db`
  jusqu'à 6 fois dans un seul run (`index`, jusqu'à 4× `reindex`, `reindex-final`) **sur le même
  fichier DB**, sans jamais le supprimer entre les appels. Vérifié à la main (`arch_callgraph_ocaml`
  et `decision-lint --db` réappliqués sur une DB existante) : ni doublon, ni erreur — les deux
  outils remplacent proprement leurs propres tables à chaque appel.
- **Performance.** Chaque scénario réel prend ~2s (IT1/IT3, convergence en 1 itération) à ~2-3s
  (IT2, 4 itérations réelles à cause du gouverneur `max_iters`) ; le test complet tourne en
  ~6 secondes. Pas de préoccupation de lenteur pour un job CI dédié.

## 6. Fichiers touchés

### `epure-team/arch-index` — branche `claude/pcc-binaries-jalon3`

Commit `6009aed` :
- `scripts/pcc/pcc-index` (nouveau)
- `scripts/pcc/pcc-dossier` (nouveau)
- `scripts/pcc/pcc-preflight` (nouveau)
- `selftest-pcc.sh` (nouveau)
- `.github/workflows/ci.yml` (câblage de `selftest-pcc.sh` dans l'étape "Shell integration tests")

### `epure-team/cabal-workflow-runner` — branche `claude/pcc-integration-jalon3`

- `bin/dune` — extraction de `Runner` (bin/runner.ml) dans sa propre bibliothèque `cwr_runner`
- `bin/backend_cabal.ml` — référence `Cwr_runner.Runner.make`/`make_pinned` au lieu de
  `Runner.make`/`make_pinned` (aucun changement de logique)
- `test/dune` — `data_only_dirs fixtures` (empêche dune de construire le fixture comme faisant
  partie du build par défaut) ; nouvel exécutable `test_pcc_integration` + règle
  `(alias pcc-integration)` (délibérément hors de `@runtest`)
- `test/test_pcc_integration.ml` (nouveau) — le test d'intégration lui-même
- `test/fixtures/pcc-repo/` (nouveau) — template de fixture OCaml/dune (jamais commité comme
  dépôt git imbriqué : `.git` généré à la volée par le test, sur une copie fraîche à chaque
  scénario)
- `.github/workflows/ci.yml` — jobs `arch-index-availability` (skip bruyant) et
  `pcc-integration` (checkout + build des deux dépôts, assertions de présence des binaires,
  exécution de `dune build @pcc-integration`)
- `JALON3-rapport.md` (ce fichier)
