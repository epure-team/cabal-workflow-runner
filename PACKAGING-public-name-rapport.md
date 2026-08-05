# cwr — publier `cabal_workflow_runner` comme bibliothèque consommable — rapport

Branche : `packaging/public-name-cabal-workflow-runner`, base `main` @ `e6bbbd8`.
Code/commits en anglais (convention du dépôt) ; rapport en français (convention du brief).

## 1. Le diff exact

```diff
diff --git a/cabal_workflow_runner.opam b/cabal_workflow_runner.opam
index 89ba388..68c6a5e 100644
--- a/cabal_workflow_runner.opam
+++ b/cabal_workflow_runner.opam
@@ -11,7 +11,7 @@ homepage: "https://github.com/epure-team/cabal-workflow-runner"
 bug-reports: "https://github.com/epure-team/cabal-workflow-runner/issues"
 depends: [
   "ocaml" {>= "5.3.0"}
-  "dune" {>= "3.13"}
+  "dune" {>= "3.13" & >= "3.13"}
   "cabal"
   "yojson"
   "eio" {>= "1.0"}
diff --git a/lib/dune b/lib/dune
index 85a1cf0..523242c 100644
--- a/lib/dune
+++ b/lib/dune
@@ -1,5 +1,6 @@
 (library
  (name cabal_workflow_runner)
+ (public_name cabal_workflow_runner)
  (foreign_stubs
   (language c)
   (names secure_fs_stubs)
```

Deux fichiers touchés, `lib/dune` (1 ligne ajoutée) et `cabal_workflow_runner.opam` (régénéré par
`dune build`, jamais édité à la main).

## 2. Réponses aux [VÉRIFIE]

1. **Build/test internes non cassés.**
   - `opam exec -- dune build` : exit 0, aucune sortie (silence = succès dune).
   - `opam exec -- dune test` : **161 tests, tous `[OK]`** — 15 (`proof-carrying-change`) + 21
     (`cwr-attestation`) + 125 (`cabal_workflow_runner`) — plus `approval ledger selftest: PASS`.
     Dépasse le "146+" mentionné dans le brief (le nombre exact de tests a un peu bougé depuis la
     rédaction du brief, rien d'anormal). Sortie complète collée en §3 du rapport de session (pas
     reproduite ici pour la lisibilité — disponible sur demande, ou en relançant
     `opam exec -- dune test`).
   - `bin/dune` et `test/dune` référencent toujours `cabal_workflow_runner` par son `(name)`, pas
     par un chemin d'install — dune continue de résoudre la lib locale sans ambiguïté à
     l'intérieur du workspace cwr. Confirmé par le build/test verts ci-dessus (si la résolution
     locale avait cassé, `bin/main.ml` et tout `test/*.ml` auraient échoué à compiler).
   - **Aucune collision de nom findlib** : `ocamlfind list | grep cabal_workflow_runner` ne
     retourne qu'**une seule** entrée avant et après le changement (avant : présente avec
     `version: n/a`, c'est-à-dire enregistrée par opam mais sans bibliothèque installée dessous —
     exactement le symptôme du gap ; après : `cabal_workflow_runner (version: fe59ef8)`, avec les
     `.cma`/`.cmxa`/`.a`/`.cmi` réellement présents sous
     `<switch>/lib/cabal_workflow_runner/`).

2. **`.opam` régénéré, diff cohérent.** Le seul changement dans `depends` est
   `"dune" {>= "3.13"}` -> `"dune" {>= "3.13" & >= "3.13"}` — une contrainte dupliquée mais
   **sémantiquement identique** (aucune dépendance perdue, aucune borne resserrée/élargie). C'est
   dune qui ajoute automatiquement une borne `dune >= 3.13` liée à l'installation d'une lib
   publique portant des `foreign_stubs`, et qui ne dé-duplique pas contre la borne déjà posée par
   `(lang dune 3.13)` du `dune-project`. Esthétiquement redondant, fonctionnellement sans effet ;
   non retouché à la main (consigne du brief : ne jamais éditer le `.opam` généré directement).

3. **Lien des stubs C depuis l'extérieur — vérifié empiriquement, §3 ci-dessous.** Le
   `dune-package` installé porte désormais `(sections (lib .) (libexec .) (bin ../../bin)
   (doc ...) (stublibs ../stublibs))` — les stubs sont bien dans le plan d'install dune généré
   automatiquement par `public_name` + `foreign_stubs`, sans configuration additionnelle.

4. **Rien d'autre changé.** `git diff --stat main packaging/public-name-cabal-workflow-runner` :
   ```
    cabal_workflow_runner.opam | 2 +-
    lib/dune                   | 1 +
    2 files changed, 2 insertions(+), 1 deletion(-)
   ```
   Aucun `.ml`/`.mli`/`.c` touché. `bin/`, `test/`, les workflows d'exemple : intouchés.

5. **Version de `(lang dune ...)`.** `3.13` (déjà déclarée par `dune-project`) a suffi ;
   `public_name` + `foreign_stubs` installés sans bump. Aucune erreur de build réclamant une
   version dune supérieure.

## 3. Test du consommateur externe (le critère clé)

Projet jetable, **hors** de cwr, à `/workspace/cwr-consumer-test` :

`dune-project` :
```
(lang dune 3.13)
```

`bin/dune` :
```
(executable
 (name t)
 (libraries cabal_workflow_runner))
```

`bin/t.ml` (référence un symbole de chaque module critique, y compris `Secure_fs` pour forcer le
lien des stubs C `secure_fs_stubs.c`) :
```ocaml
let () =
  ignore Cabal_workflow_runner.Engine.run ;
  ignore Cabal_workflow_runner.Validate.workflow ;
  ignore Cabal_workflow_runner.Canonical_json.to_string ;
  ignore Cabal_workflow_runner.Secure_fs.ledger_open ;
  ignore Cabal_workflow_runner.Ledger.to_ndjson ;
  print_endline "links against cabal_workflow_runner"
```

Commandes et sortie réelles :
```
$ opam pin add cabal_workflow_runner \
    "git+https://github.com/epure-team/cabal-workflow-runner.git#packaging/public-name-cabal-workflow-runner"
...
-> installed cabal_workflow_runner.~dev
Done.

$ cd /workspace/cwr-consumer-test && dune build
$ echo $?
0

$ ./_build/default/bin/t.exe
links against cabal_workflow_runner
$ echo $?
0
```

**Critère satisfait** : le binaire compile, **linke** (y compris `secure_fs_stubs`, via
`Secure_fs.ledger_open` forcé dans `t.ml` — aucune erreur de symbole), et s'exécute. Le pin
pointe sur la branche `packaging/public-name-cabal-workflow-runner` poussée sur
`epure-team/cabal-workflow-runner` (pas un chemin local — un vrai pin git, comme le ferait
`cabal-chat`).

## 4. Confirmation qu'aucun `.ml/.mli/.c` de `lib/` n'a bougé

```
$ git diff --stat main packaging/public-name-cabal-workflow-runner
 cabal_workflow_runner.opam | 2 +-
 lib/dune                   | 1 +
 2 files changed, 2 insertions(+), 1 deletion(-)
```
Confirmé : zéro fichier sous `lib/*.ml`/`*.mli`/`*.c` dans le diff. La copie de code reste
byte-identique à `e6bbbd8` ; seule sa publication change, exactement l'objectif énoncé en §0 du
brief.

## 5. Gap #2 (cabal) — mentionné, non traité

Comme demandé, non traité ici : `Claude_code.available`/`Codex_cli.available` (utilisés par cwr's
propre `bin/backend_cabal.ml`) semblent absents du `.mli` public actuel de `cabal`
(`epure-team/cabal`, vérifié par grep exhaustif de `^val` dans `claude_code.mli` lors du travail
`cabal-chat` précédent). C'est un gap côté `cabal`, indépendant de ce dé-vendoring, et il ne
bloque rien ici. À ouvrir comme petite PR `cabal` distincte, non prioritaire.

## 6. Non fait, et pourquoi

- **Tests `pcc-integration`** (`test/test_pcc_integration.ml`, alias séparé, exige un checkout
  `arch-index` sibling construit avec `pcc-index`/`pcc-dossier`/`pcc-preflight`/`arch-impact`/
  `arch-rules` sur `PATH`) : **non exécutés**. `arch-index` est présent dans cette session mais
  non construit (dépendances propres : `sqlite3`, `caqti`, `tls-eio`, etc., non nécessaires au
  changement de packaging lui-même). Ce test valide le comportement du moteur cwr contre l'outillage
  arch-index — orthogonal à un changement qui ne touche aucun `.ml`/`.mli`/`.c`. La suite standard
  (161 tests, §2 point 1) plus la preuve du consommateur externe (§3, le critère explicitement
  désigné comme clé par le brief) couvrent ce qu'un changement packaging-only peut faire dévier.
  Si souhaité, je peux construire `arch-index` et relancer ce job dans un tour de suite.
- **PR GitHub** : la branche est poussée ; je n'ai pas ouvert de pull request (pas demandé
  explicitement dans ce tour — je le fais sur confirmation).

## 7. Fichiers touchés + commit

- **`epure-team/cabal-workflow-runner`**, branche `packaging/public-name-cabal-workflow-runner` :
  - `lib/dune` (+1 ligne : `(public_name cabal_workflow_runner)`)
  - `cabal_workflow_runner.opam` (régénéré par `dune build`)
  - ce rapport, `PACKAGING-public-name-rapport.md`
  - commit : `lib: publish cabal_workflow_runner as an installable opam library`
