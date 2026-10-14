# AUDIT.md — EthnoBotaPhyWR pre-publication audit (initial state)

Date: 2026-10-14 · Branch: `polish/pre-publication` · Auditor: automated review

## 1. What exists

| Aspect | State |
|---|---|
| Language / build | R package (DESCRIPTION/NAMESPACE), R >= 4.5.0, roxygen2 docs, man/ present |
| Version | 0.0.0.9001 (unreleased dev version) |
| Author | Adriano Rutz (ORCID 0000-0003-0443-9902), AGPL-3 declared in DESCRIPTION |
| Exported API | Single function `ethnobotafever_tree()` (885-line pipeline, R/ethnobotafever_tree.R) |
| Internal API | `get_trees.R` (OTT tree build + `.rda` cache), `get_results_wd.R` (SPARQL via qlever.dev), `get_npclassifier_taxonomy.R` (NPClassifier scrape), `calculate_branches_values.R` (tree node stats), `plot_trees.R` (ggtree/ggtreeExtra figures), `normalize_ott_ids.R` |
| Data | `data/`: 3 curated orchid datasets (compounds 3055×10, compound_classes 1574×5, ethnobotanical 419×3) + 2 tree-cache `.rda`; `data-raw/`: source TSVs + 3 preparation scripts + last-run log + 3 result TSVs |
| Article | `index.qmd` (Quarto article, the repo's "paper"); `index.html` (2.6 MB, committed) |
| Reproduction entry point | `inst/scripts/ethnobotafever_tree.R` (hard-coded repo-relative paths, **sources a remote file from GitHub**, writes into `data-raw/` and `man/figures/`) |
| Docs | `vignettes/orchids_datasets.qmd`; `man/*.Rd`; README.md generated from README.qmd |
| CI | 1 workflow: `format-check.yaml` (air format only) |
| Metadata | CITATION.cff (555 lines, mostly dependency refs), codemeta.json, prek.toml pre-commit hooks |
| Tests | **NONE** — `tests/` is empty |
| License file | **MISSING** — AGPL declared in DESCRIPTION but no LICENSE file in tree; `.Rbuildignore` even excludes `LICENSE.md` |
| Lockfile / container | **NONE** — no renv.lock, no Dockerfile/devcontainer |

### How the article's results are produced
`inst/scripts/ethnobotafever_tree.R` → `ethnobotafever_tree(taxon_name = "Orchidaceae", …)`:
1. `cascade::taxon_name_to_qid()` + SPARQL (qlever.dev) → subtaxa, OTT IDs, compounds, uses
2. Merge with bundled orchid datasets
3. `rotl` → induced subtrees (cached as `data/trees_*.rda`, key `tree-v1`)
4. `calculate_branches_values()` + `plot_trees()` → 3 PDFs in `man/figures/`
5. 3 TSVs in `data-raw/`; log in `data-raw/Orchidaceae_log.txt` (last run 2026-07-29)

### Verified working
- `R CMD INSTALL .` — OK (all deps present locally)
- `R CMD check --no-manual --no-vignettes` — **Status: OK**
- Package loads, datasets match documented dimensions
- Network reachable: `cascade::taxon_name_to_qid("Orchidaceae")` → `Q25308`

## 2. Broken / risky

### Scientific correctness
- **P0 — `get_uses_wd()` hard-codes `wd:Q25308` (Orchidaceae) in its SPARQL query, ignoring its `taxon_qid` argument** (R/get_results_wd.R:163). For any family other than Orchidaceae the ethnobotanical-use table is silently wrong. Verified no-op for the published orchid results (`taxon_name_to_qid("Orchidaceae") == "Q25308"`), but it is a latent scientific bug. Also lacks the input validation its sibling functions have.

### Reproducibility / installability
- **P0 — No lockfile, no container.** `Remotes: adafede/cascade` (GitHub-only dep) + unpinned Suggests make installs non-reproducible across time.
- **P0 — No one-command reproduce.** The repro script must be run from the repo root (`normalizePath(file.path("data-raw", ".."))`), writes outputs into the source tree (`data-raw/`, `man/figures/`), and `source()`s `colors.R` from a GitHub `main` branch (unverified, mutable URL). Verified that `prepare_hierarchy`/`prepare_plot` do **not** need that `colors` object — the remote source is dead weight and a network hazard.
- **P1 — Portability bug:** `paste0(log_dir, taxon_name, "_log.txt")` (R/ethnobotafever_tree.R:58) breaks when `log_dir` lacks a trailing separator (e.g. Windows).
- **P1 — `data-raw/` preparation scripts use CWD-relative paths** and are not wired to any `make`/pipeline target.
- **P1 — NPClassifier client** (R/get_npclassifier_taxonomy.R): SMILES are concatenated into the URL without percent-encoding (breaks on `+`, `#`, `(`, `)`), and HTML is parsed as JSON — fragile by construction; failures silently become `NA` rows.
- **P2 — Tree cache key** (`tree-v1`) does not embed package/data versions; stale caches survive package updates.

### Tests / CI
- **P0 — Zero tests.** `tests/` is empty; no testthat infrastructure.
- **P0 — CI only checks formatting.** No R CMD check, no tests, no coverage, single OS.

### Documentation
- **P0 — Vignette example is broken:** `system.file("data-raw", "orchids_compounds.R", package = "EthnoBotaPhyWR")` can never work — `data-raw` is excluded from the installed package (`.Rbuildignore`).
- **P0 — README:** CRAN badge points at `tima.png` (wrong package), "No Maintenance Intended" badge contradicts publication goal, no license/citation/contributing/quick-start-output sections; quick start writes to `~/`.
- **P1 — CITATION.cff:** `repository: https://bioconductor.org/` is wrong; no article reference/DOI; no release date; no keywords issue. `repository-code` is correct.
- **P2 — No pkgdown** (`_pkgdown.yml` referenced in `.Rbuildignore` but absent); no CHANGELOG/CONTRIBUTING/CODE_OF_CONDUCT/SECURITY/issue templates; no `.zenodo.json`.

### Repository hygiene
- **P0 — Generated artifacts committed:** `index.html` (2.6 MB), `man/figures/*.pdf` (208 KB), `.DS_Store`, `.idea/`, `.vscode/`, empty `logs/`.
- **P1 — `.cache/*.rds` cache files exist in git history** (commit b70df35; removed from tree later). Small; history rewrite not recommended without author approval.
- **P1 — `.Rbuildignore` excludes `LICENSE.md`** (a shipped LICENSE must not be ignored) while `grateful-refs.bib` is both committed and ignored.
- **P2 — No `.gitattributes`** (line-ending/encoding risks for a package with UTF-8 declared).
- **P2 — Dead code:** commented-out blocks in R/ethnobotafever_tree.R:494-496 and R/calculate_branches_values.R:129-154; commented `@examples` in several files.
- **P2 — `logger::log_appender(appender_tee(...))` is never reset** after the pipeline (side effect leaks to the session).
- **P2 — No seeds:** no `set.seed()` anywhere (pipeline itself is deterministic modulo live APIs; documented, not a defect).

### Legal
- **P0 — No LICENSE file** despite AGPL declaration. Third-party data (Wikidata CC0, LOTUS, NPClassifier) attributed only in prose; needs a data-provenance note + checksums for Zenodo deposition.

### Secrets scan
- Clean. Only hit is minified JS inside committed `index.html` (false positive). No tokens/keys/personal data beyond author's public email (already in DESCRIPTION).

## 3. Prioritised plan

**P0 (blocks publication)**
1. Fix `get_uses_wd()` hardcoded QID (no-op for orchids — verified) + add input validation
2. Add LICENSE (AGPL-3.0 full text); fix `.Rbuildignore`; align DESCRIPTION
3. Add testthat suite + offline end-to-end test with bundled fixtures (mock Wikidata/OTT/NPClassifier)
4. CI: R CMD check + tests + coverage on ≥ ubuntu/macOS, pinned actions
5. Fix broken vignette example; README overhaul (badges, install, reproduce, license, citation)
6. CITATION.cff: correct repository, article reference (placeholder DOI), release date
7. Remove generated artifacts from git tracking; add `.gitattributes`, tidy `.gitignore`

**P1 (pre-publication polish)**
8. Dockerfile (pinned R 4.5) + verify build; document session/tool versions
9. One-command `make reproduce` (CWD-independent, output dir configurable, no remote `source()`)
10. Portability fix for log path; NPClassifier URL-encoding + error surfacing
11. renv guidance (DESCRIPTION bounds + r-universe binaries; renv optional for analysts)
12. Lintr config + clean; remove dead code; reset logger appender

**P2 (release hygiene)**
13. pkgdown site + GHA deploy; CHANGELOG/CONTRIBUTING/CODE_OF_CONDUCT/SECURITY/issue templates; Dependabot
14. Coverage report + threshold; Zenodo `.zenodo.json` + data checksums; version tag + release notes (needs author decision)

## 4. Decisions needing author input
- Article DOI / journal reference for CITATION.cff
- Publication version number (proposed: `1.0.0`)
- Confirm AGPL-3.0 (vs CC-BY for article/code split already present in index.qmd)
- Zenodo deposition of datasets (requires author account)
- History rewrite for `.cache/*.rds` (not done; not needed for publication)
