# EthnoBotaPhyWR Agent Guidelines

Read this file before modifying code. Keep this surface strictly separate from human-facing docs: never reference `AGENTS.md` or `.agents/` in `README.qmd`, `README.md`, `CONTRIBUTING.md`, `NEWS.md`, `DESCRIPTION`, `inst/`, `man/`, `R/`, or `vignettes/`.

## Verification Commands

Run the cheapest check first during iteration, then the full gate before committing:

```bash
air format . --check
jarl check .
Rscript -e "stopifnot(length(lintr::lint_package()) == 0L)"
Rscript -e "testthat::test_local(filter = 'calculate-branches-values')"
Rscript -e "testthat::test_local()"
Rscript -e "stopifnot(nrow(spelling::spell_check_package()) == 0L)"
Rscript -e "roxygen2::roxygenise()"
```

Clean build and CRAN check (run in an isolated copy via `git archive` for releases):

```bash
R CMD build --no-manual .
R CMD check --no-manual --as-cran EthnoBotaPhyWR_*.tar.gz
```

## Repo Map

- `R/` — Package source; `NAMESPACE` exports **only** `ethnobotafever_tree()` (`ethnobotafever_tree.R`) plus lazy datasets (`data.R`). All other functions (`calculate_branches_values.R`, `get_trees.R`, `get_results_wd.R`, `get_npclassifier_taxonomy.R`, `plot_trees.R`, `cascade_*.R`, `normalize_ott_ids.R`) carry `@keywords internal` or `@noRd`.
- `data/` — Lazy-loaded plain base `data.frame` `.rda` datasets (`orchids_compounds`, `orchids_compound_classes`, `orchids_ethnobotanical`) and MD5-keyed Open Tree of Life `phylo` caches (`trees_<hash>.rda`).
- `data-raw/` — Curated input TSVs, `data-raw/orchids_*.R` build scripts, summary count TSVs, `Orchidaceae_log.txt`, and raw `itol/` exports (excluded from R build tarball via `.Rbuildignore`).
- `inst/extdata/itol/` — Bundled *Orchidaceae* Newick `.tre` and iTOL v6 `.txt` files shipped inside the installed package.
- `inst/scripts/ethnobotafever_tree.R` — Reference script used to regenerate *Orchidaceae* outputs against live Wikidata (QLever), Open Tree of Life, and NPClassifier.
- `tests/testthat/` — Offline unit, pipeline-mocked, and package-invariant tests (must require zero network access).
- `vignettes/articles/` — Quarto site articles (excluded from package build via `.Rbuildignore`; built by `altdoc`).

## Non-Negotiable Invariants

1. **API & Dependencies**: Do not add runtime `Imports` to `DESCRIPTION` or export internal helpers without explicit approval.
2. **Code Style & Linting**: Zero `<<-` assignments anywhere (`jarl` forbids `undesirable_operator`; use `new.env(parent = emptyenv())` for stateful closures). Keep `.Rbuildignore` and `inst/WORDLIST` sorted with `LC_ALL=C sort -u`.
3. **Dataset Format**: All `data/orchids_*.rda` objects must have `identical(class(x), "data.frame")` (`compress = "xz"`), never `tidytable` or `data.table`.
4. **Species-Level Tree & iTOL Semantics**: Species-level `taxa_known_sub` must have `species_count = 1L` and `ethnobotanical_count` in `{0L, 1L}`; never inherit parent-genus totals. Species compound joins must preserve non-medicinal phytochemical species (`taxon.y`).
5. **Definition of Done**: `air format . --check`, `jarl check .`, `lintr::lint_package()` (0 lints), `spelling::spell_check_package()` (0 findings), and `testthat::test_local()` (all pass, no skipped/relaxed assertions) pass after final edits. Commit agent docs in standalone `chore(agents):` commits only.

See `.agents/architecture-and-pitfalls.md` for architecture boundaries, cache invalidation rules, and code pitfalls, and `CONTRIBUTING.md` for contributor setup.
