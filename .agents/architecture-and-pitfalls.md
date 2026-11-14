# Architecture & Verified Pitfalls

## Architecture & Module Boundaries

`EthnoBotaPhyWR` exposes one public function (`ethnobotafever_tree()`) and lazy-loaded *Orchidaceae* datasets:

1. **Query & ID Resolution** (`cascade_query_wikidata.R`, `cascade_taxon_name_to_qid.R`, `get_results_wd.R`, `normalize_ott_ids.R`):
   - QLever SPARQL queries retrieve family subtaxa, Open Tree of Life (OTT) IDs, Wikidata compounds (`compound_smiles_canonical`, `compound_inchikey`, `connectivity_layer`), ethnobotanical use claims, and LOTUS background frequencies.
   - `taxon_name_to_qid()` escapes quotes/backslashes cleanly; `validate_taxon_qid()` enforces `^Q[1-9][0-9]*$`.
2. **Chemical Taxonomy** (`get_npclassifier_taxonomy.R`):
   - Queries GNPS2 NPClassifier (`/classify`) using `curl::curl_escape()`. Returns a 1-element list holding a 5-column `data.frame` (`query`, `pathway`, `superclass`, `class`, `glycoside`).
3. **Tree Building & Caching** (`get_trees.R`):
   - `get_trees()` queries Open Tree of Life `induced_subtree` in batch, substitutes `exemplar_ott_id` for paraphyletic genera, matches tips by `_ott<ID>`, and grafts unplaced taxa at their congeneric MRCA.
   - `get_trees_cache_key()` computes `"tree-v2"` MD5 over sorted OTT IDs plus `df_counted` (`taxon_id_ott`, `taxon_name`, `compounds_count`, `ethnobotanical_count`, `exemplar_ott_id`), reading/writing `data/trees_<hash>.rda`.
   - Changing `df_counted` counts (genus or species) changes the MD5 hash: regenerate `data/trees_*.rda`, `git rm` superseded `.rda`/`.Rd` files, update `R/data.R`, and run `roxygen2::roxygenise()`.
4. **Comparative Methods, Visualization & Exports** (`calculate_branches_values.R`, `cascade_prepare_hierarchy.R`, `cascade_prepare_plot.R`, `plot_trees.R`):
   - `calculate_branches_values()` computes `phylo_asr` (two-pass Felsenstein pruning on Grafen branch lengths), `eb_rate` (Empirical Bayes Beta-Binomial shrinkage), `hot_node_pval`/`hot_node_qval` (clade hypergeometric BH FDR), and exact analytical Blomberg's `K` (`compute_blombergs_k()`).
   - `ethnobotafever_tree()` renders `.svg`/`.pdf`/interactive `.html` bundles via `plot_trees()` and writes 12 Newick/iTOL v6 files (`*_full_genera`, `*_phyto_genera`, `*_phyto_species`).

## Nine Verified Pitfalls

1. **`tidytable::distinct()` uses `tidyselect`**: Never call helper transforms such as `blank_to_na(pathway)` inside `tidytable::distinct(...)`. Rename plain columns in `distinct()`, then transform inside `tidytable::mutate(...)`.
2. **`with_fruit_geom` in `R/plot_trees.R`**: `ggtreeExtra:::ggplot_add.fruit_plot` uses NSE and `do.call()` in `namespace:ggtreeExtra` without importing `geom_col` or `geom_col_interactive`. Keep `with_fruit_geom` and its comment intact when editing geom rings.
3. **`ape::reorder(tree, "postorder")` repeated parents**: Postorder edge matrices list each internal parent once per child edge. Always extract internal nodes via `unique(edges[, 1L], fromLast = TRUE)` so descendants strictly precede ancestors (both `calculate_branches_values.R` and `plot_trees.R`), and scale terminal tip variance in `felsenstein_asr()` by `b_ch / tip_w`.
4. **Exact Blomberg's `K` & RNG hygiene**: `compute_blombergs_k()` computes closed-form PIC + postorder GLS MSE0/MSE on finite (`!is.na`) tips only and restores `.Random.seed` with `on.exit()` around permutations. Never impute missing trait tips to zero.
5. **Species-level joins in `R/ethnobotafever_tree.R`**: `taxa_known_sub` must keep per-species `species_count = 1L` and `ethnobotanical_count` in `{0L, 1L}` rather than inheriting parent-genus totals. `taxa_compounds_classes_sub` must join on `taxon = taxon.y` (all species QIDs) because `taxon` comes from ethnobotanical records and is `NA` for non-medicinal phytochemical species (57 of 142 *Orchidaceae* species).
6. **SMILES URL encoding**: Always use `curl::curl_escape()` in `get_npclassifier_taxonomy()`. `utils::URLencode(..., reserved = TRUE)` leaves `%10`/`%11` ring closures unescaped and corrupts formal charge `+` signs.
7. **Plain `data.frame` serialization**: Serialize `data/orchids_*.rda` strictly with `class(x) <- "data.frame"` (`compress = "xz"`). Saving `tidytable`/`data.table` classes breaks downstream plain-`data.frame` assumptions and r-universe exports.
8. **Protect TSVs and iTOL files from `trailing-whitespace`**: `prek.toml` excludes `(\.tsv$|^inst/extdata/)` from `trailing-whitespace` so empty trailing tab columns are never truncated (which previously broke `data.table::fread()` without `fill = TRUE`).
9. **`normalize_ott_ids()` numeric contract**: `normalize_ott_ids()` parses numeric/integer strings via `as.integer()` and intentionally warns (`NAs introduced by coercion`) on prefixed strings like `"ott12345"`. Do not alter without updating callers and `test-normalize-ott-ids.R`.

## Do Not Touch Without Verification

- `R/cascade_prepare_hierarchy.R` mass-conservation rollup logic (`test-cascade-hierarchy.R` pins exact counts across `"Other"` and multi-label compounds).
- `tests/testthat/test-package-invariants.R` source/installed invariants and `.lintr` linter suppressions owned by `air`/`jarl`.
- `inst/scripts/ethnobotafever_tree.R`: note it calls `library(EthnoBotaPhyWR)`, which loads the *installed* library; during development run `pkgload::load_all()` in an interactive session or `R CMD INSTALL` into a temp library first.

## Cheap Verification Loop

1. After R edits: `air format . && jarl check . && Rscript -e "testthat::test_local(filter = '<relevant>')"`
2. Before commit: `Rscript -e "stopifnot(length(lintr::lint_package()) == 0L, nrow(spelling::spell_check_package()) == 0L); testthat::test_local()"`
