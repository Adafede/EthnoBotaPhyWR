# EthnoBotaPhyWR 0.0.0.9001

## Public API and datasets

- Exports `ethnobotafever_tree()` as the package's main pipeline entry point.
- Bundles curated *Orchidaceae* datasets (`orchids_compounds`,
  `orchids_compound_classes`, `orchids_ethnobotanical`) as plain
  `data.frame` objects alongside precomputed tree caches
  (`trees_dec344ba7ea88a268b3feac0403844a3`,
  `trees_9f61da756dff1e09426c110eefce3824`) and iTOL v6 exports in
  `inst/extdata/itol/`.

## Fixes and improvements

- Replaced permutation-MSE Blomberg's $K$ heuristic and missing-tip zero
  imputation in internal `calculate_branches_values()` with exact analytical
  PIC/GLS Blomberg's $K$ restricted to finite tips, preserving `.Random.seed`
  across permutation tests.
- Fixed internal postorder node ordering (`unique(..., fromLast = TRUE)`) in
  `calculate_branches_values()` and `plot_trees()`, and scaled Felsenstein
  pruning tip variance consistently.
- Fixed species-level `ethnobotafever_tree()` outputs so
  `Orchidaceae_phyto_species_itol_species_richness.txt` uses per-species
  richness (`1`) rather than parent-genus totals, `ethnobotany` uses per-species
  counts (`{0, 1}`), and non-medicinal phytochemical species are retained in
  species-level compound exports.
- Restricted `hill_by_genus` diversity calculations in `ethnobotafever_tree()`
  to valid NPClassifier superclasses, excluding blank strings.
- Switched NPClassifier SMILES query encoding in internal
  `get_npclassifier_taxonomy()` to `curl::curl_escape()`, recovering
  classifications for SMILES containing `%10`/`%11` ring closures and formal
  charges; repaired the 19 affected rows in `data-raw/` TSVs and updated tree
  caches and iTOL outputs.
- Hardened input path validation, SPARQL literal escaping in internal
  `taxon_name_to_qid()`, strict QID regex validation, edge guards around `lm()`
  compound-richness residuals, HTML title escaping in `plot_trees()`, and
  guarded `log10()` of zero ethnobotanical counts.
- Added automated offline test suites, `.lintr` configuration, `jarl` and
  coverage CI workflows, and synced citation/package metadata.
