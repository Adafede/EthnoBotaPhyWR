# EthnoBotaPhyWR 0.0.0.9001

## New features and algorithmic improvements

- `get_trees()` queries the Open Tree of Life v3 `induced_subtree` endpoint in a
  single batch request, substitutes exemplar species OTT IDs (`exemplar_ott_id`)
  for paraphyletic genera, matches Newick tips by exact `_ott<ID>` suffix, and
  grafts unplaced species at their congeneric MRCA.
- `calculate_branches_values()` supports `method = "phylo"` with Grafen branch
  length calibration, two-pass Felsenstein pruning ML ancestral state
  reconstruction (`phylo_asr`), Empirical Bayes Beta-Binomial shrinkage
  (`eb_rate`), hypergeometric clade enrichment with Benjamini-Hochberg FDR
  (`hot_node_pval`, `hot_node_qval`, `is_hot_node`), and Blomberg's $K$
  phylogenetic signal with permutation testing.
- `ethnobotafever_tree()` computes hypergeometric chemotaxonomic scaffold
  enrichment against LOTUS, Fisher's exact test for ethnobotanical-phytochemical
  coupling across genera, NPClassifier superclass Shannon/Hill diversity
  (`shannon_diversity`, `hill_q1`), Empirical Bayes ethnobotanical rates
  (`ethnobotanical_rate_eb`), and log-log species-richness compound residuals
  (`compounds_residual`).
- `prepare_hierarchy()` performs an exact mass-conserving 3-level
  (`pathway -> superclass -> class`) rollup, preserving unclassified (`Other`)
  and multi-label compounds without tie-breaking loss.
- `plot_trees()` highlights statistically enriched ethnobotanical clades with
  red diamond markers, computes subtree summaries in $O(N)$ postorder traversal,
  and scales tip sizes by $\log_{10}$ species counts matching the legend breaks.

## Initial release

- Provides infrastructure for ethnobotanical and phytochemical data integration
  with Wikidata and the Open Tree of Life.
- Includes curated *Orchidaceae* datasets (3,055 compounds, 419 ethnobotanical
  uses, 1,574 compound classifications).
- Exports `ethnobotafever_tree()` as the main analysis pipeline function.
- Provides `get_npclassifier_taxonomy()` for compound classification via the
  NPClassifier API.
- Provides `normalize_ott_ids()` for OTT ID normalization.
- Built-in caching for phylogenetic tree construction via `get_trees_cached()`.
