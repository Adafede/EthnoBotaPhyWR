# EthnoBotaPhyWR 0.0.0.9001

## New package

- Initial development version.
- Provides infrastructure for ethnobotanical and phytochemical data
  integration with Wikidata and the Open Tree of Life.
- Includes curated *Orchidaceae* datasets (3,055 compounds, 419
  ethnobotanical uses, 1,574 compound classifications).
- Exports `ethnobotafever_tree()` as the main analysis pipeline function.
- Provides `get_npclassifier_taxonomy()` for compound classification via the
  NPClassifier API.
- Provides `normalize_ott_ids()` for OTT ID normalization.
- Provides `calculate_branches_values()` for phylogenetic branch value
  computation.
- Provides `plot_trees()` for phylogenetic tree visualization with
  ethnobotanical and phytochemical overlays.
- Built-in caching for phylogenetic tree construction via
  `get_trees_cached()`.
