start <- Sys.time()

taxon_name <- "Orchidaceae"

n_multiple_genera <- 10L
n_ethnobotanical_count_min <- 10L
n_compounds_count_min <- 50L

example_family_names <- list(
  "Poaceae" = "Poaceae",
  "Fabaceae" = "Fabaceae",
  "Asteraceae" = "Asteraceae",
  "Orchidaceae" = "Orchidaceae",
  "Rosaceae" = "Rosaceae",
  "Solanaceae" = "Solanaceae"
)

path_molecules <- "~/Documents/papers/orchids/orchids_table_unique_connectivity_classified.tsv"
path_classes <- "~/Documents/papers/orchids/orchids_compounds_classes.tsv"
path_ethnobotanical <- "~/Documents/papers/orchids/Species_medicinal_output_with_ott_id.tsv"

export_tree_full <- "~/Documents/papers/orchids/tree_orchids_full.pdf"
export_tree_phyto <- "~/Documents/papers/orchids/tree_orchids_phyto.pdf"
export_tree_phyto_sub <- "~/Documents/papers/orchids/tree_orchids_phyto_sub.pdf"
export_table_classified <- "~/Documents/papers/orchids/orchids_table_unique_connectivity_classified.tsv"
export_table_counts <- "~/Documents/papers/orchids/orchids_table_counts.tsv"
export_table_counts_sub <- "~/Documents/papers/orchids/orchids_table_counts_sub.tsv"

local_cache <- ".cache/"
log_dir <- "~/Documents/papers/orchids/"

## External functions
source(
  file = "https://raw.githubusercontent.com/adafede/cascade/main/R/colors.R"
)
source(
  file = "https://raw.githubusercontent.com/adafede/cascade/main/R/prepare_hierarchy.R"
)
source(
  file = "https://raw.githubusercontent.com/adafede/cascade/main/R/prepare_plot.R"
)
source(
  file = "https://raw.githubusercontent.com/adafede/cascade/main/R/taxon_name_to_qid.R"
)

## Internal functions
source(
  file = "R/get_npclassifier_taxonomy.R"
)
source(
  file = "R/query_wdqs.R"
)
source(
  file = "R/calculate_branches_values.R"
)
source(
  file = "R/get_results_wd.R"
)
source(
  file = "R/get_trees.R"
)
source(
  file = "R/plot_trees.R"
)
source(
  file = "R/ethnobotafever_tree.R"
)

results <- ethnobotafever_tree(
  taxon_name = taxon_name,
  n_multiple_genera = n_multiple_genera,
  n_ethnobotanical_count_min = n_ethnobotanical_count_min,
  n_compounds_count_min = n_compounds_count_min,
  example_family_names = example_family_names,
  path_molecules = path_molecules,
  path_classes = path_classes,
  path_ethnobotanical = path_ethnobotanical,
  export_tree_full = export_tree_full,
  export_tree_phyto = export_tree_phyto,
  export_tree_phyto_sub = export_tree_phyto_sub,
  export_table_classified = export_table_classified,
  export_table_counts = export_table_counts,
  export_table_counts_sub = export_table_counts_sub,
  local_cache = local_cache,
  log_dir = log_dir
)

end <- Sys.time()

logger::log_success("Script finished in ", format(end - start))
