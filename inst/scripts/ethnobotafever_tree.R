start <- Sys.time()

library(EthnoBotaPhyWR)
library(ggplot2) # required because of `geom_col`

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

pkg_root <- normalizePath(file.path("data-raw", ".."), mustWork = TRUE)
cache_dir <- file.path(pkg_root, "data")
log_dir <- file.path(pkg_root, "data-raw/")
figures_dir <- file.path(pkg_root, "man", "figures")
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

path_molecules <- orchids_compounds
path_classes <- orchids_compound_classes
path_ethnobotanical <- orchids_ethnobotanical

export_tree_full <- file.path(figures_dir, "tree_orchids_full.pdf")
export_tree_phyto <- file.path(figures_dir, "tree_orchids_phyto.pdf")
export_tree_phyto_sub <- file.path(figures_dir, "tree_orchids_phyto_sub.pdf")
export_table_classified <- file.path(
  pkg_root,
  "data-raw",
  "orchids_table_unique_connectivity_classified.tsv"
)
export_table_counts <- file.path(
  pkg_root,
  "data-raw",
  "orchids_table_counts.tsv"
)
export_table_counts_sub <- file.path(
  pkg_root,
  "data-raw",
  "orchids_table_counts_sub.tsv"
)

local_cache <- file.path(pkg_root, "data")
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

## External sources
source(
  file = "https://raw.githubusercontent.com/adafede/cascade/main/R/colors.R"
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
