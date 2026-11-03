start <- Sys.time()

library(EthnoBotaPhyWR)

taxon_name <- "Orchidaceae"

n_multiple_genera <- 10L
n_ethnobotanical_count_min <- 10L
n_compounds_count_min <- 50L

example_family_names <- c(
  "Poaceae" = "Poaceae",
  "Fabaceae" = "Fabaceae",
  "Asteraceae" = "Asteraceae",
  "Orchidaceae" = "Orchidaceae",
  "Rosaceae" = "Rosaceae",
  "Solanaceae" = "Solanaceae"
)

pkg_root <- normalizePath(file.path("data-raw", ".."), mustWork = TRUE)
cache_dir <- file.path(pkg_root, "data")
log_dir <- file.path(pkg_root, "data-raw")
figures_dir <- file.path(pkg_root, "man", "figures")
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

path_molecules <- EthnoBotaPhyWR::orchids_compounds
path_classes <- EthnoBotaPhyWR::orchids_compound_classes
path_ethnobotanical <- EthnoBotaPhyWR::orchids_ethnobotanical

export_tree_full <- file.path(figures_dir, "tree_orchids_full.svg")
export_tree_phyto <- file.path(figures_dir, "tree_orchids_phyto.svg")
export_tree_phyto_sub <- file.path(figures_dir, "tree_orchids_phyto_sub.svg")
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

local_cache <- cache_dir
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

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

extdata_itol <- file.path(pkg_root, "inst", "extdata", "itol")
dir.create(extdata_itol, recursive = TRUE, showWarnings = FALSE)
itol_src <- list.files(
  file.path(pkg_root, "data-raw", "itol"),
  full.names = TRUE
)
if (length(itol_src) > 0L) {
  file.copy(itol_src, extdata_itol, overwrite = TRUE)
}

end <- Sys.time()

logger::log_success("Script finished in ", format(end - start))
