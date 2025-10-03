#' @title Get trees
#'
#' @description
#' Builds phylogenetic trees for a set of Open Tree of Life (OTT) IDs, using rotl and tidytable. Returns a list with taxa not in the tree, the full tree, and a tree for taxa with compounds.
#'
#' @param ott_ids Integer vector. OTT IDs to include in the tree.
#' @param df_counted Data.frame. Must contain columns 'taxon_id_ott', 'compounds_count', and 'ethnobotanical_count'.
#' @param taxon_name Character. Name of the taxon for filtering.
#'
#' @return List with elements: taxa_not_in_tree (data.frame), tree_taxa_full (phylo), tree_taxa_phyto (phylo).
#'
#' @examples
#' NULL
get_trees <- function(ott_ids, df_counted, taxon_name) {
  # Input validation
  if (!is.numeric(ott_ids)) {
    stop("ott_ids must be numeric (integer vector of OTT IDs)")
  }
  if (!is.data.frame(df_counted)) {
    stop("df_counted must be a data.frame")
  }
  if (!is.character(taxon_name) || length(taxon_name) != 1) {
    stop("taxon_name must be a single character string")
  }
  required_cols <- c("taxon_id_ott", "compounds_count", "ethnobotanical_count")
  if (!all(required_cols %in% names(df_counted))) {
    stop(
      "df_counted must contain columns: ",
      paste(required_cols, collapse = ", ")
    )
  }

  ott_in_tree_tax <- ott_in_tree |>
    rotl::taxonomy_taxon_info(
      include_lineage = TRUE,
      include_terminal_descendants = TRUE
    ) |>
    rotl::tax_lineage() |>
    tidytable::bind_rows(.id = "id") |>
    tidytable::group_by(id) |>
    tidytable::filter(grep(pattern = taxon_name, x = name, fixed = TRUE)) |>
    tidytable::ungroup() |>
    tidytable::pull(id) |>
    as.integer()

  # Identify taxa not in the tree but with relevant data
  taxa_not_in_tree <- df_counted[
    !df_counted$taxon_id_ott %in% ott_in_tree
  ] |>
    tidytable::filter(
      compounds_count > 0L |
        ethnobotanical_count > 0L
    ) |>
    tidytable::rename(ethnobotanical_species_count = ethnobotanical_count)

  # Build full tree and tree for taxa with compounds
  tree_taxa_full <- ott_in_tree_tax |>
    rotl::tol_induced_subtree()
  tree_taxa_phyto <- ott_in_tree_tax[
    ott_in_tree_tax %in%
      df_counted$taxon_id_ott[
        df_counted$compounds_count >= 1
      ]
  ] |>
    rotl::tol_induced_subtree()

  return(list(
    taxa_not_in_tree = taxa_not_in_tree,
    tree_taxa_full = tree_taxa_full,
    tree_taxa_phyto = tree_taxa_phyto
  ))
}

#' @title Get trees (memoised)
#'
#' @description
#' Returns a memoised version of get_trees for caching repeated calls.
#'
#' @param ... Arguments passed to get_trees.
#'
#' @return Memoised function.
#'
#' @examples
#' NULL
get_trees_mem <- function(...) {
  memoise::memoise(get_trees, ...)
}
