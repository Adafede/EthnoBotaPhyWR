get_trees <- function(ott_ids, df_counted, taxon_name) {
  ott_in_tree <- ott_ids[rotl::is_in_tree(ott_ids)] |>
    as.integer()

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

  taxa_not_in_tree <- df_counted[
    !df_counted$taxon_id_ott %in% ott_in_tree
  ] |>
    tidytable::filter(
      compounds_count > 0L |
        ethnobotanical_count > 0L
    ) |>
    tidytable::rename(ethnobotanical_species_count = ethnobotanical_count)

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
    "taxa_not_in_tree" = taxa_not_in_tree,
    "tree_taxa_full" = tree_taxa_full,
    "tree_taxa_phyto" = tree_taxa_phyto
  ))
}

get_trees_mem <- function(...) {
  memoise::memoise(get_trees, ...)
}
