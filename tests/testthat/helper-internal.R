pkg_internal <- function(name) {
  get(name, envir = asNamespace("EthnoBotaPhyWR"), inherits = FALSE)
}

make_plotted_classes <- function(df) {
  pkg_internal("prepare_hierarchy")(df, type = "literature") |>
    pkg_internal("prepare_plot")() |>
    tidytable::rename(taxon_name = species)
}

make_orphan_df <- function(df_counted, rows = integer(0)) {
  orphan <- df_counted[rows, , drop = FALSE]
  names(orphan)[names(orphan) == "ethnobotanical_count"] <-
    "ethnobotanical_species_count"
  orphan
}
