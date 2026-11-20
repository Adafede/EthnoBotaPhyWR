call_ethnobotafever_tree <- function(taxon_name, n_multiple_genera) {
  EthnoBotaPhyWR::ethnobotafever_tree(
    taxon_name = taxon_name,
    n_multiple_genera = n_multiple_genera,
    n_ethnobotanical_count_min = 0,
    n_compounds_count_min = 0,
    example_family_names = NULL,
    path_molecules = NULL,
    path_classes = NULL,
    path_ethnobotanical = NULL,
    export_tree_full = NULL,
    export_tree_phyto = NULL,
    export_tree_phyto_sub = NULL,
    export_table_classified = NULL,
    export_table_counts = NULL,
    export_table_counts_sub = NULL,
    local_cache = NULL,
    log_dir = NULL
  )
}

test_that("ethnobotafever_tree validates numeric arguments", {
  expect_error(call_ethnobotafever_tree("Orchidaceae", -1), "non-negative")
})

test_that("ethnobotafever_tree validates taxon_name", {
  expect_error(call_ethnobotafever_tree("", 1), "taxon_name")
})
