test_that("ethnobotafever_tree produces species-level counts, superclass Hill diversity, and exports offline", {
  skip_if_not_installed("ggtree")
  skip_if_not_installed("ggtreeExtra")

  out_dir <- tempfile("pipeline_offline_")
  dir.create(out_dir, recursive = TRUE)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  cache_dir <- file.path(out_dir, "cache")
  itol_dir <- file.path(out_dir, "itol")

  mock_subtaxa <- data.frame(
    taxon = c(
      "Q10",
      "Q20",
      "Q30",
      "Q11",
      "Q12",
      "Q13",
      "Q21",
      "Q22",
      "Q31"
    ),
    taxon_name = c(
      "GenAlpha",
      "GenBeta",
      "GenGamma",
      "GenAlpha one",
      "GenAlpha two",
      "GenAlpha three",
      "GenBeta one",
      "GenBeta two",
      "GenGamma one"
    ),
    taxon_rank = c(
      "Q34740",
      "Q34740",
      "Q34740",
      "Q7432",
      "Q7432",
      "Q7432",
      "Q7432",
      "Q7432",
      "Q7432"
    ),
    stringsAsFactors = FALSE
  )

  mock_otts <- data.frame(
    taxon = mock_subtaxa$taxon,
    taxon_id_ott = c(
      "10",
      "20",
      "30",
      "11",
      "12",
      "13",
      "21",
      "22",
      "31"
    ),
    stringsAsFactors = FALSE
  )

  mock_compounds <- data.frame(
    compound = c("QC1", "QC2", "QC3", "QC4"),
    compound_smiles_canonical = c("CCO", "CCC", "CCN", "CC=O"),
    compound_inchikey = c(
      "IKONE-0000-01",
      "IKTWO-0000-01",
      "IKTHR-0000-01",
      "IKFOU-0000-01"
    ),
    taxon = c("Q11", "Q12", "Q21", "Q31"),
    stringsAsFactors = FALSE
  )
  # Give Q31 two distinct superclasses via two compounds in the same species
  mock_compounds <- rbind(
    mock_compounds,
    data.frame(
      compound = "QC5",
      compound_smiles_canonical = "CCCC",
      compound_inchikey = "IKFIV-0000-01",
      taxon = "Q31",
      stringsAsFactors = FALSE
    )
  )

  mock_uses <- data.frame(
    taxon = c("Q11", "Q13", "Q21"),
    taxon_id_ott = c("11", "13", "21"),
    stringsAsFactors = FALSE
  )

  mock_lotus <- data.frame(
    compound = c("QC1", "QL1", "QL2"),
    compound_smiles_canonical = c("CCO", "c1ccccc1", "O"),
    compound_inchikey = c("IKONE-0000-01", "IKOUTA-0000-01", "IKOUTB-0000-01"),
    taxon = c("Q11", "Q999", "Q998"),
    stringsAsFactors = FALSE
  )

  seed_classes <- data.frame(
    query = c("CCO", "CCC", "CCN", "CC=O", "CCCC"),
    pathway = c("Terpenoids", "Terpenoids", "", "Terpenoids", "Terpenoids"),
    superclass = c(
      "Monoterpenoids",
      "Sesquiterpenoids",
      "",
      "Monoterpenoids",
      "Sesquiterpenoids"
    ),
    class = c("Iridoids", "Eudesmanes", "", "Iridoids", "Eudesmanes"),
    glycoside = c(FALSE, FALSE, FALSE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )

  testthat::local_mocked_bindings(
    taxon_name_to_qid = function(taxon_name) "Q1000",
    get_subtaxa_wd = function(taxon_qid) mock_subtaxa,
    get_ott_ids_wd = function(taxon_qid) mock_otts,
    get_compounds_wd = function(taxon_qid) mock_compounds,
    get_uses_wd = function(taxon_qid) mock_uses,
    get_lotus_wd = function() mock_lotus,
    get_trees_cached = function(ott_ids, df_counted, taxon_name, cache_dir) {
      tips <- df_counted$taxon_name
      tr <- ape::stree(length(tips), type = "star")
      tr$tip.label <- tips
      tr$tip_id_ott <- df_counted$taxon_id_ott
      tr$edge.length <- rep(1, nrow(tr$edge))
      orphan <- df_counted[0, , drop = FALSE]
      names(orphan)[names(orphan) == "ethnobotanical_count"] <-
        "ethnobotanical_species_count"
      list(
        taxa_not_in_tree = orphan,
        tree_taxa_full = tr,
        tree_taxa_phyto = tr
      )
    },
    .package = "EthnoBotaPhyWR"
  )

  res <- EthnoBotaPhyWR::ethnobotafever_tree(
    taxon_name = "Testaceae",
    n_multiple_genera = 1,
    n_ethnobotanical_count_min = 1,
    n_compounds_count_min = 1,
    example_family_names = NULL,
    path_molecules = NULL,
    path_classes = seed_classes,
    path_ethnobotanical = NULL,
    export_tree_full = file.path(out_dir, "full.svg"),
    export_tree_phyto = file.path(out_dir, "phyto.svg"),
    export_tree_phyto_sub = file.path(out_dir, "phyto_sub.svg"),
    export_table_classified = file.path(out_dir, "classified.tsv"),
    export_table_counts = file.path(out_dir, "counts.tsv"),
    export_table_counts_sub = file.path(out_dir, "counts_sub.tsv"),
    local_cache = cache_dir,
    log_dir = out_dir
  )

  sub_df <- res$df_taxa_known_sub
  expect_true(all(sub_df$species_count == 1L))
  expect_identical(
    sub_df$ethnobotanical_count[sub_df$taxon_name == "GenAlpha one"],
    1L
  )
  expect_identical(
    sub_df$ethnobotanical_count[sub_df$taxon_name == "GenAlpha two"],
    0L
  )
  expect_identical(
    sub_df$ethnobotanical_count[sub_df$taxon_name == "GenGamma one"],
    0L
  )

  cnt_df <- res$df_taxa_ethno_counted
  # GenBeta had only blank/unclassified compound ("") -> diversity must be 0
  expect_equal(cnt_df$shannon_diversity[cnt_df$taxon_name == "GenBeta"], 0)
  expect_equal(cnt_df$hill_q1[cnt_df$taxon_name == "GenBeta"], 0)
  # GenGamma had 1:1 Monoterpenoids + Sesquiterpenoids -> log(2), Hill q1 = 2
  expect_equal(
    cnt_df$shannon_diversity[cnt_df$taxon_name == "GenGamma"],
    round(log(2), 4),
    tolerance = 1e-4
  )
  expect_equal(
    cnt_df$hill_q1[cnt_df$taxon_name == "GenGamma"],
    2,
    tolerance = 1e-4
  )

  # Non-medicinal species with compounds must appear in species multibar iTOL
  mb_file <- file.path(
    itol_dir,
    "Testaceae_phyto_species_itol_compounds_multibar.txt"
  )
  expect_true(file.exists(mb_file))
  mb_lines <- readLines(mb_file)
  expect_true(any(startsWith(mb_lines, "GenAlpha_two\t")))
  expect_true(any(startsWith(mb_lines, "GenGamma_one\t")))

  expect_true(file.exists(file.path(out_dir, "full.svg")))
  expect_true(file.exists(file.path(out_dir, "full.pdf")))
  expect_gt(file.info(file.path(out_dir, "full.pdf"))$size, 0)
  expect_true(file.exists(file.path(out_dir, "counts.tsv")))
  expect_true(file.exists(file.path(out_dir, "counts_sub.tsv")))
  expect_true(file.exists(res$log_file))
})
