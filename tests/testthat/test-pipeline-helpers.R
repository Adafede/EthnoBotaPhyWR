test_that("prepare_hierarchy, prepare_plot, and plot_trees work offline", {
  ns <- asNamespace("EthnoBotaPhyWR")
  prep_h <- get("prepare_hierarchy", envir = ns, inherits = FALSE)
  prep_p <- get("prepare_plot", envir = ns, inherits = FALSE)
  plot_tr <- get("plot_trees", envir = ns, inherits = FALSE)
  get_tr <- get("get_trees", envir = ns, inherits = FALSE)

  df_counted <- data.frame(
    taxon = c("Q1", "Q2", "Q3"),
    taxon_name = c("GenusA", "GenusB", "GenusC"),
    taxon_id_ott = c(NA_character_, NA_character_, NA_character_),
    compounds_count = c(2L, 1L, 0L),
    species_count = c(5L, 2L, 1L),
    ethnobotanical_count = c(1L, 0L, 2L),
    stringsAsFactors = FALSE
  )

  trees <- get_tr(
    ott_ids = df_counted$taxon_id_ott,
    df_counted = df_counted,
    taxon_name = "Orchidaceae"
  )
  expect_s3_class(trees$tree_taxa_full, "phylo")
  expect_s3_class(trees$tree_taxa_phyto, "phylo")

  df_compounds <- data.frame(
    smiles_2D = c("CCO", "CCC", "CCN"),
    organism = c("GenusA", "GenusA", "GenusB"),
    best_candidate_1 = c("Terpenoids", "Alkaloids", "Other"),
    best_candidate_2 = c("Monoterpenoids", "Indole alkaloids", NA_character_),
    best_candidate_3 = c("Iridoids", "Simple indole", NA_character_),
    sample = c("GenusA", "GenusA", "GenusB"),
    species = c("GenusA", "GenusA", "GenusB"),
    stringsAsFactors = FALSE
  )

  hierarchy <- prep_h(df_compounds, type = "literature")
  plotted_classes <- prep_p(hierarchy) |>
    tidytable::rename(taxon_name = species)

  expect_true(all(
    c("taxon_name", "ids", "values", "color") %in% names(plotted_classes)
  ))

  if (
    tryCatch(
      {
        loadNamespace("ggtree")
        TRUE
      },
      error = function(e) FALSE
    )
  ) {
    tree_multi <- ape::read.tree(text = "(GenusA:1,(GenusB:1,GenusC:1):1);")
    plots <- plot_tr(
      taxa_not_in_tree = trees$taxa_not_in_tree,
      tree_taxa_full = tree_multi,
      tree_taxa_phyto = tree_multi,
      df_counted = df_counted,
      df_classes = plotted_classes
    )

    expect_s3_class(plots$plot_tree_taxa_bars, "gg")
    expect_s3_class(plots$plot_tree_taxa_phyto_bars, "gg")
    expect_s3_class(plots$plot_orphan_taxa, "gg")
  }
})

test_that("Wikidata and NPClassifier helpers validate inputs", {
  ns <- asNamespace("EthnoBotaPhyWR")
  q_wd <- get("query_wikidata", envir = ns, inherits = FALSE)
  t_qid <- get("taxon_name_to_qid", envir = ns, inherits = FALSE)
  sub_wd <- get("get_subtaxa_wd", envir = ns, inherits = FALSE)
  npc <- get("get_npclassifier_taxonomy", envir = ns, inherits = FALSE)

  expect_error(q_wd(123), "sparql_query")
  expect_error(q_wd("SELECT *", timeout = -1), "timeout")
  expect_error(t_qid(""), "taxon_name")
  expect_error(sub_wd(c("Q1", "Q2")), "taxon_qid")
  expect_error(npc(123), "smiles")
})
