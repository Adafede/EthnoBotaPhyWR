test_that("prepare_hierarchy, prepare_plot, and plot_trees work offline", {
  plot_tr <- pkg_internal("plot_trees")
  get_tr <- pkg_internal("get_trees")

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

  plotted_classes <- make_plotted_classes(df_compounds)

  expect_true(all(
    c("taxon_name", "ids", "values", "color") %in% names(plotted_classes)
  ))

  if (requireNamespace("ggtree", quietly = TRUE)) {
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
  q_wd <- pkg_internal("query_wikidata")
  t_qid <- pkg_internal("taxon_name_to_qid")
  sub_wd <- pkg_internal("get_subtaxa_wd")
  npc <- pkg_internal("get_npclassifier_taxonomy")

  expect_error(q_wd(123), "sparql_query")
  expect_error(q_wd("SELECT *", timeout = -1), "timeout")
  expect_error(t_qid(""), "taxon_name")
  expect_error(sub_wd(c("Q1", "Q2")), "taxon_qid")
  expect_error(npc(123), "smiles")
})
