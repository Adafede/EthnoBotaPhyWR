test_that("plot_trees handles empty orphans, >60-tip clade collapsing, and fan layout", {
  skip_if_not_installed("ggtree")
  skip_if_not_installed("ggtreeExtra")

  ns <- asNamespace("EthnoBotaPhyWR")
  prep_h <- get("prepare_hierarchy", envir = ns, inherits = FALSE)
  prep_p <- get("prepare_plot", envir = ns, inherits = FALSE)
  plot_tr <- get("plot_trees", envir = ns, inherits = FALSE)

  set.seed(123)
  n_tips <- 65L
  tip_names <- sprintf("Genus%02d", seq_len(n_tips))
  tr_full <- ape::rcoal(n_tips, tip.label = tip_names)
  tr_phyto <- ape::keep.tip(tr_full, tip_names[1:6])

  df_counted <- data.frame(
    taxon = paste0("Q", seq_len(n_tips)),
    taxon_name = tip_names,
    taxon_id_ott = as.character(seq_len(n_tips)),
    compounds_count = c(5L, 3L, 2L, 0L, 0L, 1L, rep(0L, n_tips - 6L)),
    species_count = c(10L, 8L, 4L, 20L, 5L, 2L, rep(3L, n_tips - 6L)),
    ethnobotanical_count = c(2L, 0L, 1L, 4L, 1L, 0L, rep(0L, n_tips - 6L)),
    stringsAsFactors = FALSE
  )

  df_comp <- data.frame(
    organism = c("Genus01", "Genus01", "Genus02", "Genus03", "Genus06"),
    species = c("Genus01", "Genus01", "Genus02", "Genus03", "Genus06"),
    sample = c("Genus01", "Genus01", "Genus02", "Genus03", "Genus06"),
    best_candidate_1 = c(
      "Terpenoids",
      "Alkaloids",
      "Terpenoids",
      "Other",
      "Terpenoids"
    ),
    best_candidate_2 = c(
      "Monoterpenoids",
      "Indoles",
      "Diterpenoids",
      NA,
      "Monoterpenoids"
    ),
    best_candidate_3 = c("Iridoids", "Simple", "Abietanes", NA, "Iridoids"),
    stringsAsFactors = FALSE
  )
  df_classes <- prep_h(df_comp, type = "literature") |>
    prep_p() |>
    tidytable::rename(taxon_name = species)

  orphan_df <- df_counted[0, , drop = FALSE]
  names(orphan_df)[names(orphan_df) == "ethnobotanical_count"] <-
    "ethnobotanical_species_count"

  plots <- plot_tr(
    taxa_not_in_tree = orphan_df,
    tree_taxa_full = tr_full,
    tree_taxa_phyto = tr_phyto,
    df_counted = df_counted,
    df_classes = df_classes,
    collapse_inactive = TRUE
  )

  expect_s3_class(plots$plot_tree_taxa_bars, "gg")
  expect_s3_class(plots$plot_tree_taxa_circular_bars, "gg")
  expect_s3_class(plots$plot_tree_taxa_phyto_bars, "gg")
  expect_s3_class(plots$plot_orphan_taxa, "gg")
  expect_lt(
    sum(plots$plot_tree_taxa_bars$data$isTip, na.rm = TRUE),
    n_tips
  )
})

test_that("export_itol_files and export_tree_bundle write valid offline outputs", {
  skip_if_not_installed("ggtree")
  skip_if_not_installed("ggtreeExtra")

  ns <- asNamespace("EthnoBotaPhyWR")
  prep_h <- get("prepare_hierarchy", envir = ns, inherits = FALSE)
  prep_p <- get("prepare_plot", envir = ns, inherits = FALSE)
  exp_itol <- get("export_itol_files", envir = ns, inherits = FALSE)

  tr <- ape::read.tree(text = "(Alpha_ott1:1,(Beta_ott2:1,Gamma_ott3:1):1);")
  df_cnt <- data.frame(
    taxon_name = c("Alpha", "Beta", "Gamma"),
    compounds_count = c(4L, 1L, 0L),
    species_count = c(10L, 3L, 2L),
    ethnobotanical_count = c(2L, 0L, 1L),
    stringsAsFactors = FALSE
  )
  df_cls <- data.frame(
    organism = c("Alpha", "Beta"),
    species = c("Alpha", "Beta"),
    sample = c("Alpha", "Beta"),
    best_candidate_1 = c("Terpenoids", "Alkaloids"),
    best_candidate_2 = c("Monoterpenoids", "Indoles"),
    best_candidate_3 = c("Iridoids", "Simple"),
    stringsAsFactors = FALSE
  ) |>
    prep_h(type = "literature") |>
    prep_p() |>
    tidytable::rename(taxon_name = species)

  out_dir <- tempfile("itol_test_")
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  written <- exp_itol(
    tree = tr,
    df_counted = df_cnt,
    df_classes = df_cls,
    out_dir = out_dir,
    prefix = "Test_genera",
    genus = TRUE
  )

  expect_true(all(file.exists(written)))
  expect_length(written, 4L)

  tr_written <- ape::read.tree(file.path(out_dir, "Test_genera_tree.tre"))
  expect_setequal(tr_written$tip.label, c("Alpha", "Beta", "Gamma"))

  ethno_lines <- readLines(file.path(
    out_dir,
    "Test_genera_itol_ethnobotany.txt"
  ))
  expect_identical(ethno_lines[1L], "DATASET_GRADIENT")
  data_rows <- ethno_lines[
    (which(ethno_lines == "DATA") + 1L):length(ethno_lines)
  ]
  data_ids <- vapply(strsplit(data_rows, "\t"), `[`, character(1L), 1L)
  expect_true(all(data_ids %in% tr_written$tip.label))

  if (requireNamespace("svglite", quietly = TRUE)) {
    exp_bundle <- get("export_tree_bundle", envir = ns, inherits = FALSE)
    p_min <- ggplot2::ggplot(
      data.frame(x = 1, y = 1),
      ggplot2::aes(x = x, y = y)
    ) +
      ggplot2::geom_point()
    svg_out <- file.path(out_dir, "bundle.svg")
    pdf_out <- file.path(out_dir, "bundle.pdf")
    bundle_written <- expect_no_warning(
      exp_bundle(
        plot_obj = p_min,
        girafe_obj = NULL,
        layout_cfg = list(export_width = 6, export_height = 4),
        file_path = svg_out
      )
    )
    expect_true(all(c(svg_out, pdf_out) %in% bundle_written))
    expect_true(file.exists(svg_out))
    expect_true(file.exists(pdf_out))
    expect_gt(file.info(pdf_out)$size, 0)
  }
})

test_that("inject_tree_search_bar escapes HTML title and orphan plot handles zero counts", {
  skip_if_not_installed("ggtree")
  skip_if_not_installed("ggtreeExtra")

  ns <- asNamespace("EthnoBotaPhyWR")
  inj <- get("inject_tree_search_bar", envir = ns, inherits = FALSE)
  plot_tr <- get("plot_trees", envir = ns, inherits = FALSE)
  prep_h <- get("prepare_hierarchy", envir = ns, inherits = FALSE)
  prep_p <- get("prepare_plot", envir = ns, inherits = FALSE)

  tmp_html <- tempfile(fileext = ".html")
  on.exit(unlink(tmp_html), add = TRUE)
  writeLines("<html><body><div>ok</div></body></html>", tmp_html)
  inj(tmp_html, title = 'Tree <script>alert("xss")</script>')
  html_out <- paste(readLines(tmp_html), collapse = "\n")
  expect_false(grepl("<script>alert", html_out, fixed = TRUE))
  expect_true(grepl(
    "&lt;script&gt;alert(&quot;xss&quot;)",
    html_out,
    fixed = TRUE
  ))

  tr <- ape::read.tree(text = "(GenA:1,GenB:1);")
  df_cnt <- data.frame(
    taxon = c("Q1", "Q2", "Q3"),
    taxon_name = c("GenA", "GenB", "GenOrphan"),
    taxon_id_ott = c("1", "2", "3"),
    compounds_count = c(2L, 0L, 1L),
    species_count = c(5L, 2L, 3L),
    ethnobotanical_count = c(1L, 0L, 0L),
    stringsAsFactors = FALSE
  )
  df_cls <- data.frame(
    organism = c("GenA", "GenOrphan"),
    species = c("GenA", "GenOrphan"),
    sample = c("GenA", "GenOrphan"),
    best_candidate_1 = c("Terpenoids", "Alkaloids"),
    best_candidate_2 = c("Monoterpenoids", "Indoles"),
    best_candidate_3 = c("Iridoids", "Simple"),
    stringsAsFactors = FALSE
  ) |>
    prep_h(type = "literature") |>
    prep_p() |>
    tidytable::rename(taxon_name = species)

  orphan_df <- df_cnt[3L, , drop = FALSE]
  names(orphan_df)[names(orphan_df) == "ethnobotanical_count"] <-
    "ethnobotanical_species_count"

  res <- plot_tr(
    taxa_not_in_tree = orphan_df,
    tree_taxa_full = tr,
    tree_taxa_phyto = tr,
    df_counted = df_cnt,
    df_classes = df_cls
  )
  expect_s3_class(res$plot_orphan_taxa, "gg")
})
