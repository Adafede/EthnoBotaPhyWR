test_that("get_trees_cache_key normalizes integer/character OTT IDs and ignores row order", {
  key_fn <- get(
    "get_trees_cache_key",
    envir = asNamespace("EthnoBotaPhyWR"),
    inherits = FALSE
  )

  df1 <- data.frame(
    taxon_name = c("GenusB", "GenusA"),
    taxon_id_ott = c("200", "100"),
    compounds_count = c(2L, 5L),
    ethnobotanical_count = c(1L, 0L),
    exemplar_ott_id = c("201", "101"),
    stringsAsFactors = FALSE
  )
  df2 <- df1[2:1, , drop = FALSE]
  df2$taxon_id_ott <- c(100L, 200L)
  df2$exemplar_ott_id <- c(101L, 201L)

  k1 <- key_fn(c("200", "100"), df1, "Orchidaceae")
  k2 <- key_fn(c(100L, 200L), df2, "Orchidaceae")
  k3 <- key_fn(c(100L, 200L), df2, "Fabaceae")

  expect_identical(k1, k2)
  expect_false(identical(k1, k3))
})

test_that("get_trees_cached uses cache hit and writes missing cache offline", {
  cached_fn <- get(
    "get_trees_cached",
    envir = asNamespace("EthnoBotaPhyWR"),
    inherits = FALSE
  )

  cache_dir <- tempfile("tree_cache_")
  dir.create(cache_dir, recursive = TRUE)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  df <- data.frame(
    taxon_name = c("GenusA", "GenusB"),
    taxon_id_ott = c("10", "20"),
    compounds_count = c(3L, 0L),
    ethnobotanical_count = c(0L, 2L),
    stringsAsFactors = FALSE
  )

  fake_res <- list(
    taxa_not_in_tree = df[0, , drop = FALSE],
    tree_taxa_full = ape::read.tree(text = "(GenusA:1,GenusB:1);"),
    tree_taxa_phyto = ape::read.tree(text = "(GenusA:1,GenusB:1);")
  )

  state <- new.env(parent = emptyenv())
  state$call_count <- 0L
  testthat::local_mocked_bindings(
    get_trees = function(ott_ids, df_counted, taxon_name) {
      state$call_count <- state$call_count + 1L
      fake_res
    },
    .package = "EthnoBotaPhyWR"
  )

  res1 <- cached_fn(c(10L, 20L), df, "TestFamily", cache_dir)
  expect_identical(state$call_count, 1L)
  expect_length(list.files(cache_dir, pattern = "\\.rda$"), 1L)

  res2 <- cached_fn(c(10L, 20L), df, "TestFamily", cache_dir)
  expect_identical(state$call_count, 1L)
  expect_identical(res2$tree_taxa_full$tip.label, res1$tree_taxa_full$tip.label)
})

test_that("get_trees validates inputs and maps mocked OTL subtrees with exemplar swaps", {
  get_tr <- get(
    "get_trees",
    envir = asNamespace("EthnoBotaPhyWR"),
    inherits = FALSE
  )

  df <- data.frame(
    taxon_name = c("GenusA", "GenusB", "GenusC"),
    taxon_id_ott = c("101", "102", "103"),
    exemplar_ott_id = c("201", "202", "203"),
    compounds_count = c(4L, 1L, 0L),
    ethnobotanical_count = c(1L, 0L, 0L),
    species_count = c(10L, 5L, 2L),
    stringsAsFactors = FALSE
  )

  expect_error(get_tr("101", list(), "Fam"), "data.frame")
  expect_error(get_tr("101", df[, 1:2, drop = FALSE], "Fam"), "must contain")

  state <- new.env(parent = emptyenv())
  state$call_idx <- 0L
  testthat::local_mocked_bindings(
    curl_fetch_memory = function(url, handle = NULL) {
      state$call_idx <- state$call_idx + 1L
      if (state$call_idx == 1L) {
        # First pass marks ott101 as broken and returns ott103
        payload <- list(
          newick = "(GenusA_ott201:1,GenusC_ott103:1);",
          broken = list(ott101 = "broken_monophyly")
        )
      } else {
        payload <- list(
          newick = "(GenusA_ott201:1,GenusC_ott103:1);"
        )
      }
      list(
        status_code = 200L,
        content = charToRaw(jsonlite::toJSON(payload, auto_unbox = TRUE))
      )
    },
    .package = "curl"
  )

  res <- get_tr(c("101", "102", "103"), df, "Fam")
  expect_s3_class(res$tree_taxa_full, "phylo")
  expect_true("GenusA" %in% res$tree_taxa_full$tip.label)
  # GenusB had phyto signal (compounds_count == 1) and was missing from Newick
  expect_identical(res$taxa_not_in_tree$taxon_name, "GenusB")
})
