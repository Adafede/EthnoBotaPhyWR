test_that("prepare_hierarchy conserves mass across all 3 levels and maps Other", {
  prep_h <- get(
    "prepare_hierarchy",
    envir = asNamespace("EthnoBotaPhyWR"),
    inherits = FALSE
  )

  df <- data.frame(
    organism = c("SpA", "SpA", "SpA", "SpB", "SpB", "SpC"),
    species = c("SpA", "SpA", "SpA", "SpB", "SpB", "SpC"),
    sample = c("SpA", "SpA", "SpA", "SpB", "SpB", "SpC"),
    best_candidate_1 = c(
      "Terpenoids",
      "Terpenoids",
      "Terpenoids",
      NA_character_,
      "notAnnotated",
      "Alkaloids"
    ),
    best_candidate_2 = c(
      "Monoterpenoids",
      "Sesquiterpenoids",
      NA_character_,
      NA_character_,
      "notClassified",
      "Indole alkaloids"
    ),
    best_candidate_3 = c(
      "Iridoids",
      "Eudesmane sesquiterpenoids",
      NA_character_,
      NA_character_,
      NA_character_,
      "Simple indole"
    ),
    weight = c(1, 2, 1, 1, 1, 3),
    stringsAsFactors = FALSE
  )

  res <- prep_h(df, type = "literature")
  total_weight <- sum(df$weight)

  lvl1 <- res[res$parents == "", ]
  lvl2 <- res[res$parents %in% lvl1$ids, ]
  lvl3 <- res[res$parents %in% lvl2$ids, ]

  expect_equal(sum(lvl1$values), total_weight)
  expect_equal(sum(lvl2$values), total_weight)
  expect_equal(sum(lvl3$values), total_weight)

  expect_true("Other" %in% lvl1$ids)
  expect_equal(sum(lvl1$values[lvl1$ids == "Other"]), 2)
})

test_that("prepare_hierarchy folds superclasses beyond top 4 per pathway and handles empty df", {
  prep_h <- get(
    "prepare_hierarchy",
    envir = asNamespace("EthnoBotaPhyWR"),
    inherits = FALSE
  )

  empty_df <- data.frame(
    organism = character(0),
    species = character(0),
    sample = character(0),
    best_candidate_1 = character(0),
    best_candidate_2 = character(0),
    best_candidate_3 = character(0),
    stringsAsFactors = FALSE
  )
  empty_res <- prep_h(empty_df, type = "literature")
  expect_equal(nrow(empty_res), 0L)
  expect_true(all(
    c("parents", "ids", "labels", "values") %in% names(empty_res)
  ))

  df6 <- data.frame(
    organism = rep("SpA", 6),
    species = rep("SpA", 6),
    sample = rep("SpA", 6),
    best_candidate_1 = rep("Terpenoids", 6),
    best_candidate_2 = paste0("Super", seq_len(6)),
    best_candidate_3 = paste0("Cls", seq_len(6)),
    weight = c(10, 9, 8, 7, 2, 1),
    stringsAsFactors = FALSE
  )
  res6 <- prep_h(df6, type = "literature")
  lvl2 <- res6[res6$parents == "Terpenoids", ]
  expect_true("Terpenoids Other" %in% lvl2$labels)
  expect_equal(lvl2$values[lvl2$labels == "Terpenoids Other"], 3)
})

test_that("prepare_plot assigns valid hex colors and expected ordering", {
  prep_h <- get(
    "prepare_hierarchy",
    envir = asNamespace("EthnoBotaPhyWR"),
    inherits = FALSE
  )
  prep_p <- get(
    "prepare_plot",
    envir = asNamespace("EthnoBotaPhyWR"),
    inherits = FALSE
  )

  df <- data.frame(
    organism = c("SpA", "SpA", "SpB", "SpC"),
    species = c("SpA", "SpA", "SpB", "SpC"),
    sample = c("SpA", "SpA", "SpB", "SpC"),
    best_candidate_1 = c("Terpenoids", "Alkaloids", "Terpenoids", "Other"),
    best_candidate_2 = c("Monoterpenoids", "Indoles", "Diterpenoids", NA),
    best_candidate_3 = c("Iridoids", "Simple", "Abietanes", NA),
    stringsAsFactors = FALSE
  )

  plotted <- prep_h(df, type = "literature") |>
    prep_p()

  expect_true(all(
    c("species", "ids", "labels", "parents", "values", "color") %in%
      names(plotted)
  ))
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", as.character(plotted$color))))
  expect_equal(
    as.integer(tail(plotted$group[plotted$parents == "Other"], 1)),
    666L
  )
})
