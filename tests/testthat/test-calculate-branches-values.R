test_that("calculate_branches_values computes tip and internal node values", {
  calc <- get(
    "calculate_branches_values",
    envir = asNamespace("EthnoBotaPhyWR"),
    inherits = FALSE
  )
  tree <- ape::read.tree(text = "(A,(B,C));")
  df <- data.frame(
    taxon_name = c("A", "B", "C"),
    value = c(1, 2, 4),
    stringsAsFactors = FALSE
  )

  result <- calc(tree, df, "value", "score")

  tip_values <- result$score[result$is_tip]
  expect_equal(sort(tip_values), c(1, 2, 4))
  expect_equal(result$count[result$node == 4], mean(c(1, 2, 4)))
  expect_equal(result$count[result$node == 5], mean(c(2, 4)))
  expect_equal(
    result$score[result$node == 5],
    mean(c(mean(c(2, 4)), mean(c(1, 2, 4))))
  )
})

test_that("calculate_branches_values rejects invalid inputs", {
  calc <- get(
    "calculate_branches_values",
    envir = asNamespace("EthnoBotaPhyWR"),
    inherits = FALSE
  )
  tree <- ape::read.tree(text = "(A,(B,C));")
  df <- data.frame(
    taxon_name = c("A", "B", "C"),
    value = c(1, 2, 4),
    stringsAsFactors = FALSE
  )

  expect_error(calc("not a tree", df, "value", "score"), "phylo")
  expect_error(calc(tree, df, "missing_column", "score"), "variable")
})

test_that("calculate_branches_values phylo method and EB shrinkage respect topology", {
  calc <- get(
    "calculate_branches_values",
    envir = asNamespace("EthnoBotaPhyWR"),
    inherits = FALSE
  )
  # Asymmetric tree: A is sister to clade (B, C, D)
  tree <- ape::read.tree(text = "(A:1,((B:1,C:1):1,D:1):1);")
  df <- data.frame(
    taxon_name = c("A", "B", "C", "D"),
    value = c(10, 0, 0, 0),
    species_count = c(10, 10, 10, 10),
    stringsAsFactors = FALSE
  )
  res_mean <- calc(tree, df, "value", "score", method = "mean")
  res_phylo <- calc(tree, df, "value", "score", method = "phylo")

  # Root (node 5): flat tip mean is 10/4 = 2.5, whereas phylogenetic pruning
  # weights sister lineage A more strongly than 1/4 because (B,C,D) share ancestry
  expect_equal(res_mean$count[res_mean$node == 5], 2.5)
  expect_gt(
    res_phylo$phylo_asr[res_phylo$node == 5],
    res_mean$count[res_mean$node == 5]
  )
  expect_true(all(is.finite(res_phylo$eb_rate)))
})
