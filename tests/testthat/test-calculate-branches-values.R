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
