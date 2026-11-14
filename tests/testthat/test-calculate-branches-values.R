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

test_that("felsenstein_asr respects postorder leaf-before-clade order and tip weights", {
  calc <- get(
    "calculate_branches_values",
    envir = asNamespace("EthnoBotaPhyWR"),
    inherits = FALSE
  )

  # ape::reorder.phylo(tr, "postorder") emits root's leaf edge first here
  tr_leaf_first <- ape::read.tree(text = "(A:1,(B:1,C:1):1);")
  df_vals <- data.frame(
    taxon_name = c("A", "B", "C"),
    value = c(0, 10, 10),
    stringsAsFactors = FALSE
  )
  res1 <- calc(tr_leaf_first, df_vals, "value", "score", method = "phylo")
  root_node <- ape::Ntip(tr_leaf_first) + 1L
  # Var(child clade root) = 1 + 1/(1+1) = 1.5; weights proportional to 1/1 and 1/1.5
  # => root ASR = (1 * 0 + (1/1.5) * 10) / (1 + 1/1.5) = 4.0
  expect_equal(res1$phylo_asr[res1$node == root_node], 4.0, tolerance = 1e-6)

  # Positive terminal branch scaling when tip_weight > 1
  tr_star <- ape::read.tree(text = "(A:1,B:1,C:1);")
  df_w <- data.frame(
    taxon_name = c("A", "B", "C"),
    value = c(100, 0, 0),
    tip_weight = c(10, 1, 1),
    stringsAsFactors = FALSE
  )
  res2 <- calc(tr_star, df_w, "value", "score", method = "phylo")
  expect_equal(
    res2$phylo_asr[res2$node == 4L],
    (10 * 100 + 0 + 0) / 12,
    tolerance = 1e-5
  )
})

test_that("Blomberg K matches exact vcv GLS formula, ignores NAs, and preserves RNG", {
  calc <- get(
    "calculate_branches_values",
    envir = asNamespace("EthnoBotaPhyWR"),
    inherits = FALSE
  )

  ref_blomberg_k <- function(tr, x_raw) {
    keep <- which(is.finite(x_raw))
    tr_k <- if (length(keep) < length(tr$tip.label)) {
      kept_names <- tr$tip.label[keep]
      x_raw <- x_raw[keep]
      tr_sub <- ape::keep.tip(tr, keep)
      x_raw <- x_raw[match(tr_sub$tip.label, kept_names)]
      tr_sub
    } else {
      tr
    }
    x <- log1p(pmax(0, x_raw))
    cov_mat <- ape::vcv.phylo(tr_k)
    cov_inv <- solve(cov_mat)
    n <- length(x)
    one <- rep(1, n)
    a_hat <- as.numeric(
      (t(one) %*% cov_inv %*% x) / (t(one) %*% cov_inv %*% one)
    )
    dx <- x - a_hat
    mse0 <- as.numeric(t(dx) %*% dx) / (n - 1)
    mse <- as.numeric(t(dx) %*% cov_inv %*% dx) / (n - 1)
    expected_ratio <- (sum(diag(cov_mat)) - n / sum(cov_inv)) / (n - 1)
    (mse0 / mse) / expected_ratio
  }

  # Bifurcating tree with an NA tip
  set.seed(11)
  tr_bin <- ape::rcoal(16)
  vals_bin <- seq_len(16) * 2
  vals_bin[5L] <- NA_real_
  df_bin <- data.frame(
    taxon_name = tr_bin$tip.label,
    value = vals_bin,
    stringsAsFactors = FALSE
  )

  set.seed(777)
  seed_before <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
  r1 <- calc(tr_bin, df_bin, "value", "score", method = "phylo")
  seed_after <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
  r2 <- calc(tr_bin, df_bin, "value", "score", method = "phylo")

  expect_identical(seed_before, seed_after)
  expect_identical(attr(r1, "phylo_signal"), attr(r2, "phylo_signal"))

  x_ref_bin <- df_bin$value[match(tr_bin$tip.label, df_bin$taxon_name)]
  k_ref_bin <- ref_blomberg_k(tr_bin, x_ref_bin)
  expect_equal(attr(r1, "phylo_signal")$K, k_ref_bin, tolerance = 1e-10)
  expect_equal(attr(r1, "phylo_signal")$n_contrasts, 14L)

  # Polytomous tree without edge lengths (Grafen-calibrated internally)
  tr_poly <- ape::read.tree(
    text = "((T1,T2,T3,T4),(T5,T6,(T7,T8,T9)),T10);"
  )
  df_poly <- data.frame(
    taxon_name = tr_poly$tip.label,
    value = c(100, 90, 95, 88, 1, 2, 0, 1, 0, 5),
    stringsAsFactors = FALSE
  )
  r_poly <- calc(tr_poly, df_poly, "value", "score", method = "phylo")
  tr_graf <- ape::compute.brlen(tr_poly, method = "Grafen", power = 1)
  tr_graf$edge.length <- pmax(1e-8, tr_graf$edge.length)
  k_ref_poly <- ref_blomberg_k(
    tr_graf,
    df_poly$value[match(tr_poly$tip.label, df_poly$taxon_name)]
  )
  expect_equal(attr(r_poly, "phylo_signal")$K, k_ref_poly, tolerance = 1e-10)
})
