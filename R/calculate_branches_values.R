#' @title Calculate branches values
#'
#' @description
#' Calculates values for each branch (node) in a phylogenetic tree. Assigns tip
#' values from a data frame and computes internal node states via both weighted
#' descendant means (`count`) and two-pass Felsenstein pruning / belief
#' propagation ancestral state reconstruction (`phylo_asr`) on Grafen-calibrated
#' branch lengths. Also computes Empirical Bayes Beta-Binomial shrunk rates
#' (`eb_rate`), clade-level hypergeometric "hot node" enrichment p/q-values
#' (`hot_node_pval`, `hot_node_qval`, `is_hot_node`), and Blomberg's K
#' phylogenetic signal (`attr(result, "phylo_signal")`).
#'
#' @include normalize_ott_ids.R
#'
#' @param tree A `phylo` object (from the `ape` package).
#' @param df A `data.frame` containing at least a `taxon_name` column and a
#' column for the variable of interest.
#' @param variable Character. Name of the column in `df` to use for tip values.
#' @param name Character. Name for the output column in the result.
#' @param method Character. Either `"mean"` (default, unweighted/tip-weighted
#' descendant mean for backward compatibility) or `"phylo"` (Felsenstein pruning
#' ML ancestral state reconstruction on Grafen branch lengths).
#'
#' @return A `data.frame` with `parent`, `node`, `label`, `count`, `phylo_asr`,
#' `eb_rate`, `hot_node_pval`, `hot_node_qval`, `is_hot_node`, `is_tip`,
#' `parent_count`, and the calculated branch value (named by `name`).
#'
#' @section EDAM:
#' - Topic: `http://edamontology.org/topic_3293` (Phylogenetics)
#' - Operation: `http://edamontology.org/operation_0324`
#'   (Phylogenetic analysis)
#' - Data: `http://edamontology.org/data_0872` (Phylogenetic tree),
#'   `http://edamontology.org/data_3272` (Species tree)
#'
#' @keywords internal
#'
#' @examples
#' NULL
calculate_branches_values <- function(
  tree,
  df,
  variable,
  name,
  method = c("mean", "phylo")
) {
  if (!inherits(tree, "phylo")) {
    stop("tree must be a phylo object")
  }
  if (!all(c("taxon_name", variable) %in% names(df))) {
    stop("df must contain 'taxon_name' and the specified variable column")
  }
  method <- match.arg(method)

  values <- suppressWarnings(as.numeric(df[[variable]]))
  values[!is.finite(values)] <- NA_real_
  weights <- if ("tip_weight" %in% names(df)) {
    w <- suppressWarnings(as.numeric(df$tip_weight))
    w[!is.finite(w) | w <= 0] <- 1
    w
  } else {
    rep(1, nrow(df))
  }
  sp_counts <- if ("species_count" %in% names(df)) {
    s <- suppressWarnings(as.numeric(df$species_count))
    s[!is.finite(s) | s < 1] <- 1
    s
  } else {
    weights
  }

  if (!is.null(tree$tip_id_ott) && "taxon_id_ott" %in% names(df)) {
    df_ids <- normalize_ott_ids(df$taxon_id_ott)
    if (!all(is.na(df_ids))) {
      tip_ids <- normalize_ott_ids(tree$tip_id_ott)
      matched_nodes <- match(df_ids, tip_ids)
      missing_idx <- is.na(matched_nodes)
      if (any(missing_idx)) {
        matched_nodes[missing_idx] <- match(
          df$taxon_name[missing_idx],
          tree$tip.label
        )
      }
      valid_match <- !is.na(matched_nodes)
      node_values <- data.frame(
        node = matched_nodes[valid_match],
        count = values[valid_match],
        weight = weights[valid_match],
        sp_count = sp_counts[valid_match]
      )
    } else {
      node_values <- data.frame(
        node = match(df$taxon_name, tree$tip.label),
        count = values,
        weight = weights,
        sp_count = sp_counts
      )
    }
  } else {
    node_values <- data.frame(
      node = match(df$taxon_name, tree$tip.label),
      count = values,
      weight = weights,
      sp_count = sp_counts
    )
  }
  node_values <- node_values[!is.na(node_values$node), , drop = FALSE]
  node_values <- node_values[!duplicated(node_values$node), , drop = FALSE]

  n_tips <- ape::Ntip(tree)
  n_nodes <- ape::Nnode(tree)
  total_nodes <- n_tips + n_nodes
  internal_nodes <- seq.int(n_tips + 1L, length.out = n_nodes)

  # Empirical Bayes Beta-Binomial shrinkage on tip proportions (k_i / N_i)
  tip_val_vec <- rep(NA_real_, n_tips)
  tip_w_vec <- rep(1, n_tips)
  tip_sp_vec <- rep(1, n_tips)
  tip_val_vec[node_values$node] <- node_values$count
  tip_w_vec[node_values$node] <- node_values$weight
  tip_sp_vec[node_values$node] <- pmax(
    node_values$sp_count,
    ifelse(is.finite(node_values$count), node_values$count, 1)
  )

  valid_tips <- is.finite(tip_val_vec) & tip_val_vec >= 0
  tip_eb_vec <- rep(NA_real_, n_tips)
  if (any(valid_tips)) {
    k_v <- tip_val_vec[valid_tips]
    n_v <- pmax(1, tip_sp_vec[valid_tips], k_v)
    p_v <- k_v / n_v
    m_p <- sum(k_v) / sum(n_v)
    v_p <- stats::var(p_v)
    if (is.finite(m_p) && m_p > 0 && m_p < 1 && is.finite(v_p) && v_p > 0) {
      conc <- max(1, min(200, (m_p * (1 - m_p) / v_p) - 1))
      alpha0 <- m_p * conc
      beta0 <- (1 - m_p) * conc
    } else {
      alpha0 <- max(1e-3, m_p * 2)
      beta0 <- max(1e-3, (1 - m_p) * 2)
    }
    tip_eb_vec[valid_tips] <- (k_v + alpha0) / (n_v + alpha0 + beta0)
  }

  partitions <- if (n_nodes > 0L) ape::prop.part(tree) else list()
  internal_counts <- vapply(
    partitions,
    function(descendants) {
      desc_vals <- tip_val_vec[descendants]
      desc_w <- tip_w_vec[descendants]
      valid <- is.finite(desc_vals)
      if (!any(valid)) {
        NA_real_
      } else {
        sum(desc_vals[valid] * desc_w[valid]) / sum(desc_w[valid])
      }
    },
    numeric(1L)
  )

  # Clade-level hypergeometric "hot node" enrichment (Saslis-Lagoudakis et al.)
  use_species_pool <- any(tip_sp_vec > 1, na.rm = TRUE)
  total_active <- if (use_species_pool) {
    sum(pmax(0, round(tip_val_vec[valid_tips])), na.rm = TRUE)
  } else {
    sum(is.finite(tip_val_vec) & tip_val_vec > 0)
  }
  total_pool <- if (use_species_pool) {
    sum(
      pmax(
        round(tip_sp_vec[valid_tips]),
        round(tip_val_vec[valid_tips]),
        1
      ),
      na.rm = TRUE
    )
  } else {
    sum(is.finite(tip_val_vec))
  }
  internal_hot_p <- vapply(
    partitions,
    function(descendants) {
      if (length(descendants) < 2L || total_active <= 0L) {
        return(NA_real_)
      }
      d_ok <- descendants[is.finite(tip_val_vec[descendants])]
      if (length(d_ok) < 2L) {
        return(NA_real_)
      }
      if (use_species_pool) {
        k_clade <- sum(pmax(0, round(tip_val_vec[d_ok])), na.rm = TRUE)
        n_clade <- sum(
          pmax(round(tip_sp_vec[d_ok]), round(tip_val_vec[d_ok]), 1),
          na.rm = TRUE
        )
      } else {
        k_clade <- sum(tip_val_vec[d_ok] > 0)
        n_clade <- length(d_ok)
      }
      if (n_clade < 2L || k_clade <= 0L) {
        return(1)
      }
      stats::phyper(
        q = k_clade - 1L,
        m = total_active,
        n = max(0L, total_pool - total_active),
        k = n_clade,
        lower.tail = FALSE
      )
    },
    numeric(1L)
  )
  internal_hot_q <- if (length(internal_hot_p) > 0L) {
    stats::p.adjust(internal_hot_p, method = "BH")
  } else {
    numeric(0L)
  }

  # Two-pass Felsenstein pruning / belief-propagation ASR on Grafen branch lengths
  felsenstein_asr <- function(tr, tip_x, tip_w) {
    out <- rep(NA_real_, total_nodes)
    out[seq_len(n_tips)] <- tip_x
    if (n_nodes == 0L || sum(is.finite(tip_x)) == 0L) {
      return(out)
    }
    tr_cal <- tr
    if (is.null(tr_cal$edge.length) && n_tips >= 2L) {
      tr_cal <- tryCatch(
        ape::compute.brlen(tr_cal, method = "Grafen", power = 1),
        error = function(e) tr_cal
      )
    }
    tr_post <- tryCatch(
      ape::reorder.phylo(tr_cal, "postorder"),
      error = function(e) tr_cal
    )
    edges <- tr_post$edge
    elen <- if (!is.null(tr_post$edge.length)) {
      pmax(1e-4, as.numeric(tr_post$edge.length))
    } else {
      rep(1, nrow(edges))
    }
    elen[!is.finite(elen) | elen <= 0] <- 1

    mu_down <- rep(NA_real_, total_nodes)
    var_down <- rep(0, total_nodes)
    mu_down[seq_len(n_tips)] <- tip_x
    var_down[seq_len(n_tips)] <- 1 / pmax(1, tip_w) - 1

    # Upward postorder pass
    p_seq <- unique(edges[, 1L])
    for (u in p_seq) {
      e_idx <- which(edges[, 1L] == u)
      ch <- edges[e_idx, 2L]
      b_ch <- elen[e_idx]
      ok <- is.finite(mu_down[ch])
      if (any(ok)) {
        prec <- 1 / pmax(1e-6, b_ch[ok] + var_down[ch[ok]])
        sum_prec <- sum(prec)
        mu_down[u] <- sum(prec * mu_down[ch[ok]]) / sum_prec
        var_down[u] <- 1 / sum_prec
      }
    }

    # Downward preorder pass (exact global ML rerooting at internal nodes)
    mu_full <- mu_down
    prec_full <- ifelse(
      is.finite(mu_down) & var_down > 0,
      1 / var_down,
      NA_real_
    )
    for (i in rev(seq_len(nrow(edges)))) {
      p <- edges[i, 1L]
      u <- edges[i, 2L]
      if (
        u > n_tips &&
          is.finite(mu_full[p]) &&
          is.finite(prec_full[p]) &&
          is.finite(mu_down[u])
      ) {
        b_pu <- elen[i]
        prec_u_to_p <- 1 / pmax(1e-6, b_pu + var_down[u])
        prec_comp <- prec_full[p] - prec_u_to_p
        if (is.finite(prec_comp) && prec_comp > 1e-8) {
          mu_comp <- (prec_full[p] * mu_full[p] - prec_u_to_p * mu_down[u]) /
            prec_comp
          prec_p_to_u <- 1 / (b_pu + (1 / prec_comp))
          prec_full[u] <- (1 / var_down[u]) + prec_p_to_u
          mu_full[u] <- ((1 / var_down[u]) *
            mu_down[u] +
            prec_p_to_u * mu_comp) /
            prec_full[u]
        }
      }
    }
    mu_full
  }

  asr_vals <- felsenstein_asr(tree, tip_val_vec, tip_w_vec)
  eb_asr_vals <- felsenstein_asr(tree, tip_eb_vec, tip_w_vec)

  # Blomberg's K phylogenetic signal via independent contrasts on Grafen tree
  phylo_signal <- list(K = NA_real_, p_val = NA_real_, n_contrasts = 0L)
  if (
    n_tips >= 4L &&
      sum(is.finite(tip_val_vec)) >= 4L &&
      stats::sd(tip_val_vec, na.rm = TRUE) > 0
  ) {
    phylo_signal <- tryCatch(
      {
        tr_sig <- tree
        if (is.null(tr_sig$edge.length)) {
          tr_sig <- ape::compute.brlen(tr_sig, method = "Grafen", power = 1)
        }
        tr_bin <- ape::multi2di(tr_sig, random = FALSE)
        tr_bin$edge.length <- pmax(1e-5, tr_bin$edge.length)
        x_sig <- log1p(pmax(0, ifelse(is.finite(tip_val_vec), tip_val_vec, 0)))
        names(x_sig) <- tr_bin$tip.label
        pic_obs <- ape::pic(x_sig, tr_bin)
        mse_obs <- mean(pic_obs^2, na.rm = TRUE)
        var_x <- stats::var(x_sig, na.rm = TRUE)
        k_ratio <- if (is.finite(mse_obs) && mse_obs > 0) {
          var_x / mse_obs
        } else {
          NA_real_
        }
        n_perm <- 199L
        mse_perm <- replicate(n_perm, {
          x_p <- sample(x_sig)
          names(x_p) <- tr_bin$tip.label
          mean(ape::pic(x_p, tr_bin)^2, na.rm = TRUE)
        })
        k_stat <- if (is.finite(mse_obs) && mse_obs > 0) {
          stats::median(mse_perm, na.rm = TRUE) / mse_obs
        } else {
          NA_real_
        }
        p_sig <- (sum(mse_perm <= mse_obs, na.rm = TRUE) + 1) / (n_perm + 1)
        list(
          K = as.numeric(k_stat),
          p_val = as.numeric(p_sig),
          n_contrasts = length(pic_obs),
          raw_ratio = as.numeric(k_ratio)
        )
      },
      error = function(e) phylo_signal
    )
  }

  all_nodes <- seq_len(total_nodes)
  all_counts <- c(tip_val_vec, internal_counts)
  all_hot_p <- c(rep(NA_real_, n_tips), internal_hot_p)
  all_hot_q <- c(rep(NA_real_, n_tips), internal_hot_q)

  edge_idx <- match(all_nodes, tree$edge[, 2L])
  parent <- tree$edge[edge_idx, 1L]
  parent[is.na(parent)] <- all_nodes[is.na(parent)]

  node_labels <- if (!is.null(tree$node.label)) {
    as.character(tree$node.label)
  } else {
    rep(NA_character_, n_nodes)
  }
  labels <- c(as.character(tree$tip.label), node_labels)

  if (!is.null(tree$edge.length)) {
    branch_length <- tree$edge.length[edge_idx]
    if (!is.null(tree$root.edge)) {
      branch_length[parent == all_nodes] <- tree$root.edge
    }
    tree_data <- data.frame(
      parent = as.integer(parent),
      node = as.integer(all_nodes),
      branch.length = as.numeric(branch_length),
      label = labels,
      stringsAsFactors = FALSE
    )
  } else {
    tree_data <- data.frame(
      parent = as.integer(parent),
      node = as.integer(all_nodes),
      label = labels,
      stringsAsFactors = FALSE
    )
  }

  tree_data$count <- ifelse(is.finite(all_counts), all_counts, NA_real_)
  tree_data$phylo_asr <- ifelse(is.finite(asr_vals), asr_vals, NA_real_)
  tree_data$eb_rate <- ifelse(is.finite(eb_asr_vals), eb_asr_vals, NA_real_)
  tree_data$hot_node_pval <- ifelse(is.finite(all_hot_p), all_hot_p, NA_real_)
  tree_data$hot_node_qval <- ifelse(is.finite(all_hot_q), all_hot_q, NA_real_)
  tree_data$is_hot_node <- !is.na(tree_data$hot_node_qval) &
    tree_data$hot_node_qval < 0.05

  tree_data$is_tip <- tree_data$node <= n_tips
  tree_data$parent_count <- tree_data$count[match(
    tree_data$parent,
    tree_data$node
  )]
  parent_asr <- tree_data$phylo_asr[match(
    tree_data$parent,
    tree_data$node
  )]

  branch_val <- if (identical(method, "phylo")) {
    ifelse(
      test = tree_data$is_tip,
      yes = tree_data$phylo_asr,
      no = (tree_data$phylo_asr + parent_asr) / 2
    )
  } else {
    ifelse(
      test = tree_data$is_tip,
      yes = tree_data$count,
      no = (tree_data$count + tree_data$parent_count) / 2
    )
  }
  tree_data[[name]] <- ifelse(is.finite(branch_val), branch_val, NA_real_)
  attr(tree_data, "phylo_signal") <- phylo_signal

  tree_data
}
