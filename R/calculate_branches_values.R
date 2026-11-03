#' @title Calculate branches values
#'
#' @description
#' Calculates values for each branch (node) in a phylogenetic tree, assigning
#' tip values from a data frame and computing internal node values as the mean
#' of descendant tips. Returns a data frame with node-level results for
#' downstream analysis or plotting.
#'
#' @include normalize_ott_ids.R
#'
#' @param tree A `phylo` object (from the `ape` package).
#' @param df A `data.frame` containing at least a `taxon_name` column and a
#' column for the variable of interest.
#' @param variable Character. Name of the column in `df` to use for tip values.
#' @param name Character. Name for the output column in the result.
#'
#' @return A `data.frame` with `parent`, `node`, `label`, `count`, `is_tip`,
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
calculate_branches_values <- function(tree, df, variable, name) {
  if (!inherits(tree, "phylo")) {
    stop("tree must be a phylo object")
  }
  if (!all(c("taxon_name", variable) %in% names(df))) {
    stop("df must contain 'taxon_name' and the specified variable column")
  }

  values <- suppressWarnings(as.numeric(df[[variable]]))
  values[!is.finite(values)] <- NA_real_
  weights <- if ("tip_weight" %in% names(df)) {
    w <- suppressWarnings(as.numeric(df$tip_weight))
    w[!is.finite(w) | w <= 0] <- 1
    w
  } else {
    rep(1, nrow(df))
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
        weight = weights[valid_match]
      )
    } else {
      node_values <- data.frame(
        node = match(df$taxon_name, tree$tip.label),
        count = values,
        weight = weights
      )
    }
  } else {
    node_values <- data.frame(
      node = match(df$taxon_name, tree$tip.label),
      count = values,
      weight = weights
    )
  }

  n_tips <- ape::Ntip(tree)
  n_nodes <- ape::Nnode(tree)
  internal_nodes <- seq.int(n_tips + 1L, length.out = n_nodes)

  partitions <- if (n_nodes > 0L) ape::prop.part(tree) else list()
  internal_counts <- vapply(
    partitions,
    function(descendants) {
      idx <- node_values$node %in% descendants
      desc_vals <- node_values$count[idx]
      desc_w <- node_values$weight[idx]
      valid <- is.finite(desc_vals)
      if (!any(valid)) {
        NA_real_
      } else {
        sum(desc_vals[valid] * desc_w[valid]) / sum(desc_w[valid])
      }
    },
    numeric(1L)
  )

  node_values <- tidytable::bind_rows(
    node_values[, c("node", "count")],
    data.frame(node = internal_nodes, count = internal_counts)
  )

  total_nodes <- n_tips + n_nodes
  all_nodes <- seq_len(total_nodes)
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
    tree_base <- data.frame(
      parent = as.integer(parent),
      node = as.integer(all_nodes),
      branch.length = as.numeric(branch_length),
      label = labels,
      stringsAsFactors = FALSE
    )
  } else {
    tree_base <- data.frame(
      parent = as.integer(parent),
      node = as.integer(all_nodes),
      label = labels,
      stringsAsFactors = FALSE
    )
  }

  tree_data <- tidytable::left_join(tree_base, node_values, by = "node")
  tree_data$is_tip <- tree_data$node <= n_tips
  tree_data$parent_count <- tree_data$count[match(
    tree_data$parent,
    tree_data$node
  )]
  branch_val <- ifelse(
    test = tree_data$is_tip,
    yes = tree_data$count,
    no = (tree_data$count + tree_data$parent_count) / 2
  )
  tree_data[[name]] <- ifelse(is.finite(branch_val), branch_val, NA_real_)
  tree_data$count <- ifelse(
    is.finite(tree_data$count),
    tree_data$count,
    NA_real_
  )

  tree_data
}
