#' @title Calculate branches values
#'
#' @description
#' Calculates values for each branch (node) in a phylogenetic tree, assigning tip values from a data frame and computing internal node values as the mean of descendant tips. Returns a data frame with node-level results for downstream analysis or plotting.
#'
#' @param tree A phylo object (from ape package)
#' @param df A data.frame containing at least a 'taxon_name' column and a column for the variable of interest
#' @param variable Character. Name of the column in df to use for tip values
#' @param name Character. Name for the output column in the result
#'
#' @return A data.frame with node, count, is_tip, parent_count, and the calculated branch value (named by 'name')
#'
#' @examples
#' NULL
calculate_branches_values <- function(tree, df, variable, name) {
  # Validate input
  if (!inherits(tree, "phylo")) {
    stop("tree must be a phylo object")
  }
  if (!all(c("taxon_name", variable) %in% names(df))) {
    stop("df must contain 'taxon_name' and the specified variable column")
  }

  # Assign tip values
  if (!is.null(tree$tip_id_ott) && "taxon_id_ott" %in% names(df)) {
    tip_ids <- normalize_ott_ids(tree$tip_id_ott)
    df_ids <- normalize_ott_ids(df$taxon_id_ott)
    matched_nodes <- match(df_ids, tip_ids)
    valid_match <- !is.na(matched_nodes)
    node_values <- data.frame(
      node = matched_nodes[valid_match],
      count = tidytable::pull(df, !!as.name(variable))[valid_match]
    )
  } else {
    node_values <- data.frame(
      node = match(df$taxon_name, tree$tip.label),
      count = tidytable::pull(df, !!as.name(variable))
    )
  }

  # Internal nodes
  n_tips <- ape::Ntip(tree)
  n_nodes <- ape::Nnode(tree)
  internal_nodes <- (n_tips + 1):(n_tips + n_nodes)

  # Calculate internal node values as mean of descendant tip values
  internal_node_values <- purrr::map_dfr(
    internal_nodes,
    .f = function(x) {
      descendants <- phangorn::Descendants(tree, x, type = "tips")[[1]]
      data.frame(
        node = x,
        count = base::mean(
          node_values$count[node_values$node %in% descendants],
          na.rm = TRUE
        )
      )
    }
  )

  # Combine tip and internal node values
  node_values <- tidytable::bind_rows(node_values, internal_node_values)

  # Combine with tree structure
  library(ggtree)
  tree_data <- tibble::as_tibble(tree) |>
    tidytable::left_join(node_values, by = "node")
  tree_data$is_tip <- tree_data$node <= n_tips
  tree_data$parent_count <- tree_data$count[match(
    tree_data$parent,
    tree_data$node
  )]
  tree_data[[name]] <- ifelse(
    test = tree_data$is_tip,
    yes = tree_data$count,
    no = (tree_data$count + tree_data$parent_count) / 2
  )

  return(tree_data)
}

# calculate_branches_values_optimized <- function(tree, df, variable, name) {
#   n_tips <- ape::Ntip(tree)
#   n_nodes <- ape::Nnode(tree)
#
#   node_values <- numeric(n_tips + n_nodes)
#
#   tip_indices <- match(df$taxon_name, tree$tip.label)
#   valid_tips <- !is.na(tip_indices)
#   node_values[tip_indices[valid_tips]] <- df[[variable]][valid_tips]
#
#   edge_matrix <- tree$edge
#   internal_nodes <- (n_tips + 1):(n_tips + n_nodes)
#
#   descendant_means <- internal_nodes |>
#     purrr::map(.f = function(node) {
#       descendants <- phangorn::Descendants(tree, node, type = "tips")[[1]]
#       mean(node_values[descendants], na.rm = TRUE)
#     })
#
#   node_values[internal_nodes] <- descendant_means
#
#   data.table(
#     node = seq_along(node_values),
#     !!name := node_values
#   )
# }
