#' @title Calculate branches values
#'
#' @description This function calculates branches values.
#'
#' @param tree A tree
#' @param df df
#' @param variable Variable
#' @param name Name
#'
#' @return A dataframe containing the results of the calculation
#'
#' @examples NULL
calculate_branches_values <- function(tree, df, variable, name) {
  # Assign tip values
  node_values <- data.frame(
    node = match(df$taxon_name, tree$tip.label),
    count = df |>
      tidytable::pull(!!as.name(variable))
  )

  # Internal nodes
  n_tips <- tree |>
    ape::Ntip()
  n_nodes <- tree |>
    ape::Nnode()
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
  node_values <- node_values |>
    tidytable::bind_rows(internal_node_values)

  # Combine with tree structure
  library(ggtree)
  tree_data <- tree |>
    tibble::as_tibble() |>
    tidytable::left_join(node_values, by = "node") |>
    tidytable::mutate(
      is_tip = node <= n_tips,
      parent_count = count[match(parent, node)],
      !!as.name(name) := tidytable::if_else(
        is_tip,
        count, # For tips: just use tip value
        (count + parent_count) / 2 # For internal: average with parent
      )
    )

  return(tree_data)
}

calculate_branches_values_optimized <- function(tree, df, variable, name) {
  n_tips <- ape::Ntip(tree)
  n_nodes <- ape::Nnode(tree)

  node_values <- numeric(n_tips + n_nodes)

  tip_indices <- match(df$taxon_name, tree$tip.label)
  valid_tips <- !is.na(tip_indices)
  node_values[tip_indices[valid_tips]] <- df[[variable]][valid_tips]

  edge_matrix <- tree$edge
  internal_nodes <- (n_tips + 1):(n_tips + n_nodes)

  descendant_means <- internal_nodes |>
    purrr::map(.f = function(node) {
      descendants <- phangorn::Descendants(tree, node, type = "tips")[[1]]
      mean(node_values[descendants], na.rm = TRUE)
    })

  node_values[internal_nodes] <- descendant_means

  data.table(
    node = seq_along(node_values),
    !!name := node_values
  )
}
