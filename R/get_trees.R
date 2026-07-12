#' @title Get trees
#'
#' @description
#' Builds phylogenetic trees for a set of Open Tree of Life (OTT) IDs, using rotl and tidytable. Returns a list with taxa not in the tree, the full tree, and a tree for taxa with compounds.
#'
#' @param ott_ids Integer vector. OTT IDs to include in the tree.
#' @param df_counted Data.frame. Must contain columns 'taxon_id_ott', 'compounds_count', and 'ethnobotanical_count'.
#' @param taxon_name Character. Name of the taxon for filtering.
#'
#' @return List with elements: taxa_not_in_tree (data.frame), tree_taxa_full (phylo), tree_taxa_phyto (phylo).
#'
#' @examples
#' NULL
get_trees_cache_key <- function(ott_ids, df_counted, taxon_name) {
  ott_ids <- normalize_ott_ids(ott_ids)
  ott_ids <- unique(ott_ids[!is.na(ott_ids) & nzchar(ott_ids)])
  ott_ids <- sort(ott_ids)

  df_frame <- as.data.frame(df_counted, stringsAsFactors = FALSE)
  if ("taxon_id_ott" %in% names(df_frame)) {
    df_frame$taxon_id_ott <- normalize_ott_ids(df_frame$taxon_id_ott)
  }
  if ("taxon_name" %in% names(df_frame)) {
    df_frame$taxon_name <- as.character(df_frame$taxon_name)
  }
  if ("compounds_count" %in% names(df_frame)) {
    df_frame$compounds_count <- as.character(df_frame$compounds_count)
  }
  if ("ethnobotanical_count" %in% names(df_frame)) {
    df_frame$ethnobotanical_count <- as.character(df_frame$ethnobotanical_count)
  }

  df_frame <- df_frame[
    !is.na(df_frame$taxon_id_ott) & nzchar(df_frame$taxon_id_ott),
    ,
    drop = FALSE
  ]
  if (nrow(df_frame) > 0L) {
    df_frame <- unique(df_frame)
    df_frame <- df_frame[order(df_frame$taxon_id_ott, df_frame$taxon_name), , drop = FALSE]
  }

  key_parts <- c(
    as.character(taxon_name),
    paste(ott_ids, collapse = ","),
    paste(df_frame$taxon_id_ott, collapse = ","),
    paste(df_frame$taxon_name, collapse = "|"),
    paste(df_frame$compounds_count, collapse = ","),
    paste(df_frame$ethnobotanical_count, collapse = ",")
  )
  paste(key_parts, collapse = "::")
}

get_trees_cached <- function(ott_ids, df_counted, taxon_name, cache_dir) {
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  cache_key <- get_trees_cache_key(ott_ids, df_counted, taxon_name)
  cache_file <- file.path(
    cache_dir,
    paste0("trees_", openssl::md5(cache_key), ".rds")
  )

  if (file.exists(cache_file)) {
    return(readRDS(cache_file))
  }

  result <- get_trees(
    ott_ids = ott_ids,
    df_counted = df_counted,
    taxon_name = taxon_name
  )
  saveRDS(result, cache_file)
  result
}

get_trees <- function(ott_ids, df_counted, taxon_name) {
  # Input validation
  if (!(is.numeric(ott_ids) || is.character(ott_ids))) {
    stop("ott_ids must be numeric or character (integer vector of OTT IDs)")
  }
  if (!is.data.frame(df_counted)) {
    stop("df_counted must be a data.frame")
  }
  if (!is.character(taxon_name) || length(taxon_name) != 1) {
    stop("taxon_name must be a single character string")
  }
  required_cols <- c("taxon_id_ott", "compounds_count", "ethnobotanical_count")
  if (!all(required_cols %in% names(df_counted))) {
    stop(
      "df_counted must contain columns: ",
      paste(required_cols, collapse = ", ")
    )
  }

  build_placeholder_tree <- function(ott_ids_subset, df_frame = df_counted) {
    ott_ids_subset <- normalize_ott_ids(ott_ids_subset)
    ott_ids_subset <- unique(
      ott_ids_subset[!is.na(ott_ids_subset) & nzchar(ott_ids_subset)]
    )
    if (length(ott_ids_subset) == 0L) {
      return(ape::read.tree(text = "(A);"))
    }
    tip_labels <- df_frame$taxon_name[
      match(ott_ids_subset, normalize_ott_ids(df_frame$taxon_id_ott))
    ]
    tip_labels[is.na(tip_labels)] <- ott_ids_subset[is.na(tip_labels)]
    if (length(ott_ids_subset) == 1L) {
      tree <- ape::read.tree(text = paste0("(", tip_labels[1], ");"))
    } else {
      tree <- ape::stree(length(ott_ids_subset), "star")
      tree$tip.label <- tip_labels
    }
    tree$tip_id_ott <- ott_ids_subset
    tree
  }

  ott_ids_normalized <- normalize_ott_ids(ott_ids)
  ott_ids_integer <- suppressWarnings(as.integer(ott_ids_normalized))
  ott_ids_integer <- ott_ids_integer[!is.na(ott_ids_integer)]

  validate_ott_ids <- function(ott_ids_subset) {
    ott_ids_subset <- normalize_ott_ids(ott_ids_subset)
    ott_ids_subset <- unique(
      ott_ids_subset[!is.na(ott_ids_subset) & nzchar(ott_ids_subset)]
    )
    if (length(ott_ids_subset) == 0L) {
      return(list(valid = character(), invalid = character()))
    }

    ott_ids_integer <- suppressWarnings(as.integer(ott_ids_subset))
    valid_flags <- !is.na(ott_ids_integer) & rotl::is_in_tree(ott_ids_integer)

    list(
      valid = as.character(ott_ids_integer[valid_flags]),
      invalid = as.character(ott_ids_integer[!valid_flags])
    )
  }

  if (length(ott_ids_integer) == 0L) {
    tree_taxa_full <- build_placeholder_tree(character(), df_counted)
    tree_taxa_phyto <- build_placeholder_tree(character(), df_counted)
    taxa_not_in_tree <- df_counted[
      df_counted$compounds_count > 0L |
        df_counted$ethnobotanical_count > 0L,
      ,
      drop = FALSE
    ]
    names(taxa_not_in_tree)[
      names(taxa_not_in_tree) == "ethnobotanical_count"
    ] <- "ethnobotanical_species_count"
    return(list(
      taxa_not_in_tree = taxa_not_in_tree,
      tree_taxa_full = tree_taxa_full,
      tree_taxa_phyto = tree_taxa_phyto
    ))
  }

  validated_ids <- validate_ott_ids(ott_ids_normalized)

  ott_in_tree_tax <- tryCatch(
    {
      if (length(validated_ids$valid) == 0L) {
        character()
      } else {
        as.character(validated_ids$valid)
      }
    },
    error = function(e) {
      logger::log_warn(
        "Failed to resolve tree lineage for OTT IDs; keeping the original OTT IDs for tree construction."
      )
      ott_ids_normalized
    }
  )

  tree_query_ids <- unique(c(
    ott_ids_normalized,
    normalize_ott_ids(ott_in_tree_tax)
  ))
  tree_query_ids_validated <- validate_ott_ids(tree_query_ids)

  relabel_tree_with_names <- function(
    tree,
    ott_ids_subset,
    df_frame = df_counted
  ) {
    if (!inherits(tree, "phylo")) {
      return(tree)
    }

    ott_ids_subset <- normalize_ott_ids(ott_ids_subset)
    ott_ids_subset <- unique(
      ott_ids_subset[!is.na(ott_ids_subset) & nzchar(ott_ids_subset)]
    )

    if (length(ott_ids_subset) == 0L) {
      return(tree)
    }

    matched_names <- df_frame$taxon_name[
      match(ott_ids_subset, normalize_ott_ids(df_frame$taxon_id_ott))
    ]
    matched_names[is.na(matched_names)] <- ott_ids_subset[is.na(matched_names)]

    if (length(matched_names) >= length(tree$tip.label)) {
      tree$tip.label <- matched_names[seq_len(length(tree$tip.label))]
    } else {
      tree$tip.label <- c(
        matched_names,
        tree$tip.label[(length(matched_names) + 1L):length(tree$tip.label)]
      )
    }

    tree$tip_id_ott <- ott_ids_subset
    tree
  }

  build_tree <- function(ott_ids_subset, validated_ids_subset = NULL) {
    ott_ids_subset <- normalize_ott_ids(ott_ids_subset)
    ott_ids_subset <- unique(
      ott_ids_subset[!is.na(ott_ids_subset) & nzchar(ott_ids_subset)]
    )
    if (length(ott_ids_subset) == 0L) {
      return(build_placeholder_tree(character(), df_counted))
    }

    if (is.null(validated_ids_subset)) {
      validated_ids_subset <- validate_ott_ids(ott_ids_subset)
    }

    if (length(validated_ids_subset$valid) < 2L) {
      return(relabel_tree_with_names(
        build_placeholder_tree(ott_ids_subset, df_counted),
        ott_ids_subset,
        df_counted
      ))
    }

    tree_result <- tryCatch(
      rotl::tol_induced_subtree(as.integer(validated_ids_subset$valid)),
      error = function(e) {
        NULL
      }
    )

    if (!inherits(tree_result, "phylo")) {
      chunk_size <- 80L
      batch_ids <- split(
        validated_ids_subset$valid,
        ceiling(seq_along(validated_ids_subset$valid) / chunk_size)
      )

      tree_result <- NULL
      for (batch in batch_ids) {
        Sys.sleep(0.2)
        batch_tree <- tryCatch(
          rotl::tol_induced_subtree(as.integer(batch)),
          error = function(e) {
            NULL
          }
        )

        if (!inherits(batch_tree, "phylo")) {
          next
        }

        if (is.null(tree_result)) {
          tree_result <- batch_tree
        } else {
          tree_result <- ape::bind.tree(tree_result, batch_tree, where = "root")
        }
      }
    }

    if (!inherits(tree_result, "phylo")) {
      logger::log_warn(
        "Failed to build tree from OTT IDs; falling back to a star tree containing all requested IDs."
      )
      relabel_tree_with_names(
        build_placeholder_tree(ott_ids_subset, df_counted),
        ott_ids_subset,
        df_counted
      )
    } else {
      relabel_tree_with_names(
        tree_result,
        validated_ids_subset$valid,
        df_counted
      )
    }
  }

  tree_taxa_full <- build_tree(
    tree_query_ids,
    tree_query_ids_validated
  )
  tree_taxa_phyto <- build_tree(
    tree_query_ids[
      tree_query_ids %in%
        normalize_ott_ids(df_counted$taxon_id_ott[
          df_counted$compounds_count >= 1
        ])
    ],
    list(
      valid = tree_query_ids_validated$valid[
        tree_query_ids_validated$valid %in%
          normalize_ott_ids(df_counted$taxon_id_ott[
            df_counted$compounds_count >= 1
          ])
      ],
      invalid = character()
    )
  )

  tree_tip_ids_full <- normalize_ott_ids(tree_taxa_full$tip_id_ott)
  tree_tip_ids_full <- tree_tip_ids_full[
    !is.na(tree_tip_ids_full) & nzchar(tree_tip_ids_full)
  ]

  taxa_not_in_tree <- df_counted[
    !normalize_ott_ids(df_counted$taxon_id_ott) %in% tree_tip_ids_full &
      (df_counted$compounds_count > 0L | df_counted$ethnobotanical_count > 0L),
    ,
    drop = FALSE
  ]
  names(taxa_not_in_tree)[
    names(taxa_not_in_tree) == "ethnobotanical_count"
  ] <- "ethnobotanical_species_count"

  return(list(
    taxa_not_in_tree = taxa_not_in_tree,
    tree_taxa_full = tree_taxa_full,
    tree_taxa_phyto = tree_taxa_phyto
  ))
}
