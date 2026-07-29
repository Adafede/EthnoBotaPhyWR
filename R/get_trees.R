#' @title Get trees
#'
#' @description
#' Builds phylogenetic trees for a set of Open Tree of Life (OTT) IDs using the rotl service. The function returns the taxa that are absent from the requested tree, the full induced subtree, and a second tree restricted to taxa with reported compounds or ethnobotanical uses.
#'
#' @param ott_ids Integer vector. OTT IDs to include in the tree.
#' @param df_counted Data.frame. Data frame containing at least `taxon_id_ott`, `compounds_count`, and `ethnobotanical_count` columns.
#' @param taxon_name Character. Name of the focal taxon used to build the cache key and log context.
#'
#' @return A list with `taxa_not_in_tree`, `tree_taxa_full`, and `tree_taxa_phyto` elements.
#'
#' @examples
#' # get_trees_cache_key(c(1234, 5678), data.frame(taxon_id_ott = c(1234, 5678), compounds_count = c(1, 0), ethnobotanical_count = c(0, 1)), "Orchidaceae")
#' @keywords internal
get_trees_cache_key <- function(ott_ids, df_counted, taxon_name) {
  cache_version <- "tree-v1"
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
    df_frame <- df_frame[
      order(df_frame$taxon_id_ott, df_frame$taxon_name),
      ,
      drop = FALSE
    ]
  }

  key_parts <- c(
    cache_version,
    as.character(taxon_name),
    paste(ott_ids, collapse = ","),
    paste(df_frame$taxon_id_ott, collapse = ","),
    paste(df_frame$taxon_name, collapse = "|"),
    paste(df_frame$compounds_count, collapse = ","),
    paste(df_frame$ethnobotanical_count, collapse = ",")
  )
  paste(key_parts, collapse = "::")
}

#' @title Get cached trees
#'
#' @description
#' Returns a cached tree result when available, otherwise builds and serializes the tree result to disk.
#'
#' @param ott_ids Integer vector. OTT IDs to include in the tree.
#' @param df_counted Data.frame. Data frame containing at least `taxon_id_ott`, `compounds_count`, and `ethnobotanical_count` columns.
#' @param taxon_name Character. Name of the focal taxon used to derive the cache key.
#' @param cache_dir Character. Directory used for persistent on-disk caching.
#'
#' @return The cached or newly built tree result list.
get_trees_cached <- function(ott_ids, df_counted, taxon_name, cache_dir) {
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  cache_key <- get_trees_cache_key(ott_ids, df_counted, taxon_name)
  cache_file <- file.path(
    cache_dir,
    paste0("trees_", openssl::md5(cache_key), ".rda")
  )

  if (file.exists(cache_file)) {
    return(load(cache_file))
  }

  result <- get_trees(
    ott_ids = ott_ids,
    df_counted = df_counted,
    taxon_name = taxon_name
  )
  save(result, file = cache_file)
  result
}

#' @title Build trees from OTT IDs
#'
#' @description
#' Builds the full and phytochemical trees for a collection of OTT IDs, falling back to placeholder trees if Open Tree requests fail or return incomplete data.
#'
#' @param ott_ids Integer vector. OTT IDs to include in the tree.
#' @param df_counted Data.frame. Data frame containing at least `taxon_id_ott`, `compounds_count`, and `ethnobotanical_count` columns.
#' @param taxon_name Character. Name of the focal taxon used for logging and fallback naming.
#'
#' @return A list with the taxa missing from the full tree, the full tree object, and the phytochemical tree object.
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

    n_tips <- length(tree$tip.label)
    if (length(matched_names) >= n_tips) {
      tree$tip.label <- matched_names[seq_len(n_tips)]
      matched_ids <- ott_ids_subset[seq_len(n_tips)]
    } else {
      tree$tip.label <- c(
        matched_names,
        tree$tip.label[(length(matched_names) + 1L):n_tips]
      )
      matched_ids <- c(
        ott_ids_subset,
        rep(NA_character_, n_tips - length(ott_ids_subset))
      )
    }

    tree$tip_id_ott <- matched_ids
    tree
  }

  build_tree <- function(
    ott_ids_subset,
    validated_ids_subset = NULL,
    fallback_labels = NULL
  ) {
    ott_ids_subset <- normalize_ott_ids(ott_ids_subset)
    ott_ids_subset <- unique(
      ott_ids_subset[!is.na(ott_ids_subset) & nzchar(ott_ids_subset)]
    )
    fallback_labels <- unique(
      as.character(fallback_labels[
        !is.na(fallback_labels) & nzchar(fallback_labels)
      ])
    )

    if (length(ott_ids_subset) == 0L && length(fallback_labels) == 0L) {
      return(build_placeholder_tree(character(), df_counted))
    }

    if (is.null(validated_ids_subset)) {
      validated_ids_subset <- validate_ott_ids(ott_ids_subset)
    }

    append_missing_labels <- function(tree, labels) {
      if (!inherits(tree, "phylo") || length(labels) == 0L) {
        return(tree)
      }

      existing_labels <- tree$tip.label[
        !is.na(tree$tip.label) & nzchar(tree$tip.label)
      ]
      missing_labels <- labels[!labels %in% existing_labels]
      if (length(missing_labels) == 0L) {
        return(tree)
      }

      fallback_lookup <- data.frame(
        taxon_name = as.character(df_counted$taxon_name),
        taxon_id_ott = normalize_ott_ids(df_counted$taxon_id_ott),
        stringsAsFactors = FALSE
      )
      fallback_lookup <- fallback_lookup[
        !is.na(fallback_lookup$taxon_name) & nzchar(fallback_lookup$taxon_name),
      ]
      fallback_lookup <- unique(fallback_lookup)

      prior_tip_ids <- if (is.null(tree$tip_id_ott)) {
        rep(NA_character_, length(existing_labels))
      } else {
        as.character(tree$tip_id_ott)
      }

      new_tip_ids <- rep(NA_character_, length(missing_labels))
      for (i in seq_along(missing_labels)) {
        label <- missing_labels[[i]]
        matched_row <- fallback_lookup[
          fallback_lookup$taxon_name == label,
          ,
          drop = FALSE
        ]
        if (nrow(matched_row) > 0L) {
          new_tip_ids[[i]] <- matched_row$taxon_id_ott[[1L]]
        }

        tip_tree <- ape::read.tree(text = paste0("(", label, ");"))
        tree <- ape::bind.tree(tree, tip_tree, where = "root")
      }

      tree$tip_id_ott <- c(prior_tip_ids, new_tip_ids)
      tree
    }

    if (length(validated_ids_subset$valid) < 2L) {
      placeholder_tree <- relabel_tree_with_names(
        build_placeholder_tree(ott_ids_subset, df_counted),
        ott_ids_subset,
        df_counted
      )
      return(append_missing_labels(placeholder_tree, fallback_labels))
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
      tree_result <- relabel_tree_with_names(
        tree_result,
        validated_ids_subset$valid,
        df_counted
      )
      append_missing_labels(tree_result, fallback_labels)
    }
  }

  tree_taxa_full <- build_tree(
    tree_query_ids,
    tree_query_ids_validated,
    fallback_labels = unique(
      as.character(df_counted$taxon_name[
        !is.na(df_counted$taxon_name) & nzchar(df_counted$taxon_name)
      ])
    )
  )

  phyto_mask <- df_counted$compounds_count >= 1 |
    df_counted$ethnobotanical_count >= 1
  phyto_taxa_ids <- normalize_ott_ids(df_counted$taxon_id_ott[phyto_mask])
  tree_taxa_phyto <- build_tree(
    tree_query_ids[
      tree_query_ids %in% phyto_taxa_ids
    ],
    list(
      valid = tree_query_ids_validated$valid[
        tree_query_ids_validated$valid %in% phyto_taxa_ids
      ],
      invalid = character()
    ),
    fallback_labels = unique(
      as.character(df_counted$taxon_name[
        phyto_mask &
          !is.na(df_counted$taxon_name) &
          nzchar(df_counted$taxon_name)
      ])
    )
  )

  tree_tip_labels_full <- unique(as.character(
    tree_taxa_full$tip.label[
      !is.na(tree_taxa_full$tip.label) & nzchar(tree_taxa_full$tip.label)
    ]
  ))

  taxa_not_in_tree <- df_counted[
    !as.character(df_counted$taxon_name) %in% tree_tip_labels_full &
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
