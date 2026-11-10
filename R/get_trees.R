#' @title Get trees
#'
#' @description
#' Builds a deterministic cache key from OTT IDs, a counted data frame, and a
#' taxon name so that identical inputs map to the same cached tree result.
#'
#' @include normalize_ott_ids.R
#'
#' @param ott_ids Integer vector. OTT IDs to include in the tree.
#' @param df_counted Data.frame. Data frame containing at least `taxon_id_ott`,
#' `compounds_count`, and `ethnobotanical_count` columns.
#' @param taxon_name Character. Name of the focal taxon used to build the cache
#' key and log context.
#'
#' @return A character scalar cache key.
#'
#' @section EDAM:
#' - Topic: `http://edamontology.org/topic_3293` (Phylogenetics)
#' - Operation: `http://edamontology.org/operation_0324`
#'   (Phylogenetic analysis)
#'
#' @examples
#' # get_trees_cache_key(
#' #   c(1234, 5678),
#' #   data.frame(
#' #     taxon_id_ott = c(1234, 5678),
#' #     compounds_count = c(1, 0),
#' #     ethnobotanical_count = c(0, 1)
#' #   ),
#' #   "Orchidaceae"
#' # )
#' @keywords internal
get_trees_cache_key <- function(ott_ids, df_counted, taxon_name) {
  cache_version <- "tree-v2"
  ott_ids <- normalize_ott_ids(ott_ids)
  ott_ids <- unique(ott_ids[!is.na(ott_ids) & nzchar(ott_ids)])
  ott_ids <- sort(ott_ids)

  cols <- intersect(
    c(
      "taxon_id_ott",
      "taxon_name",
      "compounds_count",
      "ethnobotanical_count",
      "exemplar_ott_id"
    ),
    names(df_counted)
  )
  df_frame <- as.data.frame(df_counted, stringsAsFactors = FALSE)[,
    cols,
    drop = FALSE
  ]
  if ("taxon_id_ott" %in% names(df_frame)) {
    df_frame$taxon_id_ott <- normalize_ott_ids(df_frame$taxon_id_ott)
  }
  if ("exemplar_ott_id" %in% names(df_frame)) {
    df_frame$exemplar_ott_id <- normalize_ott_ids(df_frame$exemplar_ott_id)
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
    paste(df_frame$ethnobotanical_count, collapse = ","),
    if ("exemplar_ott_id" %in% names(df_frame)) {
      paste(df_frame$exemplar_ott_id, collapse = ",")
    }
  )
  paste(key_parts, collapse = "::")
}

#' @title Get cached trees
#'
#' @description
#' Returns a cached tree result when available, otherwise builds and serializes
#' the tree result to an `.rda` dataset file on disk.
#'
#' @param ott_ids Integer vector. OTT IDs to include in the tree.
#' @param df_counted Data.frame. Data frame containing at least `taxon_id_ott`,
#' `compounds_count`, and `ethnobotanical_count` columns.
#' @param taxon_name Character. Name of the focal taxon used to derive the cache
#' key.
#' @param cache_dir Character. Directory used for persistent on-disk `.rda`
#' caching (for example `data-raw`).
#'
#' @return The cached or newly built tree result list.
#'
#' @section EDAM:
#' - Topic: `http://edamontology.org/topic_3293` (Phylogenetics)
#' - Operation: `http://edamontology.org/operation_0324`
#'   (Phylogenetic analysis)
#' - Data: `http://edamontology.org/data_0872` (Phylogenetic tree)
#'
#' @keywords internal
get_trees_cached <- function(ott_ids, df_counted, taxon_name, cache_dir) {
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  cache_key <- get_trees_cache_key(ott_ids, df_counted, taxon_name)
  cache_hash <- unname(tools::md5sum(bytes = charToRaw(cache_key)))
  cache_object_name <- paste0("trees_", cache_hash)
  cache_file <- file.path(
    cache_dir,
    paste0(cache_object_name, ".rda")
  )

  if (file.exists(cache_file)) {
    cache_env <- new.env(parent = emptyenv())
    load(cache_file, envir = cache_env)
    if (exists(cache_object_name, envir = cache_env, inherits = FALSE)) {
      return(get(cache_object_name, envir = cache_env, inherits = FALSE))
    }
    if (exists("result", envir = cache_env, inherits = FALSE)) {
      return(get("result", envir = cache_env, inherits = FALSE))
    }
    stop("Cached tree file does not contain an expected object name")
  }

  result <- get_trees(
    ott_ids = ott_ids,
    df_counted = df_counted,
    taxon_name = taxon_name
  )
  cache_env <- list2env(list(), parent = emptyenv())
  assign(cache_object_name, result, envir = cache_env)
  save(list = cache_object_name, file = cache_file, envir = cache_env)
  result
}

#' @title Build trees from OTT IDs
#'
#' @description
#' Builds the full and phytochemical trees for a collection of OTT IDs via the
#' Open Tree of Life synthetic tree, resolving paraphyletic/broken genus nodes
#' via exemplar species OTT IDs when available and matching returned Newick tips
#' by their exact `_ott<ID>` suffix.
#'
#' @param ott_ids Integer vector. OTT IDs to include in the tree.
#' @param df_counted Data.frame. Data frame containing at least `taxon_id_ott`,
#' `compounds_count`, and `ethnobotanical_count` columns, and optionally
#' `exemplar_ott_id`.
#' @param taxon_name Character. Name of the focal taxon used for logging and
#' fallback naming.
#' @param batch_delay Numeric. Delay in seconds between batched Open Tree
#' requests.
#'
#' @return A list with the taxa missing from the full tree, the full tree
#' object, and the phytochemical tree object.
#'
#' @section EDAM:
#' - Topic: `http://edamontology.org/topic_3293` (Phylogenetics)
#' - Operation: `http://edamontology.org/operation_0324`
#'   (Phylogenetic analysis)
#' - Data: `http://edamontology.org/data_0872` (Phylogenetic tree)
#'
#' @keywords internal
get_trees <- function(ott_ids, df_counted, taxon_name, batch_delay = 0.2) {
  if (!(is.numeric(ott_ids) || is.character(ott_ids))) {
    stop("ott_ids must be numeric or character (integer vector of OTT IDs)")
  }
  if (!is.data.frame(df_counted)) {
    stop("df_counted must be a data.frame")
  }
  if (!is.character(taxon_name) || length(taxon_name) != 1L) {
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

  # Fast batch POST to Open Tree of Life v3 induced_subtree API
  query_otl_induced <- function(ids_int) {
    ids_int <- unique(as.integer(ids_int[!is.na(ids_int)]))
    if (length(ids_int) < 2L) {
      return(NULL)
    }
    tryCatch(
      {
        h <- curl::new_handle(timeout = 30L, followlocation = 1L)
        curl::handle_setheaders(h, "Content-Type" = "application/json")
        payload <- jsonlite::toJSON(
          list(ott_ids = ids_int),
          auto_unbox = TRUE
        )
        curl::handle_setopt(h, postfields = payload)
        resp <- curl::curl_fetch_memory(
          "https://api.opentreeoflife.org/v3/tree_of_life/induced_subtree",
          handle = h
        )
        body <- jsonlite::fromJSON(rawToChar(resp$content))
        if (resp$status_code == 400L && !is.null(body$unknown)) {
          bad_ids <- as.integer(sub("^ott", "", names(body$unknown)))
          ids_clean <- setdiff(ids_int, bad_ids)
          if (length(ids_clean) < 2L) {
            return(NULL)
          }
          payload2 <- jsonlite::toJSON(
            list(ott_ids = ids_clean),
            auto_unbox = TRUE
          )
          curl::handle_setopt(h, postfields = payload2)
          resp <- curl::curl_fetch_memory(
            "https://api.opentreeoflife.org/v3/tree_of_life/induced_subtree",
            handle = h
          )
          body <- jsonlite::fromJSON(rawToChar(resp$content))
        }
        if (resp$status_code >= 400L || is.null(body$newick)) {
          return(NULL)
        }
        tr <- ape::read.tree(text = body$newick)
        if (inherits(tr, "phylo")) {
          tr <- ape::collapse.singles(tr)
        }
        broken_ids <- if (length(body$broken) > 0L) {
          as.character(sub("^ott", "", names(body$broken)))
        } else {
          character()
        }
        list(tree = tr, broken = broken_ids)
      },
      error = function(e) {
        tr <- tryCatch(
          rotl::tol_induced_subtree(ids_int),
          error = function(e2) NULL
        )
        if (inherits(tr, "phylo")) {
          list(tree = tr, broken = character())
        } else {
          NULL
        }
      }
    )
  }

  extract_tip_ott <- function(labels) {
    labels <- gsub("^'+|'+$", "", as.character(labels))
    has_ott <- grepl("ott[0-9]+$", labels)
    ifelse(has_ott, sub(".*ott([0-9]+)$", "\\1", labels), NA_character_)
  }

  ord_lookup <- order(
    -(df_counted$compounds_count + df_counted$ethnobotanical_count),
    -(if ("species_count" %in% names(df_counted)) {
      df_counted$species_count
    } else {
      rep(0L, nrow(df_counted))
    }),
    as.character(df_counted$taxon_name)
  )
  df_sorted <- df_counted[ord_lookup, , drop = FALSE]

  df_lookup <- data.frame(
    taxon_name = as.character(df_sorted$taxon_name),
    taxon_id_ott = normalize_ott_ids(df_sorted$taxon_id_ott),
    exemplar_ott_id = if ("exemplar_ott_id" %in% names(df_sorted)) {
      normalize_ott_ids(df_sorted$exemplar_ott_id)
    } else {
      rep(NA_character_, nrow(df_sorted))
    },
    stringsAsFactors = FALSE
  )
  df_lookup <- df_lookup[
    !is.na(df_lookup$taxon_name) & nzchar(df_lookup$taxon_name),
    ,
    drop = FALSE
  ]
  df_lookup <- df_lookup[!duplicated(df_lookup$taxon_name), , drop = FALSE]
  has_ott_id <- !is.na(df_lookup$taxon_id_ott) & nzchar(df_lookup$taxon_id_ott)
  df_lookup <- df_lookup[
    !has_ott_id | !duplicated(df_lookup$taxon_id_ott),
    ,
    drop = FALSE
  ]

  build_tree <- function(
    ott_ids_subset,
    fallback_labels = NULL,
    append_unplaced = TRUE
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

    sub_map <- df_lookup[
      df_lookup$taxon_id_ott %in%
        ott_ids_subset |
        df_lookup$taxon_name %in% fallback_labels,
      ,
      drop = FALSE
    ]
    if (nrow(sub_map) == 0L) {
      sub_map <- data.frame(
        taxon_name = ott_ids_subset,
        taxon_id_ott = ott_ids_subset,
        exemplar_ott_id = NA_character_,
        stringsAsFactors = FALSE
      )
    }
    sub_map$query_ott <- sub_map$taxon_id_ott
    no_ott <- is.na(sub_map$query_ott) | !nzchar(sub_map$query_ott)
    sub_map$query_ott[no_ott] <- sub_map$exemplar_ott_id[no_ott]

    q_int <- suppressWarnings(as.integer(sub_map$query_ott))
    res1 <- query_otl_induced(q_int[!is.na(q_int)])

    for (iter in 1:2) {
      if (is.null(res1) || !inherits(res1$tree, "phylo")) {
        break
      }
      tip_otts1 <- extract_tip_ott(res1$tree$tip.label)
      missing_in_tr <- !(sub_map$query_ott %in% tip_otts1) |
        (sub_map$query_ott %in% res1$broken)
      can_swap <- missing_in_tr &
        !is.na(sub_map$exemplar_ott_id) &
        nzchar(sub_map$exemplar_ott_id) &
        sub_map$exemplar_ott_id != sub_map$query_ott
      if (!any(can_swap)) {
        break
      }
      sub_map$query_ott[can_swap] <- sub_map$exemplar_ott_id[can_swap]
      q_int2 <- suppressWarnings(as.integer(sub_map$query_ott))
      res2 <- query_otl_induced(q_int2[!is.na(q_int2)])
      if (!is.null(res2) && inherits(res2$tree, "phylo")) {
        res1 <- res2
      } else {
        break
      }
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

      prior_tip_ids <- if (is.null(tree$tip_id_ott)) {
        rep(NA_character_, length(tree$tip.label))
      } else {
        as.character(tree$tip_id_ott)
      }
      new_tip_ids <- df_lookup$taxon_id_ott[
        match(missing_labels, df_lookup$taxon_name)
      ]

      for (i in seq_along(missing_labels)) {
        label <- missing_labels[i]
        safe_lbl <- gsub(" ", "_", label, fixed = TRUE)
        tip_tree <- ape::read.tree(text = paste0("(", safe_lbl, ");"))
        tip_tree$tip.label <- label
        gen_prefix <- sub("[ _].*", "", label)
        congeneric <- which(
          sub("[ _].*", "", tree$tip.label) == gen_prefix &
            tree$tip.label != label
        )
        where_node <- if (length(congeneric) >= 2L) {
          mrca_nd <- ape::getMRCA(tree, congeneric)
          if (!is.null(mrca_nd) && !is.na(mrca_nd)) mrca_nd else "root"
        } else if (length(congeneric) == 1L) {
          parent_nd <- tree$edge[tree$edge[, 2L] == congeneric[1L], 1L]
          if (length(parent_nd) == 1L) parent_nd else "root"
        } else {
          "root"
        }
        tree <- ape::bind.tree(tree, tip_tree, where = where_node)
      }

      tree$tip_id_ott <- c(prior_tip_ids, new_tip_ids)
      tree
    }

    if (is.null(res1) || !inherits(res1$tree, "phylo")) {
      logger::log_warn(
        "Failed to build tree from OTT IDs; ",
        "falling back to a star tree containing all requested IDs."
      )
      placeholder_tree <- build_placeholder_tree(ott_ids_subset, df_counted)
      if (append_unplaced) {
        return(append_missing_labels(placeholder_tree, fallback_labels))
      }
      return(placeholder_tree)
    }

    tree_result <- res1$tree
    tip_otts <- extract_tip_ott(tree_result$tip.label)
    idx_match <- match(tip_otts, sub_map$query_ott)
    matched_names <- sub_map$taxon_name[idx_match]
    matched_orig_otts <- sub_map$taxon_id_ott[idx_match]

    # Fallback if a tip label had no _ott suffix (e.g., mocked phylo in tests)
    unresolved_tips <- is.na(matched_names)
    if (any(unresolved_tips)) {
      clean_lbl <- gsub("_", " ", sub("_ott[0-9]+$", "", tree_result$tip.label))
      idx_lbl <- match(clean_lbl[unresolved_tips], sub_map$taxon_name)
      matched_names[unresolved_tips] <- ifelse(
        !is.na(idx_lbl),
        sub_map$taxon_name[idx_lbl],
        clean_lbl[unresolved_tips]
      )
      matched_orig_otts[unresolved_tips] <- ifelse(
        !is.na(idx_lbl),
        sub_map$taxon_id_ott[idx_lbl],
        tip_otts[unresolved_tips]
      )
    }

    tree_result$tip.label <- as.character(matched_names)
    tree_result$tip_id_ott <- as.character(matched_orig_otts)

    if (append_unplaced) {
      tree_result <- append_missing_labels(tree_result, fallback_labels)
    }
    tree_result
  }

  tree_query_ids <- unique(ott_ids_normalized)
  phyto_mask <- df_counted$compounds_count >= 1 |
    df_counted$ethnobotanical_count >= 1
  phyto_taxa_ids <- normalize_ott_ids(df_counted$taxon_id_ott[phyto_mask])
  phyto_labels <- unique(
    as.character(df_counted$taxon_name[
      phyto_mask &
        !is.na(df_counted$taxon_name) &
        nzchar(df_counted$taxon_name)
    ])
  )

  # Only append unplaced active (phytochemical/ethnobotanical) taxa to full tree
  # so pruned hybrid/empty names do not create a synthetic root comb
  tree_taxa_full <- build_tree(
    tree_query_ids,
    fallback_labels = phyto_labels,
    append_unplaced = FALSE
  )

  tree_taxa_phyto <- build_tree(
    tree_query_ids[tree_query_ids %in% phyto_taxa_ids],
    fallback_labels = phyto_labels,
    append_unplaced = FALSE
  )

  tree_tip_labels_full <- unique(as.character(
    tree_taxa_full$tip.label[
      !is.na(tree_taxa_full$tip.label) & nzchar(tree_taxa_full$tip.label)
    ]
  ))

  in_tree_tips <- as.character(df_counted$taxon_name) %in%
    tree_tip_labels_full
  taxa_not_in_tree <- df_counted[!in_tree_tips & phyto_mask, , drop = FALSE]
  names(taxa_not_in_tree)[
    names(taxa_not_in_tree) == "ethnobotanical_count"
  ] <- "ethnobotanical_species_count"

  list(
    taxa_not_in_tree = taxa_not_in_tree,
    tree_taxa_full = tree_taxa_full,
    tree_taxa_phyto = tree_taxa_phyto
  )
}
