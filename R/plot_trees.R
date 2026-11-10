#' @title Plot trees
#'
#' @description
#' Plots phylogenetic trees with ethnobotanical and phytochemical overlays.
#' Uses Grafen-calibrated branch lengths with Felsenstein pruning ML ancestral
#' state reconstruction (`method = "phylo"`), highlights statistically enriched
#' ethnobotanical clades (`is_hot_node`, FDR `q < 0.05`) with red
#' diamond markers, scales tip points by `log10` species richness and
#' compound bars by `log(1 + n)`, and exports static vector (SVG/PDF),
#' interactive HTML (`ggiraph`), and iTOL v6 files.
#'
#' @param taxa_not_in_tree Data.frame. Taxa that are absent from the tree and
#' should be shown in the orphan panel.
#' @param tree_taxa_full phylo. Full tree object used for the overview plot.
#' @param tree_taxa_phyto phylo. Tree object restricted to taxa with reported
#' compounds or uses.
#' @param df_counted Data.frame. Count table containing tip labels and
#' summary counts such as `species_count` and `ethnobotanical_count`.
#' @param df_classes Data.frame. Classification table providing the compound
#' superclass values, colours, and per-taxon annotations.
#' @param offset Numeric. Optional override for the fruit-panel offset.
#' @param adjust_point Numeric. Optional override for the x-position of
#' orphan-panel points.
#' @param adjust_text Numeric. Optional override for the x-position of
#' orphan-panel labels.
#' @param adjust_margin Numeric. Optional override for the orphan-panel left
#' margin.
#' @param genus Logical. If `TRUE`, tip labels are reduced to genus-level names.
#' @param drop Logical. If `TRUE`, tips not present in `df_counted` are dropped
#' before plotting.
#' @param collapse_inactive Logical. If `TRUE` (default), collapses purely
#' unstudied clades and polytomy siblings in the rectangular `tree_taxa_full`
#' plot when the tree has more than 60 tips, while keeping the circular SI
#' overview uncollapsed.
#'
#' @return A list containing the rectangular and circular tree plots,
#' interactive `girafe` widgets (when `ggiraph` is installed), the orphan-taxon
#' plot, the orphan data frame, and the derived layout settings.
#'
#' @section EDAM:
#' - Topic: `http://edamontology.org/topic_3293` (Phylogenetics)
#' - Operation: `http://edamontology.org/operation_0567`
#'   (Phylogenetic tree visualisation)
#' - Data: `http://edamontology.org/data_0872` (Phylogenetic tree)
#'
#' @keywords internal
#'
#' @examples
#' # plot_trees(...)
plot_trees <- function(
  taxa_not_in_tree,
  tree_taxa_full,
  tree_taxa_phyto,
  df_counted,
  df_classes,
  offset = NULL,
  adjust_point = NULL,
  adjust_text = NULL,
  adjust_margin = NULL,
  genus = TRUE,
  drop = FALSE,
  collapse_inactive = TRUE
) {
  stopifnot(is.data.frame(taxa_not_in_tree))
  stopifnot(inherits(tree_taxa_full, "phylo"))
  stopifnot(inherits(tree_taxa_phyto, "phylo"))
  stopifnot(is.data.frame(df_counted))
  stopifnot(is.data.frame(df_classes))

  use_ggiraph <- requireNamespace("ggiraph", quietly = TRUE)

  label_math_10 <- function(x) {
    parse(text = paste0("10^", x))
  }

  with_fruit_geom <- function(expr, interactive = FALSE) {
    env <- globalenv()
    geom_fn <- if (interactive) {
      ggiraph::geom_col_interactive
    } else {
      ggplot2::geom_col
    }
    had_geom <- exists("geom_col", envir = env, inherits = FALSE)
    old_geom <- if (had_geom) get("geom_col", envir = env, inherits = FALSE)
    assign("geom_col", geom_fn, envir = env)
    on.exit(
      {
        if (had_geom) {
          assign("geom_col", old_geom, envir = env)
        } else if (exists("geom_col", envir = env, inherits = FALSE)) {
          rm("geom_col", envir = env)
        }
      },
      add = TRUE
    )
    force(expr)
  }

  format_tip_labels <- function(labels, genus_mode) {
    labels <- as.character(labels)
    if (genus_mode) {
      sub("_.*", "", labels)
    } else {
      labels <- sub("_ott.*", "", labels)
      gsub("_", " ", labels, fixed = TRUE)
    }
  }

  drop_tree_tips <- function(tree, tips) {
    if (length(tips) == 0L) {
      return(tree)
    }
    drop_idx <- if (is.character(tips)) {
      which(tree$tip.label %in% tips)
    } else {
      as.integer(tips)
    }
    drop_idx <- unique(drop_idx[!is.na(drop_idx) & drop_idx >= 1L])
    if (length(drop_idx) == 0L || length(drop_idx) >= length(tree$tip.label)) {
      return(tree)
    }
    if (
      !is.null(tree$tip_id_ott) &&
        length(tree$tip_id_ott) == length(tree$tip.label)
    ) {
      kept_ott <- tree$tip_id_ott[-drop_idx]
      tree <- ape::drop.tip(tree, tip = drop_idx)
      tree$tip_id_ott <- kept_ott
    } else {
      tree <- ape::drop.tip(tree, tip = drop_idx)
      tree$tip_id_ott <- NULL
    }
    tree
  }

  scale_tip_sizes <- function(counts) {
    counts <- suppressWarnings(as.numeric(counts))
    counts[!is.finite(counts) | counts < 1] <- 1
    if (length(counts) == 0L) {
      return(numeric(0))
    }
    log10(counts)
  }

  scale_fruit_values_by_taxon <- function(df_cls, df_cnt) {
    if (nrow(df_cls) == 0L) {
      df_cls$values_plot <- numeric(0)
      df_cls$bar_frac <- numeric(0)
      df_cls$display_count <- numeric(0)
      return(df_cls)
    }
    vals <- suppressWarnings(as.numeric(df_cls$values))
    vals[!is.finite(vals) | vals < 0] <- 0
    totals <- stats::ave(vals, df_cls$label, FUN = sum)
    cnt_lookup <- df_cnt[!duplicated(df_cnt$label), , drop = FALSE]
    cnt_match <- if ("compounds_count" %in% names(cnt_lookup)) {
      suppressWarnings(as.numeric(
        cnt_lookup$compounds_count[match(df_cls$label, cnt_lookup$label)]
      ))
    } else {
      rep(NA_real_, nrow(df_cls))
    }
    display_count <- ifelse(
      is.finite(cnt_match) & cnt_match > 0,
      cnt_match,
      totals
    )
    log_totals <- log1p(display_count)
    max_log <- max(log_totals, na.rm = TRUE)
    if (!is.finite(max_log) || max_log <= 0) {
      df_cls$bar_frac <- rep(0, nrow(df_cls))
      df_cls$values_plot <- rep(0, nrow(df_cls))
    } else {
      df_cls$bar_frac <- ifelse(
        totals > 0 & display_count > 0,
        log_totals / max_log,
        0
      )
      df_cls$values_plot <- ifelse(
        totals > 0,
        (vals / totals) * df_cls$bar_frac,
        0
      )
    }
    df_cls$display_count <- display_count
    df_cls
  }

  build_hierarchical_legend_spec <- function(df_cls, tip_labels = NULL) {
    sub_df <- as.data.frame(df_cls, stringsAsFactors = FALSE)
    sub_df <- sub_df[!is.na(sub_df$ids), , drop = FALSE]
    if (!is.null(tip_labels)) {
      sub_df <- sub_df[sub_df$taxon_name %in% tip_labels, , drop = FALSE]
    }
    if (nrow(sub_df) == 0L) {
      return(list(
        limits = character(0),
        values = character(0),
        labels = character(0),
        alphas = numeric(0)
      ))
    }

    sub_df$id_chr <- as.character(sub_df$ids)
    sub_df$col_chr <- as.character(sub_df$color)
    sub_df$pathway_chr <- if ("parents" %in% names(sub_df)) {
      ifelse(
        is.na(sub_df$parents) | !nzchar(as.character(sub_df$parents)),
        sub("-.*", "", sub_df$id_chr),
        as.character(sub_df$parents)
      )
    } else {
      sub("-.*", "", sub_df$id_chr)
    }
    sub_df$grp_int <- if ("group" %in% names(sub_df)) {
      suppressWarnings(as.integer(sub_df$group))
    } else {
      as.integer(factor(
        sub_df$pathway_chr,
        levels = unique(sub_df$pathway_chr)
      ))
    }
    sub_df$subgrp_int <- if ("subgroup" %in% names(sub_df)) {
      suppressWarnings(as.integer(sub_df$subgroup))
    } else {
      seq_len(nrow(sub_df))
    }

    uniq <- sub_df[
      !duplicated(sub_df$id_chr),
      c("id_chr", "col_chr", "pathway_chr", "grp_int", "subgrp_int"),
      drop = FALSE
    ]
    uniq <- uniq[order(uniq$grp_int, uniq$subgrp_int), , drop = FALSE]

    limits <- character(0)
    values <- character(0)
    labels <- character(0)

    clean_pathway_title <- function(pw) {
      pw <- gsub(" and ", " & ", pw, fixed = TRUE)
      paste0(toupper(pw), " :")
    }

    clean_sub_label <- function(id_str, pw_raw) {
      short <- sub("^[^-]+-", "", id_str)
      if (!nzchar(short)) {
        return("Unclassified")
      }
      prefix <- paste0(pw_raw, " ")
      if (startsWith(short, prefix)) {
        short <- substr(short, nchar(prefix) + 1L, nchar(short))
      }
      if (identical(short, "Other")) {
        return("Other superclass")
      }
      short
    }

    for (g in unique(uniq$grp_int)) {
      rows_g <- uniq[uniq$grp_int == g, , drop = FALSE]
      if (nrow(rows_g) > 5L) {
        rows_g <- rows_g[1:5, , drop = FALSE]
      }
      pw_raw <- rows_g$pathway_chr[1L]
      hdr_id <- paste0("__hdr_", g)
      limits <- c(limits, hdr_id)
      values <- c(values, NA_character_)
      labels <- c(labels, clean_pathway_title(pw_raw))

      for (j in seq_len(nrow(rows_g))) {
        id_j <- rows_g$id_chr[j]
        limits <- c(limits, id_j)
        values <- c(values, rows_g$col_chr[j])
        labels <- c(labels, clean_sub_label(id_j, rows_g$pathway_chr[j]))
      }

      n_pad <- 5L - nrow(rows_g)
      if (n_pad > 0L) {
        for (k in seq_len(n_pad)) {
          pad_id <- paste0("__pad_", g, "_", k)
          limits <- c(limits, pad_id)
          values <- c(values, NA_character_)
          labels <- c(labels, "")
        }
      }
    }

    names(values) <- limits
    names(labels) <- limits
    alphas <- ifelse(is.na(values), 0, 1)

    list(
      limits = limits,
      values = values,
      labels = labels,
      alphas = alphas
    )
  }

  collapse_unstudied_clades <- function(
    tree,
    df_cnt,
    df_cls,
    genus_mode = TRUE
  ) {
    n_orig <- length(tree$tip.label)
    if (n_orig <= 60L) {
      return(list(tree = tree, df_counted = df_cnt))
    }
    has_ethno <- is.finite(df_cnt$ethnobotanical_count) &
      df_cnt$ethnobotanical_count > 0
    has_comp <- if ("compounds_count" %in% names(df_cnt)) {
      is.finite(df_cnt$compounds_count) & df_cnt$compounds_count > 0
    } else {
      rep(FALSE, nrow(df_cnt))
    }
    active_cnt <- df_cnt$taxon_name[has_ethno | has_comp]
    active_cls <- df_cls$taxon_name[
      !is.na(df_cls$ids) & is.finite(df_cls$values) & df_cls$values > 0
    ]
    active_set <- unique(c(active_cnt, active_cls))
    if (length(active_set) == 0L || sum(tree$tip.label %in% active_set) == 0L) {
      return(list(tree = tree, df_counted = df_cnt))
    }

    cnt_lookup <- df_cnt |>
      tidytable::distinct(taxon_name, .keep_all = TRUE)
    sp_match <- cnt_lookup$species_count[match(
      tree$tip.label,
      cnt_lookup$taxon_name
    )]
    sp_match[!is.finite(sp_match) | sp_match < 1] <- 1

    tip_weight <- stats::setNames(rep(1L, n_orig), tree$tip.label)
    tip_species <- stats::setNames(as.numeric(sp_match), tree$tip.label)
    tip_base <- stats::setNames(tree$tip.label, tree$tip.label)
    tip_members <- stats::setNames(as.list(tree$tip.label), tree$tip.label)

    clean_node_label <- function(lbl) {
      if (is.null(lbl) || is.na(lbl) || !nzchar(lbl) || grepl("^mrca", lbl)) {
        return(NA_character_)
      }
      lbl <- sub(" ott[0-9]+$", "", lbl)
      lbl <- sub(" [(].*[)]$", "", lbl)
      if (
        lbl %in%
          c(
            "Eukaryota",
            "Euphyllophyta",
            "Mesangiospermae",
            "Pezizomycotina"
          )
      ) {
        return(NA_character_)
      }
      lbl
    }

    format_collapsed_name <- function(base_nm, total_w, existing) {
      extra <- max(1L, as.integer(total_w) - 1L)
      cand <- sprintf("%s (+%d)", base_nm, extra)
      if (cand %in% existing) {
        cand <- make.unique(c(existing, cand))[length(existing) + 1L]
      }
      cand
    }

    postorder_subtrees <- function(tr) {
      n_t <- length(tr$tip.label)
      n_i <- tr$Nnode
      if (n_i <= 0L) {
        return(list())
      }
      tr_p <- tryCatch(
        ape::reorder.phylo(tr, "postorder"),
        error = function(e) tr
      )
      desc_list <- vector("list", n_t + n_i)
      for (k in seq_len(n_t)) {
        desc_list[[k]] <- k
      }
      e_mat <- tr_p$edge
      for (u in unique(e_mat[, 1L])) {
        ch <- e_mat[e_mat[, 1L] == u, 2L]
        desc_list[[u]] <- unlist(desc_list[ch], use.names = FALSE)
      }
      desc_list[(n_t + 1L):(n_t + n_i)]
    }

    repeat {
      n_before <- length(tree$tip.label)
      if (n_before <= 3L) {
        break
      }

      parts <- postorder_subtrees(tree)
      n_tips <- length(tree$tip.label)
      is_act <- tip_base[tree$tip.label] %in% active_set
      inact_clade <- vapply(
        parts,
        function(idx) !any(is_act[idx]) && length(idx) > 1L,
        logical(1L)
      )

      if (any(inact_clade)) {
        inodes <- n_tips + which(inact_clade)
        p_of <- tree$edge[match(inodes, tree$edge[, 2L]), 1L]
        max_inodes <- inodes[!(p_of %in% inodes)]
        to_drop <- integer(0)

        for (nd in max_inodes) {
          desc <- parts[[nd - n_tips]]
          lbls <- tree$tip.label[desc]
          best_local <- which.max(tip_species[lbls])
          rep_idx <- desc[best_local]
          rep_lbl <- lbls[best_local]
          total_w <- sum(tip_weight[lbls])
          total_sp <- sum(tip_species[lbls])
          members <- unique(unlist(tip_members[lbls], use.names = FALSE))
          nl <- if (!is.null(tree$node.label)) {
            clean_node_label(tree$node.label[nd - n_tips])
          } else {
            NA_character_
          }
          base_nm <- if (!is.na(nl) && !(nl %in% tip_base[tree$tip.label])) {
            nl
          } else {
            tip_base[rep_lbl]
          }
          new_lbl <- format_collapsed_name(
            base_nm,
            total_w,
            tree$tip.label[-rep_idx]
          )
          tree$tip.label[rep_idx] <- new_lbl
          if (
            !is.null(tree$tip_id_ott) &&
              length(tree$tip_id_ott) >= rep_idx
          ) {
            tree$tip_id_ott[rep_idx] <- NA_character_
          }
          tip_weight[[new_lbl]] <- total_w
          tip_species[[new_lbl]] <- total_sp
          tip_base[[new_lbl]] <- base_nm
          tip_members[[new_lbl]] <- members
          to_drop <- c(to_drop, desc[-best_local])
        }
        tree <- drop_tree_tips(tree, to_drop)
      }

      n_tips <- length(tree$tip.label)
      is_act <- tip_base[tree$tip.label] %in% active_set
      edges <- tree$edge
      dir_inact <- edges[
        edges[, 2L] <= n_tips & !is_act[edges[, 2L]],
        ,
        drop = FALSE
      ]
      by_p <- split(dir_inact[, 2L], dir_inact[, 1L])
      to_drop2 <- integer(0)

      for (sibs in by_p) {
        if (length(sibs) > 1L) {
          lbls <- tree$tip.label[sibs]
          best_local <- which.max(tip_species[lbls])
          rep_idx <- sibs[best_local]
          rep_lbl <- lbls[best_local]
          total_w <- sum(tip_weight[lbls])
          total_sp <- sum(tip_species[lbls])
          members <- unique(unlist(tip_members[lbls], use.names = FALSE))
          base_nm <- tip_base[rep_lbl]
          new_lbl <- format_collapsed_name(
            base_nm,
            total_w,
            tree$tip.label[-rep_idx]
          )
          tree$tip.label[rep_idx] <- new_lbl
          if (
            !is.null(tree$tip_id_ott) &&
              length(tree$tip_id_ott) >= rep_idx
          ) {
            tree$tip_id_ott[rep_idx] <- NA_character_
          }
          tip_weight[[new_lbl]] <- total_w
          tip_species[[new_lbl]] <- total_sp
          tip_base[[new_lbl]] <- base_nm
          tip_members[[new_lbl]] <- members
          to_drop2 <- c(to_drop2, sibs[-best_local])
        }
      }
      if (length(to_drop2) > 0L) {
        tree <- drop_tree_tips(tree, to_drop2)
      }
      if (length(tree$tip.label) == n_before) {
        break
      }
    }

    kept_labels <- tree$tip.label
    collapsed_summary <- data.frame(
      taxon_name = kept_labels,
      plot_label = kept_labels,
      label = kept_labels,
      species_count = as.numeric(tip_species[kept_labels]),
      ethnobotanical_count = ifelse(
        kept_labels %in% df_cnt$taxon_name,
        df_cnt$ethnobotanical_count[match(kept_labels, df_cnt$taxon_name)],
        0
      ),
      tip_weight = as.numeric(tip_weight[kept_labels]),
      is_collapsed = as.integer(tip_weight[kept_labels]) > 1L,
      collapsed_members = vapply(
        kept_labels,
        function(k) {
          m <- tip_members[[k]]
          if (length(m) <= 1L) {
            ""
          } else if (length(m) <= 10L) {
            paste(m, collapse = ", ")
          } else {
            paste0(
              paste(m[1:10], collapse = ", "),
              ", ... (+",
              length(m) - 10L,
              " more)"
            )
          }
        },
        character(1L)
      ),
      stringsAsFactors = FALSE
    )

    list(tree = tree, df_counted = collapsed_summary)
  }

  tree_taxa_full$tip.label <- format_tip_labels(tree_taxa_full$tip.label, genus)
  tree_taxa_phyto$tip.label <- format_tip_labels(
    tree_taxa_phyto$tip.label,
    genus
  )

  df_counted_plot <- as.data.frame(df_counted, stringsAsFactors = FALSE) |>
    tidytable::mutate(
      taxon_name_original = taxon_name,
      plot_label = format_tip_labels(taxon_name, genus),
      label = plot_label,
      taxon_name = plot_label,
      species_count = pmax(1, as.numeric(species_count), na.rm = TRUE),
      species_count = ifelse(is.finite(species_count), species_count, 1),
      ethnobotanical_count = pmax(
        0,
        as.numeric(ethnobotanical_count),
        na.rm = TRUE
      ),
      ethnobotanical_count = ifelse(
        is.finite(ethnobotanical_count),
        ethnobotanical_count,
        0
      ),
      tip_weight = 1,
      is_collapsed = FALSE,
      collapsed_members = ""
    )

  df_classes_plot <- as.data.frame(df_classes, stringsAsFactors = FALSE) |>
    tidytable::mutate(
      taxon_name_original = taxon_name,
      plot_label = format_tip_labels(taxon_name, genus),
      label = plot_label,
      taxon_name = plot_label,
      values = suppressWarnings(as.numeric(values)),
      values = ifelse(is.finite(values), values, 0)
    ) |>
    scale_fruit_values_by_taxon(df_counted_plot) |>
    tidytable::mutate(
      bar_tooltip = sprintf(
        paste0(
          "<b>%s</b><br/>Class: %s<br/>",
          "Class count: %s<br/>Taxon total compounds: %s"
        ),
        label,
        ifelse(is.na(ids), "Unclassified", as.character(ids)),
        format(round(values, 1), trim = TRUE),
        format(round(display_count, 1), trim = TRUE)
      )
    )

  if (drop) {
    labelz <- setdiff(tree_taxa_full$tip.label, df_counted_plot$taxon_name)
    labelz_phyto <- setdiff(
      tree_taxa_phyto$tip.label,
      df_counted_plot$taxon_name
    )
    tree_taxa_full <- drop_tree_tips(tree_taxa_full, labelz)
    tree_taxa_phyto <- drop_tree_tips(tree_taxa_phyto, labelz_phyto)
  }

  tree_taxa_full_raw <- tree_taxa_full
  df_counted_full <- df_counted_plot
  if (isTRUE(collapse_inactive)) {
    collapsed_res <- collapse_unstudied_clades(
      tree = tree_taxa_full,
      df_cnt = df_counted_plot,
      df_cls = df_classes_plot,
      genus_mode = genus
    )
    tree_taxa_full <- collapsed_res$tree
    df_counted_full <- collapsed_res$df_counted
  }

  visual_str_width <- function(s) {
    vapply(
      as.character(s),
      function(x) {
        if (is.na(x) || !nzchar(x)) {
          return(0)
        }
        ch <- strsplit(x, "")[[1L]]
        sum(
          ifelse(
            ch %in%
              c(
                "i",
                "I",
                "l",
                "j",
                "t",
                "f",
                "r",
                "1",
                "(",
                ")",
                " ",
                ".",
                "-"
              ),
            0.52,
            ifelse(
              ch %in% c("M", "W", "m", "w", "Q", "G", "O", "D", "C", "U", "+"),
              1.32,
              1.0
            )
          )
        )
      },
      numeric(1L)
    )
  }

  estimate_layout <- function(
    tree,
    preferred_offset = NULL,
    genus_mode = TRUE
  ) {
    tip_labels <- tree$tip.label[
      !is.na(tree$tip.label) & nzchar(tree$tip.label)
    ]
    n_tips <- max(1L, length(tip_labels))
    max_label_width <- max(nchar(tip_labels), 0L, na.rm = TRUE)
    max_visual_width <- max(visual_str_width(tip_labels), 1, na.rm = TRUE)

    p_tmp <- ggtree::ggtree(tree, layout = "rectangular")
    tree_span <- max(1, max(p_tmp$data$x, na.rm = TRUE))

    label_size <- if (n_tips <= 160L) {
      2.9
    } else if (n_tips <= 320L) {
      2.5
    } else {
      2.2
    }

    tip_label_offset <- 0.008 * tree_span

    fruit_width <- if (genus_mode) 0.36 else 0.34
    export_width <- max(
      18,
      min(24, 15 + 0.15 * max_label_width)
    )
    export_height <- max(
      14,
      min(64, 5.2 + 0.145 * n_tips)
    )

    panel_w_in <- max(10, (export_width - 0.6) / 1.10)
    label_w_in <- max_visual_width * label_size * 0.0218 + 0.14
    rel_label_frac <- min(0.24, label_w_in / panel_w_in)
    auto_offset <- (1 + fruit_width + 0.03) *
      rel_label_frac /
      (1 - rel_label_frac)

    fruit_offset <- preferred_offset %||%
      max(0.092, min(0.20, auto_offset))

    orphan_point_x <- adjust_point %||% -0.2
    orphan_text_x <- adjust_text %||% -0.05

    list(
      fruit_offset = fruit_offset,
      fruit_width = fruit_width,
      tip_label_offset = tip_label_offset,
      tree_span = tree_span,
      label_size = label_size,
      max_visual_width = max_visual_width,
      orphan_point_x = orphan_point_x,
      orphan_text_x = orphan_text_x,
      export_width = export_width,
      export_height = export_height
    )
  }

  max_display_compounds <- if (nrow(df_classes_plot) > 0L) {
    max(df_classes_plot$display_count, 0, na.rm = TRUE)
  } else {
    0
  }

  compound_summary_by_label <- if (nrow(df_classes_plot) > 0L) {
    df_classes_plot |>
      tidytable::filter(!is.na(ids) & values > 0) |>
      tidytable::summarise(
        tip_bar_frac = max(bar_frac, 0, na.rm = TRUE),
        bar_count_num = max(display_count, 0, na.rm = TRUE),
        compound_summary = paste0(
          "Reported compounds: ",
          format(round(max(display_count, 0, na.rm = TRUE), 1), trim = TRUE),
          " across ",
          length(unique(ids)),
          " superclasses"
        ),
        .by = label
      )
  } else {
    data.frame(
      label = character(0),
      tip_bar_frac = numeric(0),
      bar_count_num = numeric(0),
      compound_summary = character(0),
      stringsAsFactors = FALSE
    )
  }

  enrich_tree_data <- function(tree_df, df_cnt) {
    meta_cols <- intersect(
      c(
        "label",
        "species_count",
        "ethnobotanical_count",
        "tip_weight",
        "is_collapsed",
        "collapsed_members"
      ),
      names(df_cnt)
    )
    res <- tree_df |>
      tidytable::left_join(
        as.data.frame(df_cnt)[, meta_cols, drop = FALSE] |>
          tidytable::distinct(label, .keep_all = TRUE),
        by = "label"
      ) |>
      tidytable::left_join(compound_summary_by_label, by = "label") |>
      tidytable::distinct(node, .keep_all = TRUE) |>
      tidytable::mutate(
        species_count = pmax(1, as.numeric(species_count), na.rm = TRUE),
        species_count = ifelse(is.finite(species_count), species_count, 1),
        ethnobotanical_count = ifelse(
          is.finite(ethnobotanical_count),
          ethnobotanical_count,
          0
        ),
        tip_bar_frac = ifelse(
          is.finite(tip_bar_frac) & tip_bar_frac > 0,
          tip_bar_frac,
          0
        ),
        bar_count_label = ifelse(
          tip_bar_frac > 0 & is.finite(bar_count_num) & bar_count_num > 0,
          as.character(as.integer(round(bar_count_num))),
          ""
        ),
        is_collapsed = ifelse(is.na(is_collapsed), FALSE, is_collapsed),
        tip_shape = ifelse(is_collapsed, 17L, 16L),
        label_face = ifelse(is_collapsed, "plain", "italic"),
        tip_size = if (genus) {
          scale_tip_sizes(species_count)
        } else {
          rep(0.4, length(species_count))
        },
        ethnobotanical_species_average = ifelse(
          is.finite(ethnobotanical_species_average) &
            ethnobotanical_species_average > 0,
          ethnobotanical_species_average,
          NA_real_
        ),
        is_hot_node = !is_tip &
          !is.na(hot_node_qval) &
          is.finite(hot_node_qval) &
          hot_node_qval < 0.05,
        tooltip_text = ifelse(
          is_tip,
          sprintf(
            paste0(
              "<b>%s</b>%s<br/>Known species: %d<br/>",
              "Ethnobotanical species: %d (EB rate: %.1f%%)%s"
            ),
            ifelse(is.na(label), "", label),
            ifelse(
              is_collapsed & nzchar(collapsed_members),
              paste0("<br/>Collapsed: ", collapsed_members),
              ""
            ),
            as.integer(round(species_count)),
            as.integer(round(ethnobotanical_count)),
            ifelse(is.finite(eb_rate), 100 * eb_rate, 0),
            ifelse(
              !is.na(compound_summary) & nzchar(compound_summary),
              paste0("<br/>", compound_summary),
              ""
            )
          ),
          sprintf(
            paste0(
              "<b>Clade %s</b><br/>Felsenstein ML ASR: %.2f<br/>",
              "Empirical Bayes rate: %.1f%%<br/>Hot-clade FDR q = %.3g"
            ),
            ifelse(is.na(label) | !nzchar(label), paste0("#", node), label),
            ifelse(is.finite(phylo_asr), phylo_asr, 0),
            ifelse(is.finite(eb_rate), 100 * eb_rate, 0),
            ifelse(is.finite(hot_node_qval), hot_node_qval, 1)
          )
        )
      )
    res
  }

  logger::log_trace(
    "Calculating ethnobotanical use at different levels of the tree..."
  )
  raw_bv_full <- calculate_branches_values(
    tree = tree_taxa_full,
    df = df_counted_full,
    variable = "ethnobotanical_count",
    name = "ethnobotanical_species_average",
    method = "phylo"
  )
  tree_data <- enrich_tree_data(raw_bv_full, df_counted_full)

  raw_bv_uncollapsed <- calculate_branches_values(
    tree = tree_taxa_full_raw,
    df = df_counted_plot,
    variable = "ethnobotanical_count",
    name = "ethnobotanical_species_average",
    method = "phylo"
  )
  phylo_signal_full <- attr(raw_bv_uncollapsed, "phylo_signal")
  tree_data_raw <- enrich_tree_data(raw_bv_uncollapsed, df_counted_plot)

  raw_bv_phyto <- calculate_branches_values(
    tree = tree_taxa_phyto,
    df = df_counted_plot,
    variable = "ethnobotanical_count",
    name = "ethnobotanical_species_average",
    method = "phylo"
  )
  phylo_signal_phyto <- attr(raw_bv_phyto, "phylo_signal")
  tree_data_phyto <- enrich_tree_data(raw_bv_phyto, df_counted_plot)

  layout_full <- estimate_layout(tree_taxa_full, offset, genus_mode = genus)
  layout_phyto <- estimate_layout(tree_taxa_phyto, offset, genus_mode = genus)
  layout_circular <- list(
    export_width = 28,
    export_height = 30
  )

  logger::log_trace("Plotting ...")

  base_tree_theme <- ggplot2::theme(
    legend.text = ggplot2::element_text(colour = "#1e293b", size = 8.5),
    legend.title = ggplot2::element_text(
      colour = "#0f172a",
      face = "bold",
      size = 9.5
    ),
    legend.key = ggplot2::element_blank(),
    legend.key.size = grid::unit(0.38, "cm"),
    legend.spacing.x = grid::unit(0.18, "cm"),
    legend.spacing.y = grid::unit(0.16, "cm"),
    legend.box.background = ggplot2::element_rect(
      fill = "#f8fafc",
      colour = "#e2e8f0",
      linewidth = 0.4
    ),
    legend.box.margin = ggplot2::margin(8, 14, 8, 14),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "vertical",
    plot.caption = ggplot2::element_text(color = "#475569"),
    plot.margin = grid::unit(c(26, 28, 16, 14), "pt"),
    text = ggplot2::element_text(color = "#1e293b")
  )

  build_tree_bars_plot <- function(tree, tr_data, layout_cfg) {
    max_x <- layout_cfg$tree_span
    label_x <- max_x + layout_cfg$tip_label_offset
    has_collapsed <- any(tr_data$is_collapsed, na.rm = TRUE)

    p_base <- ggtree::ggtree(
      tree,
      layout = "rectangular",
      linewidth = 0.45,
      mapping = ggplot2::aes(color = log10(ethnobotanical_species_average))
    ) |>
      ggtree::`%<+%`(tr_data) +
      khroma::scale_color_batlow(
        name = "Known ethnobotanical species",
        range = c(0, 0.80),
        na.value = "#64748b",
        labels = label_math_10
      ) +
      ggplot2::geom_segment(
        data = function(d) d[d$isTip & d$x < max_x, , drop = FALSE],
        mapping = ggplot2::aes(
          x = x,
          xend = max_x,
          y = y,
          yend = y
        ),
        linetype = "dotted",
        linewidth = 0.25,
        inherit.aes = FALSE,
        color = "#9ca3af"
      )

    if (use_ggiraph) {
      p_base <- p_base +
        ggiraph::geom_point_interactive(
          data = function(d) {
            d[!d$isTip & !is.na(d$is_hot_node) & d$is_hot_node, , drop = FALSE]
          },
          mapping = ggplot2::aes(
            x = x,
            y = y,
            tooltip = tooltip_text,
            data_id = as.character(node)
          ),
          shape = 18L,
          size = 3.2,
          color = "#dc2626",
          inherit.aes = FALSE
        ) +
        ggiraph::geom_point_interactive(
          data = function(d) d[d$isTip, , drop = FALSE],
          mapping = ggplot2::aes(
            x = x,
            y = y,
            size = tip_size,
            shape = tip_shape,
            tooltip = tooltip_text,
            data_id = label
          )
        ) +
        ggiraph::geom_text_interactive(
          data = function(d) d[d$isTip, , drop = FALSE],
          mapping = ggplot2::aes(
            x = label_x,
            y = y,
            label = label,
            fontface = label_face,
            tooltip = tooltip_text,
            data_id = label
          ),
          hjust = 0,
          size = layout_cfg$label_size
        )
    } else {
      p_base <- p_base +
        ggplot2::geom_point(
          data = function(d) {
            d[!d$isTip & !is.na(d$is_hot_node) & d$is_hot_node, , drop = FALSE]
          },
          mapping = ggplot2::aes(x = x, y = y),
          shape = 18L,
          size = 3.2,
          color = "#dc2626",
          inherit.aes = FALSE
        ) +
        ggtree::geom_tippoint(
          mapping = ggplot2::aes(size = tip_size, shape = tip_shape)
        ) +
        ggtree::geom_tiplab(
          mapping = ggplot2::aes(fontface = label_face),
          size = layout_cfg$label_size,
          align = TRUE,
          hjust = 0,
          offset = layout_cfg$tip_label_offset,
          linesize = 0.25
        )
    }

    p_base <- p_base +
      ggplot2::scale_shape_identity(
        name = "Tip representation",
        breaks = c(16L, 17L),
        labels = c("Single taxon", "Collapsed unstudied clade (+N)"),
        guide = if (has_collapsed) "legend" else "none"
      ) +
      ggplot2::scale_size(
        name = "Known species",
        labels = label_math_10,
        range = c(1.6, 5.4),
        breaks = c(0, 1, 2, 3),
        limits = c(0, 4),
        guide = if (genus) "legend" else "none"
      ) +
      base_tree_theme

    leg_spec <- build_hierarchical_legend_spec(df_classes_plot, tree$tip.label)
    df_cls_sub <- df_classes_plot
    if (length(leg_spec$limits) > 0L) {
      df_cls_sub$ids <- factor(
        as.character(df_cls_sub$ids),
        levels = leg_spec$limits
      )
    }

    fruit_mapping <- if (use_ggiraph) {
      ggplot2::aes(
        y = label,
        x = values_plot,
        fill = ids,
        tooltip = bar_tooltip,
        data_id = label
      )
    } else {
      ggplot2::aes(y = label, x = values_plot, fill = ids)
    }

    bar_x0 <- max_x * (1 + layout_cfg$fruit_offset)
    bar_span <- max_x * layout_cfg$fruit_width
    n_tips_tree <- max(1L, length(tree$tip.label))
    max_log_val <- log1p(max(1, max_display_compounds))
    raw_ticks <- c(0, 1, 10, 50, 100, 250, 500, 1000)
    keep_ticks <- raw_ticks[raw_ticks <= max(1, max_display_compounds)]
    if (
      length(keep_ticks) > 0L &&
        max_display_compounds > max(keep_ticks) * 1.15
    ) {
      keep_ticks <- c(keep_ticks, as.integer(round(max_display_compounds)))
    }
    tick_x_vals <- bar_x0 + bar_span * (log1p(keep_ticks) / max_log_val)
    tick_df <- data.frame(
      tick_x = tick_x_vals,
      tick_y = rep(n_tips_tree + 0.9, length(keep_ticks)),
      tick_label = as.character(keep_ticks),
      stringsAsFactors = FALSE
    )
    tick_bottom_df <- data.frame(
      tick_x = tick_x_vals,
      tick_y = rep(0.25, length(keep_ticks)),
      tick_label = as.character(keep_ticks),
      stringsAsFactors = FALSE
    )
    pos_tick_x <- tick_x_vals[keep_ticks > 0]
    grid_df <- if (length(pos_tick_x) > 0L) {
      data.frame(
        tick_x = pos_tick_x,
        y = rep(0.45, length(pos_tick_x)),
        tick_y = rep(n_tips_tree + 0.65, length(pos_tick_x)),
        stringsAsFactors = FALSE
      )
    } else {
      data.frame(
        tick_x = numeric(0),
        y = numeric(0),
        tick_y = numeric(0),
        stringsAsFactors = FALSE
      )
    }

    tip_rows <- p_base$data[
      p_base$data$isTip &
        !is.na(p_base$data$tip_bar_frac) &
        p_base$data$tip_bar_frac > 0,
      ,
      drop = FALSE
    ]
    leader_df <- if (nrow(tip_rows) > 0L) {
      vw_i <- visual_str_width(tip_rows$label)
      max_vw <- max(1, layout_cfg$max_visual_width)
      label_col_w <- max(0.01 * max_x, bar_x0 - label_x)
      x_start <- label_x +
        label_col_w * pmin(0.95, (vw_i / max_vw) * 0.90 + 0.04)
      x_end <- rep(bar_x0 - 0.005 * max_x, nrow(tip_rows))
      keep_ldr <- x_end > (x_start + 0.006 * max_x)
      data.frame(
        x = x_start[keep_ldr],
        tick_x = x_end[keep_ldr],
        y = tip_rows$y[keep_ldr],
        stringsAsFactors = FALSE
      )
    } else {
      data.frame(
        x = numeric(0),
        tick_x = numeric(0),
        y = numeric(0),
        stringsAsFactors = FALSE
      )
    }

    taxon_hdr_label <- if (has_collapsed) {
      "Taxon (+N collapsed)"
    } else if (genus) {
      "Genus"
    } else {
      "Species / Taxon"
    }

    p_out <- with_fruit_geom(
      p_base +
        ggplot2::geom_segment(
          data = leader_df,
          mapping = ggplot2::aes(
            x = x,
            xend = tick_x,
            y = y,
            yend = y
          ),
          linetype = "dotted",
          linewidth = 0.25,
          color = "#cbd5e1",
          inherit.aes = FALSE
        ) +
        ggtreeExtra::geom_fruit(
          data = df_cls_sub,
          geom = "geom_col",
          mapping = fruit_mapping,
          offset = layout_cfg$fruit_offset,
          pwidth = layout_cfg$fruit_width,
          axis.params = list(axis = "none"),
          show.legend = c(fill = TRUE, size = FALSE, colour = FALSE)
        ) +
        ggplot2::geom_segment(
          data = grid_df,
          mapping = ggplot2::aes(
            x = tick_x,
            xend = tick_x,
            y = y,
            yend = tick_y
          ),
          linetype = "dashed",
          linewidth = 0.22,
          color = "#cbd5e1",
          alpha = 0.55,
          inherit.aes = FALSE
        ) +
        ggplot2::annotate(
          "segment",
          x = bar_x0,
          xend = bar_x0,
          y = 0.25,
          yend = n_tips_tree + 0.90,
          color = "#64748b",
          linewidth = 0.35
        ) +
        ggplot2::geom_text(
          data = function(d) {
            d[d$isTip & nzchar(d$bar_count_label), , drop = FALSE]
          },
          mapping = ggplot2::aes(
            x = bar_x0 + bar_span * tip_bar_frac + 0.008 * max_x,
            y = y,
            label = bar_count_label
          ),
          hjust = 0,
          size = layout_cfg$label_size * 0.80,
          color = "#334155",
          inherit.aes = FALSE
        ) +
        ggplot2::annotate(
          "segment",
          x = 0,
          xend = max_x * 0.96,
          y = n_tips_tree + 0.85,
          yend = n_tips_tree + 0.85,
          color = "#cbd5e1",
          linewidth = 0.30
        ) +
        ggplot2::annotate(
          "segment",
          x = label_x,
          xend = bar_x0 - 0.01 * max_x,
          y = n_tips_tree + 0.85,
          yend = n_tips_tree + 0.85,
          color = "#cbd5e1",
          linewidth = 0.30
        ) +
        ggplot2::annotate(
          "segment",
          x = bar_x0,
          xend = bar_x0 + bar_span,
          y = n_tips_tree + 0.85,
          yend = n_tips_tree + 0.85,
          color = "#64748b",
          linewidth = 0.35
        ) +
        ggplot2::annotate(
          "segment",
          x = bar_x0,
          xend = bar_x0 + bar_span,
          y = 0.25,
          yend = 0.25,
          color = "#64748b",
          linewidth = 0.35
        ) +
        ggplot2::geom_segment(
          data = tick_df,
          mapping = ggplot2::aes(
            x = tick_x,
            xend = tick_x,
            y = tick_y - 0.22,
            yend = tick_y + 0.22
          ),
          color = "#64748b",
          linewidth = 0.35,
          inherit.aes = FALSE
        ) +
        ggplot2::geom_text(
          data = tick_df,
          mapping = ggplot2::aes(
            x = tick_x,
            y = tick_y + 0.45,
            label = tick_label
          ),
          size = layout_cfg$label_size * 0.76,
          color = "#475569",
          vjust = 0,
          inherit.aes = FALSE
        ) +
        ggplot2::geom_segment(
          data = tick_bottom_df,
          mapping = ggplot2::aes(
            x = tick_x,
            xend = tick_x,
            y = tick_y - 0.22,
            yend = tick_y + 0.22
          ),
          color = "#64748b",
          linewidth = 0.35,
          inherit.aes = FALSE
        ) +
        ggplot2::geom_text(
          data = tick_bottom_df,
          mapping = ggplot2::aes(
            x = tick_x,
            y = tick_y - 0.45,
            label = tick_label
          ),
          size = layout_cfg$label_size * 0.76,
          color = "#475569",
          vjust = 1,
          inherit.aes = FALSE
        ) +
        ggplot2::annotate(
          "text",
          x = max_x * 0.48,
          y = n_tips_tree + 2.45,
          label = "Phylogenetic topology (Open Tree of Life)",
          size = layout_cfg$label_size * 0.86,
          fontface = "bold",
          color = "#1e293b",
          vjust = 0
        ) +
        ggplot2::annotate(
          "text",
          x = label_x,
          y = n_tips_tree + 2.45,
          label = taxon_hdr_label,
          size = layout_cfg$label_size * 0.86,
          fontface = "bold",
          color = "#1e293b",
          hjust = 0,
          vjust = 0
        ) +
        ggplot2::annotate(
          "text",
          x = bar_x0 + bar_span / 2,
          y = n_tips_tree + 2.45,
          label = "Reported compounds (log scale)",
          size = layout_cfg$label_size * 0.86,
          fontface = "bold",
          color = "#1e293b",
          vjust = 0
        ) +
        ggplot2::scale_fill_manual(
          name = "Compound pathway & superclass (5 shades per pathway row)",
          values = leg_spec$values,
          labels = leg_spec$labels,
          limits = leg_spec$limits,
          drop = FALSE,
          na.value = "transparent"
        ) +
        ggplot2::guides(
          fill = ggplot2::guide_legend(
            ncol = 6L,
            byrow = TRUE,
            title.position = "top",
            order = 1L,
            override.aes = list(
              alpha = leg_spec$alphas,
              tooltip = NULL,
              data_id = NULL
            )
          ),
          colour = ggplot2::guide_colorbar(
            title.position = "top",
            title.hjust = 0.5,
            barwidth = grid::unit(5.5, "cm"),
            barheight = grid::unit(0.32, "cm"),
            order = 2L
          ),
          size = ggplot2::guide_legend(
            title.position = "top",
            title.hjust = 0.5,
            nrow = 1L,
            override.aes = list(shape = 16L, colour = "#334155"),
            order = 3L
          ),
          shape = if (has_collapsed) {
            ggplot2::guide_legend(
              title.position = "top",
              title.hjust = 0.5,
              nrow = 1L,
              override.aes = list(size = 3.2, colour = "#334155"),
              order = 4L
            )
          } else {
            "none"
          }
        ) +
        ggplot2::coord_cartesian(clip = "off"),
      interactive = use_ggiraph
    )
    p_out
  }

  build_circular_si_plot <- function(tree, tr_data) {
    n_tips <- max(1L, length(tree$tip.label))
    lbl_size <- if (n_tips > 600L) {
      0.82
    } else if (n_tips > 250L) {
      1.2
    } else {
      1.8
    }

    p_circ <- suppressMessages(
      ggtree::ggtree(
        tree,
        layout = "fan",
        open.angle = 12,
        root.position = 8,
        linewidth = 0.25,
        mapping = ggplot2::aes(color = log10(ethnobotanical_species_average))
      ) |>
        ggtree::`%<+%`(tr_data)
    ) +
      khroma::scale_color_batlow(
        name = "Known ethnobotanical species",
        range = c(0, 0.80),
        na.value = "#64748b",
        labels = label_math_10
      )

    if (use_ggiraph) {
      p_circ <- p_circ +
        ggiraph::geom_point_interactive(
          data = function(d) d[d$isTip, , drop = FALSE],
          mapping = ggplot2::aes(
            x = x,
            y = y,
            size = tip_size,
            tooltip = tooltip_text,
            data_id = label
          ),
          alpha = 0.85
        )
    } else {
      p_circ <- p_circ +
        ggtree::geom_tippoint(
          mapping = ggplot2::aes(size = tip_size),
          alpha = 0.85
        )
    }

    p_circ <- p_circ +
      ggtree::geom_tiplab(
        size = lbl_size,
        fontface = "italic",
        offset = 0.18
      ) +
      ggplot2::scale_size(
        name = "Known species",
        labels = label_math_10,
        range = c(0.6, 2.8),
        breaks = c(0, 1, 2, 3),
        limits = c(0, 4)
      ) +
      base_tree_theme

    leg_spec <- build_hierarchical_legend_spec(df_classes_plot, tree$tip.label)
    df_cls_sub <- df_classes_plot
    if (length(leg_spec$limits) > 0L) {
      df_cls_sub$ids <- factor(
        as.character(df_cls_sub$ids),
        levels = leg_spec$limits
      )
    }

    fruit_mapping <- if (use_ggiraph) {
      ggplot2::aes(
        y = label,
        x = values_plot,
        fill = ids,
        tooltip = bar_tooltip,
        data_id = label
      )
    } else {
      ggplot2::aes(y = label, x = values_plot, fill = ids)
    }

    max_r <- max(p_circ$data$x, na.rm = TRUE)
    circ_bar_r0 <- max_r * (1 + 0.026)
    circ_bar_span <- max_r * 0.20
    max_log_c <- log1p(max(1, max_display_compounds))
    c_ticks <- c(0, 10, 100, 500)
    c_ticks <- c_ticks[c_ticks <= max(1, max_display_compounds)]
    circ_tick_df <- data.frame(
      tick_x = circ_bar_r0 + circ_bar_span * (log1p(c_ticks) / max_log_c),
      tick_y = rep(n_tips + max(0.15, n_tips * 0.008), length(c_ticks)),
      tick_label = as.character(c_ticks),
      stringsAsFactors = FALSE
    )

    suppressWarnings(
      suppressMessages(
        with_fruit_geom(
          p_circ +
            ggtreeExtra::geom_fruit(
              data = df_cls_sub,
              geom = "geom_col",
              mapping = fruit_mapping,
              offset = 0.026,
              pwidth = 0.20,
              axis.params = list(axis = "none"),
              grid.params = list(
                linetype = "dashed",
                color = "#cbd5e1",
                size = 0.25
              ),
              show.legend = c(fill = TRUE, size = FALSE, colour = FALSE)
            ) +
            ggplot2::geom_text(
              data = circ_tick_df,
              mapping = ggplot2::aes(
                x = tick_x,
                y = tick_y,
                label = tick_label
              ),
              size = 2.2,
              fontface = "bold",
              color = "#475569",
              na.rm = TRUE,
              inherit.aes = FALSE
            ) +
            ggplot2::scale_fill_manual(
              name = "Compound pathway & superclass (5 shades per pathway row)",
              values = leg_spec$values,
              labels = leg_spec$labels,
              limits = leg_spec$limits,
              drop = FALSE,
              na.value = "transparent"
            ) +
            ggplot2::guides(
              fill = ggplot2::guide_legend(
                ncol = 6L,
                byrow = TRUE,
                title.position = "top",
                order = 1L,
                override.aes = list(
                  alpha = leg_spec$alphas,
                  tooltip = NULL,
                  data_id = NULL
                )
              ),
              colour = ggplot2::guide_colorbar(
                title.position = "top",
                title.hjust = 0.5,
                barwidth = grid::unit(5.5, "cm"),
                barheight = grid::unit(0.32, "cm"),
                order = 2L
              ),
              size = ggplot2::guide_legend(
                title.position = "top",
                title.hjust = 0.5,
                nrow = 1L,
                override.aes = list(shape = 16L, colour = "#334155"),
                order = 3L
              )
            ),
          interactive = use_ggiraph
        )
      )
    )
  }

  plot_tree_taxa_bars <- build_tree_bars_plot(
    tree_taxa_full,
    tree_data,
    layout_full
  )
  plot_tree_taxa_circular_bars <- build_circular_si_plot(
    tree_taxa_full_raw,
    tree_data_raw
  )
  plot_tree_taxa_phyto_bars <- build_tree_bars_plot(
    tree_taxa_phyto,
    tree_data_phyto,
    layout_phyto
  )

  orphan_plot_data <- data.frame()
  if (nrow(taxa_not_in_tree) > 0L) {
    orphan_plot_data <- as.data.frame(
      taxa_not_in_tree,
      stringsAsFactors = FALSE
    ) |>
      tidytable::left_join(
        df_classes_plot |>
          tidytable::select(
            taxon_name_original,
            ids_class = ids,
            values_class = values,
            values_plot_class = values_plot,
            color_class = color
          ) |>
          tidytable::distinct(),
        by = c("taxon_name" = "taxon_name_original")
      ) |>
      tidytable::mutate(
        ids = ids_class,
        values = values_class,
        values_plot = values_plot_class,
        color = color_class,
        plot_label = format_tip_labels(taxon_name, genus),
        species_count = pmax(1, as.numeric(species_count), na.rm = TRUE),
        species_count = ifelse(is.finite(species_count), species_count, 1),
        tip_size = if (genus) {
          scale_tip_sizes(species_count)
        } else {
          rep(0.4, length(species_count))
        },
        ethnobotanical_species_count = pmax(
          0,
          as.numeric(ethnobotanical_species_count),
          na.rm = TRUE
        ),
        ethnobotanical_species_count = ifelse(
          is.finite(ethnobotanical_species_count),
          ethnobotanical_species_count,
          0
        ),
        values = suppressWarnings(as.numeric(values)),
        values = ifelse(is.finite(values), values, 0),
        values_plot = ifelse(is.finite(values_plot), values_plot, 0)
      )
  }

  if (nrow(orphan_plot_data) > 0L) {
    orphan_leg <- build_hierarchical_legend_spec(
      df_classes_plot,
      orphan_plot_data$plot_label
    )
    if (length(orphan_leg$limits) > 0L) {
      orphan_plot_data$ids <- factor(
        as.character(orphan_plot_data$ids),
        levels = orphan_leg$limits
      )
    }
    plot_orphan_taxa <- orphan_plot_data |>
      ggplot2::ggplot(mapping = ggplot2::aes(y = plot_label)) +
      ggplot2::geom_point(
        mapping = ggplot2::aes(
          x = layout_full$orphan_point_x,
          size = tip_size,
          color = log10(ethnobotanical_species_count)
        ),
        shape = 16
      ) +
      ggplot2::scale_size(
        name = "Known species",
        labels = label_math_10,
        range = c(1.6, 5.4),
        breaks = c(0, 1, 2, 3),
        limits = c(0, 4)
      ) +
      ggplot2::geom_col(
        mapping = ggplot2::aes(x = values_plot, y = plot_label, fill = ids),
        inherit.aes = FALSE,
        show.legend = c(fill = TRUE, size = FALSE, colour = FALSE)
      ) +
      ggplot2::scale_fill_manual(
        name = "Compound pathway & superclass (5 shades per pathway row)",
        values = orphan_leg$values,
        labels = orphan_leg$labels,
        limits = orphan_leg$limits,
        drop = FALSE,
        na.value = "transparent"
      ) +
      ggplot2::geom_text(
        mapping = ggplot2::aes(
          x = layout_full$orphan_text_x,
          y = plot_label,
          label = plot_label,
          color = log10(ethnobotanical_species_count)
        ),
        fontface = "italic",
        hjust = 0
      ) +
      khroma::scale_color_batlow(
        name = "Known ethnobotanical species",
        range = c(0, 0.80),
        na.value = "#4a4a4a",
        labels = label_math_10
      ) +
      ggplot2::scale_x_continuous(n.breaks = 4L) +
      ggplot2::scale_y_discrete(limits = rev) +
      ggplot2::theme_minimal() +
      ggplot2::guides(
        fill = ggplot2::guide_legend(
          ncol = 6L,
          byrow = TRUE,
          title.position = "top",
          override.aes = list(alpha = orphan_leg$alphas)
        )
      ) +
      ggplot2::theme(
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        legend.key = ggplot2::element_blank(),
        legend.position = "left",
        legend.text = ggplot2::element_text(colour = "#2b2b2b"),
        plot.caption = ggplot2::element_text(color = "#4a4a4a"),
        plot.margin = grid::unit(c(6, 8, 6, 8), "pt"),
        text = ggplot2::element_text(color = "#2b2b2b")
      )
  } else {
    plot_orphan_taxa <- ggplot2::ggplot() +
      ggplot2::theme_void()
  }

  build_girafe_widget <- function(plot_obj, layout_cfg) {
    if (!use_ggiraph) {
      return(NULL)
    }
    ggiraph::girafe(
      ggobj = plot_obj,
      width_svg = layout_cfg$export_width,
      height_svg = layout_cfg$export_height,
      options = list(
        ggiraph::opts_hover(
          css = "stroke:#000000;stroke-width:1.5px;fill-opacity:1;"
        ),
        ggiraph::opts_tooltip(
          css = paste0(
            "background-color:#1e293b;color:#f8fafc;padding:8px 12px;",
            "border-radius:6px;font-family:system-ui,sans-serif;",
            "font-size:12px;line-height:1.4;",
            "box-shadow:0 4px 12px rgba(0,0,0,0.25);"
          )
        ),
        ggiraph::opts_zoom(min = 0.5, max = 10),
        ggiraph::opts_toolbar(saveaspng = TRUE)
      )
    )
  }

  list(
    plot_tree_taxa_bars = plot_tree_taxa_bars,
    plot_tree_taxa_circular_bars = plot_tree_taxa_circular_bars,
    plot_tree_taxa_phyto_bars = plot_tree_taxa_phyto_bars,
    interactive_tree_taxa_bars = build_girafe_widget(
      plot_tree_taxa_bars,
      layout_full
    ),
    interactive_tree_taxa_circular_bars = build_girafe_widget(
      plot_tree_taxa_circular_bars,
      layout_circular
    ),
    interactive_tree_taxa_phyto_bars = build_girafe_widget(
      plot_tree_taxa_phyto_bars,
      layout_phyto
    ),
    plot_orphan_taxa = plot_orphan_taxa,
    taxa_orphan = orphan_plot_data,
    layout_full = layout_full,
    layout_circular = layout_circular,
    layout_phyto = layout_phyto,
    phylo_signal_full = phylo_signal_full,
    phylo_signal_phyto = phylo_signal_phyto,
    tree_data_phyto = tree_data_phyto
  )
}

#' @title Export tree bundle (SVG, PDF, and interactive HTML)
#'
#' @description
#' Saves the static tree plot to the target path (`.svg`), alongside a
#' publication-ready vector `.pdf` and a searchable interactive `.html` widget
#' (when `ggiraph` and `htmlwidgets` are installed).
#'
#' @param plot_obj ggplot object returned by `plot_trees()`.
#' @param girafe_obj Optional `girafe` htmlwidget returned by `plot_trees()`.
#' @param layout_cfg Layout list (`export_width`, `export_height`) from
#' `plot_trees()`.
#' @param file_path Character. Target output path (typically ending in `.svg` or
#' `.pdf`).
#' @param title Character. Title shown in the interactive HTML search bar.
#'
#' @return Character vector of written file paths.
#' @keywords internal
export_tree_bundle <- function(
  plot_obj,
  girafe_obj,
  layout_cfg,
  file_path,
  title = "Phylogenetic Tree"
) {
  w <- layout_cfg$export_width %||% 20
  h <- layout_cfg$export_height %||% 28

  ggplot2::ggsave(
    filename = file_path,
    plot = plot_obj,
    width = w,
    height = h,
    limitsize = FALSE,
    create.dir = TRUE
  )
  written <- file_path

  base_no_ext <- sub("\\.[A-Za-z0-9]+$", "", file_path)
  pdf_path <- paste0(base_no_ext, ".pdf")
  if (!identical(pdf_path, file_path)) {
    pdf_dev <- if (capabilities("cairo")) grDevices::cairo_pdf else "pdf"
    ggplot2::ggsave(
      filename = pdf_path,
      plot = plot_obj,
      device = pdf_dev,
      width = w,
      height = h,
      limitsize = FALSE,
      create.dir = TRUE
    )
    written <- c(written, pdf_path)
  }

  if (
    !is.null(girafe_obj) &&
      requireNamespace("htmlwidgets", quietly = TRUE)
  ) {
    html_path <- paste0(base_no_ext, ".html")
    htmlwidgets::saveWidget(
      widget = girafe_obj,
      file = normalizePath(html_path, mustWork = FALSE),
      selfcontained = TRUE,
      title = title
    )
    unlink(paste0(base_no_ext, "_files"), recursive = TRUE)
    if (file.exists(html_path)) {
      inject_tree_search_bar(html_path, title = title)
      written <- c(written, html_path)
    }
  }

  invisible(written)
}

#' @title Export iTOL tree and annotation files
#'
#' @description
#' Exports a Newick `.tre` file alongside three official iTOL v6 annotation
#' datasets (`DATASET_GRADIENT` for ethnobotanical species, `DATASET_SIMPLEBAR`
#' for species richness, and `DATASET_MULTIBAR` for compound superclass counts
#' with matching `microshades` hex colors) ready for drag-and-drop into
#' `https://itol.embl.de`.
#'
#' @param tree `phylo` object.
#' @param df_counted Data.frame with `taxon_name`, `species_count`, and
#' `ethnobotanical_count`.
#' @param df_classes Data.frame with `taxon_name`, `ids`, `values`, and `color`.
#' @param out_dir Character. Output directory for the iTOL files.
#' @param prefix Character. Filename prefix.
#' @param genus Logical. Whether tip labels are genus-level.
#'
#' @return Character vector of written file paths.
#' @keywords internal
export_itol_files <- function(
  tree,
  df_counted,
  df_classes,
  out_dir,
  prefix,
  genus = TRUE
) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  clean_id <- function(x) {
    x <- as.character(x)
    x <- if (genus) sub("_.*", "", x) else gsub("_", " ", sub("_ott.*", "", x))
    gsub(" ", "_", x, fixed = TRUE)
  }

  tree_itol <- tree
  tree_itol$tip.label <- clean_id(tree_itol$tip.label)
  if (!is.null(tree_itol$node.label)) {
    tree_itol$node.label <- gsub(" ", "_", tree_itol$node.label, fixed = TRUE)
  }
  tre_path <- file.path(out_dir, paste0(prefix, "_tree.tre"))
  ape::write.tree(tree_itol, file = tre_path)

  cnt <- as.data.frame(df_counted, stringsAsFactors = FALSE)
  cnt$id <- clean_id(cnt$taxon_name)
  cnt <- cnt[
    cnt$id %in% tree_itol$tip.label & !duplicated(cnt$id),
    ,
    drop = FALSE
  ]

  ethno_path <- file.path(out_dir, paste0(prefix, "_itol_ethnobotany.txt"))
  ethno_vals <- pmax(0, suppressWarnings(as.numeric(cnt$ethnobotanical_count)))
  ethno_vals[!is.finite(ethno_vals)] <- 0
  ethno_lines <- c(
    "DATASET_GRADIENT",
    "SEPARATOR TAB",
    "DATASET_LABEL\tEthnobotanical species (log10)",
    "COLOR\t#148F77",
    "COLOR_MIN\t#001959",
    "COLOR_MAX\t#E68653",
    "DATA",
    sprintf("%s\t%.4f", cnt$id, log10(1 + ethno_vals))
  )
  writeLines(ethno_lines, ethno_path, useBytes = TRUE)

  sp_path <- file.path(out_dir, paste0(prefix, "_itol_species_richness.txt"))
  sp_vals <- pmax(1, suppressWarnings(as.numeric(cnt$species_count)))
  sp_vals[!is.finite(sp_vals)] <- 1
  sp_lines <- c(
    "DATASET_SIMPLEBAR",
    "SEPARATOR TAB",
    "DATASET_LABEL\tKnown species count",
    "COLOR\t#334155",
    "DATA",
    sprintf("%s\t%d", cnt$id, as.integer(round(sp_vals)))
  )
  writeLines(sp_lines, sp_path, useBytes = TRUE)

  written <- c(tre_path, ethno_path, sp_path)

  cls <- as.data.frame(df_classes, stringsAsFactors = FALSE)
  cls$id <- clean_id(cls$taxon_name)
  cls <- cls[
    !is.na(cls$ids) & cls$id %in% tree_itol$tip.label,
    ,
    drop = FALSE
  ]
  if (nrow(cls) > 0L) {
    cls$id_chr <- as.character(cls$ids)
    cls$col_chr <- as.character(cls$color)
    u_cls <- cls[!duplicated(cls$id_chr), c("id_chr", "col_chr"), drop = FALSE]
    field_ids <- u_cls$id_chr
    field_cols <- u_cls$col_chr

    taxa_ids <- unique(cls$id)
    mat <- matrix(
      0,
      nrow = length(taxa_ids),
      ncol = length(field_ids),
      dimnames = list(taxa_ids, field_ids)
    )
    for (i in seq_len(nrow(cls))) {
      r <- cls$id[i]
      c_nm <- cls$id_chr[i]
      v <- suppressWarnings(as.numeric(cls$values[i]))
      if (is.finite(v) && v > 0) {
        mat[r, c_nm] <- mat[r, c_nm] + v
      }
    }

    row_lines <- vapply(
      taxa_ids,
      function(tid) {
        paste(
          c(tid, format(round(mat[tid, ], 2), trim = TRUE)),
          collapse = "\t"
        )
      },
      character(1L)
    )
    bar_path <- file.path(
      out_dir,
      paste0(prefix, "_itol_compounds_multibar.txt")
    )
    bar_lines <- c(
      "DATASET_MULTIBAR",
      "SEPARATOR TAB",
      "DATASET_LABEL\tCompound superclasses",
      "COLOR\t#4E7705",
      paste(c("FIELD_COLORS", field_cols), collapse = "\t"),
      paste(c("FIELD_LABELS", field_ids), collapse = "\t"),
      "DATA",
      unname(row_lines)
    )
    writeLines(bar_lines, bar_path, useBytes = TRUE)
    written <- c(written, bar_path)
  }

  invisible(written)
}

#' @keywords internal
inject_tree_search_bar <- function(html_path, title = "Phylogenetic Tree") {
  lines <- readLines(html_path, warn = FALSE, encoding = "UTF-8")
  body_idx <- grep("<body[^>]*>", lines)
  end_idx <- grep("</body>", lines)
  if (length(body_idx) == 0L || length(end_idx) == 0L) {
    return(invisible(FALSE))
  }

  toolbar_html <- sprintf(
    paste0(
      '<div id="eb-search-bar" style="position:sticky;top:0;z-index:9999;',
      "background:#0f172a;color:#f8fafc;padding:10px 16px;",
      "font-family:system-ui,-apple-system,sans-serif;font-size:13px;",
      "display:flex;align-items:center;gap:12px;",
      'box-shadow:0 2px 8px rgba(0,0,0,0.2);">',
      '<strong style="font-size:14px;">%s</strong>',
      '<input id="eb-search-input" type="search" ',
      'placeholder="Search taxon or clade (e.g. Dendrobium, Vanilla)..." ',
      'style="padding:5px 10px;border-radius:4px;border:1px solid #475569;',
      'background:#1e293b;color:#f8fafc;width:320px;font-size:13px;" />',
      '<button id="eb-prev-btn" style="padding:4px 10px;border-radius:4px;',
      "border:1px solid #475569;background:#334155;color:#f8fafc;",
      'cursor:pointer;">Prev</button>',
      '<button id="eb-next-btn" style="padding:4px 10px;border-radius:4px;',
      "border:1px solid #475569;background:#334155;color:#f8fafc;",
      'cursor:pointer;">Next</button>',
      '<span id="eb-search-status" style="color:#94a3b8;">',
      "Hover tips/bars for details; scroll/drag inside tree to zoom/pan.",
      "</span></div>"
    ),
    title
  )

  search_js <- paste0(
    "<script>",
    "(function(){",
    "var input=document.getElementById('eb-search-input');",
    "var status=document.getElementById('eb-search-status');",
    "var prevBtn=document.getElementById('eb-prev-btn');",
    "var nextBtn=document.getElementById('eb-next-btn');",
    "var matches=[],idx=0;",
    "function clearHighlights(){",
    "document.querySelectorAll('.eb-match-text').forEach(function(el){",
    "el.style.fill=el.getAttribute('data-orig-fill')||'';",
    "el.style.fontWeight=el.getAttribute('data-orig-fw')||'';",
    "el.classList.remove('eb-match-text');});",
    "}",
    "function focusMatch(i){",
    "if(!matches.length)return;",
    "idx=(i+matches.length)%matches.length;",
    "var el=matches[idx];",
    "el.scrollIntoView({behavior:'smooth',block:'center',inline:'center'});",
    "status.textContent=(idx+1)+' of '+matches.length+' matches';",
    "}",
    "function runSearch(){",
    "clearHighlights();matches=[];idx=0;",
    "var q=(input.value||'').trim().toLowerCase();",
    "if(!q){status.textContent='Hover tips/bars for details;';return;}",
    "document.querySelectorAll('svg text').forEach(function(el){",
    "var txt=(el.textContent||'').toLowerCase();",
    "if(txt.indexOf(q)!==-1){",
    "if(!el.hasAttribute('data-orig-fill'))",
    "el.setAttribute('data-orig-fill',el.style.fill||'');",
    "if(!el.hasAttribute('data-orig-fw'))",
    "el.setAttribute('data-orig-fw',el.style.fontWeight||'');",
    "el.style.fill='#dc2626';el.style.fontWeight='900';",
    "el.classList.add('eb-match-text');matches.push(el);",
    "}});",
    "if(matches.length){focusMatch(0);}else{status.textContent='0 matches';}",
    "}",
    "input.addEventListener('input',runSearch);",
    "nextBtn.addEventListener('click',function(){focusMatch(idx+1);});",
    "prevBtn.addEventListener('click',function(){focusMatch(idx-1);});",
    "})();</script>"
  )

  b1 <- body_idx[1L]
  lines[b1] <- paste0(lines[b1], "\n", toolbar_html)
  e1 <- end_idx[length(end_idx)]
  lines[e1] <- paste0(search_js, "\n", lines[e1])
  writeLines(lines, html_path, useBytes = TRUE)
  invisible(TRUE)
}
