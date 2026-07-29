#' @title Plot trees
#'
#' @description
#' Plots phylogenetic trees with ethnobotanical and phytochemical overlays. The helper formats tip labels consistently, derives spacing from the tree geometry, and returns the main tree plots plus the orphan-taxon panel.
#'
#' @param taxa_not_in_tree Data.frame. Taxa that are absent from the tree and should be shown in the orphan panel.
#' @param tree_taxa_full phylo. Full tree object used for the overview plot.
#' @param tree_taxa_phyto phylo. Tree object restricted to taxa with reported compounds or uses.
#' @param df_counted Data.frame. Count table containing tip labels and summary counts such as `species_count` and `ethnobotanical_count`.
#' @param df_classes Data.frame. Classification table providing the compound superclass values, colours, and per-taxon annotations.
#' @param offset Numeric. Optional override for the tip-label offset used by the tree plots.
#' @param adjust_point Numeric. Optional override for the x-position of orphan-panel points.
#' @param adjust_text Numeric. Optional override for the x-position of orphan-panel labels.
#' @param adjust_margin Numeric. Optional override for the orphan-panel left margin.
#' @param genus Logical. If `TRUE`, tip labels are reduced to genus-level names.
#' @param drop Logical. If `TRUE`, tips not present in `df_counted` are dropped before plotting.
#'
#' @return A list containing the main tree plots, the orphan-taxon plot, the orphan data frame, and the derived layout settings.
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
  drop = FALSE
) {
  stopifnot(is.data.frame(taxa_not_in_tree))
  stopifnot(inherits(tree_taxa_full, "phylo"))
  stopifnot(inherits(tree_taxa_phyto, "phylo"))
  stopifnot(is.data.frame(df_counted))
  stopifnot(is.data.frame(df_classes))

  format_tip_labels <- function(labels, genus_mode) {
    labels <- as.character(labels)
    if (genus_mode) {
      labels <- stringi::stri_replace_all_regex(labels, "_.*", "")
      stringi::stri_replace_all_regex(labels, " .*", "")
    } else {
      stringi::stri_replace_all_regex(labels, "_ott.*", "") |>
        stringi::stri_replace_all_regex("_", " ")
    }
  }

  scale_tip_sizes <- function(counts) {
    counts <- suppressWarnings(as.numeric(counts))
    counts[!is.finite(counts)] <- 1
    counts <- pmax(1, counts)
    if (length(counts) == 0L) {
      return(numeric(0))
    }
    log_counts <- log1p(counts)
    max_log <- max(log_counts, na.rm = TRUE)
    if (!is.finite(max_log) || max_log <= 0) {
      return(rep(1.8, length(counts)))
    }
    scales::rescale(log_counts, to = c(1.8, 6.2), from = c(0, max_log))
  }

  scale_fruit_values <- function(values) {
    values <- suppressWarnings(as.numeric(values))
    values[!is.finite(values)] <- 0
    max_val <- max(values, na.rm = TRUE)
    if (!is.finite(max_val) || max_val <= 0) {
      return(rep(0, length(values)))
    }
    values / max_val
  }

  tree_taxa_full$tip.label <- format_tip_labels(tree_taxa_full$tip.label, genus)
  tree_taxa_phyto$tip.label <- format_tip_labels(
    tree_taxa_phyto$tip.label,
    genus
  )

  df_counted_plot <- tibble::as_tibble(df_counted) |>
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
      )
    )
  df_classes_plot <- tibble::as_tibble(df_classes) |>
    tidytable::mutate(
      taxon_name_original = taxon_name,
      plot_label = format_tip_labels(taxon_name, genus),
      label = plot_label,
      taxon_name = plot_label,
      values = suppressWarnings(as.numeric(values)),
      values = ifelse(is.finite(values), values, 0),
      values_plot = scale_fruit_values(values)
    )

  if (drop) {
    labelz <- setdiff(tree_taxa_full$tip.label, df_counted_plot$taxon_name)
    labelz_phyto <- setdiff(
      tree_taxa_phyto$tip.label,
      df_counted_plot$taxon_name
    )
    tree_taxa_full <- ape::drop.tip(tree_taxa_full, tip = labelz)
    tree_taxa_phyto <- ape::drop.tip(tree_taxa_phyto, tip = labelz_phyto)
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
    max_label_width <- max(nchar(tip_labels), na.rm = TRUE)
    tree_span <- max(1, max(ape::node.depth.edgelength(tree), na.rm = TRUE))

    fruit_offset <- preferred_offset %||%
      max(
        0.04,
        min(0.10, 0.04 + 0.0018 * log1p(n_tips) + 0.0016 * max_label_width)
      )

    fruit_width <- if (genus_mode) {
      max(0.24, min(0.34, 0.24 + 0.0018 * log1p(n_tips)))
    } else {
      max(0.22, min(0.32, 0.22 + 0.0018 * log1p(n_tips)))
    }

    tip_label_offset <- max(
      0.006,
      min(0.028, 0.008 + 0.0006 * max_label_width + 0.0006 * log1p(n_tips))
    )
    tree_xlim <- max(1, tree_span * 0.72)

    orphan_point_x <- adjust_point %||% -0.2
    orphan_text_x <- adjust_text %||% -0.05
    export_width <- max(
      12,
      min(30, 10 + 0.04 * n_tips + 0.03 * max_label_width)
    )
    export_height <- max(
      12,
      min(24, 10 + 0.03 * n_tips + 0.02 * max_label_width)
    )

    list(
      fruit_offset = fruit_offset,
      fruit_width = fruit_width,
      tip_label_offset = tip_label_offset,
      tree_xlim = tree_xlim,
      orphan_point_x = orphan_point_x,
      orphan_text_x = orphan_text_x,
      export_width = export_width,
      export_height = export_height
    )
  }

  logger::log_trace(
    "Calculating ethnobotanical use at different levels of the tree..."
  )
  tree_data <- calculate_branches_values(
    tree = tree_taxa_full,
    df = df_counted_plot,
    variable = "ethnobotanical_count",
    name = "ethnobotanical_species_average"
  )
  tree_data_phyto <- calculate_branches_values(
    tree = tree_taxa_phyto,
    df = df_counted_plot,
    variable = "ethnobotanical_count",
    name = "ethnobotanical_species_average"
  )

  tree_data <- tree_data |>
    tidytable::left_join(
      df_counted_plot |> tidytable::distinct(label, species_count)
    ) |>
    tidytable::distinct(node, .keep_all = TRUE) |>
    tidytable::mutate(
      species_count = pmax(1, as.numeric(species_count), na.rm = TRUE),
      species_count = ifelse(is.finite(species_count), species_count, 1),
      tip_size = if (genus) {
        scale_tip_sizes(species_count)
      } else {
        rep(2.8, length(species_count))
      },
      ethnobotanical_species_average = ifelse(
        is.finite(ethnobotanical_species_average) &
          ethnobotanical_species_average > 0,
        ethnobotanical_species_average,
        NA_real_
      )
    )
  tree_data_phyto <- tree_data_phyto |>
    tidytable::left_join(
      df_counted_plot |> tidytable::distinct(label, species_count)
    ) |>
    tidytable::distinct(node, .keep_all = TRUE) |>
    tidytable::mutate(
      species_count = pmax(1, as.numeric(species_count), na.rm = TRUE),
      species_count = ifelse(is.finite(species_count), species_count, 1),
      tip_size = if (genus) {
        scale_tip_sizes(species_count)
      } else {
        rep(2.8, length(species_count))
      },
      ethnobotanical_species_average = ifelse(
        is.finite(ethnobotanical_species_average) &
          ethnobotanical_species_average > 0,
        ethnobotanical_species_average,
        NA_real_
      )
    )

  layout_full <- estimate_layout(tree_taxa_full, offset, genus_mode = genus)
  layout_phyto <- estimate_layout(tree_taxa_phyto, offset, genus_mode = genus)

  logger::log_trace("Plotting ...")
  requireNamespace(ggtree)
  requireNamespace(ggplot2)
  geom_col <- ggplot2::geom_col

  plot_tree_taxa <- ggtree::ggtree(
    tree_taxa_full,
    layout = "rectangular",
    mapping = ggplot2::aes(color = log10(ethnobotanical_species_average))
  ) %<+%
    tree_data +
    ggtree::xlim_tree(layout_full$tree_xlim) +
    khroma::scale_color_batlow(
      name = "Known ethnobotanical species",
      na.value = "#767676",
      labels = scales::label_math()
    ) +
    ggtree::geom_tiplab(
      size = ggplot2::rel(1),
      align = TRUE,
      hjust = 0,
      offset = layout_full$tip_label_offset,
      linesize = 0.2
    ) +
    ggtree::geom_tippoint(
      mapping = ggplot2::aes(size = tip_size)
    ) +
    ggplot2::scale_size(
      name = "Known species",
      labels = scales::label_math(),
      range = c(1.8, 6.2),
      breaks = c(1.8, 3.0, 6.2),
      limits = c(1.8, 6.2)
    ) +
    ggplot2::theme(
      legend.text = ggplot2::element_text(colour = "#767676"),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      plot.caption = ggtext::element_markdown(color = "#767676"),
      plot.margin = grid::unit(c(6, 8, 6, 8), "pt"),
      text = ggplot2::element_text(face = "bold", color = "#767676")
    )

  taxa_classes_tree <- df_classes_plot |>
    tidytable::filter(!is.na(ids)) |>
    tidytable::filter(taxon_name %in% tree_taxa_full$tip.label) |>
    tidytable::distinct(ids, color) |>
    tidytable::arrange(color) |>
    tidytable::mutate(ids = ids |> factor(), color = color |> factor())

  plot_tree_taxa_bars <- plot_tree_taxa +
    ggtreeExtra::geom_fruit(
      data = df_classes_plot,
      geom = geom_col,
      mapping = ggplot2::aes(y = label, x = values_plot, fill = ids),
      offset = layout_full$fruit_offset,
      pwidth = layout_full$fruit_width,
      axis.params = list(
        axis = "none"
      )
    ) +
    ggplot2::scale_fill_manual(
      name = "Compound superclass",
      values = taxa_classes_tree$color |> as.character(),
      na.value = "grey"
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(ncol = 8L))

  plot_tree_taxa_phyto <- ggtree::ggtree(
    tree_taxa_phyto,
    layout = "rectangular",
    mapping = ggplot2::aes(color = log10(ethnobotanical_species_average))
  ) %<+%
    tree_data_phyto +
    ggtree::xlim_tree(layout_phyto$tree_xlim) +
    khroma::scale_color_batlow(
      name = "Known ethnobotanical species",
      na.value = "#767676",
      labels = scales::label_math()
    ) +
    ggtree::geom_tiplab(
      align = TRUE,
      hjust = 0,
      offset = layout_phyto$tip_label_offset,
      linesize = 0.2
    ) +
    ggtree::geom_tippoint(
      mapping = ggplot2::aes(size = tip_size)
    ) +
    ggplot2::scale_size(
      name = "Known species",
      labels = scales::label_math(),
      range = c(1.8, 6.2),
      breaks = c(1.8, 3.0, 6.2),
      limits = c(1.8, 6.2)
    ) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.text = ggplot2::element_text(colour = "#767676"),
      plot.caption = ggtext::element_markdown(color = "#767676"),
      plot.margin = grid::unit(c(6, 8, 6, 8), "pt"),
      text = ggplot2::element_text(face = "bold", color = "#767676")
    )

  taxa_classes_phyto <- df_classes_plot |>
    tidytable::filter(!is.na(ids)) |>
    tidytable::filter(taxon_name %in% tree_taxa_phyto$tip.label) |>
    tidytable::distinct(ids, color) |>
    tidytable::arrange(color) |>
    tidytable::mutate(ids = ids |> factor(), color = color |> factor())

  plot_tree_taxa_phyto_bars <- plot_tree_taxa_phyto +
    ggtreeExtra::geom_fruit(
      data = df_classes_plot,
      geom = geom_col,
      mapping = ggplot2::aes(y = label, x = values_plot, fill = ids),
      offset = layout_phyto$fruit_offset,
      pwidth = layout_phyto$fruit_width,
      axis.params = list(
        axis = "none"
      )
    ) +
    ggplot2::scale_fill_manual(
      name = "Compound superclass",
      values = taxa_classes_phyto$color |> as.character(),
      na.value = "grey"
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(ncol = 8L))

  orphan_plot_data <- tibble::tibble()
  if (nrow(taxa_not_in_tree) > 0) {
    orphan_plot_data <- taxa_not_in_tree |>
      tibble::as_tibble() |>
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
          rep(2.8, length(species_count))
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

  if (nrow(orphan_plot_data) > 0) {
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
        labels = scales::label_math(),
        range = c(1.8, 6.2),
        breaks = c(1.8, 3.0, 6.2),
        limits = c(1.8, 6.2)
      ) +
      ggplot2::geom_col(
        mapping = ggplot2::aes(x = values_plot, y = plot_label, fill = ids),
        inherit.aes = FALSE
      ) +
      ggplot2::scale_fill_manual(
        name = "Compound superclass",
        values = unique(
          orphan_plot_data$color |> stats::na.omit() |> as.character()
        ),
        na.value = "grey"
      ) +
      ggplot2::geom_text(
        mapping = ggplot2::aes(
          x = layout_full$orphan_text_x,
          y = plot_label,
          label = plot_label,
          color = log10(ethnobotanical_species_count)
        ),
        hjust = 0
      ) +
      khroma::scale_color_batlow(
        name = "Known ethnobotanical species",
        na.value = "#767676",
        labels = scales::label_math()
      ) +
      ggplot2::scale_x_continuous(n.breaks = 4L) +
      ggplot2::scale_y_discrete(limits = rev) +
      ggplot2::theme_minimal() +
      ggplot2::guides(fill = ggplot2::guide_legend(ncol = 8L)) +
      ggplot2::theme(
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        legend.position = "left",
        legend.text = ggplot2::element_text(colour = "#767676"),
        plot.caption = ggtext::element_markdown(color = "#767676"),
        plot.margin = grid::unit(c(6, 8, 6, 8), "pt"),
        text = ggplot2::element_text(face = "bold", color = "#767676")
      )
  } else {
    plot_orphan_taxa <- ggplot2::ggplot() +
      ggplot2::theme_void()
  }

  return(list(
    plot_tree_taxa_bars = plot_tree_taxa_bars,
    plot_tree_taxa_phyto_bars = plot_tree_taxa_phyto_bars,
    plot_orphan_taxa = plot_orphan_taxa,
    taxa_orphan = orphan_plot_data,
    layout_full = layout_full,
    layout_phyto = layout_phyto
  ))
}
