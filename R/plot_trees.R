#' @title Plot trees
#'
#' @description This function plots trees.
#'
#' @param taxa_not_in_tree taxa_not_in_tree
#' @param tree_taxa_full tree_taxa_full
#' @param tree_taxa_phyto tree_taxa_phyto
#' @param df_counted df_counted
#' @param df_classes df_classes
#' @param offset offset
#' @param adjust_point adjust_point
#' @param adjust_text adjust_text
#' @param adjust_margin adjust_margin
#' @param genus genus
#' @param drop drop
#'
#' @return Trees
#'
#' @examples NULL
plot_trees <- function(
    taxa_not_in_tree,
    tree_taxa_full,
    tree_taxa_phyto,
    df_counted,
    df_classes,
    offset,
    adjust_point,
    adjust_text,
    adjust_margin,
    genus = TRUE,
    drop = FALSE) {
  if (genus == TRUE) {
    tree_taxa_full$tip.label <-
      gsub(
        pattern = "_.*",
        replacement = "",
        x = tree_taxa_full$tip.label
      )
    tree_taxa_phyto$tip.label <-
      gsub(
        pattern = "_.*",
        replacement = "",
        x = tree_taxa_phyto$tip.label
      )
  } else {
    tree_taxa_full$tip.label <-
      gsub(
        pattern = "_ott.*",
        replacement = "",
        x = tree_taxa_full$tip.label
      ) |>
      gsub(
        pattern = "_",
        replacement = " ",
      )
    tree_taxa_phyto$tip.label <-
      gsub(
        pattern = "_ott.*",
        replacement = "",
        x = tree_taxa_phyto$tip.label
      ) |>
      gsub(
        pattern = "_",
        replacement = " ",
      )
  }

  if (drop == TRUE) {
    labelz <- tree_taxa_full$tip.label[
      !tree_taxa_full$tip.label %in% df_counted$taxon_name
    ]
    labelz_phyto <- tree_taxa_phyto$tip.label[
      !tree_taxa_phyto$tip.label %in% df_counted$taxon_name
    ]

    tree_taxa_full <- tree_taxa_full |>
      ape::drop.tip(tip = labelz)

    tree_taxa_phyto <- tree_taxa_phyto |>
      ape::drop.tip(tip = labelz_phyto)
  }

  logger::log_trace(
    "Calculating ethnobotanical use at different levels of the tree..."
  )
  tree_data <- tree_taxa_full |>
    calculate_branches_values(
      df = df_counted,
      variable = "ethnobotanical_count",
      name = "ethnobotanical_species_average"
    )
  tree_data_phyto <- tree_taxa_phyto |>
    calculate_branches_values(
      df = df_counted,
      variable = "ethnobotanical_count",
      name = "ethnobotanical_species_average"
    )
  tree_data <- tree_data |>
    tidytable::left_join(
      df_counted |>
        tidytable::distinct(label = taxon_name, species_count)
    ) |>
    tidytable::distinct(node, .keep_all = TRUE)
  tree_data_phyto <- tree_data_phyto |>
    tidytable::left_join(
      df_counted |>
        tidytable::distinct(label = taxon_name, species_count)
    ) |>
    tidytable::distinct(node, .keep_all = TRUE)

  logger::log_trace("Plotting ...")
  library(ggtree)
  library(ggplot2)
  plot_tree_taxa <- ggtree::ggtree(
    tree_taxa_full,
    layout = "rectangular",
    mapping = ggplot2::aes(
      color = ethnobotanical_species_average |>
        log10()
    )
  ) %<+%
    df_counted %<+%
    tree_data +
    khroma::scale_color_batlow(
      name = "Known ethnobotanical species",
      na.value = "grey80",
      labels = scales::label_math()
    ) +
    ggtree::geom_tiplab(size = ggplot2::rel(1), offset = ggplot2::rel(0.6)) +
    ggtree::geom_tippoint(
      mapping = ggplot2::aes(
        size = tree_data$species_count |>
          log10()
      )
    ) +
    ggplot2::scale_size(name = "Known species", labels = scales::label_math()) +
    ggplot2::theme(
      legend.text = ggplot2::element_text(colour = "grey30"),
      legend.position = "left",
      plot.caption = ggtext::element_markdown(color = "grey30"),
      text = ggplot2::element_text(face = "bold", color = "grey30")
    )
  zzz <- tree_taxa_full$tip.label |>
    unique()
  taxa_classes_tree <- df_classes |>
    tidytable::filter(!is.na(ids)) |>
    tidytable::filter(taxon_name %in% zzz) |>
    tidytable::distinct(ids, color) |>
    tidytable::arrange(color) |>
    tidytable::mutate(
      ids = ids |>
        factor(),
      color = color |>
        factor()
    )
  plot_tree_taxa_bars <- plot_tree_taxa +
    ggtreeExtra::geom_fruit(
      data = df_classes,
      geom = geom_col,
      mapping = ggplot2::aes(y = taxon_name, x = values, fill = ids),
      offset = ggplot2::rel(0.1),
      pwidth = ggplot2::rel(1.2),
      axis.params = list(
        axis = "x",
        vjust = -1,
        text.size = ggplot2::rel(5),
        line.size = ggplot2::rel(1),
      )
    ) +
    ggplot2::scale_fill_manual(
      name = "Compound superclass",
      values = taxa_classes_tree$color |>
        as.character(),
      na.value = "grey"
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(ncol = 1))

  plot_tree_taxa_phyto <- ggtree::ggtree(
    tree_taxa_phyto,
    layout = "rectangular",
    mapping = ggplot2::aes(
      color = ethnobotanical_species_average |>
        log10()
    )
  ) %<+%
    df_counted %<+%
    tree_data_phyto +
    khroma::scale_color_batlow(
      name = "Known ethnobotanical species",
      na.value = "grey80",
      labels = scales::label_math()
    ) +
    ggtree::geom_tiplab(offset = ggplot2::rel(offset)) +
    ggtree::geom_tippoint(
      mapping = ggplot2::aes(
        size = tree_data_phyto$species_count |>
          log10()
      )
    ) +
    ggplot2::scale_size(
      name = "Known species",
      labels = scales::label_math()
    ) +
    ggplot2::theme(
      legend.position = "left",
      legend.text = ggplot2::element_text(colour = "grey30"),
      plot.caption = ggtext::element_markdown(color = "grey30"),
      text = ggplot2::element_text(face = "bold", color = "grey30")
    )
  taxa_classes_phyto <- df_classes |>
    tidytable::filter(!is.na(ids)) |>
    tidytable::filter(taxon_name %in% tree_taxa_phyto$tip.label) |>
    tidytable::distinct(ids, color) |>
    tidytable::arrange(color) |>
    tidytable::mutate(
      ids = ids |>
        factor(),
      color = color |>
        factor()
    )
  plot_tree_taxa_phyto_bars <- plot_tree_taxa_phyto +
    ggtreeExtra::geom_fruit(
      data = df_classes,
      geom = geom_col,
      mapping = ggplot2::aes(y = taxon_name, x = values, fill = ids),
      offset = ggplot2::rel(offset / 2),
      pwidth = ggplot2::rel(1.2),
      axis.params = list(
        axis = "x",
        vjust = 1,
        text.size = ggplot2::rel(4),
        line.size = ggplot2::rel(1),
      )
    ) +
    ggplot2::scale_fill_manual(
      name = "Compound superclass",
      values = taxa_classes_phyto$color |>
        as.character(),
      na.value = "grey"
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(ncol = 1L))

  taxa_orphan <- df_classes |>
    tidytable::filter(!is.na(ids)) |>
    tidytable::filter(!taxon_name %in% taxa_not_in_tree$tip.label) |>
    tidytable::filter(!taxon_name %in% tree_taxa_phyto$tip.label)

  if (taxa_not_in_tree |> nrow() > 0) {
    plot_orphan_taxa <- taxa_not_in_tree |>
      tidytable::full_join(taxa_orphan) |>
      ggplot2::ggplot(mapping = ggplot2::aes(y = taxon_name)) +
      ggplot2::geom_point(
        mapping = ggplot2::aes(
          x = adjust_point,
          size = species_count |>
            log10(),
          color = ethnobotanical_species_count |>
            log10()
        ),
        shape = 16
      ) +
      ggplot2::scale_size(
        name = "Known species",
        labels = scales::label_math()
      ) +
      ggplot2::geom_col(
        mapping = ggplot2::aes(x = values, y = taxon_name, fill = ids)
      ) +
      ggplot2::scale_fill_manual(
        name = "Compound superclass",
        values = levels(taxa_orphan$color),
        na.value = "grey"
      ) +
      ggplot2::geom_text(
        mapping = ggplot2::aes(
          x = adjust_text,
          y = taxon_name,
          label = taxon_name,
          color = ethnobotanical_species_count |>
            log10()
        ),
        hjust = 0
      ) +
      khroma::scale_color_batlow(
        name = "Known ethnobotanical species",
        na.value = "grey80",
        labels = scales::label_math()
      ) +
      ggplot2::scale_x_continuous(n.breaks = 4L) +
      ggplot2::scale_y_discrete(limits = rev) +
      ggplot2::theme_minimal() +
      ggplot2::guides(fill = ggplot2::guide_legend(ncol = 1)) +
      ggplot2::theme(
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        legend.position = "left",
        legend.text = ggplot2::element_text(colour = "grey30"),
        plot.caption = ggtext::element_markdown(color = "grey30"),
        plot.margin = ggplot2::margin(
          t = 0,
          r = 10,
          b = 10,
          l = adjust_margin
        ),
        text = ggplot2::element_text(face = "bold", color = "grey30")
      )
  } else {
    plot_orphan_taxa <- ggplot2::ggplot() +
      ggplot2::theme_void()
  }

  return(list(
    "plot_tree_taxa_bars" = plot_tree_taxa_bars,
    "plot_tree_taxa_phyto_bars" = plot_tree_taxa_phyto_bars,
    "plot_orphan_taxa" = plot_orphan_taxa,
    "taxa_orphan" = taxa_orphan
  ))
}
