## Vendored from cascade (https://github.com/adafede/cascade)
## Maintainer: Adriano Rutz <adafede@gmail.com>
##
## This function was vendored from cascade's R/prepare_plot.R to
## avoid pulling in the full cascade dependency tree. It depends on
## color palettes defined in R/cascade_colors.R.
##
## Original file: R/prepare_plot.R in adafede/cascade

#' @title Prepare plot
#'
#' @description
#' Prepares hierarchical compound classification data for plotting by assigning
#' group ranks, microshades palettes, and ordered factor levels.
#'
#' @param dataframe Data.frame. Output of `prepare_hierarchy()`.
#' @param organism Character. Column name identifying the organism or species.
#'
#' @return A `tidytable` prepared for plotting.
#'
#' @section EDAM:
#' - Topic: `http://edamontology.org/topic_2258` (Cheminformatics)
#' - Operation: `http://edamontology.org/operation_0337` (Visualisation)
#'
#' @keywords internal
prepare_plot <- function(dataframe, organism = "species") {
  fct_reorder2_base <- function(f, x, y, desc = TRUE) {
    f <- as.factor(f)
    if (nlevels(f) <= 1L) {
      return(f)
    }
    summary_vals <- tapply(seq_along(x), f, function(i) {
      xi <- x[i]
      yi <- y[i]
      keep <- !(is.na(xi) | is.na(yi))
      if (!any(keep)) {
        return(yi[NA_integer_])
      }
      yi[keep][[order(xi[keep], decreasing = TRUE)[[1L]]]]
    })
    factor(f, levels = levels(f)[order(summary_vals, decreasing = desc)])
  }

  presamples <- dataframe |>
    tidytable::ungroup() |>
    tidytable::filter(
      parents != "" &
        !grepl(pattern = "-", x = parents)
    ) |>
    tidytable::filter(!is.na(get(organism))) |>
    tidytable::mutate(species = get(organism)) |>
    tidytable::group_by(parents) |>
    tidytable::mutate(valuez = sum(values)) |>
    tidytable::ungroup() |>
    tidytable::arrange(tidytable::desc(values)) |>
    tidytable::arrange(tidytable::desc(valuez)) |>
    tidytable::mutate(
      group = factor(parents, levels = unique(parents)) |>
        as.integer()
    ) |>
    tidytable::group_by(group) |>
    tidytable::mutate(
      subgroup = factor(x = ids, levels = unique(ids)) |>
        as.integer() |>
        tidytable::dense_rank()
    ) |>
    tidytable::group_by(sample) |>
    tidytable::mutate(tot = sum(values)) |>
    tidytable::ungroup()

  tempval <- presamples |>
    tidytable::filter(parents == "Other") |>
    tidytable::distinct(group) |>
    tidytable::pull()

  if (length(tempval) != 0L) {
    samples_0 <- presamples |>
      tidytable::filter(group == tempval) |>
      tidytable::mutate(group = 666)

    samples_1 <- presamples |>
      tidytable::filter(group > tempval) |>
      tidytable::mutate(group = group - 1)

    samples_2 <- presamples |>
      tidytable::filter(group < tempval)

    samples <- tidytable::bind_rows(samples_0, samples_1, samples_2)
  } else {
    samples <- presamples
  }

  shade_mat <- do.call(rbind, microshades)
  grey_vec <- microshades_grey[[1L]]
  is_other <- grepl(pattern = "Other", x = samples$parents, fixed = TRUE)
  s_idx <- ifelse(
    samples$subgroup >= 1L & samples$subgroup <= ncol(shade_mat),
    as.integer(samples$subgroup),
    NA_integer_
  )
  g_idx <- ifelse(
    samples$group >= 1L & samples$group <= nrow(shade_mat),
    as.integer(samples$group),
    NA_integer_
  )
  samples$color <- ifelse(
    is_other,
    grey_vec[s_idx],
    shade_mat[cbind(g_idx, s_idx)]
  )

  samples <- samples |>
    tidytable::mutate(relative = values / tot) |>
    tidytable::ungroup() |>
    tidytable::arrange(subgroup) |>
    tidytable::arrange(group) |>
    tidytable::mutate(idz = paste(group, subgroup))

  quickfix <- samples |>
    tidytable::filter(is.na(color)) |>
    tidytable::group_by(group, subgroup) |>
    tidytable::mutate(subsub = tidytable::cur_group_id()) |>
    tidytable::mutate(color = paste0("#00", sprintf("%04d", subsub))) |>
    tidytable::select(-subsub) |>
    tidytable::ungroup()

  samples <- tidytable::bind_rows(
    samples |> tidytable::filter(!is.na(color)),
    quickfix
  )

  samples$ids <- fct_reorder2_base(
    f = samples$ids,
    x = samples$group,
    y = samples$idz,
    desc = FALSE
  )

  samples$color <- fct_reorder2_base(
    f = samples$color,
    x = samples$group,
    y = samples$idz,
    desc = FALSE
  )

  samples$sample <- fct_reorder2_base(
    f = samples$sample,
    x = samples$valuez,
    y = samples$sample,
    desc = TRUE
  )

  samples
}
