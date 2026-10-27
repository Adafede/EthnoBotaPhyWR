## Vendored from cascade (https://github.com/adafede/cascade)
## Maintainer: Adriano Rutz <adafede@gmail.com>
##
## This function was vendored from cascade's R/prepare_plot.R to
## avoid pulling in the full cascade dependency tree. It depends on
## color palettes defined in cascade/colors.R (also vendored).
##
## Original file: R/prepare_plot.R in adafede/cascade

#' Prepare plot
#'
#' @param dataframe Dataframe
#' @param organism Organism
#'
#' @return A dataframe prepared for plots
#' @keywords internal
prepare_plot <- function(dataframe, organism = "species") {
  presamples <- dataframe |>
    tidytable::ungroup() |>
    tidytable::filter(
      parents != "" &
        !grepl(pattern = "-", x = parents)
    ) |>
    tidytable::filter(
      parents != "" &
        !grepl(pattern = "-", x = parents)
    ) |>
    tidytable::filter(!is.na(get(organism))) |>
    tidytable::mutate(species = get(organism)) |>
    tidytable::group_by(parents) |>
    tidytable::mutate(valuez = sum(values)) |>
    tidytable::ungroup() |>
    tidytable::arrange(desc(values)) |>
    tidytable::arrange(desc(valuez)) |>
    tidytable::mutate(
      group = factor(parents, levels = unique(parents)) |>
        as.integer()
    ) |>
    tidytable::group_by(group) |>
    tidytable::mutate(
      subgroup = factor(x = ids, levels = unique(ids)) |>
        as.integer()
    ) |>
    tidytable::mutate(
      subgroup = subgroup |>
        tidytable::dense_rank()
    ) |>
    tidytable::rowwise() |>
    tidytable::group_by(sample) |>
    tidytable::mutate(tot = sum(values)) |>
    tidytable::ungroup()

  tempval <- presamples |>
    tidytable::filter(parents == "Other") |>
    tidytable::distinct(group) |>
    tidytable::pull()

  if (length(tempval) != 0) {
    samples_0 <- presamples |>
      tidytable::filter(group == tempval) |>
      tidytable::rowwise() |>
      tidytable::mutate(group = 666) |>
      tidytable::ungroup()

    samples_1 <- presamples |>
      tidytable::filter(group > tempval) |>
      tidytable::rowwise() |>
      tidytable::mutate(group = group - 1) |>
      tidytable::ungroup()

    samples_2 <- presamples |>
      tidytable::filter(group < tempval)

    samples <- tidytable::bind_rows(samples_0, samples_1, samples_2)
  } else {
    samples <- presamples
  }

  samples <- samples |>
    tidytable::mutate_rowwise(
      color = tidytable::if_else(
        condition = grepl(
          pattern = "Other",
          x = parents,
          fixed = TRUE
        ),
        true = purrr::pluck(
          microshades_grey,
          1,
          subgroup,
          .default = NA_character_
        ),
        false = purrr::pluck(
          microshades,
          group,
          subgroup,
          .default = NA_character_
        )
      )
    ) |>
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

  samples <-
    tidytable::bind_rows(
      samples |>
        tidytable::filter(!is.na(color)),
      quickfix
    )

  samples$ids <-
    forcats::fct_reorder2(
      .f = samples$ids,
      .x = samples$group,
      .y = samples$idz,
      .desc = FALSE
    )

  samples$color <-
    forcats::fct_reorder2(
      .f = samples$color,
      .x = samples$group,
      .y = samples$idz,
      .desc = FALSE
    )

  samples$sample <-
    forcats::fct_reorder2(
      .f = samples$sample,
      .x = samples$valuez,
      .y = samples$sample,
      .desc = TRUE
    )

  return(samples)
}
