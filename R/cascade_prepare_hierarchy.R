## Vendored from cascade (https://github.com/adafede/cascade)
## Maintainer: Adriano Rutz <adafede@gmail.com>
##
## This function was vendored from cascade's R/prepare_hierarchy.R to
## avoid pulling in the full cascade dependency tree. It prepares
## NPClassifier taxonomy results into a hierarchical data structure for
## plotting.
##
## Original file: R/prepare_hierarchy.R in adafede/cascade

#' @title Prepare hierarchy
#'
#' @description
#' Transforms NPClassifier pathway, superclass, and class annotations into an
#' exact mass-conserving 3-level parent-child hierarchy suitable for
#' `prepare_plot()`. Ranks top pathways and top 4 named superclasses per pathway
#' by marginal abundance (with deterministic tie-breaking) and folds remaining
#' superclasses into `<Pathway> Other`.
#'
#' @param dataframe Data.frame. Input table of classified structures.
#' @param type Character. Mode (`"literature"` or `"analysis"`).
#' @param detector Character. Detector parameter retained for compatibility.
#' @param rescale Logical. Rescale flag retained for compatibility.
#'
#' @return A `tidytable` with prepared hierarchy counts (`parents`, `ids`,
#' `labels`, `sample`, `species`, `values`).
#'
#' @section EDAM:
#' - Topic: `http://edamontology.org/topic_2258` (Cheminformatics)
#' - Operation: `http://edamontology.org/operation_2990` (Classification)
#'
#' @keywords internal
prepare_hierarchy <- function(
  dataframe,
  type = "analysis",
  detector = "ms",
  rescale = FALSE
) {
  stopifnot(
    "'type' must be either 'analysis' or 'literature'" = type %in%
      c("analysis", "literature")
  )

  df <- as.data.frame(dataframe, stringsAsFactors = FALSE)
  if (nrow(df) == 0L) {
    return(tidytable::tidytable(
      parents = character(0),
      ids = character(0),
      labels = character(0),
      sample = character(0),
      species = character(0),
      values = numeric(0)
    ))
  }

  clean_token <- function(x) {
    x <- trimws(as.character(x))
    bad <- is.na(x) |
      !nzchar(x) |
      grepl(
        "notAnnotated|notClassified|notConfident|^not",
        x,
        ignore.case = TRUE
      ) |
      x == "Other"
    ifelse(bad, NA_character_, x)
  }

  pw <- clean_token(df$best_candidate_1)
  pw[is.na(pw)] <- "Other"
  sc <- clean_token(df$best_candidate_2)
  cl <- clean_token(df$best_candidate_3)

  sample_col <- if ("sample" %in% names(df)) {
    as.character(df$sample)
  } else if ("organism" %in% names(df)) {
    as.character(df$organism)
  } else {
    as.character(df$species)
  }
  species_col <- if ("species" %in% names(df)) {
    as.character(df$species)
  } else {
    sample_col
  }
  w_col <- if ("weight" %in% names(df)) {
    w <- suppressWarnings(as.numeric(df$weight))
    ifelse(is.finite(w) & w > 0, w, 1)
  } else {
    rep(1, nrow(df))
  }

  dt <- data.table::data.table(
    pw = pw,
    sc = sc,
    cl = cl,
    sample = sample_col,
    species = species_col,
    w = w_col
  )
  dt <- dt[!is.na(sample) & !is.na(species)]
  if (nrow(dt) == 0L) {
    return(tidytable::tidytable(
      parents = character(0),
      ids = character(0),
      labels = character(0),
      sample = character(0),
      species = character(0),
      values = numeric(0)
    ))
  }

  max_pws <- length(microshades)
  pw_totals <- dt[pw != "Other", .(tot = sum(w)), by = pw][order(-tot, pw)]
  top_pws <- utils::head(pw_totals$pw, max_pws)
  dt[
    !pw %in% top_pws,
    `:=`(
      sc = ifelse(pw == "Other", sc, paste("Other", pw)),
      pw = "Other"
    )
  ]

  sc_totals <- dt[!is.na(sc), .(tot = sum(w)), by = .(pw, sc)][
    order(pw, -tot, sc)
  ]
  sc_totals[, rnk := seq_len(.N), by = pw]
  top_sc <- sc_totals[rnk <= 4L, .(pw, sc, keep = TRUE)]

  dt <- merge(dt, top_sc, by = c("pw", "sc"), all.x = TRUE, sort = FALSE)
  dt[,
    sc_label := ifelse(
      !is.na(keep),
      sc,
      ifelse(pw == "Other", "Other Other", paste(pw, "Other"))
    )
  ]
  dt[,
    cl_label := ifelse(
      !is.na(cl) & cl != sc_label,
      cl,
      paste(sc_label, "Other")
    )
  ]
  dt[, sc_id := paste(pw, sc_label, sep = "-")]
  dt[, cl_id := paste(sc_label, cl_label, sep = "-")]

  lvl1 <- dt[,
    .(parents = "", values = sum(w)),
    by = .(ids = pw, labels = pw, sample, species)
  ]
  lvl2 <- dt[,
    .(values = sum(w)),
    by = .(parents = pw, ids = sc_id, labels = sc_label, sample, species)
  ]
  lvl3 <- dt[,
    .(values = sum(w)),
    by = .(parents = sc_id, ids = cl_id, labels = cl_label, sample, species)
  ]

  tidytable::bind_rows(lvl1, lvl2, lvl3) |>
    tidytable::filter(!is.na(sample))
}
