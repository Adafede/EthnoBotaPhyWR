#' @title Normalize OTT IDs
#'
#' @description
#' Converts Open Tree of Life (OTT) identifiers to a consistent character
#' representation while preserving missing values as `NA_character_`.
#'
#' @param x Vector of OTT IDs or values that should be coerced to OTT IDs.
#'
#' @return A character vector with the normalized OTT IDs.
#'
#' @section EDAM:
#' - Topic: `http://edamontology.org/topic_0637` (Taxonomy)
#' - Operation: `http://edamontology.org/operation_1812` (Data parsing)
#' - Data: `http://edamontology.org/data_0842` (Identifier),
#'   `http://edamontology.org/data_1868` (Taxon)
#'
#' @keywords internal
normalize_ott_ids <- function(x) {
  x <- as.integer(x)
  ifelse(is.na(x), NA_character_, as.character(x))
}
