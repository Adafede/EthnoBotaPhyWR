#' @title Normalize OTT IDs
#'
#' @description
#' Converts OTT IDs to a consistent character representation while preserving
#' missing values as `NA`.
#'
#' @param x Vector of OTT IDs or values that should be coerced to OTT IDs.
#'
#' @return A character vector with the normalized OTT IDs.
#' @keywords internal
normalize_ott_ids <- function(x) {
  x <- as.integer(x)
  ifelse(is.na(x), NA_character_, as.character(x))
}
