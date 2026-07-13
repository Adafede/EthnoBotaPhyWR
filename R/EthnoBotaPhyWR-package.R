#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

.datatable.aware <- TRUE # nolint: object_name_linter. datatable convention.

#' @title Normalize OTT IDs
#'
#' @description
#' Converts OTT IDs to a consistent character representation while preserving missing values as `NA`.
#'
#' @param x Vector of OTT IDs or values that should be coerced to OTT IDs.
#'
#' @return A character vector with the normalized OTT IDs.
#' @keywords internal
normalize_ott_ids <- function(x) {
  x <- suppressWarnings(as.integer(x))
  ifelse(is.na(x), NA_character_, as.character(x))
}

#' @title Resolve cache directory
#'
#' @description
#' Resolves a persistent cache directory from an optional local path or the package cache location.
#'
#' @param local_cache Character. Optional local cache directory. If `NULL` or empty, the package cache directory is used.
#'
#' @return A normalized path to the cache directory.
#' @keywords internal
resolve_cache_dir <- function(local_cache = NULL) {
  if (
    is.null(local_cache) || length(local_cache) != 1L || !nzchar(local_cache)
  ) {
    return(tools::R_user_dir("EthnoBotaPhyWR", which = "cache"))
  }

  if (grepl("^(~|/|[A-Za-z]:)", local_cache)) {
    cache_dir <- local_cache
  } else {
    cache_dir <- file.path(getwd(), local_cache)
  }

  normalizePath(cache_dir, winslash = "/", mustWork = FALSE)
}

#' @title Package startup message
#'
#' @description
#' Emits a short startup message when the package is attached.
#'
#' @param libname Character. Library path where the package is attached.
#' @param pkgname Character. Name of the attached package.
#'
#' @keywords internal
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    sprintf("Welcome to %s v%s", pkgname, utils::packageVersion(pkgname))
  )
}
