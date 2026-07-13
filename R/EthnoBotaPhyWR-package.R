#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

.datatable.aware <- TRUE # nolint: object_name_linter. datatable convention.

normalize_ott_ids <- function(x) {
  x <- suppressWarnings(as.integer(x))
  ifelse(is.na(x), NA_character_, as.character(x))
}

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

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    sprintf("Welcome to %s v%s", pkgname, utils::packageVersion(pkgname))
  )
}
