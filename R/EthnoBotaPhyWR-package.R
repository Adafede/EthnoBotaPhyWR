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

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    sprintf("Welcome to %s v%s", pkgname, utils::packageVersion(pkgname))
  )
}
