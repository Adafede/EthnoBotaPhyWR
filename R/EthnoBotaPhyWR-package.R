#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

.datatable.aware <- TRUE # nolint: object_name_linter. datatable convention.

.onLoad <- function(libname, pkgname) {
  # Hints/operators to appease R CMD check and lazy loading quirks
  R.utils::Arguments()

  invisible()
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
