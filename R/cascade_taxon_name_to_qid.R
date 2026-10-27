## Vendored from cascade (https://github.com/adafede/cascade)
## Maintainer: Adriano Rutz <adafede@gmail.com>
##
## Vendored to avoid cascade's heavy dependency tree. Only the taxon name
## to QID lookup is needed here.
##
## Original file: R/taxon_name_to_qid.R in adafede/cascade

#' Taxon name to QID
#'
#' @keywords internal
#' @param taxon_name Taxon name
#'
#' @return A QID
#'
#' @examples \dontrun{
#' taxon_name_to_qid(taxon_name = "Gentiana lutea")
#' }
taxon_name_to_qid <- function(taxon_name) {
  # Fetch the SPARQL query template from remote repository
  query_template <- "https://adafede.github.io/sparql-examples/examples/Taxa/wd_taxa_name_to_qid.rq" |>
    readLines() |>
    paste0(collapse = "\n")

  # Substitute the taxon name parameter
  query <- gsub(
    pattern = "Gentiana lutea",
    replacement = taxon_name,
    x = query_template,
    fixed = FALSE
  )

  # Execute query and extract QID
  query_wikidata(sparql_query = query) |>
    tidytable::pull(taxon) |>
    gsub(pattern = "http://www.wikidata.org/entity/", replacement = "")
}
