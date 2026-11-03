## Vendored from cascade (https://github.com/adafede/cascade)
## Maintainer: Adriano Rutz <adafede@gmail.com>
##
## Vendored to avoid cascade's heavy dependency tree. Only the taxon name
## to QID lookup is needed here.
##
## Original file: R/taxon_name_to_qid.R in adafede/cascade

#' @title Taxon name to Wikidata QID
#'
#' @description
#' Resolves a scientific taxon name (`wdt:P225`) to its Wikidata QID via the
#' QLever Wikidata SPARQL endpoint with retry backoff.
#'
#' @param taxon_name Character. Scientific name of the taxon.
#'
#' @return A character vector of matching Wikidata QIDs.
#'
#' @section EDAM:
#' - Topic: `http://edamontology.org/topic_0637` (Taxonomy)
#' - Operation: `http://edamontology.org/operation_2422` (Data retrieval)
#' - Data: `http://edamontology.org/data_1868` (Taxon)
#'
#' @keywords internal
#'
#' @examples \dontrun{
#' taxon_name_to_qid(taxon_name = "Gentiana lutea")
#' }
taxon_name_to_qid <- function(taxon_name) {
  if (
    !is.character(taxon_name) || length(taxon_name) != 1L || !nzchar(taxon_name)
  ) {
    stop("taxon_name must be a single non-empty character string")
  }

  query <- paste0(
    "PREFIX wdt: <http://www.wikidata.org/prop/direct/>\n",
    "SELECT DISTINCT ?taxon WHERE {\n",
    "  ?taxon wdt:P225 \"",
    taxon_name,
    "\".\n",
    "}\n"
  )

  query_wikidata_with_retry(
    endpoint = "https://qlever.dev/api/wikidata",
    sparql_query = query
  ) |>
    tidytable::pull(taxon) |>
    sub(
      pattern = "^https?://www\\.wikidata\\.org/entity/",
      replacement = ""
    )
}
