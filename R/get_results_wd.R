#' @title Query Wikidata with retries
#'
#' @description
#' Runs a Wikidata query and retries transient failures such as rate limits.
#'
#' @param ... Arguments passed to `query_wikidata()`.
#' @param max_retries Integer. Maximum number of attempts before failing.
#' @param base_delay Numeric. Base delay for retry backoff.
#'
#' @return The query result returned by `query_wikidata()`.
#'
#' @section EDAM:
#' - Topic: `http://edamontology.org/topic_3077` (Data acquisition)
#' - Operation: `http://edamontology.org/operation_2422` (Data retrieval)
#'
#' @keywords internal
query_wikidata_with_retry <- function(..., max_retries = 5L, base_delay = 2) {
  attempt <- 1L
  while (attempt <= max_retries) {
    result <- tryCatch(
      query_wikidata(...),
      error = function(e) {
        if (
          grepl(
            "429|Too Many Requests|rate limit|HTTP",
            e$message,
            ignore.case = TRUE
          ) &&
            attempt < max_retries
        ) {
          logger::log_warn(
            "Wikidata request failed with a transient error; retrying in ",
            base_delay * attempt,
            " seconds"
          )
          Sys.sleep(base_delay * attempt)
          NULL
        } else {
          stop(e)
        }
      }
    )
    if (!is.null(result)) {
      return(result)
    }
    attempt <- attempt + 1L
  }
  stop("Wikidata query failed after retries")
}

validate_taxon_qid <- function(taxon_qid) {
  if (!is.character(taxon_qid) || length(taxon_qid) != 1L) {
    stop("taxon_qid must be a single character string (Wikidata QID)")
  }
}

#' @title Get subtaxa from Wikidata
#'
#' @description
#' Retrieves all subtaxa and their ranks for a given Wikidata taxon QID.
#'
#' @param taxon_qid Character. Wikidata QID of the focal taxon.
#'
#' @return A data frame with the retrieved taxon names and ranks.
#'
#' @section EDAM:
#' - Topic: `http://edamontology.org/topic_0637` (Taxonomy)
#' - Operation: `http://edamontology.org/operation_2422` (Data retrieval)
#' - Data: `http://edamontology.org/data_1868` (Taxon)
#'
#' @keywords internal
get_subtaxa_wd <- function(taxon_qid) {
  validate_taxon_qid(taxon_qid)
  query_wikidata_with_retry(
    endpoint = "https://qlever.dev/api/wikidata",
    sparql_query = paste0(
      "PREFIX wd: <http://www.wikidata.org/entity/>\n",
      "PREFIX wdt: <http://www.wikidata.org/prop/direct/>\n",
      "SELECT DISTINCT ?taxon ?taxon_name ?taxon_rank WHERE {\n",
      "  ?taxon (wdt:P171*) wd:",
      taxon_qid,
      ".\n",
      "  ?taxon wdt:P105 ?taxon_rank.\n",
      "  ?taxon wdt:P225 ?taxon_name.\n",
      "}\n"
    )
  )
}

#' @title Get OTT IDs from Wikidata
#'
#' @description
#' Retrieves the Open Tree of Life identifiers for all subtaxa of the given
#' Wikidata taxon.
#'
#' @param taxon_qid Character. Wikidata QID of the focal taxon.
#'
#' @return A data frame with the retrieved taxon identifiers and OTT IDs.
#'
#' @section EDAM:
#' - Topic: `http://edamontology.org/topic_3293` (Phylogenetics)
#' - Operation: `http://edamontology.org/operation_2422` (Data retrieval)
#' - Data: `http://edamontology.org/data_1868` (Taxon)
#'
#' @keywords internal
get_ott_ids_wd <- function(taxon_qid) {
  validate_taxon_qid(taxon_qid)
  query_wikidata_with_retry(
    endpoint = "https://qlever.dev/api/wikidata",
    sparql_query = paste0(
      "PREFIX wd: <http://www.wikidata.org/entity/>\n",
      "PREFIX wdt: <http://www.wikidata.org/prop/direct/>\n",
      "SELECT DISTINCT ?taxon ?taxon_id_ott WHERE {\n",
      "  ?taxon (wdt:P171*) wd:",
      taxon_qid,
      ".\n",
      "  ?taxon wdt:P9157 ?taxon_id_ott.\n",
      "}\n"
    )
  )
}

#' @title Get compounds from Wikidata
#'
#' @description
#' Retrieves compound annotations for a given Wikidata taxon QID.
#'
#' @param taxon_qid Character. Wikidata QID of the focal taxon.
#'
#' @return A data frame with compound, SMILES, InChIKey, and taxon identifiers.
#'
#' @section EDAM:
#' - Topic: `http://edamontology.org/topic_2258` (Cheminformatics)
#' - Operation: `http://edamontology.org/operation_2422` (Data retrieval)
#' - Data: `http://edamontology.org/data_1463` (Small molecule structure)
#' - Format: `http://edamontology.org/format_1196` (SMILES),
#'   `http://edamontology.org/format_1199` (InChIKey)
#'
#' @keywords internal
get_compounds_wd <- function(taxon_qid) {
  validate_taxon_qid(taxon_qid)
  query_wikidata_with_retry(
    endpoint = "https://qlever.dev/api/wikidata",
    sparql_query = paste0(
      "PREFIX wd: <http://www.wikidata.org/entity/>\n",
      "PREFIX wdt: <http://www.wikidata.org/prop/direct/>\n",
      "SELECT DISTINCT ?compound ?compound_smiles_canonical ",
      "?compound_inchikey ?taxon\n",
      "WHERE {\n",
      "  ?taxon (wdt:P171*) wd:",
      taxon_qid,
      ".\n",
      "  ?compound wdt:P703 ?taxon.\n",
      "  ?compound wdt:P233 ?compound_smiles_canonical.\n",
      "  ?compound wdt:P235 ?compound_inchikey.\n",
      "}\n"
    )
  )
}

#' @title Get ethnobotanical uses from Wikidata
#'
#' @description
#' Retrieves ethnobotanical-use annotations for a given Wikidata taxon QID.
#'
#' @param taxon_qid Character. Wikidata QID of the focal taxon.
#'
#' @return A data frame with the retrieved use information.
#'
#' @section EDAM:
#' - Topic: `http://edamontology.org/topic_0780` (Plant biology)
#' - Operation: `http://edamontology.org/operation_2422` (Data retrieval)
#'
#' @keywords internal
get_uses_wd <- function(taxon_qid) {
  validate_taxon_qid(taxon_qid)
  query_wikidata_with_retry(
    endpoint = "https://qlever.dev/api/wikidata",
    sparql_query = paste0(
      "PREFIX p: <http://www.wikidata.org/prop/>\n",
      "PREFIX ps: <http://www.wikidata.org/prop/statement/>\n",
      "PREFIX pr: <http://www.wikidata.org/prop/reference/>\n",
      "PREFIX prov: <http://www.w3.org/ns/prov#>\n",
      "PREFIX wd: <http://www.wikidata.org/entity/>\n",
      "PREFIX wdt: <http://www.wikidata.org/prop/direct/>\n",
      "SELECT DISTINCT ?taxon ?taxon_id_ott ?taxon_use ?ref WHERE {\n",
      "  ?taxon (wdt:P171*) wd:",
      taxon_qid,
      ".\n",
      "  ?taxon wdt:P9157 ?taxon_id_ott;\n",
      "  p:P366 ?use_statement.\n",
      "  ?use_statement ps:P366 ?taxon_use;\n",
      "  prov:wasDerivedFrom/pr:P248 ?ref.\n",
      "}\n"
    )
  )
}

#' @title Get LOTUS data from Wikidata
#'
#' @description
#' Retrieves LOTUS compound annotations from Wikidata.
#'
#' @return A data frame with compound, SMILES, InChIKey, and taxon identifiers.
#'
#' @section EDAM:
#' - Topic: `http://edamontology.org/topic_2258` (Cheminformatics)
#' - Operation: `http://edamontology.org/operation_2422` (Data retrieval)
#' - Data: `http://edamontology.org/data_1463` (Small molecule structure)
#'
#' @keywords internal
get_lotus_wd <- function() {
  query_wikidata_with_retry(
    endpoint = "https://qlever.dev/api/wikidata",
    sparql_query = paste0(
      "PREFIX wdt: <http://www.wikidata.org/prop/direct/>\n",
      "SELECT DISTINCT ?compound ?compound_smiles_canonical ",
      "?compound_inchikey ?taxon\n",
      "WHERE {\n",
      "  ?compound wdt:P703 ?taxon.\n",
      "  ?compound wdt:P233 ?compound_smiles_canonical.\n",
      "  ?compound wdt:P235 ?compound_inchikey.\n",
      "}\n"
    )
  )
}
