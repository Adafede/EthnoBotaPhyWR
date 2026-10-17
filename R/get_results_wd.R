#' @title Query Wikidata with retries
#'
#' @description
#' Runs a Wikidata query and retries transient failures such as rate limits.
#'
#' @param ... Arguments passed to `cascade::query_wikidata()`.
#' @param max_retries Integer. Maximum number of attempts before failing.
#' @param base_delay Numeric. Base delay for retry backoff.
#'
#' @return The query result returned by `cascade::query_wikidata()`.
#' @keywords internal
query_wikidata_with_retry <- function(..., max_retries = 5L, base_delay = 2) {
  attempt <- 1L
  while (attempt <= max_retries) {
    result <- tryCatch(
      cascade::query_wikidata(...),
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

#' @title Get subtaxa from Wikidata
#'
#' @description
#' Retrieves all subtaxa and their ranks for a given Wikidata taxon QID.
#'
#' @param taxon_qid Character. Wikidata QID of the focal taxon.
#'
#' @return A data frame with the retrieved taxon names and ranks.
#' @keywords internal
get_subtaxa_wd <- function(taxon_qid) {
  if (!is.character(taxon_qid) || length(taxon_qid) != 1) {
    stop("taxon_qid must be a single character string (Wikidata QID)")
  }
  # SPARQL query to get all subtaxa, their names, and ranks
  query_wikidata_with_retry(
    endpoint = "https://qlever.dev/api/wikidata",
    sparql_query = paste0(
      "
  PREFIX wd: <http://www.wikidata.org/entity/>
  PREFIX wdt: <http://www.wikidata.org/prop/direct/>
  SELECT DISTINCT ?taxon ?taxon_name ?taxon_rank WHERE {
  ?taxon (wdt:P171*) wd:",
      taxon_qid,
      ".
  ?taxon wdt:P105 ?taxon_rank.
  ?taxon wdt:P225 ?taxon_name.
  }
  "
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
#' @keywords internal
get_ott_ids_wd <- function(taxon_qid) {
  if (!is.character(taxon_qid) || length(taxon_qid) != 1) {
    stop("taxon_qid must be a single character string (Wikidata QID)")
  }
  # SPARQL query to get all subtaxa and their OTT IDs
  query_wikidata_with_retry(
    endpoint = "https://qlever.dev/api/wikidata",
    sparql_query = paste0(
      "
  PREFIX wd: <http://www.wikidata.org/entity/>
  PREFIX wdt: <http://www.wikidata.org/prop/direct/>
  SELECT DISTINCT ?taxon ?taxon_id_ott WHERE {
  ?taxon (wdt:P171*) wd:",
      taxon_qid,
      ".
  ?taxon wdt:P9157 ?taxon_id_ott.
  }
  "
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
#' @keywords internal
get_compounds_wd <- function(taxon_qid) {
  if (!is.character(taxon_qid) || length(taxon_qid) != 1) {
    stop("taxon_qid must be a single character string (Wikidata QID)")
  }
  # SPARQL query to get all compounds, their SMILES, and InChIKey for the taxon
  query_wikidata_with_retry(
    endpoint = "https://qlever.dev/api/wikidata",
    sparql_query = paste0(
      "
  PREFIX wd: <http://www.wikidata.org/entity/>
  PREFIX wdt: <http://www.wikidata.org/prop/direct/>
  SELECT DISTINCT ?compound ?compound_smiles_canonical ?compound_inchikey ?taxon
  WHERE {
  ?taxon (wdt:P171*) wd:",
      taxon_qid,
      ".
  ?compound wdt:P703 ?taxon.
  ?compound wdt:P233 ?compound_smiles_canonical.
  ?compound wdt:P235 ?compound_inchikey.
  }
  "
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
#' @keywords internal
get_uses_wd <- function(taxon_qid) {
  if (!is.character(taxon_qid) || length(taxon_qid) != 1L) {
    stop("taxon_qid must be a single character string (Wikidata QID)")
  }
  query_wikidata_with_retry(
    endpoint = "https://qlever.dev/api/wikidata",
    sparql_query = paste0(
      "
  PREFIX p: <http://www.wikidata.org/prop/>
  PREFIX ps: <http://www.wikidata.org/prop/statement/>
  PREFIX pr: <http://www.wikidata.org/prop/reference/>
  PREFIX prov: <http://www.w3.org/ns/prov#>
  PREFIX wd: <http://www.wikidata.org/entity/>
  PREFIX wdt: <http://www.wikidata.org/prop/direct/>
  SELECT DISTINCT ?taxon ?taxon_id_ott ?taxon_use ?ref WHERE {
    ?taxon (wdt:P171*) wd:",
      taxon_qid,
      ".
    ?taxon wdt:P9157 ?taxon_id_ott;
    p:P366 ?use_statement.
    ?use_statement ps:P366 ?taxon_use;
    prov:wasDerivedFrom/pr:P248 ?ref.
  }
  "
    )
  )
}

#' @title Get LOTUS data from Wikidata
#'
#' @description
#' Retrieves LOTUS compound annotations from Wikidata.
#'
#' @return A data frame with compound, SMILES, InChIKey, and taxon identifiers.
#' @keywords internal
get_lotus_wd <- function() {
  query_wikidata_with_retry(
    endpoint = "https://qlever.dev/api/wikidata",
    sparql_query = paste0(
      "
  PREFIX wdt: <http://www.wikidata.org/prop/direct/>
  SELECT DISTINCT ?compound ?compound_smiles_canonical ?compound_inchikey ?taxon
  WHERE {
  ?compound wdt:P703 ?taxon.
  ?compound wdt:P233 ?compound_smiles_canonical.
  ?compound wdt:P235 ?compound_inchikey.
  }
  "
    )
  )
}
