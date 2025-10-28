#' @title Get subtaxa from Wikidata
#'
#' @description
#' Retrieves all subtaxa for a given taxon QID from Wikidata using a SPARQL query.
#'
#' @param taxon_qid Character. Wikidata QID of the taxon.
#'
#' @return A data.frame with columns: taxon, taxon_name, taxon_rank.
#'
#' @examples
#' # get_subtaxa_wd("Q25308")
get_subtaxa_wd <- function(taxon_qid) {
  if (!is.character(taxon_qid) || length(taxon_qid) != 1) {
    stop("taxon_qid must be a single character string (Wikidata QID)")
  }
  # SPARQL query to get all subtaxa, their names, and ranks
  query_wdqs(
    endpoint = "https://qlever.dev/api/wikidata",
    query = paste0(
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
#' Retrieves Open Tree of Life (OTT) IDs for all subtaxa of a given taxon QID from Wikidata.
#'
#' @param taxon_qid Character. Wikidata QID of the taxon.
#'
#' @return A data.frame with columns: taxon, taxon_id_ott.
#'
#' @examples
#' # get_ott_ids_wd("Q25308")
get_ott_ids_wd <- function(taxon_qid) {
  if (!is.character(taxon_qid) || length(taxon_qid) != 1) {
    stop("taxon_qid must be a single character string (Wikidata QID)")
  }
  # SPARQL query to get all subtaxa and their OTT IDs
  query_wdqs(
    endpoint = "https://qlever.dev/api/wikidata",
    query = paste0(
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
#' Retrieves all compounds associated with a given taxon QID from Wikidata.
#'
#' @param taxon_qid Character. Wikidata QID of the taxon.
#'
#' @return A data.frame with columns: compound, compound_smiles_canonical, compound_inchikey, taxon.
#'
#' @examples
#' # get_compounds_wd("Q25308")
get_compounds_wd <- function(taxon_qid) {
  if (!is.character(taxon_qid) || length(taxon_qid) != 1) {
    stop("taxon_qid must be a single character string (Wikidata QID)")
  }
  # SPARQL query to get all compounds, their SMILES, and InChIKey for the taxon
  query_wdqs(
    endpoint = "https://qlever.dev/api/wikidata",
    query = paste0(
      "
  PREFIX wd: <http://www.wikidata.org/entity/>
  PREFIX wdt: <http://www.wikidata.org/prop/direct/>
  SELECT DISTINCT ?compound ?compound_smiles_canonical ?compound_inchikey ?taxon WHERE {
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

#' @title Get uses WD
#'
#' @description This function gets uses from Wikidata.
#'
#' @include query_wdqs.R
#'
#' @param taxon_qid A taxon QID
#'
#' @return A dataframe containing the results of the query
#'
#' @examples NULL
get_uses_wd <- function(taxon_qid) {
  query_wdqs(
    endpoint = "https://qlever.dev/api/wikidata",
    query = paste0(
      "
  PREFIX p: <http://www.wikidata.org/prop/>
  PREFIX ps: <http://www.wikidata.org/prop/statement/>
  PREFIX pr: <http://www.wikidata.org/prop/reference/>
  PREFIX prov: <http://www.w3.org/ns/prov#>
  PREFIX wd: <http://www.wikidata.org/entity/>
  PREFIX wdt: <http://www.wikidata.org/prop/direct/>
  SELECT DISTINCT ?taxon ?taxon_id_ott ?taxon_use ?ref WHERE {
    ?taxon (wdt:P171*) wd:Q25308;
    wdt:P9157 ?taxon_id_ott;
    p:P366 ?use_statement.
    ?use_statement ps:P366 ?taxon_use;
    prov:wasDerivedFrom/pr:P248 ?ref.
  }
  "
    )
  )
}

#' @title Get LOTUS WD
#'
#' @description This function gets LOTUS from Wikidata.
#'
#' @include query_wdqs.R
#'
#' @return A dataframe containing the results of the query
#'
#' @examples NULL
get_lotus_wd <- function() {
  query_wdqs(
    endpoint = "https://qlever.dev/api/wikidata",
    query = paste0(
      "
  PREFIX wdt: <http://www.wikidata.org/prop/direct/>
  SELECT DISTINCT ?compound ?compound_smiles_canonical ?compound_inchikey ?taxon WHERE {
  ?compound wdt:P703 ?taxon.
  ?compound wdt:P233 ?compound_smiles_canonical.
  ?compound wdt:P235 ?compound_inchikey.
  }
  "
    )
  )
}
