#' @title Get subtaxa WD
#'
#' @description This function gets subtaxa from Wikidata.
#'
#' @include query_wdqs.R
#'
#' @param taxon_qid A taxon QID
#'
#' @return A dataframe containing the results of the query
#'
#' @examples NULL
get_subtaxa_wd <- function(taxon_qid) {
  query_wdqs(
    endpoint = "https://qlever.cs.uni-freiburg.de/api/wikidata",
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

#' @title Get OTT IDs WD
#'
#' @description This function gets OTT IDs from Wikidata.
#'
#' @include query_wdqs.R
#'
#' @param taxon_qid A taxon QID
#'
#' @return A dataframe containing the results of the query
#'
#' @examples NULL
get_ott_ids_wd <- function(taxon_qid) {
  query_wdqs(
    endpoint = "https://qlever.cs.uni-freiburg.de/api/wikidata",
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

#' @title Get compounds WD
#'
#' @description This function gets compounds from Wikidata.
#'
#' @include query_wdqs.R
#'
#' @param taxon_qid A taxon QID
#'
#' @return A dataframe containing the results of the query
#'
#' @examples NULL
get_compounds_wd <- function(taxon_qid) {
  query_wdqs(
    endpoint = "https://qlever.cs.uni-freiburg.de/api/wikidata",
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
    endpoint = "https://qlever.cs.uni-freiburg.de/api/wikidata",
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
    endpoint = "https://qlever.cs.uni-freiburg.de/api/wikidata",
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
