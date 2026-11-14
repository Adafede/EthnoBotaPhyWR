#' @title Get NPClassifier taxonomy
#'
#' @description
#' Classifies a vector or list of SMILES strings using the NPClassifier API.
#' Returns a list of one-row data frames with classification results for each
#' SMILES string.
#'
#' @param smiles Character vector or list. SMILES strings to classify.
#' @param timeout Numeric. Timeout in seconds for each API request.
#'
#' @return A list of one-row data frames with columns: `query`, `pathway`,
#' `superclass`, `class`, `glycoside`.
#'
#' @details
#' Queries the NPClassifier API via a pooled `curl` handle and parses the JSON
#' payload. If an API call fails, `NA` values are returned for that entry.
#'
#' @section EDAM:
#' - Topic: `http://edamontology.org/topic_2258` (Cheminformatics)
#' - Operation: `http://edamontology.org/operation_2990` (Classification)
#' - Format: `http://edamontology.org/format_1196` (SMILES)
#'
#' @keywords internal
#'
#' @examples
#' # get_npclassifier_taxonomy(c("CCO", "CCN"))
get_npclassifier_taxonomy <- function(smiles, timeout = 30) {
  if (!is.character(smiles) && !is.list(smiles)) {
    stop("smiles must be a character vector or list")
  }

  handle <- curl::new_handle(
    timeout = as.integer(timeout),
    followlocation = 1L
  )
  base_url <- "https://npclassifier.gnps2.org/classify?smiles="

  extract_first <- function(x) {
    if (length(x) > 0L) x[[1L]] else NA
  }

  lapply(smiles, function(x) {
    if (length(x) != 1L || is.na(x) || !nzchar(trimws(as.character(x)))) {
      return(data.frame(
        query = if (length(x) == 1L) as.character(x) else NA_character_,
        pathway = NA_character_,
        superclass = NA_character_,
        class = NA_character_,
        glycoside = NA,
        stringsAsFactors = FALSE
      ))
    }
    tryCatch(
      {
        url_id <- paste0(base_url, curl::curl_escape(as.character(x)))
        resp <- curl::curl_fetch_memory(url_id, handle = handle)
        if (resp$status_code >= 400L) {
          stop("HTTP error ", resp$status_code)
        }
        result <- jsonlite::fromJSON(rawToChar(resp$content))
        data.frame(
          query = as.character(x),
          pathway = as.character(extract_first(result$pathway_results)),
          superclass = as.character(extract_first(result$superclass_results)),
          class = as.character(extract_first(result$class_results)),
          glycoside = as.logical(extract_first(result$isglycoside)),
          stringsAsFactors = FALSE
        )
      },
      error = function(e) {
        data.frame(
          query = as.character(x),
          pathway = NA_character_,
          superclass = NA_character_,
          class = NA_character_,
          glycoside = NA,
          stringsAsFactors = FALSE
        )
      }
    )
  })
}
