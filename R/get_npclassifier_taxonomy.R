#' @title Get NPClassifier taxonomy
#'
#' @description
#' Classifies a vector or list of SMILES strings using the NPClassifier API. Returns a data.frame with classification results for each SMILES.
#'
#' @param smiles Character vector or list. SMILES strings to classify.
#'
#' @return A data.frame with columns: query, pathway, superclass, class, glycoside.
#'
#' @details
#' For each SMILES, this function queries the NPClassifier API and parses the result. If the API call fails, NA values are returned for that entry.
#'
#' @examples
#' # get_npclassifier_taxonomy(c("CCO", "CCN"))
get_npclassifier_taxonomy <- function(smiles, timeout = 30) {
  # Input validation
  if (!is.character(smiles) && !is.list(smiles)) {
    stop("smiles must be a character vector or list")
  }

  purrr::map(
    .progress = TRUE,
    .x = smiles,
    .f = function(
      x,
      url = "https://npclassifier.gnps2.org",
      order = "/classify?smiles="
    ) {
      tryCatch(
        {
          # Query NPClassifier API for each SMILES string
          url_id <- paste0(url, order, utils::URLencode(x, reserved = TRUE))
          result <- rvest::read_html(url_id, timeout = timeout) |>
            rvest::html_text() |>
            jsonlite::fromJSON()

          # Parse result into tibble
          tibble::tibble(
            query = x,
            pathway = if (!purrr::is_empty(result$pathway_results)) {
              result$pathway_results
            } else {
              NA
            },
            superclass = if (!purrr::is_empty(result$superclass_results)) {
              result$superclass_results
            } else {
              NA
            },
            class = if (!purrr::is_empty(result$class_results)) {
              result$class_results
            } else {
              NA
            },
            glycoside = if (!purrr::is_empty(result$isglycoside)) {
              result$isglycoside
            } else {
              NA
            }
          )
        },
        error = function(e) {
          # On error, return NA for all fields
          tibble::tibble(
            query = x,
            pathway = NA,
            superclass = NA,
            class = NA,
            glycoside = NA
          )
        }
      )
    }
  )
}
