#' @title Get NPClassifier taxonomy
#'
#' @description This function classifies (list of) SMILES using NPClassifier API
#'
#' @param smiles A (list of) SMILES
#'
#' @return A tibble of npclassified SMILES
#'
#' @examples NULL
get_npclassifier_taxonomy <- function(smiles) {
  purrr::map(
    .progress = TRUE,
    .x = smiles,
    .f = function(x,
                  url = "https://npclassifier.gnps2.org",
                  order = "/classify?smiles=") {
      tryCatch(
        {
          url_id <- paste0(url, order, x)
          result <- rvest::read_html(url_id) |>
            rvest::html_text() |>
            jsonlite::fromJSON()

          df <- tibble::tibble(
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

          return(df)
        },
        error = function(e) {
          df <- tibble::tibble(
            query = x,
            pathway = NA,
            superclass = NA,
            class = NA,
            glycoside = NA
          )
          return(df)
        }
      )
    }
  )
}
