#' @title Query WDQS
#'
#' @description This function performs a SPARQL query against WDQS (Wikidata
#'              Query Service) or another endpoint.
#'
#' @param query A SPARQL query
#' @param remove_url Remove URL? Boolean
#' @param endpoint The endpoint used. Is prefixes are given, can use Qlever.
#' @param agent Agent to be polite
#' @param headers Call headers
#'
#' @return A dataframe containing the results of the query
#'
#' @examples NULL
query_wdqs <- function(
    query,
    remove_url = TRUE,
    endpoint = "https://query.wikidata.org/sparql",
    agent = "https://github.com/bearloga/WikidataQueryServiceR",
    headers = "application/sparql-results+json") {
  temp <- httr2::request(endpoint) |>
    httr2::req_url_query(query = query) |>
    httr2::req_user_agent(string = agent) |>
    httr2::req_headers(Accept = headers) |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  if (length(temp$results$bindings) == 0) {
    return(tidytable::tidytable(matrix(
      character(),
      nrow = 0,
      ncol = length(temp$head$vars),
      dimnames = list(c(), unlist(temp$head$vars))
    )))
  }

  bindings <- temp$results$bindings
  n_rows <- length(bindings)
  col_names <- temp$head$vars
  n_cols <- length(col_names)

  result_matrix <- matrix(
    character(n_rows * n_cols),
    nrow = n_rows,
    ncol = n_cols,
    dimnames = list(NULL, col_names)
  )

  for (j in seq_len(n_cols)) {
    col_name <- col_names[[j]]
    for (i in seq_len(n_rows)) {
      binding <- bindings[[i]][[col_name]]
      if (!is.null(binding)) {
        result_matrix[i, j] <- binding$value
      }
    }
  }

  df <- result_matrix |>
    tidytable::as_tidytable()

  if (n_rows > 0) {
    first_binding <- bindings[[1]]
    datetime_cols <- character(0)

    for (col_name in col_names) {
      binding <- first_binding[[col_name]]
      if (!is.null(binding) && !is.null(binding$datatype) &&
        binding$datatype == "http://www.w3.org/2001/XMLSchema#dateTime") {
        datetime_cols <- c(datetime_cols, col_name)
      }
    }

    # Convert datetime columns if any exist
    if (length(datetime_cols) > 0) {
      df <- df |>
        tidytable::mutate(
          tidytable::across(
            tidytable::all_of(datetime_cols),
            .fns = as.POSIXct,
            format = "%Y-%m-%dT%H:%M:%SZ",
            tz = "GMT"
          )
        )
    }
  }
  if (remove_url) {
    char_cols <- sapply(df, is.character)
    if (any(char_cols)) {
      df <- df |>
        tidytable::mutate(
          tidytable::across(
            tidytable::where(is.character),
            .fns = stringi::stri_replace_first_regex,
            pattern = "^http://www\\.wikidata\\.org/entity/",
            replacement = ""
          )
        )
    }
  }
  return(df)
}
