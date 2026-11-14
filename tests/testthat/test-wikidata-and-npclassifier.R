test_that("query_wikidata parses CSV responses and strips Wikidata entity IRIs", {
  q_wd <- get(
    "query_wikidata",
    envir = asNamespace("EthnoBotaPhyWR"),
    inherits = FALSE
  )

  testthat::local_mocked_bindings(
    curl_download = function(url, destfile, handle = NULL, quiet = TRUE) {
      writeLines(
        c(
          "taxon,taxon_name",
          "http://www.wikidata.org/entity/Q100,Dendrobium"
        ),
        destfile
      )
      destfile
    },
    .package = "curl"
  )

  res <- q_wd(
    sparql_query = "SELECT ?taxon ?taxon_name WHERE {}",
    endpoint = "https://qlever.dev/api/wikidata",
    fallback = FALSE
  )
  expect_equal(nrow(res), 1L)
  expect_identical(res$taxon, "Q100")
  expect_identical(res$taxon_name, "Dendrobium")
})

test_that("query_wikidata_with_retry retries transient HTTP 429 errors", {
  q_retry <- get(
    "query_wikidata_with_retry",
    envir = asNamespace("EthnoBotaPhyWR"),
    inherits = FALSE
  )

  state <- new.env(parent = emptyenv())
  state$attempts <- 0L
  state$attempts_err <- 0L
  testthat::local_mocked_bindings(
    query_wikidata = function(...) {
      state$attempts <- state$attempts + 1L
      if (state$attempts < 2L) {
        stop("HTTP 429 Too Many Requests")
      }
      data.frame(taxon = "Q1", stringsAsFactors = FALSE)
    },
    .package = "EthnoBotaPhyWR"
  )

  res <- q_retry(
    endpoint = "https://qlever.dev/api/wikidata",
    sparql_query = "SELECT ?taxon WHERE {}",
    max_retries = 3L,
    base_delay = 0.001
  )
  expect_identical(state$attempts, 2L)
  expect_identical(res$taxon, "Q1")

  # Non-transient error must abort immediately without retrying
  testthat::local_mocked_bindings(
    query_wikidata = function(...) {
      state$attempts_err <- state$attempts_err + 1L
      stop("Malformed syntax")
    },
    .package = "EthnoBotaPhyWR"
  )
  expect_error(
    q_retry(
      endpoint = "https://qlever.dev/api/wikidata",
      sparql_query = "BAD",
      max_retries = 3L,
      base_delay = 0.001
    ),
    "Malformed syntax"
  )
  expect_identical(state$attempts_err, 1L)
})

test_that("get_npclassifier_taxonomy parses valid payloads and returns NA on failure", {
  npc <- get(
    "get_npclassifier_taxonomy",
    envir = asNamespace("EthnoBotaPhyWR"),
    inherits = FALSE
  )

  testthat::local_mocked_bindings(
    curl_fetch_memory = function(url, handle = NULL) {
      if (grepl("BAD", url, fixed = TRUE)) {
        return(list(status_code = 500L, content = raw(0)))
      }
      payload <- list(
        pathway_results = list("Alkaloids"),
        superclass_results = list("Indole alkaloids"),
        class_results = list("Simple indole alkaloids"),
        isglycoside = list(FALSE)
      )
      list(
        status_code = 200L,
        content = charToRaw(jsonlite::toJSON(payload, auto_unbox = TRUE))
      )
    },
    .package = "curl"
  )

  out <- npc(c("CCO", "BAD_SMILES"))
  expect_length(out, 2L)
  expect_identical(out[[1L]]$pathway, "Alkaloids")
  expect_identical(out[[1L]]$glycoside, FALSE)
  expect_true(is.na(out[[2L]]$pathway))
  expect_true(is.na(out[[2L]]$glycoside))
})
