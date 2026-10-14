test_that("normalize_ott_ids handles NA and vectors", {
  x <- c("1", "2", NA, "abc", 3)
  expect_equal(
    get(
      "normalize_ott_ids",
      envir = asNamespace("EthnoBotaPhyWR"),
      inherits = FALSE
    )(c(1, 2, NA)),
    c("1", "2", NA_character_)
  )
})
