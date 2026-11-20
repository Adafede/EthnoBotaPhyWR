test_that("normalize_ott_ids handles NA and vectors", {
  expect_equal(
    pkg_internal("normalize_ott_ids")(c(1, 2, NA)),
    c("1", "2", NA_character_)
  )
})
test_that("normalize_ott_ids warns on non-numeric input", {
  expect_warning(
    pkg_internal("normalize_ott_ids")(c("1", "abc", 3)),
    "NAs introduced"
  )
})
