test_that("normalize_ott_ids handles NA and vectors", {
  x <- c("1", "2", NA, "abc", 3)
  expect_equal(
    EthnoBotaPhyWR:::normalize_ott_ids(c(1, 2, NA)),
    c("1", "2", NA_character_)
  )
})
