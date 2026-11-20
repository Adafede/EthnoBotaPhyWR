test_that("cache key is deterministic", {
  d <- data.frame(
    taxon_name = c("A", "B"),
    taxon_id_ott = c("123", "456"),
    compounds_count = c(1L, 0L),
    ethnobotanical_count = c(0L, 2L)
  )
  key_fn <- pkg_internal("get_trees_cache_key")
  k1 <- key_fn(c(123, 456), d, "Orchidaceae")
  k2 <- key_fn(c(456, 123), d, "Orchidaceae")
  expect_equal(k1, k2)
})
