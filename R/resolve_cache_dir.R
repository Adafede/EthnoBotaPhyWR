#' @title Resolve cache directory
#'
#' @description
#' Resolves a persistent cache directory.
#'
#' @param local_cache Character. Optional cache path. If `NULL` or empty, uses `tools::R_user_dir("EthnoBotaPhyWR", which = "cache")`.
#'
#' @return A normalized path to the cache directory.
#' @keywords internal
resolve_cache_dir <- function(local_cache = NULL) {
  if (
    is.null(local_cache) || length(local_cache) != 1L || !nzchar(local_cache)
  ) {
    return(tools::R_user_dir("EthnoBotaPhyWR", which = "cache"))
  }

  normalizePath(local_cache, winslash = "/", mustWork = FALSE)
}
