#' Get Rsequoia2 cache configuration
#'
#' Reads the cache registry from `seq_cache.yaml` and returns either the full
#' configuration or the resolved cache information for one key.
#'
#' @param key `character` Cache key to retrieve. If `NULL`, the full YAML
#' configuration is returned.
#' @param filepath `character` Optional path to a cache configuration file.
#' Mostly useful for tests.
#'
#' @return If `key = NULL`, a `list` containing the full cache configuration.
#'   Otherwise, a `list` with `label`, `path`, and `description`.
#'
#' @export
#'
seq_cache <- function(key = NULL, filepath = NULL) {

  if (is.null(filepath)) {
    filepath <- system.file("config/seq_cache.yaml", package = "Rsequoia2")
  }

  if (!file.exists(filepath)) {
    cli::cli_abort("Cache config file not found: {.file {filepath}}")
  }

  cfg <- yaml::read_yaml(filepath)
  if (is.null(cfg) || length(cfg) == 0) {
    cli::cli_abort("Cache config file is empty: {.file {filepath}}")
  }

  if (is.null(key)){
    return(cfg)
  }

  # Validate requested key
  if (!key %in% names(cfg)) {
    cli::cli_abort(c(
      "Unknown cache key.",
      "x" = "Unknown key: {.val {key}}",
      "i" = "Available keys: {.vals {names(cfg)}}"
    ))
  }

  item <- cfg[[key]]
  cache_dir <- tools::R_user_dir("Rsequoia2", which = "cache") |>
    file.path(do.call(file.path, as.list(item$cache_dir)))

  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  cache_dir <- normalizePath(cache_dir, winslash = "/", mustWork = FALSE)

  list(
    label = item$label,
    path = cache_dir,
    description = item$description
  )
}
