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

#' Report Rsequoia2 cache usage
#'
#' Prints a small CLI summary of the total cache size and the size of each
#' configured cache folder.
#'
#' @param filepath `character`. Optional path to a cache configuration file.
#'   Mostly useful for tests. If `NULL`, the package cache configuration is used.
#'
#' @return Invisibly returns a `data.frame` with cache key, label, path, size in
#'   bytes, and formatted size.
#'
#' @export
seq_cache_report <- function(filepath = NULL) {

  cfg <- seq_cache(filepath = filepath)

  dir_size <- function(path) {
    if (!dir.exists(path)) {
      return(0)
    }

    files <- list.files(
      path,
      recursive = TRUE,
      full.names = TRUE,
      all.files = TRUE,
      no.. = TRUE,
      include.dirs = FALSE
    )

    files <- files[!dir.exists(files)]

    sum(file.info(files)$size, na.rm = TRUE)
  }

  format_size <- function(x) {
    format(
      structure(x, class = "object_size"),
      units = "auto"
    )
  }

  report <- lapply(names(cfg), function(key) {

    cache <- seq_cache(key, filepath = filepath)
    size <- dir_size(cache$path)

    data.frame(
      key = key,
      label = cache$label,
      path = cache$path,
      size = size,
      size_pretty = format_size(size),
      stringsAsFactors = FALSE
    )
  })

  report <- do.call(rbind, report)
  total <- sum(report$size, na.rm = TRUE)

  label_width <- max(nchar(report$label), na.rm = TRUE)

  cli::cli_div(theme = list(
    span.cache_total = list(color = "green", "font-weight" = "bold"),
    span.cache_size = list(color = "cyan")
  ))

  cli::cli_h1("Rsequoia2 cache")

  cli::cli_text(
    "Total: {.cache_total {format_size(total)}}"
  )

  cli::cli_h2("Detail")
  for (i in seq_len(nrow(report))) {
    cli::cli_text(
      " {.cache_label {report$label[i]}}: {.cache_size {report$size_pretty[i]}}"
    )
  }

  cli::cli_end()

  return(invisible(report))
}
