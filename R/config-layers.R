#' Helper to find layer configuration keys
#'
#' This helper function searches for keys defined in `inst/config/seq_layers.yaml`.
#' Keys are matched using a regular-expression `pattern`.
#'
#'
#' @param pattern `character` or `NULL`; regular-expression pattern used to
#' filter keys. If `NULL`, all keys defined in the configuration are returned.
#' @param reduce `logical`; if `TRUE` (default), reduce each matching key.
#' @param filepath `character` or `NULL`; optional override for the path to the
#' YAML configuration file. Mainly used for testing purposes.
#'
#' @details
#' **Reduction process**
#' Configuration keys typically follow a three/four-part structure such as
#' `"v.seq.parca.poly"` or `"r.ortho.irc"`. When `reduce = TRUE`, each key is
#' split on `"."` and only the third element is returned (s). This provides a
#' short, semantic identifier for internal use.
#'
#' ```
#' "v.seq.parca.poly" → "parca"
#' "r.ortho.irc"      → "irc"
#' ```
#'
#' If multiple full keys reduce to the same name, the function aborts to
#' prevent ambiguity.
#'
#' @return
#' A character vector of matching keys.
#' If `reduce = TRUE`, the returned keys are the reduced forms.
#' If `pattern = NULL`, all configuration keys are returned.
#'
#' @examples
#' \dontrun{
#' # List all available layer keys
#' get_keys()
#'
#' # List full keys matching "parca"
#' get_keys("parca", reduce = FALSE)
#'
#' # Return reduced keys (3rd element of each dotted key)
#' get_keys("parca", reduce = TRUE)
#' }
#'
#' @export
get_keys <- function(pattern = NULL, reduce = TRUE, filepath = NULL){

  # Resolve config path
  if (is.null(filepath)) {
    filepath <- system.file("config/seq_layers.yaml", package = "Rsequoia2")
  }

  cfg <- yaml::read_yaml(filepath)

  # Return all keys if no pattern is provided
  if (is.null(pattern)){
    return(names(cfg))
  }

  # Filter matching keys
  keys <- grep(pattern, names(cfg), value = TRUE)

  # No match → informative abort
  if (length(keys) == 0){
    red_warn <- cli::combine_ansi_styles("red", "bold")
    cli::cli_abort(c(
      "x" = "{.arg pattern} {.val {pattern}} does not exist.",
      "i" = "Valid keys are defined in {.path inst/config/seq_layers.yaml}.",
      "!" = red_warn("This file is part of the package and must not be modified.")
    ))
  }

  # Reduction logic
  if (reduce) {

    reduced <- vapply(strsplit(keys, "\\."), `[`, character(1), 3)

    # Detect duplicates after reduction
    if (anyDuplicated(reduced)) {
      dupes <- unique(reduced[duplicated(reduced)])
      cli::cli_abort(c(
        "x" = "Key reduction produced duplicated names.",
        "!" = "Duplicated reduced names: {.val {dupes}}",
        "i" = "Disable with {.code reduce = FALSE}."
      ))
    }

    return(reduced)
  }

  # If reduce = FALSE
  return(keys)
}

#' Retrieve layer metadata from the Sequoia configuration
#'
#' Resolves a layer key against the config defined in
#' `inst/config/seq_layers.yaml` and `inst/config/seq_path.yaml` and returns
#' the associated metadata: name, extension, filename, path, fullpath
#'
#' @param key `character` Name of a layer key to match against the entries
#' defined in `inst/config/seq_layers.yaml`. (see *Details* for partial matching).
#' @param verbose `logical` If `TRUE`, display messages.
#'
#' @details
#' The function resolves the input `key` using **partial matching** against
#' the keys defined in `inst/config/seq_layers.yaml`.
#'
#' - If exactly **one** entry matches, it is selected.
#'   For example, `key = "znieff1"` can be used to match
#'   `"v.mnhn.znieff1.poly"` when this is the only key containing `"znieff1"`.
#'
#' - If **multiple** entries match, the function aborts and displays the
#'   ambiguous keys, prompting the user to provide a more specific key.
#'
#' Note: **`seq_layers.yaml` is part of the package and must not be modified.**
#'
#' @return A named list containing layer metadata, including:
#' \itemize{
#'   \item \code{key}: the resolved configuration key
#'   \item \code{name}: the layer name
#'   \item \code{ext}: the file extension
#'   \item \code{family}: the resolved namespace family
#'   \item \code{path}: the base directory for the layer
#'   \item \code{full_path}: the complete filesystem path to the layer
#' }
#'
#' @examples
#' \dontrun{
#' # Retrieve metadata for a layer
#' l <- seq_layer("parca")
#'
#' l$full_path
#' }
#'
#' @export
#'
seq_layer <- function(key, verbose = FALSE){

  cfg_layer <- system.file("config/seq_layers.yaml", package = "Rsequoia2") |>
    yaml::read_yaml()

  cfg_path <- system.file("config/seq_path.yaml", package = "Rsequoia2") |>
      yaml::read_yaml()

  match_key <- seq_key(key, allow_multiple = FALSE)

  # metadata fetching ----
  layer <- cfg_layer[[match_key]]
  filename <- sprintf("%s.%s", layer$name, layer$ext)

  family <- cfg_path$namespace[startsWith(match_key, names(cfg_path$namespace))]
  family <- unname(unlist(family))

  path <- if (!is.null(family)) cfg_path$path[[family]] else NULL
  full_path <- if (!is.null(family)) file.path(path, filename) else NULL

  type <- switch(
    sub("\\..*$", "", match_key),
    v = "vect",
    r = "rast",
    x = "xlsx",
    NA
  )

  layer_metadata <- list(
    key = match_key,
    name = layer$name,
    ext = layer$ext,
    filename = filename,
    family = family,
    path = path,
    full_path = full_path,
    type = type
  )

  if (verbose) {
    cli::cli_alert_success(
      "Resolved {.arg key} {.val {key}} to {.file {full_path}}."
    )
  }

  return(layer_metadata)
}

#' Resolve a layer configuration key
#'
#' Resolves a user-provided key or pattern against the keys defined in
#' `inst/config/seq_layers.yaml`.
#'
#' @param key `character` Search pattern to resolve.
#' @param allow_multiple `logical` If `TRUE`, allow and return multiple matches.
#'
#' @return A character vector of matching configuration keys.
#'
#' @details
#' Partial matching is used. If no key matches, the function aborts.
#' If multiple keys match and `allow_multiple = FALSE`, the function aborts.
#'
#' @export
seq_key <- function(key = NULL, allow_multiple = FALSE){

  cfg_layer <- system.file("config/seq_layers.yaml", package = "Rsequoia2") |>
    yaml::read_yaml()

  all_keys <- names(cfg_layer)
  if (is.null(key)){
    return(all_keys)
  }
  keys <- grep(key, all_keys, value = TRUE)

  no_match <- length(keys) == 0
  if (no_match){
    red_warn <- cli::combine_ansi_styles("red", "bold")
    cli::cli_abort(c(
      "x" = "{.arg key} {.val {key}} does not exist.",
      "i" = "Valid keys are defined in {.path inst/config/seq_layers.yaml}.",
      "!" = red_warn("This file is part of the package and must not be modified.")
    ))
  }

  if (!allow_multiple){
    multiple_match <- length(keys) > 1
    if (multiple_match){
      cli::cli_abort(c(
        "!" = "Multiple matches for {.arg key} {.val {key}} :",
        "i" = cli::cli_fmt(cli::cli_ul(sprintf("{.val %s}", keys)))
      ))
    }
  }

  return(keys)
}
