#' Extract forest ID from a matrice.xlsx file
#'
#' Searches for a single Excel file matching `*_matrice.xlsx` in a directory,
#' reads its `IDENTIFIANT` column, and returns the unique forest ID.
#'
#' @param dirname `character` Directory where the matrice file is located.
#' Defaults to the current working directory.
#' @param verbose `logical` If `TRUE`, display messages.
#'
#' @return Invisibly returns a single forest ID (character scalar).
#'
#' @export
get_id <- function(dirname = ".", verbose = FALSE) {

  m <- read_matrice(dirname)
  id <- unique(m$IDENTIFIANT)

  # Success
  if (verbose){
    cli::cli_inform(c(
      "v" = "Detected forest ID {.val {id}}."
    ))
  }

  return(invisible(id))
}

#' Construct filepath for a given layer key
#'
#' Builds the expected output filename for a given layer key by combining:
#' (1) the forest ID extracted from the local *_matrice.xlsx file, and
#' (2) the corresponding layer definition stored in `inst/config/seq_layers.yaml`.
#'
#' @param key `character` Name of a layer key to match against the entries
#' defined in `inst/config/seq_layers.yaml`. (see *Details* for partial matching).
#' @param dirname `character` Directory where the matrice file is located.
#' Defaults to the current working directory.
#' @param verbose `logical` If `TRUE`, display messages.
#'
#' @details
#' The function resolves the input `key` using **partial matching** against
#' keys defined in `inst/config/seq_layers.yaml`.
#'
#' If exactly **one** entry matches, it is selected : for example,
#' `key = "znieff1"` can be used to match `v.mnhn.znieff1.poly` because only
#' one key contain `"znieff1`
#'
#' If **multiple** entries match, the user is shown the ambiguous options and
#' must provide a more specific key.
#'
#' Note: **`seq_layers.yaml` is part of the package and should not be modified.**
#'
#' @return Invisibly returns a single forest ID (character scalar).
#'
get_path <- function(key, dirname = ".", verbose = FALSE){

  cfg_path <- system.file("config/seq_layers.yaml", package = "Rsequoia2")
  cfg <- yaml::read_yaml(cfg_path)

  all_key <- names(cfg)
  match_key <- grep(key, all_key, value = TRUE)

  if (length(match_key) > 1){
    cli::cli_abort(
      c(
        "!" = "Multiple match for {.arg key} {.val {key}} :",
        "i" = cli::cli_fmt(cli::cli_ul(match_key))
      )
    )
  }

  if (length(match_key) == 0){
    red_warn <- cli::combine_ansi_styles("red", "bold")
    cli::cli_abort(c(
      "x" = "{.arg key} {.val {key}} does not exist.",
      "i" = "Valid keys are defined in {.path inst/config/seq_layers.yaml}.",
      "!" = red_warn("This file is part of the package and must not be modified.")
    ))
  }

  entry <- cfg[[match_key]]
  id <- get_id(dirname)
  filename <- sprintf("%s_%s.%s", id, entry$name, entry$ext)
  path <- file.path(dirname, filename)
  names(path) <- match_key

  if (verbose) {
    cli::cli_alert_success(
      "Resolved {.arg key} {.val {key}} to {.file {filename}}."
    )
  }

  return(path)
}

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

#' Load layer config from the Sequoia configuration
#'
#' Internal helper used to determine layer info like name or extension are stored
#' in `inst/config/seq_layerss.yaml`.
#'
#' @inheritParams get_keys
#'
#' @return A list describing matched layer
#'
#' @examples
#' \dontrun{
#' # List all available tables
#' names(seq_table())
#'
#' # Load field keys for the "parcelle" table
#' seq_table("parca")
#' }
#'
seq_layer <- function(pattern = NULL, filepath = NULL){
  # Use test path if provided
  if (is.null(filepath)) {
    filepath <- system.file("config/seq_layers.yaml", package = "Rsequoia2")
  }

  cfg <- yaml::read_yaml(filepath)

  if (is.null(pattern)){
    return(cfg)
  }

  key <- get_keys(pattern, reduce = FALSE)

  return(cfg[[key]])

}
