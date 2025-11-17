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

  path <- list.files(dirname, pattern = "_matrice\\.xlsx$", full.names = TRUE)

  # No file
  if (length(path) == 0) {
    cli::cli_abort(c(
      "!" = "No {.val *_matrice.xlsx} file found in {.path {normalizePath(dirname)}}.",
      "i" = "See {.fn Rsequoia2::create_matrice} to generate one."
    ))
  }

  # Multiple files
  if (length(path) > 1) {
    cli::cli_abort(c(
      "!" = "Multiple {.val *_matrice.xlsx} files found in {.path {normalizePath(dirname)}}.",
      "x" = "Only one matrice file is allowed.",
      "v" = "Files found: {paste(basename(path), collapse = ', ')}"
    ))
  }

  # 2) Read the file
  matrice <- openxlsx2::read_xlsx(path)

  # Missing IDENTIFIANT column
  if (!"IDENTIFIANT" %in% names(matrice)) {
    cli::cli_abort(c(
      "!" = "Column {.field IDENTIFIANT} is missing in {.file {basename(path)}}."
    ))
  }

  # Extract ID
  id <- unique(matrice$IDENTIFIANT)
  id <- id[!is.na(id) & nzchar(trimws(id))]   # remove NA + empty + spaces-only

  # Empty IDs
  if (length(id) == 0) {
    cli::cli_abort(c(
      "!" = "Column {.field IDENTIFIANT} is empty."
    ))
  }

  # Multiple distinct IDs
  if (length(id) > 1) {
    cli::cli_abort(c(
      "!" = "Multiple IDs detected in column {.field IDENTIFIANT}.",
      "x" = "Only one unique ID is expected.",
      "v" = "IDs found: {paste(id, collapse = ', ')}"
    ))
  }

  # Success
  if (verbose){
    cli::cli_inform(c(
      "v" = "Detected forest ID {.val {id}} from {.file {basename(path)}}."
    ))
  }

  return(invisible(id))
}

#' Construct filepath for a given layer key
#'
#' Builds the expected output filename for a given layer key by combining:
#' (1) the forest ID extracted from the local *_matrice.xlsx file, and
#' (2) the corresponding layer definition stored in `inst/config/seq_layer.yaml`.
#'
#' @param key `character`
#' @param dirname `character` Directory where the matrice file is located.
#' Defaults to the current working directory.
#' @param verbose `logical` If `TRUE`, display messages.
#'
#' @details
#' The function resolves the input `key` using **partial matching** against
#' keys defined in `inst/config/seq_layer.yaml`.
#'
#' If exactly **one** entry matches, it is selected : for example,
#' `key = "znieff1"` can be used to match `v.inpn.znieff1.poly` because only
#' one key contain `"znieff1`
#'
#' If **multiple** entries match, the user is shown the ambiguous options and
#' must provide a more specific key.
#'
#' Note: **`seq_layer.yaml` is part of the package and should not be modified.**
#'
#' @return Invisibly returns a single forest ID (character scalar).
#'
get_path <- function(key, dirname = ".", verbose = FALSE){

  cfg_path <- system.file("config/seq_layer.yaml", package = "Rsequoia2")
  cfg <- yaml::read_yaml(cfg_path)

  all_key <- names(cfg)
  match_key <- grepv(key, all_key)

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
      "i" = "Valid keys are defined in {.path inst/config/seq_layer.yaml}.",
      "!" = red_warn("This file is part of the package and must not be modified.")
    ))
  }

  entry <- cfg[[match_key]]
  id <- get_id(dirname)
  filename <- sprintf("%s_%s.%s", id, entry$name, entry$ext)
  path <- file.path(dirname, filename)

  if (verbose) {
    cli::cli_alert_success(
      "Resolved {.arg key} {.val {key}} to {.file {filename}}."
    )
  }

  return(path)
}
