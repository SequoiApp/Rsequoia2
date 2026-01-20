#' Open file based on key name
#'
#' @param key `character` Name of a layer key to match against the entries
#' defined in `inst/config/seq_layers.yaml`. (see *Details* for partial matching).
#' @param dirname `character` Directory where the matrice file is located.
#' Defaults to the current working directory.
#' @param verbose `logical` If `TRUE`, display messages.
#'
#' @importFrom sf read_sf
#' @importFrom terra rast
#'
#' @return Object of class `SpatRaster` for raster and `sf` for vector
#'
#' @export
seq_read <- function(key, dirname = ".", verbose = FALSE) {

  layer_info <- seq_layer(key, verbose = FALSE)
  filename <- layer_info$filename
  full_key <- layer_info$key
  type <- layer_info$type

  # recursive search on layer name only
  # \\. is necessary to avoid multi match on UA SEQ_UA_poly.geojson & SEQ_UA_poly_20260116T135813.geojson
  path <- list.files(
    path = dirname,
    pattern = filename,
    ignore.case = TRUE,
    full.names = TRUE,
    recursive = TRUE
  )

  no_match <- length(path) == 0
  if (no_match) {
    cli::cli_abort("Layer {.file {filename}} doesn't exist.")
  }

  multiple_match <- length(path) > 1
  if (multiple_match) {
    cli::cli_abort(c(
      "!" = "Multiple layer {.file {filename}} found:",
      "i" = cli::cli_fmt(cli::cli_ul(sprintf("{.file %s}", path)))
    ))
  }

  is_vector <- (type == "vect")
  if (is_vector) {
    v <- sf::read_sf(path)
    if (verbose) {
      cli::cli_alert_success(
        "Loaded vector layer {.val {key}} from {.file {basename(path)}}."
      )
    }
    return(invisible(v))
  }

  is_raster <- (type == "rast")
  if (is_raster) {
    r <- terra::rast(path)
    if (verbose) {
      cli::cli_alert_success(
        "Loaded raster layer {.val {key}} from {.file {basename(path)}}."
      )
    }
    return(invisible(r))
  }

  is_xlsx <- (type == "xlsx")
  if (is_xlsx) {
    x <- openxlsx2::read_xlsx(path, na.strings = "", skip_empty_rows = TRUE, skip_empty_cols = TRUE)
    if (verbose) {
      cli::cli_alert_success(
        "Loaded xlsx table {.val {key}} from {.file {basename(path)}}."
      )
    }
    return(invisible(x))
  }
}

#' Write a spatial object based on a layer key
#'
#' @inheritParams seq_read
#' @param id `character` Identifier of the project that will be added to the filename.
#' Default to `NULL` i.e. no identifier added.
#' @param x An `sf` object (for vector outputs) or a `SpatRaster` (for raster outputs).
#' @param overwrite `logical` If `TRUE`, file is overwritten.
#'
#' @return Invisibly returns the filepath used for writing.
#'
#' @importFrom sf write_sf
#' @importFrom terra writeRaster
#'
#' @export
seq_write <- function(x, key, dirname = ".", id = NULL, verbose = FALSE, overwrite = FALSE) {

  layer_info <- seq_layer(key, verbose = FALSE)
  key <- layer_info$key
  filename <- layer_info$filename
  relative_path <- layer_info$path
  type <- layer_info$type

  if (!is.null(id)){
    filename <- sprintf("%s_%s", id, filename)
  }

  full_path <- if (is.null(relative_path)) filename else file.path(relative_path, filename)
  path <- file.path(dirname, full_path)
  names(path) <- key

  if (file.exists(path) && !overwrite) {
    cli::cli_warn(
      c(
        "!" = "{.file {basename(path)}} already exists.",
        "i" = "Use {.arg overwrite = TRUE} to replace it."
      )
    )
    return(invisible(path))
  }

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  is_vector <- type == "vect"
  if (is_vector) {
    if (!inherits(x, c("sf", "sfc"))) {
      cli::cli_abort(c(
        "!" = "Object supplied for {.arg x} is not an {.cls sf} object.",
        "i" = "Vector layers must be written using {.val v.*} keys."
      ))
    }

    sf::write_sf(x, path, delete_dsn = overwrite)

    if (verbose) {
      cli::cli_alert_success(
        "Layer {.val {key}} with {nrow(x)} feature{?s} saved to {.file {full_path}}."
      )
    }

    return(invisible(path))
  }

  is_raster <- type == "rast"
  if (is_raster) {
    if (!inherits(x, "SpatRaster")) {
      cli::cli_abort(c(
        "!" = "Object supplied for {.arg x} is not a {.cls SpatRaster} object.",
        "i" = "Raster layers must be written using {.val r.*} keys."
      ))
    }

    terra::writeRaster(x, path, overwrite = overwrite)

    if (verbose) {
      cli::cli_alert_success(
        "Layer {.val {key}} saved to {.file {full_path}}."
      )
    }

    return(invisible(path))
  }

  is_xlsx <- type == "xlsx"
  if (is_xlsx) {
    if (!inherits(x, "data.frame")) {
      cli::cli_abort(c(
        "!" = "Object supplied for {.arg x} is not a {.cls data.frame}.",
        "i" = "Table layers must be written using {.val x.*} keys."
      ))
    }

    seq_xlsx(x, filename = path, overwrite = overwrite, verbose = FALSE)

    if (verbose) {
      cli::cli_alert_success(
        "Table {.val {key}} saved to {.file {full_path}}."
      )
    }

    return(invisible(path))
  }

  cli::cli_abort(
    "Unsupported layer type {.val {type}} for key {.val {key}}."
  )

}
