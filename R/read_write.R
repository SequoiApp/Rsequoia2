#' Open file based on key name
#'
#' @inheritParams get_path
#'
#' @importFrom sf read_sf
#' @importFrom terra rast
#'
#' @return Object of class `SpatRaster` for raster and `sf` for vector
#'
#' @export
seq_read <- function(key, dirname = ".", verbose = FALSE) {

  path <- get_path(key, dirname, verbose = FALSE)
  key <- names(path)

  file_not_exist <- !file.exists(path)
  if (file_not_exist) {
      cli::cli_abort(c(
        "!" = "File {.file {basename(path)}} for key {.val {key}} doesn't exist.",
        "i" = "{.code NULL} is returned."
      ))
    return(invisible(NULL))
  }

  is_vector <- startsWith(key, "v.")
  if (is_vector) {
    if (verbose) {
      cli::cli_alert_success(
        "Loaded vector layer {.val {key}} from {.file {basename(path)}}."
      )
    }
    return(invisible(sf::read_sf(path)))
  }

  is_raster <- startsWith(key, "r.")
  if (is_raster) {
    if (verbose) {
      cli::cli_alert_success(
        "Loaded raster layer {.val {key}} from {.file {basename(path)}}."
      )
    }

    return(invisible(terra::rast(path)))
  }

  is_xlsx <- startsWith(key, "x.")
  if (is_xlsx) {
    if (verbose) {
      cli::cli_alert_success(
        "Loaded xlsx {.val {key}} from {.file {basename(path)}}."
      )
    }
    return(invisible(openxlsx2::read_xlsx(
      path,
      na.strings = "",
      skip_empty_rows = TRUE,
      skip_empty_cols = TRUE)))
  }
}

#' Write a spatial object based on a layer key
#'
#' @inheritParams get_path
#' @param x An `sf` object (for vector outputs) or a `SpatRaster` (for raster outputs).
#' @param overwrite `logical` If `TRUE`, file is overwritten.
#'
#' @return Invisibly returns the filepath used for writing.
#'
#' @importFrom sf write_sf
#' @importFrom terra writeRaster
#'
#' @export
seq_write <- function(x, key, dirname = ".", verbose = FALSE, overwrite = FALSE) {

  path <- get_path(key, dirname, verbose = FALSE)
  key <- names(path)

  if (file.exists(path) && !overwrite) {
    cli::cli_warn(
      c(
        "!" = "{.file {basename(path)}} already exists.",
        "i" = "Use {.arg overwrite = TRUE} to replace it."
      )
    )
    return(invisible(path))
  }

  is_vector <- startsWith(key, "v.")
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
        "Vector layer {.val {key}} saved to {.file {basename(path)}}."
      )
    }

    return(invisible(path))
  }


  is_raster <- startsWith(key, "r.")
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
        "Raster layer {.val {key}} saved to {.file {basename(path)}}."
      )
    }

    return(invisible(path))
  }

}
