#' Optimize downloaded altimetry tiles
#'
#' Builds a virtual raster from downloaded tiles, crops and masks it to the
#' requested area, then projects it to the requested CRS when needed.
#'
#' @param files `character`; Raster tile paths.
#' @param x `sf` or `sfc`; Geometry used to crop and mask the raster.
#' @param crs Target CRS.
#' @param crop `logical`; If `TRUE`, crop the raster before masking it.
#'
#' @return A `SpatRaster`.
#'
#' @keywords internal
#' @noRd
.altimetry_transformer <- function(files, x, crs = 2154, crop = TRUE) {
  r <- terra::vrt(files, options = "-hidenodata")

  x <- sf::st_transform(x, terra::crs(r))
  x <- terra::vect(x)

  if (crop) r <- terra::crop(r, x)
  r <- terra::mask(r, x)

  target_crs <- sf::st_crs(crs)
  if (!terra::same.crs(r, target_crs$wkt)) {
    r <- terra::project(r, target_crs$wkt)
  }

  r
}


#' Return the expected path of an altimetry layer
#'
#' @keywords internal
#' @noRd
.altimetry_path <- function(dirname, id, key) {
  if (is.null(id)) return(NULL)

  layer <- seq_layer(key, verbose = FALSE)

  normalizePath(
    file.path(
      dirname,
      layer$path,
      sprintf("%s_%s.%s", id, layer$name, layer$ext)
    ),
    winslash = "/",
    mustWork = FALSE
  )
}


#' Fetch altimetry layers
#'
#' Internal worker used by `fetch_altimetry()` and [seq_altimetry()]. LiDAR is
#' tried first by default, with RGE ALTI as fallback.
#'
#' @inheritParams get_lidar
#' @inheritParams get_rge
#' @param dirname `character`; Output directory.
#' @param id Optional Sequoia project identifier.
#' @param key `character`; Products to retrieve.
#'
#' @return Invisibly returns a named list of written paths.
#'
#' @keywords internal
#' @noRd
.altimetry_fetcher <- function(
    x,
    dirname,
    id = NULL,
    key = c("mnt", "mns", "mnh"),
    buffer = 200,
    res = 1,
    crs = 2154,
    cache = NULL,
    overwrite = FALSE,
    verbose = TRUE) {

  key <- match.arg(key, c("mnt", "mns", "mnh"), several.ok = TRUE)

  if (verbose){
    cli::cli_h1("ALTIMETRY")
  }

  fetch_source <- function(provider) {
    paths <- list()

    for (product in key) {
      seq_key <- paste("r.alt", product, provider, sep = ".")
      path <- .altimetry_path(dirname, id, seq_key)

      if (!is.null(path) && file.exists(path) && !overwrite) {
        if (verbose) {
          cli::cli_alert_info("Using existing {.file {basename(path)}}.")
        }
        paths[[product]] <- path
        next
      }

      if (verbose) {
        cli::cli_progress_message(
          "Downloading {toupper(product)} {toupper(provider)} product..."
        )
      }

      r <- switch(
        provider,
        lidar = get_lidar(
          x = x,
          key = product,
          buffer = buffer,
          crs = crs,
          cache = cache,
          overwrite = overwrite,
          verbose = verbose
        ),
        rge = get_rge(
          x = x,
          key = product,
          buffer = buffer,
          res = res,
          crs = crs,
          verbose = verbose
        )
      )

      paths[[product]] <- seq_write(
        r,
        key = seq_key,
        dirname = dirname,
        id = id,
        overwrite = overwrite,
        verbose = verbose
      )
    }

    paths
  }

  paths <- tryCatch(
    fetch_source("lidar"),
    error = function(e) {
      if (verbose) {
        cli::cli_alert_warning(
          "LiDAR unavailable. Falling back to RGE ALTI."
        )
        cli::cli_alert_info(conditionMessage(e))
      }

      fetch_source("rge")
    }
  )

  invisible(paths)
}


#' Fetch altimetry layers for an area
#'
#' Retrieves altimetry rasters around `x` and writes them to `dirname`.
#'
#' @inheritParams get_lidar
#' @inheritParams get_rge
#' @param dirname `character`; Output directory.
#' @param key `character`; Products to retrieve.
#'
#' @return Invisibly returns a named list of written paths.
#'
#' @keywords internal
#' @noRd
fetch_altimetry <- function(
    x,
    dirname,
    key = c("mnt", "mns", "mnh"),
    buffer = 200,
    res = 1,
    crs = 2154,
    cache = NULL,
    overwrite = FALSE,
    verbose = TRUE) {

  .altimetry_fetcher(
    x = x,
    dirname = dirname,
    key = key,
    buffer = buffer,
    res = res,
    crs = crs,
    cache = cache,
    overwrite = overwrite,
    verbose = verbose
  )
}


#' Download altimetry layers for a Sequoia project
#'
#' Downloads MNT, MNS and/or MNH rasters from LiDAR HD, falling back to RGE
#' ALTI when requested, then creates the terrain layers.
#'
#' @inheritParams get_lidar
#' @inheritParams get_rge
#' @inheritParams seq_write
#' @param key `character`; Products to retrieve.
#'
#' @return Invisibly returns a named list of written paths.
#'
#' @seealso [get_lidar()], [get_rge()], [seq_terrain()]
#'
#' @export
seq_altimetry <- function(
    dirname = ".",
    key = c("mnt", "mns", "mnh"),
    buffer = 200,
    res = 1,
    crs = 2154,
    cache = NULL,
    overwrite = FALSE,
    verbose = TRUE) {

  ctx <- .seq_context(dirname)

  paths <- .altimetry_fetcher(
    x = ctx$parca,
    dirname = dirname,
    id = ctx$id,
    key = key,
    buffer = buffer,
    res = res,
    crs = crs,
    cache = cache,
    overwrite = overwrite,
    verbose = verbose
  )

  seq_terrain(
    dirname = dirname,
    unit = "percent",
    overwrite = overwrite,
    verbose = verbose
  )

  invisible(paths)
}
