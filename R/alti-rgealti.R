#' Download an altimetry raster from IGN RGE ALTI
#'
#' Downloads an MNT, MNS or MNH from the IGN WMS service for the area covering
#' `x`. The MNH is computed from the downloaded MNT and MNS.
#'
#' @param x `sf` or `sfc`; Geometry located in France.
#' @param key `character`; RGE ALTI product to retrieve. One of `"mnt"`,
#'   `"mns"` or `"mnh"`.
#' @param buffer `numeric`; Buffer around `x` (in **meters**) used to enlarge
#' the download area.
#' @param res `numeric`; resolution specified in the units of the coordinate
#' system (see [happign::get_wms_raster()])
#' @param crs `numeric` or `character`; CRS of the returned raster (see
#' [happign::get_wms_raster()])
#' @param minmax `numeric`; Accepted MNH range.
#' @param verbose `logical`; If `TRUE`, display progress and informational messages.
#'
#' @return `SpatRaster` object from `terra` package
#'
#' @seealso [happign::get_wms_raster()]
#'
#' @export
get_rge <- function(
    x,
    key = c("mnt", "mns", "mnh"),
    buffer = 200,
    res = 1,
    crs = 2154,
    minmax = c(0, 50),
    verbose = TRUE) {

  key <- match.arg(key)

  if (!inherits(x, c("sf", "sfc"))) {
    cli::cli_abort("{.arg x} must be {.cls sf} or {.cls sfc}, not {.cls {class(x)}}.")
  }

  if (key == "mnh") {
    mnt <- get_rge(x, "mnt", buffer, res, crs, minmax, verbose)
    mns <- get_rge(x, "mns", buffer, res, crs, minmax, verbose)
    r <- get_chm(dem = mnt, dsm = mns, minmax = minmax)
    names(r) <- "mnh_rge"
    return(invisible(r))
  }

  x <- sf::st_transform(x, 2154)
  x_env <- seq_envelope(x, buffer)

  layer <- switch(
    key,
    mnt = "ELEVATION.ELEVATIONGRIDCOVERAGE.HIGHRES",
    mns = "ELEVATION.ELEVATIONGRIDCOVERAGE.HIGHRES.MNS"
  )

  if (verbose) {
    pb <- cli::cli_progress_message(
      "Downloading {toupper(key)} RGE ALTI dataset...",
      .auto_close = FALSE
    )
  }

  files <- vapply(seq_len(nrow(x_env)), function(i) {
    file <- file.path(tempdir(), sprintf("rge_%s_%03d.tif", key, i))

    suppressWarnings(happign::get_wms_raster(
      x = x_env[i, ],
      layer = layer,
      rgb = FALSE,
      res = res,
      crs = crs,
      filename = file,
      overwrite = TRUE,
      verbose = verbose
    ))

    file
  }, character(1))

  if (verbose) cli::cli_progress_done(pb)
  if (verbose) cli::cli_progress_message("Optimizing raster...")

  r <- .altimetry_transformer(files, x_env, crs, crop = FALSE)
  names(r) <- paste0(key, "_rge")

  invisible(r)
}

#' Download Digital Elevation Model (DEM) raster from IGN RGE ALTI
#'
#' Compatibility wrapper around [get_rge()] for an MNT.
#'
#' @inheritParams get_rge
#'
#' @return `SpatRaster` object from `terra` package
#'
#' @seealso [happign::get_wms_raster()]
#'
#' @export
get_dem <- function(x, buffer = 200, res = 1, crs = 2154, verbose = TRUE) {
  r <- get_rge(x, "mnt", buffer, res, crs, verbose = verbose)
  names(r) <- "dem_rgealti"
  invisible(r)
}

#' Download Digital Surface Model (DSM) raster from IGN RGE ALTI
#'
#' Compatibility wrapper around [get_rge()] for an MNS.
#'
#' @inheritParams get_rge
#'
#' @return `SpatRaster` object from `terra` package
#'
#' @seealso [happign::get_wms_raster()]
#'
#' @export
get_dsm <- function(x, buffer = 200, res = 1, crs = 2154, verbose = TRUE) {
  r <- get_rge(x, "mns", buffer, res, crs, verbose = verbose)
  names(r) <- "dsm_rgealti"
  invisible(r)
}

#' Compute Canopy Height Model (CHM)
#'
#' Computes a **Canopy Height Model** (CHM = DSM - DEM) either by:
#'   - downloading the necessary DEM and DSM from IGN WMS services using `x`, or
#'   - using DEM and DSM rasters directly supplied by the user.
#'
#' When `x` is provided, both DEM and DSM are automatically fetched via
#' [get_dem()] and [get_dsm()].
#' When `x` is not provided, both `dem` and `dsm` must be manually supplied.
#'
#' Output values outside `minmax` are clamped: negative values are set to `NA`
#' and excessively high values are capped.
#'
#' @inheritParams get_rge
#' @param dem `SpatRaster` representing ground elevation (DEM). Must be supplied
#' only when `x` is `NULL`.
#' @param dsm A `SpatRaster` representing surface elevation (DSM). Must be supplied
#' only when `x` is `NULL`.
#' @param minmax `numeric` length-2 vector giving the accepted CHM range as
#' `c(min, max)`. Default: `c(0, 50)`.
#' @param ... Additional parameters passed to [get_dem()] and [get_dem()] when
#' `x` is supplied.
#'
#' @return A `SpatRaster` containing the CHM.
#'
#' @examples
#' \dontrun{
#' # Automatic download mode
#' chm <- get_chm(x = my_polygon)
#'
#' # Manual mode
#' dem <- get_dem(my_polygon)
#' dsm <- get_dsm(my_polygon)
#' chm <- get_chm(dem = dem, dsm = dsm)
#' }
#'
#' @export
get_chm <- function(x = NULL, dem = NULL, dsm = NULL, minmax = c(0, 50), ...){

  # --- Invalid case: user mixed modes ---------------------------------------
  if (!is.null(x) && (!is.null(dem) || !is.null(dsm))) {
    cli::cli_abort(c(
      "x" = "Invalid input: {.arg x} cannot be used together with {.arg dem}/{.arg dsm}.",
      "i" = "When {.arg x} is {.code NULL}, provide both {.arg dem} and {.arg dsm}.",
      "i" = "To download them automatically, provide {.arg x} only."
    ))
  }

  # --- Mode 1: x isn't provided -> compute DEM & DSM ----------------------------
  if (!is.null(x)) {
    dem <- get_dem(x, ...)
    dsm <- get_dsm(x, ...)
  }

  # --- Mode 2: x is NULL -> dem & dsm must be provided ------------------------
  if (is.null(dem) || is.null(dsm)) {
    cli::cli_abort(c(
      "x" = "{.arg dem} and {.arg dsm} must be provided when {.arg x} is NULL.",
      "i" = "You supplied: dem = {is.null(dem)}, dsm = {is.null(dsm)}."
    ))
  }

  dem[dem < 0] <- 0
  dsm[dsm < 0] <- 0

  chm <- dsm - dem

  chm[chm < minmax[1]] <- NA  # Remove negative value
  chm[chm > minmax[2]] <- minmax[2] # Remove height more than 50m

  names(chm) <- "chm_rgealti"

  return(chm)

}
