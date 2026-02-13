#' Download Digital Elevation Model (DEM) raster from IGN RGEAlti
#'
#' Downloads DEM from the IGN WMS service for the area covering `x` expanded
#' with a buffer.
#' The result is returned as a masked `SpatRaster`, clipped to the buffer
#' geometry to keep file size minimal.
#'
#' @param x `sf` or `sfc`; Geometry located in France.
#' @param buffer `numeric`; Buffer around `x` (in **meters**) used to enlarge
#' the download area.
#' @param res `numeric`; resolution specified in the units of the coordinate
#' system (see [happign::get_wms_raster()])
#' @param crs `numeric` or `character`; CRS of the returned raster (see
#' [happign::get_wms_raster()])
#' @param verbose `logical`; If `TRUE`, display messages.
#'
#' @return `SpatRaster` object from `terra` package
#'
#' @seealso [happign::get_wms_raster()]
#'
#' @export
get_dem <- function(x, buffer = 200, res = 1, crs = 2154, verbose = TRUE) {

  if (!inherits(x, c("sf", "sfc"))) {
    cli::cli_abort("{.arg x} must be {.cls sf} or {.cls sfc}, not {.cls {class(x)}}.")
  }

  x <- sf::st_transform(x, 2154)
  x_env <- envelope(x, buffer)

  if (verbose) {cli::cli_alert_info("Downloading DEM dataset...")}

  pb <- cli::cli_progress_bar("DEM", total = nrow(x_env), clear = TRUE)
  tmp <- tempdir()
  files <- c()
  for (i in seq_len(nrow(x_env))){

    if (verbose) {cli::cli_progress_update(id = pb)}

    file <- sprintf(file.path(tmp, sprintf("r_%03d.tif", i)))

    r <- happign::get_wms_raster(
      x_env[i, ],
      layer = "ELEVATION.ELEVATIONGRIDCOVERAGE.HIGHRES",
      rgb = FALSE,
      res = res,
      crs = crs,
      filename = file,
      overwrite = TRUE,
      verbose = TRUE) |> suppressWarnings()

    files <- c(files, file)
  }
  cli::cli_progress_done(id = pb)

  v <- terra::vrt(files, options = c("-hidenodata"))

  if (verbose) {cli::cli_alert_info("Raster size optimization...")}
  r_mask <- terra::mask(v, x_env)

  return(invisible(r_mask))
}

#' Download Digital Surface Model (DSM) raster from IGN RGEAlti
#'
#' Downloads DSM from the IGN WMS service for the area covering `x` expanded
#' with a buffer.
#' The result is returned as a masked `SpatRaster`, clipped to the buffer
#' geometry to keep file size minimal.
#'
#' @inheritParams get_dem
#'
#' @return `SpatRaster` object from `terra` package
#'
#' @seealso [happign::get_wms_raster()]
#'
#' @export
get_dsm <- function(x, buffer = 200, res = 1, crs = 2154, verbose = TRUE) {

  if (!inherits(x, c("sf", "sfc"))) {
    cli::cli_abort("{.arg x} must be {.cls sf} or {.cls sfc}, not {.cls {class(x)}}.")
  }

  x <- sf::st_transform(x, 2154)
  x_env <- envelope(x, buffer)

  if (verbose) {cli::cli_alert_info("Downloading DSM dataset...")}

  pb <- cli::cli_progress_bar("DSM", total = nrow(x_env), clear = TRUE)
  tmp <- tempdir()
  files <- c()
  for (i in seq_len(nrow(x_env))){

    if (verbose) {cli::cli_progress_update(id = pb)}

    file <- sprintf(file.path(tmp, sprintf("r_%03d.tif", i)))

    happign::get_wms_raster(
      x_env[i, ],
      layer = "ELEVATION.ELEVATIONGRIDCOVERAGE.HIGHRES.MNS",
      rgb = FALSE,
      res = res,
      crs = crs,
      filename = file,
      overwrite = TRUE,
      verbose = TRUE) |> suppressWarnings()

    files <- c(files, file)
  }
  cli::cli_progress_done(id = pb)

  v <- terra::vrt(files, options = c("-hidenodata"))

  if (verbose) {cli::cli_alert_info("Raster size optimization...")}
  r_mask <- terra::mask(v, x_env)

  return(invisible(r_mask))
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
#' @inheritParams get_dem
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

  names(chm) <- "chm"

  return(chm)

}

#' Compute a Slope Raster from a DEM
#'
#' Computes a slope raster (in degrees) either by:
#'   - downloading a DEM automatically using `x`, or
#'   - using a DEM raster supplied manually through the `dem` argument.
#'
#' When `x` is provided, the DEM is obtained with [get_dem()], using by
#' default a resolution equal to `agg`.
#' When `x` is not provided, a DEM must be supplied via `dem`.
#'
#' If the DEM resolution is finer than `agg`, the raster is aggregated to
#' avoid artefacts and reduce computation time.
#'
#' @inheritParams get_chm
#' @param agg `numeric`; Target resolution (in meters) to which the DEM is
#' aggregated if its native resolution is finer. Default: `5`.
#' @param verbose `logical`; If `TRUE`, display messages.
#' @param ... Additional parameters passed to [get_dem()] when `x` is supplied.
#'
#' @details
#' Slope is computed with `terra::terrain()` using Horn's 8-neighbor algorithm,
#' and returned in **degrees**.
#'
#' Aggregation is performed with `terra::aggregate()` using a mean function.
#' This is recommended when high-resolution DEMs (<5 m) would otherwise
#' generate line artefacts.
#'
#' @return A `SpatRaster` containing slope values in degrees.
#'
#' @examples
#' \dontrun{
#' # Automatic download mode
#' s <- get_slope(x = my_polygon, buffer = 200)
#'
#' # Manual mode
#' dem <- get_dem(my_polygon)
#' s <- get_slope(dem = dem)
#' }
#' @export
#'
get_slope <- function(x = NULL, dem = NULL, agg = 5, verbose = TRUE, ...){

  if (!is.null(x) && !is.null(dem)) {
    cli::cli_abort(c(
      "x" = "Invalid input: {.arg x} cannot be used together with {.arg dem}.",
      "i" = "When {.arg x} is {.code NULL}, provide both {.arg dem}.",
      "i" = "To download them automatically, provide {.arg x} only."
    ))
  }

  if (!is.null(x)) {
    # By default res = agg
    args <- utils::modifyList(list("res" = agg), list(...))
    dem  <- do.call(get_dem, c(list(x), args))
  }

  if (is.null(dem)) {
    cli::cli_abort(c(
      "x" = "{.arg dem} must be provided when {.arg x} is NULL."
    ))
  }

  resolution <- terra::res(dem)[1]
  if (resolution < agg){
    if (verbose) cli::cli_alert_info("Aggregating DEM: {resolution}m -> {agg}m to avoid computational artefacts.")
    dem <- terra::aggregate(dem, fact = agg, fun = mean)
  }

  slope <- terra::terrain(dem, v="slope", neighbors=8, unit="degrees")

  return(slope)
}

#' Compute a Aspect Raster from a DEM
#'
#' Computes a aspect raster (in degrees) either by:
#'   - downloading a DEM automatically using `x`, or
#'   - using a DEM raster supplied manually through the `dem` argument.
#'
#' When `x` is provided, the DEM is obtained with [get_dem()], using by
#' default a resolution equal to `agg`.
#' When `x` is not provided, a DEM must be supplied via `dem`.
#'
#' If the DEM resolution is finer than `agg`, the raster is aggregated to
#' avoid artefacts and reduce computation time.
#'
#' @inheritParams get_slope
#'
#' @details
#' Aspect is computed with `terra::terrain()` using Horn's 8-neighbor algorithm,
#' and returned in **degrees**.
#'
#' Aggregation is performed with `terra::aggregate()` using a mean function.
#' This is recommended when high-resolution DEMs (<5 m) would otherwise
#' generate line artefacts.
#'
#' @return A `SpatRaster` containing aspect values in degrees.
#'
#' @examples
#' \dontrun{
#' # Automatic download mode
#' s <- get_aspect(x = my_polygon, buffer = 200)
#'
#' # Manual mode
#' dem <- get_dem(my_polygon)
#' s <- get_aspect(dem = dem)
#' }
#'
#' @export
get_aspect <- function(x = NULL, dem = NULL, agg = 5, verbose = TRUE, ...){

  if (!is.null(x) && !is.null(dem)) {
    cli::cli_abort(c(
      "x" = "Invalid input: {.arg x} cannot be used together with {.arg dem}.",
      "i" = "When {.arg x} is {.code NULL}, provide both {.arg dem}.",
      "i" = "To download them automatically, provide {.arg x} only."
    ))
  }

  if (!is.null(x)) {
    # By default res = agg
    args <- utils::modifyList(list("res" = agg), list(...))
    dem  <- do.call(get_dem, c(list(x), args))
  }

  if (is.null(dem)) {
    cli::cli_abort(c(
      "x" = "{.arg dem} must be provided when {.arg x} is NULL."
    ))
  }

  resolution <- terra::res(dem)[1]
  if (resolution < agg){
    if (verbose) cli::cli_alert_info("Aggregating DEM: {resolution}m -> {agg}m to avoid computational artefacts.")
    dem <- terra::aggregate(dem, fact = agg, fun = mean)
  }

  aspect <- terra::terrain(dem, v="aspect", neighbors=8, unit="degrees")

  return(aspect)
}

#' Download and Compute Elevation Products for a Sequoia Project
#'
#' Downloads elevation datasets (DEM, DSM) from the IGN WMS, computes derived
#' products (CHM, slope, aspect), and writes all resulting rasters into the
#' Sequoia project directory.
#'
#' This function is a high-level convenience wrapper around:
#' - [get_dem()] - Digital Elevation Model
#' - [get_dsm()] - Digital Surface Model
#' - [get_chm()] - Canopy Height Model
#' - [get_slope()] - Slope raster
#' - [get_aspect()] - Aspect raster
#'
#' It automatically reads the project's `parca` layer, downloads the necessary
#' elevation data for its extent (with an optional buffer), computes all derived
#' products, and saves them using [seq_write()].
#'
#' @inheritParams get_dem
#' @inheritParams seq_write
#'
#' @return A named list of file paths written by [seq_write()], one per `type`.
#'
#' @seealso [get_dem()], [get_dsm()], [get_chm()], [get_slope()], [get_aspect()]
#'
#' @export
#'
seq_elevation <- function(
    dirname = ".",
    buffer = 200,
    res = 1,
    crs = 2154,
    overwrite = FALSE,
    verbose = TRUE) {

  seq_get_or_read <- function(key, compute_fn, id) {
    layer_info <- seq_layer(key)
    full_path <- layer_info$full_path
    path <- file.path(dirname, full_path)

    if (!overwrite && file.exists(path)) {
      return(list(path = path, data = seq_read(key, dirname = dirname)))
    }

    x <- compute_fn()
    path <- seq_write(x, key, dirname = dirname, id = id, verbose = verbose, overwrite = overwrite)

    return(list(path = path, data = x))
  }

  parca <- seq_read("v.seq.parca.poly", dirname = dirname)
  identifier <- seq_field("identifier")$name
  id <- unique(parca[[identifier]])

  if (verbose){
    cli::cli_h1("ELEVATION")
  }

  # DEM ----
  dem <- seq_get_or_read(
    "r.alt.mnt",
    function() {get_dem(parca, buffer = buffer, res = res, crs = crs, verbose = verbose)},
    id = id
  )

  # DSM ----
  dsm <- seq_get_or_read(
    "r.alt.mns",
    function() {get_dsm(parca, buffer = buffer, res = res, crs = crs, verbose = verbose)},
    id = id
  )

  # CHM ----
  chm <- seq_get_or_read(
    "r.alt.mnh",
    function() {get_chm(x = NULL, dem = dem$data, dsm = dsm$data, verbose = verbose)},
    id = id
  )

  # SLOPE ----
  slope <- seq_get_or_read(
    "r.alt.pente",
    function() {get_slope(x = NULL, dem = dem$data, verbose = verbose)},
    id = id
  )

  # ASPECT ----
  aspect <- seq_get_or_read(
    "r.alt.expo",
    function() {get_aspect(x = NULL, dem = dem$data, verbose = verbose)},
    id = id
  )

  return(invisible(c(dem$path, dsm$path, chm$path, slope$path, aspect$path) |> as.list()))
}

