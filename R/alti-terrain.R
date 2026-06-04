#' Prepare DEM for terrain derivatives
#'
#' Aggregates a DEM to a coarser working resolution before computing terrain
#' derivatives such as slope or aspect.
#'
#' @param dem SpatRaster; DEM/MNT raster.
#' @param agg numeric(1); Target working resolution in meters. If NULL,
#' no aggregation is applied.
#' @param verbose logical(1); If TRUE, display messages.
#'
#' @return A SpatRaster, unchanged or aggregated.
#'
#' @importFrom cli cli_abort cli_alert_info cli_alert_warning
#' @importFrom terra aggregate res
#'
#' @export
seq_aggregate_dem <- function(dem, agg = 5, verbose = TRUE) {

  # --- Validate arguments ---
  if (missing(dem) || is.null(dem)) {
    cli::cli_abort("Argument {.arg dem} is missing.")
  }

  if (!inherits(dem, "SpatRaster")) {
    cli::cli_abort("{.arg dem} must be a {.cls SpatRaster}.")
  }

  if (!is.null(agg)) {
    if (!is.numeric(agg) ||
        length(agg) != 1L ||
        is.na(agg) ||
        !is.finite(agg) ||
        agg <= 0) {
      cli::cli_abort(
        "{.arg agg} must be a positive numeric scalar or {.val NULL}."
      )
    }
  }

  if (!is.logical(verbose) ||
      length(verbose) != 1L ||
      is.na(verbose)) {
    cli::cli_abort("{.arg verbose} must be TRUE or FALSE.")
  }

  # --- No aggregation requested ---
  if (is.null(agg)) {
    return(dem)
  }

  # --- Current resolution ---
  res <- terra::res(dem)

  if (any(!is.finite(res)) || any(res <= 0)) {
    cli::cli_abort("Unable to determine a valid raster resolution.")
  }

  if (length(unique(round(res, 8))) > 1 && verbose) {
    cli::cli_alert_warning(
      "Raster has non-square cells; using the smallest resolution."
    )
  }

  resolution <- min(res)

  # --- Aggregation needed? ---
  if (resolution >= agg) {
    return(dem)
  }

  fact <- round(agg / resolution)

  if (!is.finite(fact) || fact < 1) {
    cli::cli_abort(
      "Failed to compute a valid aggregation factor."
    )
  }

  if (fact == 1L) {
    return(dem)
  }

  # --- Aggregate ---
  if (verbose) {
    cli::cli_alert_info(
      "Aggregating DEM: {round(resolution, 1)} m -> {round(fact * resolution, 1)} m to avoid terrain artefacts."
    )
  }

  terra::aggregate(
    dem,
    fact = fact,
    fun = mean,
    na.rm = TRUE
  )
}

#' Compute a Slope Raster from a DEM
#'
#' Computes a slope raster in percent from a DEM, either by:
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
#' @param agg `numeric`; Target resolution, in meters, to which the DEM is
#' aggregated if its native resolution is finer. Default: `5`.
#' @param verbose `logical`; If `TRUE`, display messages.
#' @param ... Additional parameters passed to [get_dem()] when `x` is supplied.
#'
#' @details
#' Slope is computed with `terra::terrain()` using Horn's 8-neighbor algorithm.
#' The slope is first computed in degrees, then converted to percent using:
#'
#' `tan(slope_degrees * pi / 180) * 100`
#'
#' A 45-degree slope is therefore equal to a 100 percent slope.
#'
#' @return A `SpatRaster` containing slope values in percent.
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
#' @inheritParams get_chm
#' @param agg `numeric`; Target resolution (in meters) to which the DEM is
#' aggregated if its native resolution is finer. Default: `5`.
#' @param unit `character`; "percent", "radians" or "degrees". Default to "percent".
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
get_slope <- function(
    x = NULL,
    dem = NULL,
    agg = 5,
    unit = "percent",
    verbose = TRUE,
    ...) {

  unit <- match.arg(unit, c("percent", "radians", "degrees"))

  if (!is.null(x) && !is.null(dem)) {
  cli::cli_abort(c(
      "Invalid input.",
      "x" = "{.arg x} cannot be used together with {.arg dem}.",
      "i" = "Provide either {.arg x} or {.arg dem}."
    ))
  }

  if (!is.null(x)) {
    args <- utils::modifyList(list(res = agg), list(...))
    dem <- do.call(get_dem, c(list(x = x), args))
  }

  if (is.null(dem)) {
    cli::cli_abort("{.arg dem} must be provided when {.arg x} is NULL.")
  }

  dem <- seq_aggregate_dem(dem = dem, agg = agg, verbose = verbose)

  # If percent, then it's compute in radians
  unit <- if (unit == "degrees") "degrees" else "radians"
  slope <- terra::terrain(dem, v = "slope", neighbors = 8, unit)

  if (unit == "percent") {
    slope <- tan(slope) * 100
  }

  names(slope) <- "slope"

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
      "Invalid input.",
      "x" = "{.arg x} cannot be used together with {.arg dem}.",
      "i" = "Provide either {.arg x} or {.arg dem}."
    ))
  }

  if (!is.null(x)) {
    args <- utils::modifyList(list(res = agg), list(...))
    dem <- do.call(get_dem, c(list(x = x), args))
  }

  if (is.null(dem)) {
    cli::cli_abort("{.arg dem} must be provided when {.arg x} is NULL.")
  }

  dem <- seq_aggregate_dem(dem = dem, agg = agg, verbose = verbose)
  aspect <- terra::terrain(dem,v = "aspect",neighbors = 8, unit = "degrees")
  names(aspect) <- "aspect"

  return(aspect)
}

#' Compute a hillshade raster from a DHM
#'
#' Computes a shaded relief raster from a Digital Height Model using
#' slope and aspect derived directly from the DHM.
#'
#' @param r `SpatRaster`; Digital Height Model raster.
#' @param angle `numeric`; Sun elevation angle in degrees.
#' @param direction `numeric`; Sun direction/azimuth in degrees.
#'
#' @return A `SpatRaster` containing hillshade values.
#'
#' @export
get_shade <- function(
    r,
    angle = 30,
    direction = c(225, 270, 315, 360)
) {

  if (!inherits(r, "SpatRaster")) {
    cli::cli_abort("{.arg dhm} must be a {.cls SpatRaster} object.")
  }

  slope <- terra::terrain(r, "slope", unit = "radians")
  aspect <- terra::terrain(r, "aspect", unit = "radians")
  shade <- terra::shade(slope, aspect, angle=angle, direction=direction)
  shade <- Reduce(terra::mean, shade)
  shade <- terra::mean(shade)

  shade <- terra::clamp(shade, lower = 0, upper = 1, values = TRUE)
  shade <- terra::round(shade * 255)

  names(shade) <- "dhm_shade"

  return(shade)
}

#' Create terrain derivative layers for a Sequoia project
#'
#' Uses the project's MNT raster to compute slope and aspect rasters, then
#' writes them to the project directory with [seq_write()].
#'
#' @inheritParams seq_write
#' @inheritParams get_slope
#' @param verbose `logical`; If `TRUE`, display messages.
#'
#' @return Invisibly returns a named `character` vector of output raster paths.
#'
#' @seealso [get_slope()], [get_aspect()]
#'
#' @export
seq_terrain <- function(
    dirname = ".",
    agg = 5,
    unit = "percent",
    overwrite = FALSE,
    verbose = TRUE
) {

  if (verbose) {
    cli::cli_h1("TERRAIN")
  }

  dem <- tryCatch(
    seq_read("r.alt.mnt.lidar",dirname = dirname,verbose = FALSE),
    error = function(e_lidar) {
      tryCatch(
        seq_read("r.alt.mnt.rge",dirname = dirname,verbose = FALSE),
        error = function(e_rgealti) {
          cli::cli_abort(c(
            "No MNT raster found for terrain calculation.",
            "x" = "Neither {.val r.alt.mnt.lidar} nor {.val r.alt.mnt.rge} could be read.",
            "i" = "Run {.fun seq_lidar} or {.fun seq_rgealti} first to create MNT data."
          ))
        }
      )
    }
  )

  dhm <- tryCatch(
    seq_read("r.alt.mnh.lidar",dirname = dirname,verbose = FALSE),
    error = function(e_lidar) {
      tryCatch(
        seq_read("r.alt.mnh.rge",dirname = dirname,verbose = FALSE),
        error = function(e_rgealti) {
          cli::cli_abort(c(
            "No MNH raster found for terrain calculation.",
            "x" = "Neither {.val r.alt.mnh.lidar} nor {.val r.alt.mnh.rge} could be read.",
            "i" = "Run {.fun seq_lidar} or {.fun seq_rgealti} first to create MNH data."
          ))
        }
      )
    }
  )

  parca <- seq_read("v.seq.parca.poly", dirname = dirname, verbose = FALSE)
  identifier <- seq_field("identifier")$name
  id <- unique(parca[[identifier]])

  terrain_keys <- c(slope = "r.alt.pente", aspect = "r.alt.expo")

  raster_path <- function(key) {
    meta <- seq_layer(key, verbose = FALSE)
    filename <- sprintf("%s_%s.%s", id, meta$name, meta$ext)
    path <- file.path(dirname, meta$path, filename)

    return(normalizePath(path, winslash = "/", mustWork = FALSE))
  }

  # SLOPE ----
  slope_key <- "r.alt.pente"
  slope_path <- raster_path(slope_key)

  if (file.exists(slope_path) && !overwrite) {
    if (verbose) {
      cli::cli_alert_info("Using existing {.file {basename(slope_path)}}.")
    }
  }

  if (!file.exists(slope_path) || overwrite) {
    slope <- get_slope(
      x = NULL,
      dem = dem,
      agg = agg,
      unit = unit,
      verbose = verbose
    )

    slope_path <- seq_write(
      slope,
      key = slope_key,
      dirname = dirname,
      id = id,
      overwrite = overwrite,
      verbose = verbose
    )
  }

  # ASPECT ----
  aspect_key <- "r.alt.expo"
  aspect_path <- raster_path(aspect_key)

  if (file.exists(aspect_path) && !overwrite) {
    if (verbose) {
      cli::cli_alert_info("Using existing {.file {basename(aspect_path)}}.")
    }
  }

  if (!file.exists(aspect_path) || overwrite) {
    aspect <- get_aspect(x = NULL, dem = dem, agg = agg, verbose = verbose)
    aspect_path <- seq_write(
      aspect,
      key = aspect_key,
      dirname = dirname,
      id = id,
      overwrite = overwrite,
      verbose = verbose
    )
  }

  # SHADE_MNH ----
  ombrage_key <- "r.alt.ombrage.mnh"
  ombrage_path <- raster_path(ombrage_key)

  if (file.exists(ombrage_path) && !overwrite) {
    if (verbose) {
      cli::cli_alert_info("Using existing {.file {basename(ombrage_path)}}.")
    }
  }

  if (!file.exists(ombrage_path) || overwrite) {
    ombrage <- get_shade(r = dhm)
    ombrage_path <- seq_write(
      ombrage,
      key = ombrage_key,
      dirname = dirname,
      id = id,
      overwrite = overwrite,
      verbose = verbose
    )
  }

  paths <- list(
    slope = slope_path,
    aspect = aspect_path,
    ombrage = ombrage_path
  )

  invisible(paths)
}
