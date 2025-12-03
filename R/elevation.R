
get_dem <- function(x, buffer = 200, res = 1, crs = 2154, overwrite = FALSE, verbose = TRUE) {

  if (!inherits(x, c("sf", "sfc"))) {
    cli::cli_abort("{.arg x} must be {.cls sf} or {.cls sfc}, not {.cls {class(x)}}.")
  }

  x <- sf::st_transform(x, crs)
  x_buff <- sf::st_buffer(x, buffer)

  r <- happign::get_wms_raster(
    x_buff,
    layer = "ELEVATION.ELEVATIONGRIDCOVERAGE.HIGHRES",
    rgb = FALSE,
    res = res,
    crs = crs,
    verbose = verbose,
    overwrite = overwrite
  ) |> suppressWarnings()

  terra::mask(r, terra::vect(x_buff))
}

get_dsm <- function(x, buffer = 200, res = 1, crs = 2154, overwrite = FALSE, verbose = TRUE) {

  if (!inherits(x, c("sf", "sfc"))) {
    cli::cli_abort("{.arg x} must be {.cls sf} or {.cls sfc}, not {.cls {class(x)}}.")
  }

  x <- sf::st_transform(x, crs)
  x_buff <- sf::st_buffer(x, buffer)

  r <- happign::get_wms_raster(
    x_buff,
    layer = "ELEVATION.ELEVATIONGRIDCOVERAGE.HIGHRES.MNS",
    rgb = FALSE,
    res = res,
    crs = crs,
    verbose = verbose,
    overwrite = overwrite
  ) |> suppressWarnings()

  terra::mask(r, terra::vect(x_buff))
}

get_chm <- function(x = NULL, dem = NULL, dsm = NULL, minmax = c(0, 50), ...){

  # --- Invalid case: user mixed modes ---------------------------------------
  if (!is.null(x) && (!is.null(dem) || !is.null(dsm))) {
    cli::cli_abort(c(
      "x" = "You supplied both {.arg x} and {.arg dem}/{.arg dsm}.",
      "i" = "Either:",
      "*" = "Provide {.arg x} → DEM and DSM will be computed automatically.",
      "*" = "Or provide {.arg dem} and {.arg dsm} directly."
    ))
  }

  # --- Mode 1: x isn't provided → compute DEM & DSM ----------------------------
  if (!is.null(x)) {
    dem <- get_dem(x, ...)
    dsm <- get_dsm(x, ...)
  }

  # --- Mode 2: x is NULL → dem & dsm must be provided ------------------------
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

  return(chm)

}

get_slope <- function(x = NULL, dem = NULL, agg = 5, ...){

  # --- Invalid case: user mixed modes ---------------------------------------
  if (!is.null(x) && (!is.null(dem))) {
    cli::cli_abort(c(
      "x" = "You supplied both {.arg x} and {.arg dem}.",
      "i" = "Either:",
      "*" = "Provide {.arg x} → DEM will be computed automatically.",
      "*" = "Or provide {.arg dem}."
    ))
  }
  # --- Mode 1: x is provided → compute DEM & DSM ----------------------------
  if (!is.null(x)) {
    # By default res = agg
    if (!"res" %in% names(list(...))) {
      dem <- get_dem(x, res = agg, ...)
    } else {
      dem <- get_dem(x, ...)
    }
  }

  # --- Mode 2: x is NULL → dem & dsm must be provided ------------------------
  if (is.null(dem)) {
    cli::cli_abort(c(
      "x" = "{.arg dem} must be provided when {.arg x} is NULL."
    ))
  }

  resolution <- res(dem)[1]
  if (resolution < agg){
    cli::cli_alert_info("Aggregating DEM: {resolution}m → {agg}m to avoid computational artefacts.")
    dem <- terra::aggregate(dem, fact = agg, fun = mean)
  }

  slope <- terra::terrain(dem, v="slope", neighbors=8, unit="degrees")

  return(slope)
}

get_aspect <- function(x = NULL, dem = NULL, agg = 5, ...){
  # --- Invalid case: user mixed modes ---------------------------------------
  if (!is.null(x) && (!is.null(dem))) {
    cli::cli_abort(c(
      "x" = "You supplied both {.arg x} and {.arg dem}.",
      "i" = "Either:",
      "*" = "Provide {.arg x} → DEM will be computed automatically.",
      "*" = "Or provide {.arg dem}."
    ))
  }
  # --- Mode 1: x is provided → compute DEM & DSM ----------------------------
  if (!is.null(x)) {
    # By default res = agg
    if (!"res" %in% names(list(...))) {
      dem <- get_dem(x, res = agg, ...)
    } else {
      dem <- get_dem(x, ...)
    }
  }

  # --- Mode 2: x is NULL → dem & dsm must be provided ------------------------
  if (is.null(dem)) {
    cli::cli_abort(c(
      "x" = "{.arg dem} must be provided when {.arg x} is NULL."
    ))
  }

  resolution <- res(dem)[1]
  if (resolution < agg){
    cli::cli_alert_info("Aggregating DEM: {resolution}m → {agg}m toavoid computational artefacts.")
    dem <- terra::aggregate(dem, fact = agg, fun = mean)
  }

  aspect <- terra::terrain(dem, v="aspect", neighbors=8, unit="degrees")

  return(aspect)
}

