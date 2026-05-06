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
  x_env <- seq_envelope(x, buffer)

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
  names(r_mask) <- "dem_rgealti"

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
  x_env <- seq_envelope(x, buffer)

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
  names(r_mask) <- "dsm_rgealti"

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

  names(chm) <- "chm_rgealti"

  return(chm)

}

#' Create RGE ALTI layers for a Sequoia project
#'
#' Uses the project's _PARCA_ layer to download and write RGE ALTI MNT, MNS
#' and/or MNH rasters.
#'
#' @inheritParams get_dem
#' @inheritParams seq_write
#' @param key `character`; RGE ALTI product(s) to create. One or more of
#'   `"mnt"`, `"mns"` and `"mnh"`.
#'
#' @return Invisibly returns a named `character` vector of output raster paths.
#'
#' @export
seq_rgealti <- function(
    dirname = ".",
    key = c("mnt", "mns", "mnh"),
    buffer = 200,
    res = 1,
    crs = 2154,
    overwrite = FALSE,
    verbose = TRUE
) {

  key <- match.arg(key, several.ok = TRUE)

  if (verbose) {
    cli::cli_h1("RGE ALTI")
  }

  parca <- seq_read("v.seq.parca.poly", dirname = dirname)

  identifier <- seq_field("identifier")$name
  id <- unique(parca[[identifier]])

  key_map <- c(
    mnt = "r.alt.mnt.rge",
    mns = "r.alt.mns.rge",
    mnh = "r.alt.mnh.rge"
  )

  seq_output_path <- function(seq_key) {
    meta <- seq_layer(seq_key, verbose = FALSE)

    path <- file.path(
      dirname,
      meta$path,
      sprintf("%s_%s.%s", id, meta$name, meta$ext)
    )

    normalizePath(path, winslash = "/", mustWork = FALSE)
  }

  read_or_create <- function(one_key, compute) {
    seq_key <- key_map[[one_key]]
    path <- seq_output_path(seq_key)

    if (file.exists(path) && !overwrite) {
      if (verbose) {
        cli::cli_alert_info("Using existing {.file {basename(path)}}.")
      }

      return(path)
    }

    r <- compute()

    seq_write(
      r,
      key = seq_key,
      id = id,
      dirname = dirname,
      overwrite = overwrite,
      verbose = verbose
    )
  }

  rasters <- list()

  if ("mnt" %in% key) {
    rasters$mnt <- read_or_create(
      "mnt",
      function() {
        get_dem(
          x = parca,
          buffer = buffer,
          res = res,
          crs = crs,
          verbose = verbose
        ) |>
          seq_retry(verbose = verbose)
      }
    )
  }

  if ("mns" %in% key) {
    rasters$mns <- read_or_create(
      "mns",
      function() {
        get_dsm(
          x = parca,
          buffer = buffer,
          res = res,
          crs = crs,
          verbose = verbose
        ) |>
          seq_retry(verbose = verbose)
      }
    )
  }

  if ("mnh" %in% key) {
    rasters$mnh <- read_or_create(
      "mnh",
      function() {

        dem <- seq_read(
          key_map[["mnt"]],
          dirname = dirname,
          verbose = FALSE
        )

        dsm <- seq_read(
          key_map[["mns"]],
          dirname = dirname,
          verbose = FALSE
        )

        get_chm(
          x = NULL,
          dem = dem,
          dsm = dsm,
          verbose = verbose
        )
      }
    )
  }

  paths <- unlist(rasters, use.names = FALSE)
  names(paths) <- names(rasters)

  invisible(paths)
}
