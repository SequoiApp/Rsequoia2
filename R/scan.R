#' Download IGN scanned maps (SCAN25, SCAN100, OACI or IGN maps)
#'
#' Downloads a scanned map layer from the IGN WMS service for the area covering
#' `x`, expanded with a buffer.
#' The result is returned as a `SpatRaster` with a fixed spatial resolution.
#'
#' Supported layers include SCAN25, SCAN100, OACI aeronautical charts and
#' standard IGN maps.
#'
#' @param x `sf` or `sfc`; Geometry located in France.
#' @param type `character`; Type of scanned map to download. Must be one of:
#'   - `"scan25"`: SCAN25 topographic map
#'   - `"scan100"`: SCAN100 topographic map
#'   - `"oaci"`: OACI aeronautical chart
#'   - `"carte_ign"`: standard IGN map background
#' @param buffer `numeric`; Buffer around `x` (in **meters**) used to enlarge
#'   the download area.
#' @param res `numeric`; Spatial resolution of the output raster (in map units,
#'   usually meters).
#' @param crs `numeric` or `character`; CRS of the returned raster.
#' @param overwrite `logical`; If `TRUE`, the cached file is overwritten.
#' @param verbose `logical`; If `TRUE`, display GDAL messages during download.
#'
#' @details
#' The scanned map is retrieved for the full bounding box (bbox) of `x` expanded
#' with the specified buffer.
#'
#' To improve performance and avoid repeated downloads, the raster is cached in
#' a temporary directory. The cache key depends on the bounding box, resolution
#' and CRS. Set `overwrite = TRUE` to force a new download.
#'
#' @return A `SpatRaster` object from the `terra` package.
#'
#' @seealso [sf::gdal_utils()]
#'
#'@examples
#' \dontrun{
#' p <- sf::st_sfc(
#'   sf::st_point(c(-4.3727, 47.7982)),
#'   crs = 4326
#' )
#'
#' scan25 <- get_scan(p, type = "scan25", buffer = 500)
#' scan100 <- get_scan(p, type = "scan100", buffer = 500)
#'
#' terra::plotRGB(scan25)
#' }
#'
#' @export
get_scan <- function(
    x,
    type = c("scan25", "scan100", "oaci", "carte_ign"),
    buffer = 1000,
    res = 0.8,
    crs = 2154,
    overwrite = FALSE,
    verbose = TRUE
) {

  # checks
  if (!inherits(x, c("sf", "sfc"))) {
    cli::cli_abort(c(
      "x" = "{.arg x} is of class {.cls {class(x)}}.",
      "i" = "{.arg x} should be of class {.cls sf} or {.cls sfc}."
    ))
  }

  if (length(type) != 1 || !type %in% c("scan25", "scan100", "oaci", "carte_ign")) {
    cli::cli_abort(c(
      "x" = "{.arg type} is equal to {.val {format(type)}}.",
      "i" = "{.arg type} must be one of {.val scan25}, {.val scan100}, {.val oaci} or {.val carte_ign}."
    ))
  }

  if (!is.numeric(buffer) || buffer < 0) {
    cli::cli_abort("{.arg buffer} must be a non-negative number.")
  }

  if (!is.numeric(res) || res <= 0) {
    cli::cli_abort("{.arg res} must be a positive number.")
  }

  # geometry prep
  x <- sf::st_transform(x, crs)
  x_buff <- sf::st_buffer(x, buffer)
  x_union <- sf::st_union(x_buff)
  bb <- sf::st_bbox(x_union)

  # layer selection
  layer <- switch(
    type,
    scan25    = "SCAN25TOUR_PYR-JPEG_WLD_WM",
    scan100   = "SCAN100_PYR-JPEG_WLD_WM",
    oaci      = "SCANOACI_PYR-JPEG_WLD_WM",
    carte_ign = "GEOGRAPHICALGRIDSYSTEMS.MAPS"
  )

  crs_txt <- paste0("EPSG:", crs)

  # WMS URL
  wms_url <- sprintf(
    paste0(
      "WMS:https://data.geopf.fr/private/wms-r?",
      "SERVICE=WMS&VERSION=1.3.0&REQUEST=GetMap&",
      "LAYERS=%s&CRS=%s&FORMAT=image/geotiff&",
      "apikey=ign_scan_ws"
    ),
    layer, crs_txt
  )

  # output file (temp)
  key <- paste(
    round(unname(bb), 3),
    res,
    crs,
    sep = "_",
    collapse = "_"
  )

  out_file <- file.path(
    tempdir(),
    paste0("seq_scan_", type, "_", key, ".tif")
  )

  if (!file.exists(out_file) || overwrite) {
    warp_options <- c(
      "-of", "GTiff",
      "-te", bb$xmin, bb$ymin, bb$xmax, bb$ymax,
      "-te_srs", crs_txt,
      "-t_srs", crs_txt,
      "-tr", res, res,
      "-r", "bilinear",
      "-overwrite"
    )

    sf::gdal_utils(
      util = "warp",
      source = wms_url,
      destination = out_file,
      options = warp_options,
      quiet = !verbose
    )
  }

  # read + mask
  r <- terra::rast(out_file)

  return(r)
}

#' Download scanned IGN maps for a Sequoia project
#'
#' Downloads one or several scanned map products (SCAN25, SCAN100, OACI or
#' standard IGN maps) from the IGN WMS service for the `parca` layer of a
#' Sequoia project.
#'
#' This function is a convenience wrapper looping over `get_scan()`, allowing
#' the user to download several scanned map products in one call and
#' automatically write them to the project directory using `seq_write()`.
#'
#' @inheritParams get_scan
#' @inheritParams seq_write
#'
#' @param type `character`; One or several scanned map types to download.
#'   Must be one or more of:
#'   - `"scan25"`: SCAN25 topographic map
#'   - `"scan100"`: SCAN100 topographic map
#'   - `"oaci"`: OACI aeronautical chart
#'   - `"carte_ign"`: standard IGN map background
#'
#' @return A named list of file paths written by `seq_write()`, one per `type`.
#'
#' @seealso `get_scan()`, `seq_write()`
#'
seq_scan <- function(
    dirname = ".",
    type = c("scan25", "scan100", "oaci", "carte_ign"),
    buffer = 1000,
    res = 0.8,
    crs = 2154,
    overwrite = FALSE,
    verbose = TRUE
) {

  # read project geometry
  parca <- seq_read("v.seq.parca.poly", dirname = dirname)
  identifier <- seq_field("identifier")$name
  id <- unique(parca[[identifier]])

  if (verbose){
    cli::cli_h1("SCAN")
  }

  paths <- lapply(type, function(type) {

    try({
      r <- get_scan(
        parca,
        type = type,
        buffer = buffer,
        res = res,
        crs = crs,
        overwrite = overwrite,
        verbose = verbose
      )

      # raster validity check (black / empty raster)
      v <- terra::values(r, mat = FALSE)

      is_empty <- length(v) == 0 ||
        all(is.na(v)) ||
        suppressWarnings(max(v, na.rm = TRUE) == 0)

      if (is_empty) {
        if (verbose) {
          cli::cli_alert_info(
            "No usable data for scan type {.val {type}}."
          )
        }
        return(NULL)
      }

      # writes rasters
      key <- switch(
        type,
        scan25    = "r.scan.25",
        scan100   = "r.scan.100",
        oaci      = "r.scan.oaci",
        carte_ign = "r.scan.ign"
      )

      path <- seq_write(
        r,
        key = key,
        dirname = dirname,
        id = id,
        overwrite = overwrite,
        verbose = verbose
      )

      path
    }, silent = TRUE)
  })

  # remove empty results
  paths <- Filter(Negate(is.null), paths)

  if (length(paths) == 0) {
    if (verbose) {
      cli::cli_alert_info(
        "No scan raster could be generated for this project."
      )
    }
    return(NULL)
  }

  names(paths) <- type[seq_along(paths)]
  return(invisible(paths))
}

