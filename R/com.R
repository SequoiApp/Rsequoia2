#' Retrieve administrative boundary around an area
#'
#' Builds a convex buffer around the input geometry, retrieves commune
#' boundaries from BDTOPO, normalizes them, and returns a polygon layer.
#'
#' @param x `sf` object used as the input area.
#' @param buffer `numeric`; Buffer distance, in meters, passed to
#'   [seq_envelope()] to enlarge the data retrieval area around `x`.
#' @param verbose `logical`; If `TRUE`, display messages.
#'
#' @return An `sf` object of type `POLYGON` containing commune boundaries,
#' with standardized fields as defined by `seq_normalize("com_poly")`.
#' Returns `NULL` if no commune intersects the search area.
#'
#'
#' @export
get_com_poly <- function(x, buffer = 2000, verbose = TRUE) {

  # fetch_envelope buffer
  crs <- 2154
  x <- sf::st_transform(x, crs)
  fetch_envelope <- seq_envelope(x, buffer)

  if (verbose){
    cli::cli_alert_info("Downloading communes dataset...")
  }

  com <- happign::get_wfs(
    fetch_envelope,
    layer = "BDTOPO_V3:commune",
    verbose = FALSE
  )

  if (nrow(com) == 0) {
    return(NULL)
  }

  com <- seq_normalize(com, "com_poly") |>
    sf::st_transform(crs)

  return(invisible(com))
}

#' Build commune boundary lines from downloaded polygons
#' @keywords internal
#' @noRd
com_line_from_poly <- function(poly, clip = NULL, verbose = TRUE) {
  line <- poly_to_line(poly)

  if (is.null(clip)) {
    return(invisible(line))
  }

  line <- suppressWarnings(sf::st_intersection(line, clip))

  if (nrow(line) == 0) {
    if (verbose) {
      cli::cli_alert_warning(
        "No intersection between COMS_TOPO_line and area of interest."
      )
    }
    return(NULL)
  }

  invisible(line)
}

#' Build commune representative points from downloaded polygons
#' @keywords internal
#' @noRd
com_point_from_poly <- function(poly, clip = NULL, verbose = TRUE) {
  if (!is.null(clip)) {
    poly <- sf::st_intersection(poly, clip) |>
      suppressWarnings()

    if (nrow(poly) == 0) {
      if (verbose) {
        cli::cli_alert_warning(
          "No intersection between COMS_TOPO_point and area of interest."
        )
      }
      return(NULL)
    }
  }

  centroid <- sf::st_centroid(poly, of_largest_polygon = FALSE) |>
    suppressWarnings()

  return(centroid)
}

#' Retrieve and assemble commune boundary lines around an area
#'
#' Converts commune boundary polygons into line features, optionally
#' clipped for cartographic display.
#'
#' @param x An `sf` object used as the input area.
#' @param verbose `logical`; If `TRUE`, display progress and informational messages.
#' @param poly Optional preloaded commune polygon layer. Supplying it avoids
#'   downloading the same source data again.
#' @param graphic Logical. If `TRUE`, line geometries are clipped to a
#'   500 m convex buffer around `x` for graphical purposes.
#'
#' @return An `sf` object of type `LINESTRING` representing commune boundaries.
#'   Returns `NULL` if no commune intersects the input area.
#'
#' @details
#' The function retrieves commune polygons using `get_com_poly()`,
#' converts them to line geometries using `poly_to_line()`,
#' and optionally intersects them with a reduced convex buffer
#' to limit graphical extent.
#'
#' @seealso [get_com_poly()]
#'
#' @export
get_com_line <- function(x, graphic = FALSE, verbose = TRUE) {
  poly <- get_com_poly(x, buffer = 2000, verbose = verbose)

  if (is.null(poly)) {
    return(NULL)
  }

  clip <- if (graphic) seq_envelope(x, 500) else NULL

  com_line_from_poly(poly, clip, verbose)
}

#' Retrieve commune representative points around an area
#'
#' Computes centroid points from commune boundary polygons, optionally
#' restricted to a graphical extent.
#'
#' @param x An `sf` object used as the input area.
#' @param verbose `logical`; If `TRUE`, display progress and informational messages.
#' @param poly Optional preloaded commune polygon layer. Supplying it avoids
#'   downloading the same source data again.
#' @param graphic Logical. If `TRUE`, centroids are computed only on the
#'   intersection between commune polygons and a 500 m convex buffer
#'   around `x`, for cartographic display.
#'
#' @return An `sf` object of type `POINT` representing commune centroids.
#'   Returns `NULL` if no commune intersects the input area.
#'
#' @details
#' The function retrieves commune polygons using `get_com_poly()`,
#' then computes their centroids. When `graphic = TRUE`, centroids
#' are calculated from the clipped geometries to ensure points
#' fall within the display extent.
#'
#' @seealso [get_com_poly()]
#'
#' @export
get_com_point <- function(x, graphic = FALSE, verbose = TRUE) {
  poly <- get_com_poly(x, buffer = 2000, verbose = verbose)

  if (is.null(poly)) {
    return(NULL)
  }

  clip <- if (graphic) seq_envelope(x, 500) else NULL

  com_point_from_poly(poly, clip, verbose)
}

#' Download and write commune layers for an area
#'
#' Internal wrapper around [get_com_poly()], [get_com_line()],
#' [get_com_point()] and [seq_write()].
#'
#' Both topological (full extent) and graphical (restricted extent)
#' representations are generated when relevant.
#'
#' @inheritParams seq_write
#'
#' @details
#' Commune layers are built from BDTOPO commune boundaries intersecting
#' the project area defined by the PARCA polygon.
#'
#' The following layers are produced:
#'
#' - Topological layers: Full commune geometry (polygon, boundary lines, centroids)
#' - Graphical layers: Line and point representations clipped to a reduced
#'   convex buffer around the project area, intended for cartographic display
#'
#' @return A named list of file paths written by [seq_write()],
#' one per commune layer.
#'
#' @seealso
#' [get_com_poly()], [get_com_line()], [get_com_point()],
#' [seq_write()]
#'
#' @export
get_commune <- function(
    x,
    dirname = ".",
    id = NULL,
    verbose = TRUE,
    overwrite = FALSE) {

  if (verbose) {
    cli::cli_h1("COMMUNES")
    pb <- cli::cli_progress_message("Downloading commune layer...")
  }

  poly <- get_com_poly(x, verbose = verbose)

  if (is.null(poly)) {
    return(invisible(list()))
  }

  clip <- seq_envelope(x, 500)
  layers <- list(
    "v.com.topo.poly" = poly,
    "v.com.topo.line" = com_line_from_poly(poly),
    "v.com.topo.point" = com_point_from_poly(poly),
    "v.com.graphic.line" = com_line_from_poly(poly, clip, verbose),
    "v.com.graphic.point" = com_point_from_poly(poly, clip, verbose)
  )
  layers <- Filter(Negate(is.null), layers)

  id_field <- seq_field("identifier")$name
  paths <- lapply(names(layers), function(key) {
    layer <- layers[[key]]

    if (!is.null(id)) {
      layer[[id_field]] <- id
    }

    seq_write(
      layer,
      key,
      dirname = dirname,
      id = id,
      verbose = verbose,
      overwrite = overwrite
    )
  })
  names(paths) <- names(layers)

  invisible(paths)
}

#' Generates commune polygon, line and point layers for a Sequoia project
#'
#' Reads the project PARCA layer, retrieves its identifier, then delegates the
#' commune download and writing to [get_commune()].
#'
#' @param dirname `character` Path to the project directory.
#' @param verbose `logical` If `TRUE`, display messages.
#' @param overwrite `logical` If `TRUE`, overwrite existing files.
#'
#' @return A named list of written file paths.
#' @export
seq_com <- function(dirname = ".", verbose = TRUE, overwrite = FALSE) {
  parca <- seq_read("v.seq.parca.poly", dirname = dirname)
  id_field <- seq_field("identifier")$name
  id <- unique(parca[[id_field]])

  get_commune(
    x = parca,
    dirname = dirname,
    id = id,
    verbose = verbose,
    overwrite = overwrite
  )
}

