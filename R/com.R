#' Retrieve administrative boundary polygons around an area
#'
#' Builds a convex buffer around the input geometry, retrieves commune
#' boundaries from BDTOPO, normalizes them, and returns a polygon layer.
#'
#' @param x An `sf` object used as the input area.
#' @param verbose `logical` If `TRUE`, display messages.
#'
#' @return An `sf` object of type `POLYGON` containing commune boundaries,
#'   with standardized fields as defined by `seq_normalize("com_poly")`.
#'   Returns `NULL` if no commune intersects the search area.
#'
#' @details
#' The function builds a 2000 m convex buffer around `x`, retrieves
#' the BDTOPO commune layer (`BDTOPO_V3:commune`) intersecting this buffer,
#' and normalizes the resulting geometries and attributes.
#'
#' @export
get_com_poly <- function(x, verbose = TRUE) {

  crs <- 2154
  convex <- envelope(x, 2000)

  if (verbose){
    cli::cli_alert_info("Downloading communes dataset...")
  }

  com <- happign::get_wfs(
    convex,
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

#' Retrieve and assemble commune boundary lines around an area
#'
#' Converts commune boundary polygons into line features, optionally
#' clipped for cartographic display.
#'
#' @param x An `sf` object used as the input area.
#' @param verbose `logical` If `TRUE`, display messages.
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

  poly <- get_com_poly(x, verbose = verbose)

  if (is.null(poly)) {
    return(NULL)
  }

  line <- poly_to_line(poly)

  if (graphic){
    convex <- envelope(x, 500)
    line <- quiet(sf::st_intersection(line, convex))
  }

  invisible(line)
}

#' Retrieve commune representative points around an area
#'
#' Computes centroid points from commune boundary polygons, optionally
#' restricted to a graphical extent.
#'
#' @param x An `sf` object used as the input area.
#' @param verbose `logical` If `TRUE`, display messages.
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

  poly <- get_com_poly(x, verbose = verbose)

  if (is.null(poly)) {
    return(NULL)
  }

  if (graphic){
    convex <- envelope(x, 500)
    point <- quiet(sf::st_centroid(sf::st_intersection(poly, convex),
                                   of_largest_polygon = FALSE))
  } else {
    point <- quiet(sf::st_centroid(poly, of_largest_polygon = FALSE))
  }

  invisible(point)
}

#' Generates commune polygon, line and point layers for a Sequoia project.
#'
#' This function is a convenience wrapper around [get_com_poly()],
#' [get_com_line()] and [get_com_point()], allowing the user to retrieve
#' administrative boundary products and automatically write them to the
#' project directory using [seq_write()].
#'
#' Both topological (full extent) and graphical (restricted extent)
#' representations are generated when relevant.
#'
#' @param dirname `character` Path to the project directory.
#'   Defaults to the current working directory.
#' @inheritParams seq_write
#'
#' @details
#' Commune layers are built from BDTOPO commune boundaries intersecting
#' the project area defined by the PARCA polygon.
#'
#' The following layers are produced:
#' \describe{
#'   \item{Topological layers}{Full commune geometry (polygon, boundary lines, centroids)}
#'   \item{Graphical layers}{Line and point representations clipped to a reduced
#'   convex buffer around the project area, intended for cartographic display}
#' }
#'
#' Each layer is always written to disk using [seq_write()],
#' even when it contains no features (`nrow == 0`).
#'
#' Informational messages are displayed to indicate whether a layer
#' contains features or is empty.
#'
#' @return A named list of file paths written by [seq_write()],
#' one per commune layer.
#'
#' @seealso
#' [get_com_poly()], [get_com_line()], [get_com_point()],
#' [seq_write()]
#'
#' @export
seq_com <- function(dirname = ".", verbose = TRUE, overwrite = FALSE) {
  # tiny helper ----
  seq_write2 <- function(x, key, id) {
    x[[id_field]] <- id
    seq_write(x, key, dirname = dirname, verbose = verbose, overwrite = overwrite)
  }

  # read PARCA
  parca <- seq_read("v.seq.parca.poly", dirname = dirname)
  id_field <- seq_field("identifier")$name
  id <- unique(parca[[id_field]])

  if (verbose){
    cli::cli_h1("COMMUNES")
  }

  topo_poly <- get_com_poly(parca, verbose = verbose)
  if (!is.null(topo_poly)){
    topo_poly <- seq_write2(topo_poly, "v.com.topo.poly", id)
  }

  topo_line <- get_com_line(parca, verbose = verbose)
  if (!is.null(topo_line)){
    topo_line <- seq_write2(topo_line, "v.com.topo.line", id)
  }

  topo_point <- get_com_point(parca, verbose = verbose)
  if (!is.null(topo_point)){
    topo_point <- seq_write2(topo_point, "v.com.topo.point", id)
  }

  graphic_line <- get_com_line(parca, graphic = TRUE, verbose = verbose)
  if (!is.null(graphic_line)){
    graphic_line <- seq_write2(graphic_line, "v.com.graphic.line", id)
  }

  graphic_point <- get_com_point(parca, graphic = TRUE, verbose = verbose)
  if (!is.null(graphic_point)){
    graphic_point <- seq_write2(graphic_point, "v.com.graphic.point", id)
  }

  return(
    invisible(
      c(topo_poly, topo_line, topo_point, graphic_line, graphic_point) |> as.list()
      )
    )
}

