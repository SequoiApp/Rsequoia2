#' Retrieve administrative boundary polygons around an area
#'
#' Builds a convex buffer around the input geometry, retrieves commune
#' boundaries from BDTOPO, normalizes them, and returns a polygon layer.
#'
#' @param x An `sf` object used as the input area.
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
get_com_poly <- function(x) {

  crs <- 2154
  convex <- envelope(x, 2000)

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
get_com_line <- function(x, graphic = FALSE) {

  poly <- get_com_poly(x)

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
get_com_point <- function(x, graphic = FALSE) {

  poly <- get_com_poly(x)

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

  # read PARCA
  f_parca <- read_sf(get_path("v.seq.parca.poly", dirname = dirname))
  f_id <- get_id(dirname)
  id <- seq_field("identifiant")$name

  # create empty path list
  path <- list()

  # commune layer specifications
  layers <- list(
    topo_poly     = list(fun = get_com_poly,  key = "v.com.topo.poly",   graphic = FALSE),
    topo_line     = list(fun = get_com_line,  key = "v.com.topo.line",   graphic = FALSE),
    topo_point    = list(fun = get_com_point, key = "v.com.topo.point",  graphic = FALSE),
    graphic_line  = list(fun = get_com_line,  key = "v.com.graphic.line",  graphic = TRUE),
    graphic_point = list(fun = get_com_point, key = "v.com.graphic.point", graphic = TRUE)
  )

  for (k in names(layers)) {

    if (layers[[k]]$graphic) {
      f <- layers[[k]]$fun(f_parca, graphic = TRUE)
    } else {
      f <- layers[[k]]$fun(f_parca)
    }

    # NULL = not write
    if (is.null(f)) {

      path[[k]] <- NULL

      if (verbose) {
        cli::cli_alert_info(
          "Com {.field {k}} layer not written (no intersecting feature)"
        )
      }

      next
    }

    # id
    if (nrow(f)>0){
      f[[id]]<- f_id
    }

    # write
    f_path <- quiet(seq_write(
      f,
      layers[[k]]$key,
      dirname = dirname,
      verbose = FALSE,
      overwrite = overwrite
    ))

    path[[k]] <- f_path

    if (verbose) {
      cli::cli_alert_success(
        "Com {.field {k}} layer written with {nrow(f)} feature{?s}"
      )
    }
  }

  invisible(path)
}
