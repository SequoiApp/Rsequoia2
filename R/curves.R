#' Retrieve hypsometric curves around an area
#'
#' Builds a convex buffer around the input geometry, retrieves hypsometric
#' curves and returns an `sf` line layer.
#'
#' @param x An `sf` object defining the input area of interest.
#'
#' @return An `sf` object containing hypsometric curves.
#'
#' @details
#' The function creates a 1000 m convex buffer around the input geometry `x`
#' and retrieves hypsometric curves before returns as a single `sf` point layer.
#'
#' @export
get_curves <- function(x) {

  # convex buffer
  convex <- buffer_to_convex(x, 1000)

  # retrieve toponymic point
  curves <- get_topo(convex, "ELEVATION.CONTOUR.LINE:courbe")

  if (is.null(curves)) {
    return(NULL)
  }

  curves <- st_intersection(curves, convex) |>
    st_cast("LINESTRING") |>
    quiet()

  curves <- curves[, setdiff(names(curves), names(convex))]

  invisible(curves)
}
