#' Retrieve PRSF point features around an area
#'
#' Builds a convex buffer around the input geometry, retrieves PRSF
#' point features and returns an `sf` point layer.
#'
#' @param x An `sf` object defining the input area of interest.
#'
#' @return An `sf` object containing PRSF point features.
#'
#' @details
#' The function creates a 5000 m convex buffer around the input geometry `x`
#' and retrieves PRSF point features before returns as a single `sf` point layer.
#'
#' @export
get_prsf <- function(x) {

  # convex buffer
  convex <- envelope(x, 5000)

  # retrieve toponymic point
  prsf <- get_topo(convex, "PROTECTEDAREAS.PRSF:prsf")

  if (is.null(prsf)) {
    return(NULL)
  }

  invisible(prsf)
}
