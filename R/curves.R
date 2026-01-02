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

#' Generate hypsometric curves line layer for a Sequoia project
#'
#' Retrieves hypsometric curves line features intersecting and surrounding
#' the project area and writes the resulting layer to disk.
#'
#' @param dirname `character` Path to the project directory.
#'   Defaults to the current working directory.
#' @param verbose `logical`; whether to display informational messages.
#'   Defaults to `TRUE`.
#' @param overwrite `logical`; whether to overwrite existing files.
#'   Defaults to `FALSE`.
#'
#' @details
#' Hypsometric curves line features are retrieved using [get_curves()].
#'
#' If no hypsometric curves features are found, the function returns `NULL`
#' invisibly and no file is written.
#'
#' When features are present, the layer is written to disk using
#' [seq_write()] with the key `"v.curves.line"`.
#'
#' @return
#' Invisibly returns a named list of file paths written by [seq_write()].
#' Returns `NULL` invisibly when no hypsometric curves features are found.
#'
#' @seealso
#' [get_curves()], [seq_write()]
#'
#' @export
seq_curves <- function(
    dirname   = ".",
    verbose   = TRUE,
    overwrite = FALSE
) {

  # Read project area (PARCA)
  f_parca <- sf::read_sf(get_path("v.seq.parca.poly", dirname = dirname))
  f_id    <- get_id(dirname)

  # Retrieve toponyms
  curves <- get_curves(f_parca)

  # Exit early if nothing to write
  if (is.null(curves) || nrow(curves) == 0) {
    if (verbose) {
      cli::cli_alert_info(
        "No hypsometric curves features found: curves layer not written."
      )
    }
    return(invisible(NULL))
  }

  # Add project identifier
  id <- seq_field("identifiant")$name
  curves[[id]] <- f_id

  # Write layer
  curves_path <- quiet(seq_write(
    curves,
    "v.curves.line",
    dirname   = dirname,
    verbose   = FALSE,
    overwrite = overwrite
  ))

  if (verbose) {
    cli::cli_alert_success(
      "Hypsometric curves layer written with {nrow(topo)} feature{?s}"
    )
  }

  invisible(curves_path)
}
