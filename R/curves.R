#' Retrieve hypsometric curves around an area
#'
#' Builds a convex buffer around the input geometry, retrieves hypsometric
#' curves and returns an `sf` line layer.
#'
#' @param x An `sf` object defining the input area of interest.
#' @param verbose `logical` If `TRUE`, display messages.
#'
#' @return An `sf` object containing hypsometric curves.
#'
#' @details
#' The function creates a 1000 m convex buffer around the input geometry `x`
#' and retrieves hypsometric curves before returns as a single `sf` point layer.
#'
#' @export
get_curves <- function(x, verbose = TRUE) {

  # convex buffer
  crs <- 2154
  x <- sf::st_transform(x, crs)
  fetch_envelope <- envelope(x, 1000)

  if (verbose){
    cli::cli_alert_info("Downloading contour lines dataset...")
  }

  curves <- happign::get_wfs(
    fetch_envelope, "ELEVATION.CONTOUR.LINE:courbe", verbose = FALSE
  ) |> sf::st_transform(crs)

  if (!nrow((curves))) {
    return(NULL)
  }

  curves <- st_intersection(curves, fetch_envelope) |>
    st_cast("LINESTRING") |>
    suppressWarnings()

  return(invisible(curves))
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
#' @param verbose `logical` If `TRUE`, display messages.
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
    dirname = ".",
    verbose = TRUE,
    overwrite = FALSE
) {

  # Read project area (PARCA)
  parca <- seq_read("v.seq.parca.poly", dirname = dirname)
  id_field <- seq_field("identifiant")$name
  id <- unique(parca[[id_field]])

  if (verbose){
    cli::cli_h1("CONTOUR LINES")
  }

  # Retrieve toponyms
  curves <- get_curves(parca, verbose = verbose)

  # Exit early if nothing to write
  if (is.null(curves) || nrow(curves) == 0) {
    cli::cli_warn("No contour lines found: curves layer not written.")
    return(invisible(NULL))
  }

  # Add project identifier
  curves[[id_field]] <- id

  # Write layer
  curves_path <- seq_write(
    curves,
    "v.curves.line",
    dirname = dirname,
    verbose = verbose,
    overwrite = overwrite
  )

  return(invisible(curves_path))
}
