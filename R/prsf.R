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
  crs <- 2154
  x <- sf::st_transform(x, crs)
  fetch_envelope <- envelope(x, 5000)

  # retrieve toponymic point
  prsf <- happign::get_wfs(
    fetch_envelope, "PROTECTEDAREAS.PRSF:prsf", verbose = FALSE
  ) |> sf::st_transform(crs)

  if (!nrow(prsf)) {
    return(NULL)
  }

  return(invisible(prsf))
}

#' Generate PRSF point layer for a Sequoia project
#'
#' Retrieves PRSF point features intersecting and surrounding
#' the project area, and writes the resulting layer to disk.
#'
#' @param dirname `character` Path to the project directory.
#'   Defaults to the current working directory.
#' @param verbose `logical`; whether to display informational messages.
#'   Defaults to `TRUE`.
#' @param overwrite `logical`; whether to overwrite existing files.
#'   Defaults to `FALSE`.
#'
#' @details
#' PRSF point features are retrieved using [get_prsf()].
#'
#' If no PRSF point features are found, the function returns `NULL`
#' invisibly and no file is written.
#'
#' When features are present, the layer is written to disk using
#' [seq_write()] with the key `"v.prsf.point"`.
#'
#' @return
#' Invisibly returns a named list of file paths written by [seq_write()].
#' Returns `NULL` invisibly when no PRSF point features are found.
#'
#' @seealso
#' [get_prsf()], [seq_write()]
#'
#' @export
seq_prsf <- function(
    dirname   = ".",
    verbose   = TRUE,
    overwrite = FALSE
) {

  # Read project area (PARCA)
  f_parca <- sf::read_sf(get_path("v.seq.parca.poly", dirname = dirname))
  f_id    <- get_id(dirname)

  # Retrieve toponyms
  prsf <- get_prsf(f_parca)

  # Exit early if nothing to write
  if (is.null(prsf) || nrow(prsf) == 0) {
    if (verbose) {
      cli::cli_alert_info(
        "No PRSF features found: PRSF layer not written."
      )
    }
    return(invisible(NULL))
  }

  # Add project identifier
  id <- seq_field("identifiant")$name
  prsf[[id]] <- f_id

  # Write layer
  prsf_path <- quiet(seq_write(
    prsf,
    "v.prsf.point",
    dirname   = dirname,
    verbose   = FALSE,
    overwrite = overwrite
  ))

  if (verbose) {
    cli::cli_alert_success(
      "PRSF layer written with {nrow(topo)} feature{?s}"
    )
  }

  invisible(prsf_path)
}
