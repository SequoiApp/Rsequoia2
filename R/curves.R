#' Retrieve contour lines around an area
#'
#' Retrieves contour lines within a buffered envelope around an area.
#'
#' @param x `sf` or `sfc`; Input area.
#' @param buffer `numeric`; Buffer around `x`, in meters.
#' @param verbose `logical`; If `TRUE`, display messages.
#'
#' @return An `sf` line layer, or `NULL` if no contour line is found.
#'
#' @export
get_curves <- function(x, buffer = 5000, verbose = TRUE) {

  if (!inherits(x, c("sf", "sfc"))) {
    cli::cli_abort("{.arg x} must be {.cls sf} or {.cls sfc}.")
  }

  crs <- 2154
  x <- sf::st_transform(x, crs)

  if (verbose) cli::cli_alert_info("Downloading contour lines dataset...")

  curves <- happign::get_wfs(
    seq_envelope(x, buffer),
    layer = "ELEVATION.CONTOUR.LINE:courbe",
    verbose = FALSE
  )

  if (is.null(curves) || !nrow(curves)) {
    return(NULL)
  }

  invisible(sf::st_transform(curves, crs))
}


#' Transform contour lines
#'
#' Clips contour lines to a buffered envelope around an area.
#'
#' @inheritParams get_curves
#' @param curves `sf`; Raw contour-line layer.
#'
#' @return An `sf` line layer, or `NULL` if no line remains.
#'
#' @keywords internal
#' @noRd
.curves_transformer <- function(curves, x, buffer = 5000) {

    if (is.null(curves) || !nrow(curves)) {
      return(NULL)
    }

    curves <- suppressWarnings(
      curves |>
        sf::st_transform(sf::st_crs(x)) |>
        sf::st_intersection(seq_envelope(x, buffer)) |>
        sf::st_collection_extract("LINESTRING") |>
        sf::st_cast("LINESTRING")
    )

    if (!nrow(curves)){
      return(NULL)
    }

  curves
}


#' Fetch contour lines
#'
#' @inheritParams get_curves
#' @param dirname `character`; Output directory.
#' @param id Optional Sequoia project identifier.
#' @param overwrite `logical`; If `TRUE`, overwrite an existing layer.
#'
#' @return Invisibly returns the written layer path, or `NULL`.
#'
#' @keywords internal
#' @noRd
.curves_fetcher <- function(
    x,
    dirname,
    id = NULL,
    buffer = 5000,
    verbose = TRUE,
    overwrite = FALSE) {

  if (verbose) {
    cli::cli_h1("CONTOUR LINES")
    cli::cli_progress_message("Downloading CONTOUR LINES layer...", clear = TRUE)
  }

  curves <- get_curves(x, buffer = buffer, verbose = FALSE)
  curves <- .curves_transformer(curves, x, buffer)
  if (is.null(curves)) {
    if (verbose) cli::cli_alert_info("No contour-line features found.")
    return(invisible(NULL))
  }

  if (!is.null(id)) {
    identifier <- seq_field("identifier")$name
    curves[[identifier]] <- id
  }

  path <- seq_write(
    curves,
    key = "v.curves.line",
    dirname = dirname,
    id = id,
    verbose = verbose,
    overwrite = overwrite
  )

  invisible(path)
}


#' Fetch contour lines for an area
#'
#' Retrieves contour lines around `x` and writes them to `dirname`.
#'
#' @inheritParams get_curves
#' @param dirname `character`; Output directory.
#' @param overwrite `logical`; If `TRUE`, overwrite an existing layer.
#'
#' @return Invisibly returns the written layer path, or `NULL`.
#'
#' @keywords internal
#' @noRd
fetch_curves <- function(
    x,
    dirname,
    buffer = 5000,
    verbose = TRUE,
    overwrite = FALSE) {

  .curves_fetcher(
    x = x,
    dirname = dirname,
    buffer = buffer,
    verbose = verbose,
    overwrite = overwrite
  )
}


#' Generate contour-line layer for a Sequoia project
#'
#' Retrieves contour lines around the project area and writes the resulting
#' layer to the Sequoia project directory.
#'
#' @inheritParams get_curves
#' @inheritParams seq_write
#'
#' @return Invisibly returns the written layer path, or `NULL`.
#'
#' @seealso [get_curves()], [seq_write()]
#'
#' @export
seq_curves <- function(
    dirname = ".",
    buffer = 5000,
    verbose = TRUE,
    overwrite = FALSE) {

  ctx <- .seq_context(dirname)

  .curves_fetcher(
    x = ctx$parca,
    dirname = dirname,
    id = ctx$id,
    buffer = buffer,
    verbose = verbose,
    overwrite = overwrite
  )
}
