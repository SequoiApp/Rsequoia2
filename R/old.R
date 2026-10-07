#' Retrieve OLD features around an area
#'
#' Builds a convex buffer around the input geometry, retrieves OLD
#' features and returns an `sf` point layer.
#'
#' @param x An `sf` object defining the input area of interest.
#' @param buffer `numeric`; Buffer around `x` (in **meters**) used to enlarge
#' @param verbose `logical`; If `TRUE`, display progress and informational messages.
#'
#' @return An `sf` object containing OLD features.
#'
#' @details
#' The function creates a convex buffer around the input geometry `x`
#' and retrieves OLD features before returns as a single `sf` point layer.
#'
#' @export
get_old <- function(x, buffer = 1000, verbose = TRUE) {

  # convex buffer
  crs <- 2154
  x <- sf::st_transform(x, crs)
  fetch_envelope <- seq_envelope(x, buffer)

  if (verbose){
    cli::cli_alert_info("Downloading OLD dataset...")
  }

  # retrieve toponymic point
  old <- happign::get_wfs(
    x = fetch_envelope,
    layer = "DEBROUSSAILLEMENT:debroussaillement",
    predicate = happign::intersects(),
    verbose = FALSE
  )

  if (!nrow(old)) {
    return(NULL)
  }

  return(invisible(sf::st_transform(old, crs)))
}

#' Fetch OLD data
#'
#' @inheritParams get_old
#' @param dirname `character`; Output directory.
#' @param id Optional Sequoia project identifier.
#' @param overwrite `logical`; If `TRUE`, overwrite an existing layer.
#'
#' @return Invisibly returns the written layer path, or `NULL`.
#'
#' @keywords internal
#' @noRd
.old_fetcher <- function(
    x,
    dirname,
    id = NULL,
    buffer = 1000,
    verbose = TRUE,
    overwrite = FALSE) {

  if (verbose) {
    cli::cli_h1("OLD")
    cli::cli_progress_message("Downloading OLD layer...")
  }

  old <- get_old(x, buffer = buffer, verbose = FALSE)

  if (is.null(old)) {
    if (verbose) cli::cli_alert_warning("No OLD found.")
    return(invisible(NULL))
  }

  if (!is.null(id)) {
    identifier <- seq_field("identifier")$name
    old[[identifier]] <- id
  }

  path <- seq_write(
    old,
    key = "v.secu.old.poly",
    dirname = dirname,
    id = id,
    verbose = verbose,
    overwrite = overwrite
  )

  invisible(path)
}


#' Fetch OLD layer for an area
#'
#' Retrieves OLD points around `x` and writes the resulting layer to
#' `dirname`.
#'
#' @inheritParams get_old
#' @param dirname `character`; Output directory.
#' @param overwrite `logical`; If `TRUE`, overwrite an existing layer.
#'
#' @return Invisibly returns the written layer path, or `NULL`.
#'
#' @keywords internal
#' @noRd
fetch_old <- function(
    x,
    dirname,
    buffer = 1000,
    verbose = TRUE,
    overwrite = FALSE) {

  .old_fetcher(
    x = x,
    dirname = dirname,
    buffer = buffer,
    verbose = verbose,
    overwrite = overwrite
  )
}


#' Generate OLD layer for a Sequoia project
#'
#' Retrieves OLD features intersecting and surrounding
#' the project area, and writes the resulting layer to disk.
#'
#' @inheritParams get_old
#' @inheritParams seq_write
#'
#' @details
#' OLD features are retrieved using [get_old()].
#'
#' If no OLD features are found, the function returns `NULL`
#' invisibly and no file is written.
#'
#' When features are present, the layer is written to disk using
#' [seq_write()] with the key `"v.old.poly"`.
#'
#' @return
#' Invisibly returns a named list of file paths written by [seq_write()].
#' Returns `NULL` invisibly when no OLD features are found.
#'
#' @seealso
#' [get_prsf()], [seq_write()]
#'
#' @export
seq_old <- function(
    dirname = ".",
    buffer = 1000,
    verbose = TRUE,
    overwrite = FALSE
) {

  ctx <- .seq_context(dirname)

  .old_fetcher(
    x = ctx$parca,
    dirname = dirname,
    id = ctx$id,
    buffer = buffer,
    verbose = verbose,
    overwrite = overwrite
  )

}
