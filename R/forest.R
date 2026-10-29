#' @title Retrieve BD_FORET_V2 around an area
#'
#' @description
#' Builds a convex buffer around the input geometry, retrieves BD_FORET_V2
#' features and returns an `sf` point layer.
#'
#' @param x An `sf` object defining the input area of interest.
#' @param buffer `numeric`; Buffer around `x` (in **meters**) used to enlarge
#' @param verbose `logical`; If `TRUE`, display progress and informational messages.
#'
#' @return An `sf` object containing BD_FORET_V2 features.
#'
#' @details
#' The function creates a convex buffer around the input geometry `x`
#' and retrieves BD_FORET_V2 features before returns as a single `sf` point layer.
#'
#' @export
get_forest<- function(
    x,
    buffer = 1000,
    verbose = TRUE
) {

  if (!inherits(x, c("sf", "sfc"))) {
    cli::cli_abort("{.arg x} must be {.cls sf} or {.cls sfc}.")
  }

  crs <- 2154
  x <- sf::st_transform(x, crs)
  fetch_envelope <- seq_envelope(x, buffer)

  forest <- happign::get_wfs(
    fetch_envelope,
    layer = "LANDCOVER.FORESTINVENTORY.V2:formation_vegetale",
    predicate = happign::intersects(),
    verbose = FALSE
  )

  if (!nrow(forest)) {
    return(NULL)
  }

  return(invisible(sf::st_transform(forest, crs)))
}

#' @title Fetch BD_FORET_V2 data
#'
#' @inheritParams get_forest
#' @param dirname `character`; Output directory.
#' @param id Optional Sequoia project identifier.
#' @param overwrite `logical`; If `TRUE`, overwrite an existing layer.
#'
#' @return Invisibly returns the written layer path, or `NULL`.
#'
#' @keywords internal
#' @noRd
.forest_fetcher <- function(
    x,
    dirname,
    id = NULL,
    buffer = 1000,
    verbose = TRUE,
    overwrite = FALSE
) {

  if (verbose) {
    cli::cli_h1("BD FORET V2")
    cli::cli_progress_message("Downloading BD_FORET_V2 layer...", clear = TRUE)
  }

  forest <- get_forest(x, buffer = buffer, verbose = FALSE)

  if (is.null(forest)) {
    if (verbose) cli::cli_alert_warning("No BD_FORET_V2 found.")
    return(invisible(NULL))
  }

  if (!is.null(id)) {
    identifier <- seq_field("identifier")$name
    forest[[identifier]] <- id
  }

  path <- seq_write(
    forest,
    key = "v.forest.poly",
    dirname = dirname,
    id = id,
    verbose = verbose,
    overwrite = overwrite
  )

  return(invisible(path))
}


#' @title Fetch BD_FORET_V2 layer for an area
#'
#' @description
#' Retrieves BD_FORET_V2 points around `x` and writes the resulting layer to
#' `dirname`.
#'
#' @inheritParams get_forest
#' @param dirname `character`; Output directory.
#' @param overwrite `logical`; If `TRUE`, overwrite an existing layer.
#'
#' @return Invisibly returns the written layer path, or `NULL`.
#'
#' @keywords internal
#' @noRd
fetch_forest <- function(
    x,
    buffer = 1000,
    dirname,
    verbose = TRUE,
    overwrite = FALSE
){

  .forest_fetcher(
    x = x,
    dirname = dirname,
    buffer = buffer,
    verbose = verbose,
    overwrite = overwrite
  )
}


#' @title Generate BD_FORET_V2 layer for a Sequoia project
#'
#' @description
#' Retrieves BD_FORET_V2 features intersecting and surrounding
#' the project area, and writes the resulting layer to disk.
#'
#' @inheritParams get_forest
#' @inheritParams seq_write
#'
#' @details
#' BD_FORET_V2 features are retrieved using [get_forest()].
#'
#' If no BD_FORET_V2 features are found, the function returns `NULL`
#' invisibly and no file is written.
#'
#' When features are present, the layer is written to disk using
#' [seq_write()] with the key `"v.forest.poly"`.
#'
#' @return
#' Invisibly returns a named list of file paths written by [seq_write()].
#' Returns `NULL` invisibly when no BD_FORET_V2 features are found.
#'
#' @seealso
#' [get_forest()], [seq_write()]
#'
#' @export
seq_forest <- function(
    dirname = ".",
    buffer = 1000,
    verbose = TRUE,
    overwrite = FALSE
) {

  ctx <- .seq_context(dirname)

  .forest_fetcher(
    x = ctx$parca,
    dirname = dirname,
    id = ctx$id,
    buffer = buffer,
    verbose = verbose,
    overwrite = overwrite
  )
}
