#' Retrieve PRSF point features around an area
#'
#' Retrieves PRSF point features around an area of interest.
#'
#' @param x An `sf` or `sfc` object defining the input area.
#' @param buffer `numeric`; Buffer around `x`, in meters.
#' @param verbose `logical`; If `TRUE`, display messages.
#'
#' @return An `sf` point layer, or `NULL` if no PRSF feature is found.
#'
#' @export
get_prsf <- function(x, buffer = 5000, verbose = TRUE) {

  crs <- 2154
  x <- sf::st_transform(x, crs)

  if (verbose) cli::cli_alert_info("Downloading PRSF dataset...")

  prsf <- happign::get_wfs(
    seq_envelope(x, buffer),
    layer = "PROTECTEDAREAS.PRSF:prsf",
    predicate = happign::intersects(),
    verbose = FALSE
  )

  if (!nrow(prsf)) return(NULL)

  invisible(sf::st_transform(prsf, crs))
}

#' Fetch PRSF data
#'
#' @inheritParams get_prsf
#' @param dirname `character`; Output directory.
#' @param id Optional Sequoia project identifier.
#' @param overwrite `logical`; If `TRUE`, overwrite an existing layer.
#'
#' @return Invisibly returns the written layer path, or `NULL`.
#'
#' @keywords internal
#' @noRd
.prsf_fetcher <- function(
    x,
    dirname,
    id = NULL,
    buffer = 5000,
    verbose = TRUE,
    overwrite = FALSE) {

  if (verbose) {
    cli::cli_h1("PRSF")
    cli::cli_progress_message("Downloading PRSF layer...")
  }

  prsf <- get_prsf(x, buffer = buffer, verbose = FALSE)

  if (is.null(prsf)) {
    if (verbose) cli::cli_alert_warning("No PRSF found.")
    return(invisible(NULL))
  }

  if (!is.null(id)) {
    identifier <- seq_field("identifier")$name
    prsf[[identifier]] <- id
  }

  path <- seq_write(
    prsf,
    key = "v.secu.prsf.point",
    dirname = dirname,
    id = id,
    verbose = verbose,
    overwrite = overwrite
  )

  invisible(path)
}


#' Fetch PRSF layer for an area
#'
#' Retrieves PRSF points around `x` and writes the resulting layer to
#' `dirname`.
#'
#' @inheritParams get_prsf
#' @param dirname `character`; Output directory.
#' @param overwrite `logical`; If `TRUE`, overwrite an existing layer.
#'
#' @return Invisibly returns the written layer path, or `NULL`.
#'
#' @keywords internal
#' @noRd
fetch_prsf <- function(
    x,
    dirname,
    buffer = 5000,
    verbose = TRUE,
    overwrite = FALSE) {

  .prsf_fetcher(
    x = x,
    dirname = dirname,
    buffer = buffer,
    verbose = verbose,
    overwrite = overwrite
  )
}


#' Generate PRSF layer for a Sequoia project
#'
#' Retrieves PRSF point features around the project area and writes the
#' resulting layer to the Sequoia project directory.
#'
#' @inheritParams get_prsf
#' @inheritParams seq_write
#'
#' @return Invisibly returns the written layer path, or `NULL`.
#'
#' @seealso [get_prsf()], [seq_write()]
#'
#' @export
seq_prsf <- function(
    dirname = ".",
    buffer = 5000,
    verbose = TRUE,
    overwrite = FALSE) {

  ctx <- .seq_context(dirname)

  .prsf_fetcher(
    x = ctx$parca,
    dirname = dirname,
    id = ctx$id,
    buffer = buffer,
    verbose = verbose,
    overwrite = overwrite
  )
}
