#' Retrieve OLD features around an area
#'
#' Builds a convex buffer around the input geometry, retrieves OLD
#' features and returns an `sf` point layer.
#'
#' @param x An `sf` object defining the input area of interest.
#' @param buffer `numeric`; Buffer around `x` (in **meters**) used to enlarge
#' @param verbose `logical` If `TRUE`, display messages.
#'
#' @return An `sf` object containing OLD features.
#'
#' @details
#' The function creates a convex buffer around the input geometry `x`
#' and retrieves OLD features before returns as a single `sf` point layer.
#'
#' @export
get_old <- function(x,
                    buffer = 1000,
                    verbose = TRUE) {

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

  # read PARCA
  parca <- seq_read("v.seq.parca.poly", dirname = dirname)
  id_field <- seq_field("identifier")$name
  id <- unique(parca[[id_field]])

  if (verbose){
    cli::cli_h1("OLD")
  }

  # Retrieve toponyms
  old <- get_old(parca, buffer = buffer, verbose = verbose)

  if (!is.null(old)){
    old[[id_field]] <- id

    old <- seq_write(
      old,
      "v.secu.old.poly",
      dirname = dirname,
      id = id,
      verbose = verbose,
      overwrite = overwrite
    )
  }

  return(invisible(c(old) |> as.list()))
}
