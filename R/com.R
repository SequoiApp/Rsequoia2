#' Retrieve commune boundaries around an area
#'
#' Downloads commune polygons from BDTOPO around the input geometry and
#' normalizes their attributes.
#'
#' @param x `sf` object used as the input area.
#' @param buffer `numeric`; Buffer distance, in meters, passed to
#'   [seq_envelope()].
#' @param verbose `logical`; If `TRUE`, display messages.
#'
#' @return An `sf` polygon layer containing commune boundaries, or `NULL`
#'   if no commune is found.
#'
#' @export
get_com <- function(x, buffer = 2000, verbose = TRUE) {

  crs <- 2154

  x <- sf::st_transform(x, crs)
  envelope <- seq_envelope(x, buffer)

  if (verbose) {
    cli::cli_alert_info("Downloading communes dataset...")
  }

  com <- happign::get_wfs(
    envelope,
    layer = "BDTOPO_V3:commune",
    verbose = FALSE
  )

  if (!nrow(com)) {
    return(NULL)
  }

  com <- seq_normalize(com, "com_poly") |>
    sf::st_transform(crs)

  invisible(com)
}


#' Derive commune geometry
#'
#' Converts commune polygons to line or point geometries.
#'
#' @param x Commune polygon layer.
#' @param type Geometry type to derive: `"line"` or `"point"`.
#'
#' @return An `sf` object containing derived geometries.
#'
#' @keywords internal
#' @noRd
derive_com <- function(x, type = c("line", "point")) {

  type <- match.arg(type)

  switch(
    type,
    line = poly_to_line(x),
    point = suppressWarnings(
      sf::st_centroid(
        x,
        of_largest_polygon = FALSE
      )
    )
  )
}


#' Clip commune geometries
#'
#' Clips an existing commune layer to an area of interest.
#'
#' @param x Commune `sf` layer.
#' @param clip `sf` object used as clipping geometry.
#' @param verbose `logical`; If `TRUE`, display messages.
#'
#' @return The clipped `sf` layer, or `NULL` if there is no intersection.
#'
#' @keywords internal
#' @noRd
clip_com <- function(x, clip, verbose = TRUE) {

  out <- suppressWarnings(
    sf::st_intersection(x, clip)
  )

  if (!nrow(out)) {
    if (verbose) {
      cli::cli_alert_warning(
        "No intersection with area of interest."
      )
    }

    return(NULL)
  }

  invisible(out)
}


#' Build commune layers
#'
#' Downloads commune polygons once and derives the topological and graphical
#' representations used by Sequoia.
#'
#' @inheritParams get_com
#'
#' @return A named list of commune layers, or `NULL` if no commune is found.
#'
#' @keywords internal
#' @noRd
.build_com_layers <- function(x, verbose = TRUE) {

  poly <- get_com(x, buffer = 2000, verbose = verbose)
  if (is.null(poly)) {
    return(NULL)
  }

  graphic_poly <- clip_com(
    poly,
    seq_envelope(x, 500),
    verbose = verbose
  )

  layers <- list(
    "v.com.topo.poly" = poly,
    "v.com.topo.line" = derive_com(poly, "line"),
    "v.com.topo.point" = derive_com(poly, "point")
  )

  if (!is.null(graphic_poly)) {
    layers[["v.com.graphic.line"]] <- derive_com(
      graphic_poly,
      "line"
    )

    layers[["v.com.graphic.point"]] <- derive_com(
      graphic_poly,
      "point"
    )
  }

  return(layers)
}


#' Search commune layers
#'
#' Retrieves commune layers around `x` and writes them to `out`.
#'
#' @param x `sf` object used as the input area.
#' @param out `character`; Output directory.
#' @param verbose `logical`; If `TRUE`, display messages.
#' @param overwrite `logical`; If `TRUE`, overwrite existing files.
#'
#' @return Invisibly returns a named list of written file paths, or `NULL`
#'   if no commune is found.
#'
#' @keywords internal
#' @noRd
fetch_com <- function(
    x,
    out,
    verbose = TRUE,
    overwrite = FALSE) {

  if (verbose) {
    cli::cli_h1("COMMUNES")
  }

  layers <- .build_com_layers(
    x,
    verbose = verbose
  )

  if (is.null(layers)) {
    if (verbose) {
      cli::cli_alert_info("No commune layer found.")
    }

    return(invisible(NULL))
  }

  paths <- lapply(names(layers), function(k) {
    write_vect(
      layers[[k]],
      file.path(out, seq_layer(k)$filename),
      overwrite = overwrite,
      verbose = verbose
    )
  })

  names(paths) <- names(layers)

  return(invisible(paths))
}


#' Generate commune layers for a Sequoia project
#'
#' Retrieves commune layers around the project area and writes them to the
#' Sequoia project directory.
#'
#' The project PARCA layer is used as the search geometry and its identifier
#' is added to each generated layer before writing.
#'
#' @inheritParams seq_write
#'
#' @return Invisibly returns a named list of written file paths, or `NULL`
#'   if no commune is found.
#'
#' @seealso [get_com()]
#'
#' @export
seq_com <- function(
    dirname = ".",
    verbose = TRUE,
    overwrite = FALSE) {

  parca <- seq_read(
    "v.seq.parca.poly",
    dirname = dirname
  )

  identifier <- seq_field("identifier")$name
  id <- unique(parca[[identifier]])

  if (verbose) {
    cli::cli_h1("COMMUNES")
  }

  layers <- .build_com_layers(
    parca,
    verbose = verbose
  )

  if (is.null(layers)) {
    if (verbose) {
      cli::cli_alert_info("No commune layer found.")
    }

    return(invisible(NULL))
  }

  paths <- lapply(names(layers), function(k) {

    f <- layers[[k]]
    f[[identifier]] <- id

    seq_write(
      f,
      k,
      dirname = dirname,
      id = id,
      verbose = verbose,
      overwrite = overwrite
    )
  })

  names(paths) <- names(layers)

  return(invisible(paths))
}
