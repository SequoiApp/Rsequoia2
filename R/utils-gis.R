#' Use to get type of a geometry
#'
#' @param x `sf`
#' @importFrom sf st_geometry_type
#'
#' @export
get_geom_type <- function(x) {
  a <- list(
    POINT = "POINT",
    POINT = "MULTIPOINT",
    LINE = "LINESTRING",
    LINE = "MULTILINESTRING",
    POLYGON = "POLYGON",
    POLYGON = "MULTIPOLYGON",
    other = "GEOMETRY",
    other = "GEOMETRYCOLLECTION",
    other = "CIRCULARSTRING",
    other = "COMPOUNDCURVE",
    other = "CURVEPOLYGON",
    other = "MULTICURVE",
    other = "MULTISURFACE",
    other = "CURVE",
    other = "SURFACE",
    other = "POLYHEDRALSURFACE",
    other = "TIN",
    other = "TRIANGLE"
  )
  type <- sf::st_geometry_type(x)
  levels(type) <- a
  type <- as.character(unique(type))
  if (length(type) > 1) {
    stop("GEOMETRYCOLLECTION objects should have consistent type",
         call. = FALSE
    )
  }
  return(type)
}

#' Create an empty sf object
#'
#' This function creates an empty `sf` object with the desired geometry type
#' (point, line, polygon, etc.).
#'
#' @param geom_type `character`; one of the geometry types supported by `sf`: `POLYGON`,
#' `LINESTRING`, `POINT`, `MULTIPOLYGON`, `MULTILINESTRING`, `MULTIPOINT`.
#' @param ... Named attributes with their classes. For example, you can pass
#'  arguments like `PLACETTE = character(0)`. Each argument must be named according
#'  to the attribute, with the corresponding empty vector of the appropriate class.
#'
#' @importFrom sf st_sf st_sfc
#'
#' @return An `sf` object
#'
#' @examples
#' \dontrun{
#' empty_sf <- create_empty_sf(
#'   "LINESTRING",
#'   PLACETTE = character(0),
#'   TSE_VOL = numeric(0)
#' )
#' }
#'
#' @export
create_empty_sf <- function(geom_type, ...){
  empty_sf_geom <- sf::st_sfc()
  class(empty_sf_geom) <- c(paste0("sfc_", toupper(geom_type)), class(empty_sf_geom)[-1])
  fields <- list(..., geometry = empty_sf_geom)
  template <- sf::st_sf(fields, crs = 2154)
  return(template)
}

#' Get a border layer from polygons
#'
#' This function convert polygon to line and remove shared lines
#'
#' @param x `sf POLYGONS`, using a projected CRS
#' @note
#' If the polygon layer contains topology errors (such as contiguous
#' polygons not sharing exactly the same boundary)
#'
#' @importFrom sf st_is_longlat
#'
#' @return An `sf` object with two `MULTILINESTRING` geometry is returned
#'
#' @export
poly_to_line <- function(x) {

  if (get_geom_type(x) != "POLYGON") {
    stop(paste0('"x" should be a POLYGON sf object'), call. = FALSE)
  }

  if (sf::st_is_longlat(x)) {
    stop("This feature does not work on unprojected (long/lat) layers.",
         call. = FALSE
    )
  }

  lines <- list(
    shared = extract_shared_lines(x),
    outer  = extract_outer_lines(x)
  )

  # remove empty result (if no shared line)
  lines <- Filter(\(x) !is.null(x), lines)

  lines_sf <- do.call(
    rbind,
    lapply(names(lines), function(n) {
      sf::st_sf(type = n, geometry = lines[[n]])
    })
  )

  return(lines_sf)

}

#' Extract shared lines between polygons
#' @param x `sf`
#' @importFrom sf st_is_longlat st_geometry st_intersection st_collection_extract st_union
#' @noRd
extract_shared_lines <- function(x){

  if (get_geom_type(x) != "POLYGON") {
    stop(paste0('"x" should be a POLYGON sf object'), call. = FALSE)
  }

  if (sf::st_is_longlat(x)) {
    stop("This feature does not work on unprojected (long/lat) layers.",
         call. = FALSE
    )
  }

  x <- sf::st_geometry(x)

  shared_lines <- tryCatch({
    sf::st_intersection(x, x) |>
      sf::st_collection_extract("LINESTRING") |>
      sf::st_union() |>
      sf::st_line_merge() |>
      sf::st_cast("LINESTRING")
  },
  error = function(e) {
    NULL
  })

  return(shared_lines)
}

#' Extract outer of polygons
#' @param x `sf`
#' @importFrom sf st_is_longlat st_geometry st_union st_cast
#' @noRd
extract_outer_lines <- function(x){

  if (get_geom_type(x) != "POLYGON") {
    stop(paste0('"x" should be a POLYGON sf object'), call. = FALSE)
  }

  if (sf::st_is_longlat(x)) {
    stop("This feature does not work on unprojected (long/lat) layers.",
         call. = FALSE
    )
  }

  x <- sf::st_geometry(x)
  outer_lines <- x |>
    sf::st_union() |>
    sf::st_boundary() |>
    sf::st_cast("LINESTRING")

  return(outer_lines)
}

#' @title Dissolve polygons using buffer-based tolerance
#'
#' @description
#' Performs a topological dissolve (union) of polygon geometries using a
#' buffer-based approach to handle small gaps, overlaps, and sliver geometries.
#'
#' The method applies a positive buffer to expand geometries, merges them using
#' a union operation, and then applies a negative buffer to restore the original
#' extent. A small epsilon offset is used to avoid geometry collapse during the
#' shrinking phase.
#'
#' This approach is particularly useful when dealing with imperfect polygon
#' coverages (e.g. digitizing errors, micro-gaps, or near-adjacent features).
#'
#' @param x An `sf` object containing only `POLYGON` or `MULTIPOLYGON`
#'   geometries.
#'
#' @param buffer `numeric`; Buffer distance (in coordinate units) used to expand
#'   geometries before union. This value controls the tolerance for merging
#'   nearby or slightly disconnected polygons. Default is `5`.
#'
#' @param eps `numeric`; Small offset applied during the negative buffer step
#'   (`-buffer + eps`) to prevent topology collapse or invalid geometries.
#'   Should be a very small positive value relative to `buffer`.
#'   Default is `0.01`.
#'
#' @param endCapStyle `character`; Shape of line endings used in buffering.
#'   One of `"ROUND"`, `"FLAT"`, or `"SQUARE"`. Passed to [sf::st_buffer()].
#'
#' @param joinStyle `character`; Shape of corners in buffering.
#'   One of `"ROUND"`, `"MITRE"`, or `"BEVEL"`. Passed to [sf::st_buffer()].
#'
#' @param mitreLimit `numeric`; Mitre ratio limit (only used if
#'   `joinStyle = "MITRE"`). Passed to [sf::st_buffer()].
#'
#' @param snapping `logical`; If `TRUE`, snaps the resulting geometries back to
#'   the original input geometries using [sf::st_snap()] with the same tolerance
#'   as `buffer`. This can help reduce spatial drift introduced by buffering.
#'   Default is `FALSE`.
#'
#' @details
#' The algorithm proceeds as follows:
#' \enumerate{
#'   \item Cast geometries to `POLYGON`
#'   \item Apply a positive buffer (`+buffer`)
#'   \item Perform a geometric union (`st_union`)
#'   \item Apply a negative buffer (`-buffer + eps`)
#'   \item Validate geometries (`st_make_valid`)
#'   \item Recast to `POLYGON`
#' }
#'
#' Note that this method modifies geometry boundaries and may introduce slight
#' positional shifts. The magnitude of these shifts depends on the chosen
#' `buffer` and `eps` values.
#'
#' @return
#' An `sf` object containing dissolved polygon geometries.
#'
#' @seealso
#' [sf::st_union()], [sf::st_buffer()], [sf::st_make_valid()], [sf::st_snap()]
#'
#' @importFrom cli cli_abort
#' @importFrom sf st_cast st_buffer st_union st_make_valid st_as_sf st_snap
#'
#' @export
seq_dissolve <- function(
    x,
    buffer = 5,
    eps = 0.01,
    endCapStyle = "SQUARE",
    joinStyle = "MITRE",
    mitreLimit = 5,
    snapping = FALSE){

  if (!inherits(x, "sf")) {
    cli::cli_abort("{.arg x} must be an {.cls sf} object.")
  }

  # Geometry type validation
  geom_type <- unique(as.character(sf::st_geometry_type(x)))

  if (!all(geom_type %in% c("POLYGON", "MULTIPOLYGON"))) {
    cli::cli_abort(
      "{.arg x} must contain only POLYGON or MULTIPOLYGON geometries."
    )
  }

  res <- x |>
    sf::st_cast("POLYGON") |>
    sf::st_buffer(buffer,
                  endCapStyle = endCapStyle,
                  joinStyle = joinStyle,
                  mitreLimit = mitreLimit) |>
    sf::st_union() |>
    sf::st_buffer(-buffer + eps,
                  endCapStyle = endCapStyle,
                  joinStyle = joinStyle,
                  mitreLimit = mitreLimit) |>
    sf::st_make_valid() |>
    sf::st_as_sf() |>
    sf::st_cast("POLYGON") |>
    quiet()

  if (snapping){
    res <- sf::st_snap(res, x, buffer)
  }

  return(res)
}

#' @title Compute convex hulls from buffered geometries
#'
#' @description
#' Generates convex hull polygons from an input `sf` object after applying a
#' buffer and union operation. The buffer step allows merging nearby or
#' disconnected geometries prior to computing the convex hull, making the result
#' more spatially coherent.
#'
#' This function is useful for deriving generalized envelopes around clusters of
#' geometries while tolerating small gaps or spatial fragmentation.
#'
#' @param x An `sf` object.
#'
#' @param buffer `numeric`; Buffer distance (in CRS units) applied before union.
#'   This parameter controls the aggregation of nearby geometries. Larger values
#'   will merge more distant features. Default is `0`.
#'
#' @param crs `numeric` or `character`; Target coordinate reference system used
#'   for geometric operations. Defaults to EPSG:2154 (Lambert-93).
#'
#' @details
#' The algorithm proceeds as follows:
#' \enumerate{
#'   \item Validate geometries (`st_make_valid`)
#'   \item Reproject to the target CRS (`st_transform`)
#'   \item Apply a buffer of size `dist`
#'   \item Merge geometries (`st_union`)
#'   \item Split into individual polygon components (`st_cast`)
#'   \item Compute a convex hull for each component (`st_convex_hull`)
#'   \item Validate and return as an `sf` object
#' }
#'
#' Note that the buffer step alters the geometry extent and may introduce
#' spatial generalization. The resulting convex hulls are therefore dependent on
#' the chosen `dist` value.
#'
#' @return
#' An `sf` object containing convex hull polygons (one per connected component).
#'
#' @seealso
#' [sf::st_convex_hull()], [sf::st_union()], [sf::st_buffer()],
#' [sf::st_make_valid()]
#'
#' @importFrom sf st_make_valid st_transform st_buffer st_union st_cast st_convex_hull st_sf
#'
#' @export
seq_envelope <- function(x, buffer = 0, crs = 2154) {

  x <- sf::st_make_valid(x) |>
    sf::st_transform(crs = crs) |>
    sf::st_buffer(buffer) |>
    sf::st_union()

  # split disconnected parts
  x <- sf::st_cast(x, "POLYGON", warn = FALSE)

  envelope <- sf::st_convex_hull(x) |>
    sf::st_make_valid() |>
    sf::st_sf()

  return(envelope)
}
