#' Use to get type of a geometry
#'
#' @param x `sf`
#' @importFrom sf st_geometry_type
#'
#' @noRd
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

  lines_sf <- sf::st_sf(
    type = names(lines),
    geometry = do.call(c, lines)
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
      sf::st_union()
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
  outer_lines <- sf::st_union(x) |>
    sf::st_cast("MULTILINESTRING") |>
    sf::st_union()

  return(outer_lines)
}

#' Union of polygon with a tolernace based on buffer
#' @param x `sf`
#' @param tol `numeric`; Buffer distance used during the dissolve process to
#' close gaps and merge touching polygons.
#' @param eps `numeric`; Small post-processing offset to prevent geometry
#' collapse after the negative buffer. Usually a tiny value.
#' @importFrom sf st_is_longlat st_geometry st_union st_cast
#' @noRd
dissolve <- function(
    x,
    tol,
    eps = 0.01,
    endCapStyle = "SQUARE",
    joinStyle = "MITRE",
    mitreLimit = 5){

  if (get_geom_type(x) != "POLYGON") {
    cli::cli_abort("{.arg x} should be a POLYGON {.cls sf} object")
  }

  x_dissolved <- x |>
    sf::st_buffer(
      tol,
      endCapStyle = endCapStyle,
      joinStyle = joinStyle,
      mitreLimit = mitreLimit
    ) |>
    sf::st_union() |>
    sf::st_buffer(
      -tol + eps,
      endCapStyle = endCapStyle,
      joinStyle = joinStyle,
      mitreLimit = mitreLimit
    ) |>
    sf::st_make_valid() |>
    sf::st_snap(x, tol)

  return(x_dissolved)
}

#' Create convex polygons from buffer-expanded geometries
#'
#' This optimized version avoids unnecessary unions, ensures valid input data,
#' handles multipolygons correctly, and reduces processing time on large layers.
#'
#' @param x An `sf` object.
#' @param dist Buffer distance (CRS units).
#' @param crs EPSG code for output projection (default 4326).
#'
#' @return An `sf` object of convex polygons.
#'
#' @noRd
buffer_to_convex <- function(x, dist, crs = 2154) {

  # 1. Ensure valid input geometries
  x <- sf::st_make_valid(x)

  # 2. Buffer (always faster before union)
  buf <- sf::st_buffer(x, dist)

  # 3. Union only if multiple geometries (saves processing time)
  if (nrow(buf) > 1) {
    buf <- sf::st_union(buf)
  }

  # 4. Ensure polygons (split multipolygons if needed)
  buf <- sf::st_cast(buf, "POLYGON")

  # 5. Compute convex hull
  convex <- sf::st_convex_hull(buf)

  # 6. Final cleaning + reproject
  convex <- sf::st_make_valid(convex)
  convex <- sf::st_transform(convex, crs)

  # Wrap as sf (remove unnecessary attributes)
  sf::st_sf(geometry = convex)
}
