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
#' @export
#' @examples
#' \dontrun{
#' empty_sf <- create_empty_sf(
#'   "LINESTRING",
#'   PLACETTE = character(0),
#'   TSE_VOL = numeric(0)
#' )
#' }
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

#' Create convex envelope
#'
#' @param x An `sf` object.
#' @param dist Buffer distance in `crs` units. Default to 2154 (meter).
#' @param crs `numeric` or `character`. Defaults to EPSG:2154. See
#' [sf::st_crs()] for more details.
#'
#' @return An `sf` object of convex polygons.
#'
#' @noRd
envelope <- function(x, dist, crs = 2154) {

  x <- sf::st_make_valid(x) |>
    sf::st_transform(crs = crs) |>
    sf::st_buffer(dist) |>
    sf::st_union()

  # split disconnected parts
  x <- sf::st_cast(x, "POLYGON", warn = FALSE)

  envelope <- sf::st_convex_hull(x) |>
    sf::st_make_valid() |>
    sf::st_sf()

  return(envelope)
}


#' Clean geometry topology using GRASS GIS (via QGIS)
#'
#' Cleans the topology of an `sf` object using the GRASS GIS
#' algorithm `v.clean`, executed through QGIS via `qgisprocess`.
#'
#' This function is intended for expert use only. Some operations
#' (e.g. removing small areas or snapping vertices) may alter
#' geometries in a non-reversible way and should be applied with care,
#' especially on cadastral data.
#'
#' @param sf_obj An `sf` object whose geometry topology must be cleaned.
#' @param tool `character`, default `"snap"`. GRASS cleaning tool to use
#'   (e.g. `"snap"`, `"rmdupl"`, `"break"`, `"rmdangle"`).
#' @param snap_tolerance `numeric`, default `0.05`. Snapping tolerance
#'   in map units (usually meters).
#' @param min_area `numeric`, default `0.1`. Minimum area to preserve
#'   (in square map units).
#' @param verbose `logical` If `TRUE`, display progress messages.
#'
#' @return An `sf` object with cleaned geometries.
#'
#' @importFrom qgisprocess qgis_run_algorithm qgis_providers
#' @importFrom sf st_write st_read st_make_valid
#' @importFrom cli cli_alert_info cli_alert_success cli_abort
#'
#' @noRd
clean_topology <- function(sf_obj,
                           tool = "snap",
                           snap_tolerance = 0.05,
                           min_area = 0.1,
                           verbose = TRUE) {

  if (!inherits(sf_obj, "sf")) {
    cli::cli_abort("`sf_obj` must be an sf object.")
  }

  # Check GRASS availability via QGIS
  providers <- qgisprocess::qgis_providers()
  if (!"GRASS" %in% providers$provider_title) {
    cli::cli_abort("GRASS provider is not available in the current QGIS installation.")
  }

  # Temporary input / output files
  input_path  <- tempfile(fileext = ".gpkg")
  output_path <- tempfile(fileext = ".gpkg")

  # Write input data
  sf::st_write(sf_obj, input_path, quiet = TRUE)

  # Run GRASS v.clean via QGIS
  quiet(
    qgisprocess::qgis_run_algorithm(
      "grass:v.clean",
      input  = input_path,
      type   = "area",
      tool   = tool,
      output = output_path,
      GRASS_SNAP_TOLERANCE_PARAMETER = snap_tolerance,
      GRASS_MIN_AREA_PARAMETER       = min_area
    )
  )

  # Read result and ensure validity
  cleaned_sf <- sf::st_read(output_path, quiet = TRUE, stringsAsFactors = FALSE)
  cleaned_sf <- cleaned_sf[, -1, drop = FALSE]
  cleaned_sf <- sf::st_make_valid(cleaned_sf)
  if (verbose) cli::cli_alert_success("All UG are consistent.")
  cleaned_sf
}
