# ROAD_line to ROADWAY_poly ----
#' Create a road buffer polygon
#'
#' Generates a polygon buffer for a given road type from a line layer.
#'
#' @param x An `sf` object with road line geometries.
#' @param type Character. Road type to process (e.g., "RN", "RD").
#' @param dist Numeric. Buffer distance in layer units.
#'
#' @return An `sf` object containing a single (multi)polygon geometry for the road type.
#'
#' @keywords internal
road_buffer <- function(x, type, dist) {

  type_field <- seq_field("type")$name

  lines <- x[x[[type_field]] == type, ]
  if (nrow(lines) == 0) return(NULL)

  buf <- sf::st_simplify(
    sf::st_buffer(
      sf::st_union(sf::st_geometry(lines)),
      dist,
      nQuadSegs = 5,
      joinStyle = "ROUND",
      endCapStyle = "ROUND"
    ),
    dTolerance = 0.1,
    preserveTopology = TRUE
  )

  sf::st_sf(
    geometry = buf,
    structure(list(type), names = type_field),
    crs = sf::st_crs(x)
  )
}

#' Compute hierarchical road differences
#'
#' Performs sequential differences between buffered road polygons according to a given hierarchy.
#'
#' @param buffers A named list of `sf` polygon buffers.
#' @param order Character vector. Defines the processing order of road types.
#'
#' @return An `sf` object with combined polygons after hierarchical differences.
#'
#' @keywords internal
road_difference <- function(buffers, order) {

  out <- list()
  mask <- NULL

  for (type in order) {
    if (!type %in% names(buffers)) next

    x <- buffers[[type]]

    if (!inherits(x, "sf") || nrow(x) == 0 || all(sf::st_is_empty(x))) next

    # ensure geometry column
    if (!"geometry" %in% names(x)) sf::st_geometry(x) <- "geometry"

    if (!is.null(mask)) {
      x <- sf::st_difference(x, mask)

      if (!"geometry" %in% names(x)) sf::st_geometry(x) <- "geometry"
      if (all(sf::st_is_empty(x))) next
    }

    # keep only original type column + geometry
    type_field <- seq_field("type")$name
    cols_to_keep <- c(type_field, "geometry")
    x <- x[, intersect(cols_to_keep, names(x)), drop = FALSE]

    out[[type]] <- x
    mask <- if (is.null(mask)) x else sf::st_union(mask, x)
  }

  out <- out[!vapply(out, is.null, logical(1))]
  if (length(out) == 0) return(NULL)

  do.call(rbind, out)
}

#' Create a road external mask
#'
#' Generates a mask polygon around road lines, including buffered intersection points.
#'
#' @param x An `sf` object with road lines.
#' @param dist Numeric. Buffer distance in layer units.
#'
#' @return An `sf` object containing a polygon mask of the road network.
#'
#' @keywords internal
road_mask <- function(x, dist) {

  lines <- x[x$TYPE != "PN", ]

  if (sf::sf_use_s2()) {
    sf::sf_use_s2(FALSE)
    on.exit(sf::sf_use_s2(TRUE))
  }

  pts <- sf::st_cast(lines, "POINT", warn = F)

  geom_types <- sf::st_geometry_type(pts)
  if (any(grepl("Z", geom_types))) {
    pts <- sf::st_zm(pts)
  }
  coords <- sf::st_coordinates(pts)

  dup <- duplicated(coords) | duplicated(coords, fromLast = TRUE)
  pts_dup <- pts[dup, ]

  union <- sf::st_union(
    sf::st_buffer(sf::st_combine(lines), dist, endCapStyle = "FLAT"),
    sf::st_buffer(pts_dup, dist)
  )

  result <- sf::st_sf(geometry = union) |> sf::st_union()

  return(sf::st_sf(geometry = result))
}

#' Convert road lines to hierarchical polygons
#'
#' Creates hierarchical road polygons from line features using buffers, differences, and an external mask.
#'
#' @param x An `sf` object with road line geometries.
#' @param dist Numeric. Base buffer distance (default 3.5 units).
#'
#' @return An `sf` object with the final road polygons.
#'
#' @keywords internal
road_to_roadway <- function(x, dist = 3.5){

  if (get_geom_type(x) != "LINE") {
    cli::cli_abort('{.arg x} must be a {.val LINE} sf object.')
  }

  type_field <- seq_field("type")$name
  if (!type_field %in% names(x)){
    cli::cli_abort("{.arg x} must contain {.val {type_field} as field}.")
  }

  if (any(is.na(x[[type_field]]))) {
    cli::cli_abort(c(
      "x" = "Column {.val {type_field}} contains missing values (NA).",
      "i" = "Please remove or fill NA values before continuing."
    ))
  }

  invalid <- unique(x[[type_field]][!is.na(x[[type_field]]) & !x[[type_field]] %in% c("RN", "RD", "RC", "RF")])
  if (length(invalid)){
    cli::cli_abort("Invalid road type value(s): {invalid}.")
  }

  # buffers definition
  buffer_defs <- list(
    RN = round(dist*1.4) + 1,
    RD = round(dist*1.4),
    RC = dist,
    RF = dist
  )

  buffers <- lapply(names(buffer_defs),
                    function(t) road_buffer(x, t, buffer_defs[[t]])) |>
    quiet()
  names(buffers) <- names(buffer_defs)

  buffers <- buffers[!vapply(buffers, is.null, logical(1))]

  # hierarchical difference
  road_poly <- road_difference(buffers, c("RN", "RD", "RC", "RF")) |>
    quiet()

  # external mask
  mask <- quiet(road_mask(x, dist + 5)) |>
    sf::st_make_valid()

  road_poly <- quiet(sf::st_intersection(road_poly, mask)) |>
    sf::st_make_valid()

  return(seq_normalize(road_poly, "road_poly"))
}

# VOIDS_poly to ROADWAY_poly ----
#' Get polygons from the completed cadastral gaps layer
#'
#' Cleans polygon topology and normalizes the result as road polygons.
#'
#' @param x An `sf` polygon object from [Rsequoia2::get_voids()]
#'   completed by user
#'
#' @return An `sf` object with POLYGON or MULTIPOLYGON geometries normalized as
#'   road polygons.
#'
#' @keywords internal
voids_to_roadway <- function(x){
  if (get_geom_type(x) != "POLYGON") {
    cli::cli_abort('{.arg x} must be a {.val POLYGON} sf object.')
  }

  type_field <- seq_field("type")$name
  if (!type_field %in% names(x)){
    cli::cli_abort("{.arg x} must contain {.val {type_field} as field}.")
  }

  if (any(is.na(x[[type_field]]))) {
    cli::cli_abort(c(
      "x" = "Column {.val {type_field}} contains missing values (NA).",
      "i" = "Please remove or fill NA values before continuing."
    ))
  }

  return(seq_normalize(x, "road_poly"))
}

# ROADWAY ----
#' Get roadway polygons from lines or polygons
#'
#' Generates normalized roadway polygons from either road line features
#' or completed cadastral voids polygons.
#'
#' @param x An `sf` object containing either:
#'   - LINE geometries representing road centerlines, or
#'   - POLYGON geometries representing completed cadastral gaps.
#'
#' @return An `sf` object with POLYGON or MULTIPOLYGON geometries
#' normalized as roadway polygons.
#'
#' @details
#' The function dispatches internally depending on the geometry type:
#'
#' - If `x` contains LINESTRING geometries, road polygons are created using
#'   hierarchical buffering and masking via `road_to_roadway()`.
#'
#' - If `x` contains POLYGON geometries, the polygons are cleaned and
#'   normalized via `voids_to_roadway()`.
#'
#' The resulting polygons are standardized using `seq_normalize()`
#' with the `"road_poly"` schema.
#'
#' @keywords internal
get_roadway_poly <- function(x) {

  geom <- get_geom_type(x)

  if (geom == "POLYGON") {
    roadway_poly <- voids_to_roadway(x)
  } else if (geom == "LINE") {
    roadway_poly <- road_to_roadway(x)
  } else {
    cli::cli_abort("{.arg x} must be a {.val LINE} or {.val POLYGON} sf object.")
  }

  return(roadway_poly)
}

#' Derive roadway lines from roadway polygons
#'
#' Generates roadway border line features from roadway polygons by extracting
#' internal edges using a buffer difference and convex hull splitting.
#'
#' @param x An `sf` polygon object produced by `get_roadway_poly()`.
#'
#' @return An `sf` object with LINESTRING geometries normalized as
#' roadway lines.
#'
#' @details
#' The function derives line features from roadway polygons through the
#' following steps:
#'
#' 1. Polygon boundaries are cast to MULTILINESTRING.
#' 2. Internal edges are extracted using a negative buffer difference.
#' 3. The resulting lines are split using points derived from a simplified
#' convex hull of the polygons.
#' 4. The result is normalized with `seq_normalize()` using the
#' `"road_line"` schema.
#'
#' @keywords internal
get_roadway_line <- function(x){

  if (get_geom_type(x) != "POLYGON") {
    cli::cli_abort("{.arg x} must be a {.val POLYGON} sf object.")
  }

  cast <- sf::st_cast(x, 'MULTILINESTRING', warn = F)
  difference <- quiet(sf::st_difference(cast, sf::st_buffer(sf::st_union(x), -0.0001)))

  # mask
  mask <- sf::st_convex_hull(x) |>
    sf::st_simplify(preserveTopology = TRUE, dTolerance = 2)
  mask_pts <- sf::st_combine(mask) |>
    sf::st_cast(to = 'MULTIPOINT', warn = F)

  # split
  line <- lwgeom::st_split(difference, mask_pts)
  line <- sf::st_collection_extract(line, 'LINESTRING')

  return(seq_normalize(line, "road_line"))
}

# SEQ_ROADWAY ----
#' Generate graphic road layers for a Sequoia project
#'
#' Create graphic road layers from a completed cadastral gaps layer.
#'
#' @param dirname `character` Path to the project directory.
#'   Defaults to the current working directory.
#' @param source `character` Source layer key to use to generate polygon
#'   features. Must be `v.cad.vides.poly` or `v.road.line`.
#' @param verbose `logical`; whether to display informational messages.
#'   Defaults to `TRUE`.
#' @param overwrite `logical`; whether to overwrite existing files.
#'   Defaults to `FALSE`.
#'
#' @details
#' Features are created from a completed cadastral gaps layer obtained by using
#' `seq_voids()`.
#'
#' @return
#' Invisibly returns a named list of file paths written by [seq_write()].
#' Returns `NULL` invisibly when no features are created.
#'
#' @seealso
#' [seq_voids()], [seq_write()]
#'
#' @export
seq_roadway <- function(
    source,
    dirname = ".",
    verbose = TRUE,
    overwrite = FALSE
) {

  all_sources <- c("v.cad.vides.poly", "v.road.line")

  if (!source %in% all_sources) {
    cli::cli_abort("{.arg source} must be one of {.val {all_sources}}.")
  }

  parca <- seq_read("parca", dirname = dirname)
  id_field <- seq_field("identifier")$name
  type_field <- seq_field("type")$name

  id <- unique(parca[[id_field]])

  source <- seq_read(source, dirname = dirname)

  # Exit early if nothing to write
  if (is.null(source) || (inherits(source, "sf") && nrow(source) == 0)) {
    if (verbose) {
      cli::cli_alert_info("No features found in {.arg source}: roadway layers not written.")
    }
    return(invisible(NULL))
  }

  if (verbose){
    cli::cli_h1("GRAPHIC ROAD")
  }

  # Get roadway_poly
  poly <- get_roadway_poly(source)

  # Exit if get_roadway_poly() fail
  if (!nrow(poly) || nrow(poly) == 0) {
    if (verbose) {
      cli::cli_alert_info(
        "No features found in {.arg roadway_poly}: roadway layers not written."
      )
    }
    return(invisible(NULL))
  }

  # Get roadway_line
  line <- get_roadway_line(poly)

  # Add PN lines
  road_lines <- tryCatch(
    seq_read("v.road.line", dirname = dirname),
    error = function(e) NULL
  )

  if (!is.null(road_lines)) {
    type_field <- seq_field("type")$name
    pn <- subset(road_lines, road_lines[[type_field]] %in% "PN")

    if (nrow(pn)>0){
      pn <- sf::st_difference(pn, poly) |> quiet()
      pn <- seq_normalize(pn, "road_line")

      line <- rbind(line, pn)
    }
  }

  # Write roadway_line with id
  line[[id_field]] <- id

  line_path <- seq_write(
    line,
    "v.roadway.line",
    dirname = dirname,
    id = id,
    verbose = verbose,
    overwrite = overwrite
  )

  # Write road_poly with id
  poly[[id_field]] <- id

  poly_path <- seq_write(
    poly,
    "v.roadway.poly",
    dirname = dirname,
    id = id,
    verbose = verbose,
    overwrite = overwrite
  )

  paths <- list(line_path, poly_path)

  return(invisible(paths))
}
