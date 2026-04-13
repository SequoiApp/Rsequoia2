#' Retrieve forest vegetation polygons around an area
#'
#' @param x `sf` or `sfc`; Geometry located in France.
#' @param tol `numeric`. Minimum area threshold in square meters.
#'
#' @return An `sf` object containing forest vegetation polygons
#'
#' @details
#' The function retrieves the IGN forest mask layer within a 1000 m
#' convex buffer around `x`. Retrieved polygons are then intersected
#' with a 1500 m convex buffer, cast to polygons, and normalized.
#'
#' If no forest data are found, the function returns an empty
#' `sf` object with a standardized structure.
#'
#' @export
get_vege_poly <- function(x, tol = 500) {

  if (!inherits(x, c("sf", "sfc"))) {
    cli::cli_abort("{.arg x} must be {.cls sf} or {.cls sfc}, not {.cls {class(x)}}.")
  }

  crs <- 2154
  layer <- "IGNF_MASQUE-FORET.2021-2023:masque_foret"
  x <- sf::st_transform(x, crs)

  # standardized field names
  type <- seq_field("type")$name
  name <- seq_field("name")$name
  source <- seq_field("source")$name

  # Fetching data in 1000m distance, then crop to 1500m for large data
  fetch_envelope <- seq_envelope(x, 1000)
  control_envelope <- seq_envelope(x, 1500)
  forest <- seq_dissolve(sf::st_buffer(x, 5), 5)

  # Retrieve forest mask
  forest_mask <- happign::get_wfs(
    x = fetch_envelope, layer = layer, predicate = happign::intersects()
  )

  if (is.null(forest_mask)) {
    cli::cli_warn("No vegetation data found. Empty {.cls sf} is returned.")
    empty_sf <- create_empty_sf("POLYGON") |> seq_normalize("vct_poly")
    return(invisible(empty_sf))
  }

  # Intersection on area of interest
  forest_mask <- sf::st_transform(forest_mask, crs)
  intersect <- sf::st_intersection(forest_mask, control_envelope) |>
    quiet()

  # Remove interior rings
  # Intersection create mixed geometry type (POLYGON, MULTIPOLYGON)
  # Because st_cast cannot deal with mixed geometry, first is casted to multipoly, then poly
  external <- sf::st_cast(intersect, "MULTIPOLYGON") |>
    sf::st_cast("POLYGON", warn = FALSE) |>
    remove_interior_ring()

  # Remove forest boundary
  difference <- sf::st_difference(external, forest) |>
    sf::st_cast("MULTIPOLYGON") |>
    sf::st_cast("POLYGON") |>
    sf::st_make_valid() |>
    quiet()

  # Remove small features
  vege_poly <- remove_small_geometries(difference, tol) |>
    sf::st_make_valid()

  if (nrow(vege_poly)==0){
    return(invisible(NULL))
  }

  vege_poly[[type]] <- "FOR"
  vege_poly[[source]] <- "ignf_masque_foret"

  return(invisible(vege_poly))

}

#' Remove interior rings from polygons
#'
#' Keeps only the exterior ring of each polygon, removing holes.
#'
#' @param x `sf` or `sfc`. Input geometries (POLYGON / MULTIPOLYGON).
#'
#' @return An `sf` object with polygons stripped of interior rings.
#'
#' @keywords internal
remove_interior_ring <- function(x) {
  sf::st_sf(
    geometry = sf::st_sfc(
      lapply(sf::st_geometry(sf::st_cast(x, "POLYGON")), function(p) {
        sf::st_polygon(list(p[[1]]))  # keep only outer ring
      }),
      crs = sf::st_crs(x)
    )
  ) |> quiet()
}

#' Remove geometries below a minimum area threshold
#'
#' Filters geometries whose area is smaller than a given threshold (in m²).
#' The input is projected to a metric CRS before area computation.
#'
#' @param x `sf` or `sfc`. Input geometries (POLYGON / MULTIPOLYGON).
#' @param tol `numeric`. Minimum area threshold in square meters.
#' @param crs `numeric`. EPSG code of a projected CRS (default: 2154).
#'
#' @return An object of the same class as `x`, with geometries below the threshold removed.
#'
#' @keywords internal
remove_small_geometries <- function(x, tol, crs = 2154) {

  # --- Checks ----
  if (!inherits(x, c("sf", "sfc"))) {
    cli::cli_abort("{.arg x} must be of class {.cls sf} or {.cls sfc}.")
  }

  if (!is.numeric(tol) || length(tol) != 1) {
    cli::cli_abort("{.arg tol} must be a single numeric value (area in m^2).")
  }

  # --- Transform ----
  x_proj <- sf::st_transform(x, crs)

  # --- Validate ----
  x_proj <- sf::st_make_valid(x_proj) |> quiet()

  # --- Areas ----
  areas <- sf::st_area(x_proj)

  # --- Filter ----
  keep <- as.numeric(areas) >= tol
  n_removed <- sum(!keep)

  x_filtered <- x_proj[keep, , drop = FALSE]

  # --- Back to original CRS ----
  x_filtered <- sf::st_transform(x_filtered, sf::st_crs(x))

  return(x_filtered)
}

#' Retrieve forest vegetation lines around an area
#'
#' @inheritParams get_vege_poly
#'
#' @return An `sf` object containing forest vegetation line features.
#'
#' @details
#' The function derives forest vegetation linear features from
#' vegetation polygons obtained with [get_vege_poly()].
#'
#' GIS workflow :
#' - Polygon are intersected to a 1500m buffer;
#' - Polygon are cast to `LINESTRING`;
#' - Line are intersected with a 1500 buffer to remove intersection line
#' from first step
#'
#' If no vegetation polygons are available, the function returns
#' an empty standardized `sf` object.
#'
#' @seealso get_vege_poly
#'
#' @export
get_vege_line <- function(x) {

  if (!inherits(x, c("sf", "sfc"))) {
    cli::cli_abort("{.arg x} must be {.cls sf} or {.cls sfc}, not {.cls {class(x)}}.")
  }

  crs <- 2154
  x <- sf::st_transform(x, crs)

  # Used to remove straight line due to buffer
  cleaning_envelope <- seq_envelope(x, 1500 - 1)

  # standardized field names
  type <- seq_field("type")$name
  name <- seq_field("name")$name
  source <- seq_field("source")$name

  # retrieve vegetation polygons
  vege_poly <- get_vege_poly(x)

  if (nrow(vege_poly) == 0) {
    cli::cli_warn("No vegetation data found. Empty {.cls sf} is returned.")
    empty_sf <- create_empty_sf("LINE") |> seq_normalize("vct_line")
    return(invisible(empty_sf))
  }

  vege_line <- vege_poly |>
    poly_to_line() |>
    sf::st_intersection(cleaning_envelope) |>
    sf::st_cast("LINESTRING") |> suppressWarnings() |>
    seq_normalize("vct_line")

  vege_line[[type]] <- "FOR"
  vege_line[[source]] <- "ignf_masque_foret"

  return(invisible(vege_line))
}

#' Generate vegetation point features around an area
#'
#' @param x An `sf` object used as the input area.
#'
#' @return An `sf` object of type `POINT` containing vegetation point
#'   features with standardized Sequoia fields, including:
#'
#' @details
#' The function retrieves BD Foret V2 vegetation areas within a 1000 m convex
#' buffer around `x`. Each polygon is classified into a standardized vegetation
#' type based on its original `nature` attribute.
#'
#' `TYPE`: Vegetation type code derived from BDTOPO nature values:
#'     - `FEV`: Deciduous forest
#'     - `REV`: Coniferous forest
#'     - `FRV`: Coniferous and deciduous forest
#'     - `PEV`: Poplar plantation
#'     - `LAV`: Woody heath
#'
#'
#' Vegetation points are generated by spatial sampling using a hexagonal
#' pattern, with one point per 4 hectares of vegetation area.
#'
#' If no vegetation areas are found, the function returns an empty
#' standardized `sf` object of type `POINT`.
#'
#' @seealso [get_vege_poly()], [get_vege_line()]
#'
#' @export
get_vege_point <- function(x){

  if (!inherits(x, c("sf", "sfc"))) {
    cli::cli_abort("{.arg x} must be {.cls sf} or {.cls sfc}, not {.cls {class(x)}}.")
  }

  crs <- 2154
  layer <- "LANDCOVER.FORESTINVENTORY.V2:formation_vegetale"

  # standardized field names
  type <- seq_field("type")$name
  name <- seq_field("name")$name
  source <-  seq_field("source")$name

  # Fetching data in 1000m distance, then crop to 1500m for large data
  fetch_envelope   <- seq_envelope(x, 1000)
  control_envelope <- seq_envelope(x, 1500)
  forest <- seq_dissolve(seq_dissolve(sf::st_buffer(x, 5), 5), 5)

  # Retrieve fv data
  raw_fv <- happign::get_wfs(
    x = fetch_envelope,
    layer = "LANDCOVER.FORESTINVENTORY.V2:formation_vegetale",
    predicate = happign::intersects(),
    verbose = FALSE
  )

  if (is.null(raw_fv)) {
    cli::cli_warn("No vegetation data found. Empty {.cls sf} is returned.")
    empty_sf <- create_empty_sf("POINT") |> seq_normalize("vct_point")
    return(invisible(empty_sf))
  }

  fv <- sf::st_transform(raw_fv, crs)
  fv <- sf::st_intersection(fv, control_envelope) |> suppressWarnings()

  # Mapping type
  # https://geoservices.ign.fr/sites/default/files/2021-06/DC_BDForet_2-0.pdf
  map <- c(
    "FF1" = "FEV", "FO1" = "FEV",
    "FF2" = "REV", "FO2" = "REV",
    "FF3" = "FRV", "FO3" = "FRV",
    "FP"  = "PEV",
    "LA" = "LAV"
    )
  keys <- substr(fv$code_tfv, 1, 3)
  fv[[type]] <- unname(map[keys])

  fv <- fv[!is.na(fv[[type]]), ]

  if (nrow(fv) == 0) {
    cli::cli_warn("No vegetation data found. Empty {.cls sf} is returned.")
    empty_sf <- create_empty_sf("POINT") |> seq_normalize("vct_point")
    return(invisible(empty_sf))
  }

  fv <- seq_normalize(fv, "vct_point")
  fv[[source]] <- "ignf_bd_foretv2"

  # Create points on grid
  area_tot <- sum(st_area(fv)) |> units::set_units("ha")
  ha_per_point <- units::set_units(4, "ha")
  n_points <- max(0L, round(area_tot / ha_per_point) |> as.integer())

  if (n_points == 0) {
    cli::cli_warn("No vegetation data found. Empty {.cls sf} is returned.")
    empty_sf <- create_empty_sf("POINT") |> seq_normalize("vct_point")
    return(invisible(empty_sf))
  }

  point <- sf::st_sample(fv, n_points, type = "hexagonal") |>
    sf::st_as_sf(crs = sf::st_crs(fv)) |>
    sf::st_set_geometry("geometry") # rename sf column to geometry instead of x

  point <- sf::st_join(point, fv, join = sf::st_intersects)

  # Remove forest boundary
  vege_point <- sf::st_difference(point, forest) |>
    sf::st_cast("MULTIPOINT") |>
    sf::st_cast("POINT") |>
    sf::st_make_valid() |>
    quiet()

  return(invisible(vege_point))

}

#' Generates vegetation polygon, line and point layers for a Sequoia project.
#'
#' This function is a convenience wrapper around [get_vege_poly()],
#' [get_vege_line()] and [get_vege_point()], allowing the user to download
#' all products in one call and automatically write them to the project
#' directory using [seq_write()].
#'
#' @inheritParams seq_write
#'
#' @details
#' Each vegetation layer is always written to disk using [seq_write()],
#' even when it contains no features (`nrow == 0`).
#'
#' @return A named list of file paths written by [seq_write()],
#' one per vegetation layer.
#'
#' @seealso
#' [get_vege_poly()], [get_vege_line()], [get_vege_point()],
#' [seq_write()]
#'
seq_vege <- function(
    dirname = ".",
    verbose = TRUE,
    overwrite = FALSE
) {

  # tiny helper : avoid repeating all arg eacvh time
  seq_write2 <- function(x, key, id) {
    seq_write(x, key, dirname = dirname, id = id, verbose = verbose, overwrite = overwrite)
  }

  # read PARCA
  parca <- seq_read("v.seq.parca.poly", dirname = dirname)
  id_field <- seq_field("identifier")$name
  id <- unique(parca[[id_field]])

  if (verbose){
    cli::cli_h1("VEGETATION")
  }

  vege_poly <- get_vege_poly(parca)
  if (nrow(vege_poly) > 1){
    vege_poly[[id_field]] <- id
  }
  vege_poly <- seq_write2(vege_poly, "v.vege.poly", id)

  vege_line <- get_vege_line(parca)
  if (nrow(vege_line) > 1){
    vege_line[[id_field]] <- id
  }
  vege_line <- seq_write2(vege_line, "v.vege.line", id)

  vege_point <- get_vege_point(parca)
  if (nrow(vege_point) > 1){
    vege_point[[id_field]] <- id
  }
  vege_point <- seq_write2(vege_point, "v.vege.point", id)

  return(invisible(c(vege_poly, vege_line, vege_point) |> as.list()))
}

