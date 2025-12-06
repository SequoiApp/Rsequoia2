#' Retrieve and normalize a WFS layer with happign
#'
#' Downloads a WFS layer with happign, transforms it to the specified CRS and
#' removes Z/M coordinates.
#'
#' @param x An `sf` object used as spatial filter.
#' @param layer `character`. Name of the WFS layer to request.
#' @param crs `integer`. Target EPSG code for the output. Defaults to `2154`.
#'
#' @return An `sf` object transformed to the target CRS, `NULL` if the request fails.
#'
#' @noRd
get_topo <- function(x, layer, crs = 2154) {

  f <- happign::get_wfs(
    x = x,
    layer = layer,
    filename = NULL,
    spatial_filter = "intersects") |>
    suppressMessages() |>
    suppressWarnings()

  if (!(is.null(f))){
    f <- sf::st_transform(f, crs) |>
      sf::st_zm()
  }

  return(invisible(f))
}

#' Retrieve hydrographic polygons around an area
#'
#' @param x An `sf` object used as the input area.
#'
#' @return An `sf` object containing hydrographic polygons with two fields:
#'   * `TYPE` — hydrographic class
#'     - `RSO` = Reservoir or water tower
#'     - `SFO` = Permanent hydrographic surface
#'     - `SFI` = Intermittent hydrographic surface
#'   * `NATURE` — Original BDTOPO nature field
#'   * `NAME` — Official hydrographic name (when available)
#'
#' @details
#' The function retrieves BDTOPO layers within a 1000 m convex buffer
#' around `x`, assigns the hydrographic types, and combines them into
#' a single layer.
#'
#' @export
get_hydro_poly <- function(x){

  # convex buffer
  convex <- buffer_to_convex(x, 1000)

  # empty sf
  hydro_poly <- sf::st_sf(TYPE   = character(),
                          NATURE = character(),
                          geometry = sf::st_sfc(crs = 2154))

  # reservoir
  rso <- get_topo(convex, "BDTOPO_V3:reservoir")
  if(!(is.null(rso))){
    rso$TYPE = "RSO"
    rso <- seq_normalize(rso, "vct_poly")
    hydro_poly <- rbind(hydro_poly, rso)
  }

  # surface
  sfo <- get_topo(convex, "BDTOPO_V3:surface_hydrographique")
  if(!(is.null(sfo))){
    sfo$TYPE <- ifelse(sfo$persistance == "Permanent", "SFO", "SFI")
    sfo$NAME <- sfo$cpx_toponyme_de_plan_d_eau
    sfo <- seq_normalize(sfo, "vct_poly")
    hydro_poly <- rbind(hydro_poly, sfo)
  }

  invisible(hydro_poly)
}

#' Retrieve and assemble hydrographic lines around an area
#'
#' Builds a convex buffer around the input geometry, retrieves hydrographic
#' line features from BDTOPO, normalizes them, and returns a combined `sf`
#' linestring layer.
#'
#' @param x An `sf` object used as the input area.
#'
#' @return An `sf` object containing hydrographic line features with four fields:
#'   * `TYPE` — hydrographic class
#'     - `RUI` = Permanent hydrographic line
#'     - `RIN` = Intermittent hydrographic line
#'   * `NATURE` — Original BDTOPO nature field
#'   * `NAME` — Official hydrographic name (when available)
#'   * `OFFSET` — Offset information
#'
#' @details
#' The function retrieves BDTOPO hydrographic line segments
#' (`troncon_hydrographique`) within a 1000 m convex buffer around `x`,
#' assigns the hydrographic types, normalizes the geometries,
#' and returns them as a single combined layer.
#'
#' @export
get_hydro_line <- function(x){

  # convex buffer
  convex <- buffer_to_convex(x, 1000)

  # empty sf
  hydro_line <- sf::st_sf(TYPE   = character(),
                          NATURE = character(),
                          NAME = character(),
                          OFFSET = character(),
                          geometry = sf::st_sfc(crs = 2154))

  # troncon
  rui <- get_topo(convex, "BDTOPO_V3:troncon_hydrographique")
  if(!(is.null(rui))){
    rui$TYPE = ifelse(rui$persistance == "Permanent", "RUI", "RIN")
    rui$NAME = rui$cpx_toponyme_de_cours_d_eau
    rui <- seq_normalize(rui, "vct_line")
    hydro_line <- rbind(hydro_line, rui)
  }

  invisible(hydro_line)
}

#' Retrieve and assemble hydrographic points around an area
#'
#' Builds a convex buffer around the input geometry, retrieves hydrographic
#' point features from BDTOPO, normalizes them, and returns a combined `sf`
#' point layer.
#'
#' @param x An `sf` object used as the input area.
#'
#' @return An `sf` object containing hydrographic point features with four fields:
#'   * `TYPE` — hydrographic class
#'     - `MAR` = Pond
#'   * `NATURE` — Original BDTOPO nature field
#'   * `NAME` — Official hydrographic name (when available)
#'   * `ROTATION` — Rotation or orientation
#'
#' @details
#' The function retrieves BDTOPO hydrographic point details
#' (`detail_hydrographique`) within a 1000 m convex buffer around `x`,
#' assigns the hydrographic type, normalizes the geometries,
#' and returns them as a single combined layer.
#'
#'
#' @export
get_hydro_point <- function(x){

  # convex buffer
  convex <- buffer_to_convex(x, 1000)

  # empty sf
  hydro_point <- sf::st_sf(TYPE   = character(),
                           NATURE = character(),
                           NAME = character(),
                           ROTATION = character(),
                           geometry = sf::st_sfc(crs = 2154))

  # mare
  mar <- get_topo(convex, "BDTOPO_V3:detail_hydrographique")
  if(!(is.null(mar))){
    mar$TYPE = "MAR"
    mar$NAME = mar$toponyme
    mar <- seq_normalize(mar, "vct_point")
    hydro_point <- rbind(hydro_point, mar)
  }

  invisible(hydro_point)
}
