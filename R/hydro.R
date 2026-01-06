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
get_topo <- function(x, layer, crs = 2154, strict = TRUE) {

  f <- happign::get_wfs(
    x = x,
    layer = layer,
    filename = NULL,
    spatial_filter = "intersects") |>
    quiet()

  if (!(is.null(f))){
    f <- sf::st_transform(f, crs) |>
      sf::st_zm() |>  # Security if there Z dim in the dataset (common case in bd topo)
      quiet()
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
  convex <- envelope(x, 1000)

  # empty sf
  hydro_poly <-  create_empty_sf("POLYGON") |>
    seq_normalize("vct_poly")

  # standardized field names
  type <- seq_field("type")$name
  name <- seq_field("name")$name
  source <-  seq_field("source")$name

  # retrieve rso
  rso <- get_topo(convex, "BDTOPO_V3:reservoir")

  if(!(is.null(rso))){
    rso[[type]] <- "RSO"
    rso[[source]] <- "IGNF_BDTOPO_V3"

    rso <- seq_normalize(rso, "vct_poly")
    hydro_poly <- rbind(hydro_poly, rso)
  }

  # surface
  sfo <- get_topo(convex, "BDTOPO_V3:surface_hydrographique")

  if(!(is.null(sfo))){
    sfo[[type]] <- ifelse(sfo$persistance == "Permanent", "SFO", "SFI")
    sfo[[name]] <- sfo$cpx_toponyme_de_plan_d_eau
    sfo[[source]] <- "IGNF_BDTOPO_V3"

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
  convex <- envelope(x, 1000)

  # empty sf
  hydro_line <- create_empty_sf("LINESTRING") |>
    seq_normalize("vct_line")

  # standardized field names
  type <- seq_field("type")$name
  name <- seq_field("name")$name
  source <-  seq_field("source")$name

  # troncon
  rui <- get_topo(convex, "BDTOPO_V3:troncon_hydrographique")

  if(!(is.null(rui))){
    rui[[type]] = ifelse(rui$persistance == "Permanent", "RUI", "RIN")
    rui[[name]] = rui$cpx_toponyme_de_cours_d_eau
    rui[[source]] <- "IGNF_BDTOPO_V3"

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
  convex <- envelope(x, 1000)

  # empty sf
  hydro_point <- create_empty_sf("POINT") |>
    seq_normalize("vct_point")

  # standardized field names
  type <- seq_field("type")$name
  name <- seq_field("name")$name
  source <-  seq_field("source")$name

  # mare
  mar <- get_topo(convex, "BDTOPO_V3:detail_hydrographique")

  if(!(is.null(mar))){
    mar[[type]] <- "MAR"
    mar[[name]] <- mar$toponyme
    mar[[source]] <- "IGNF_BDTOPO_V3"

    mar <- seq_normalize(mar, "vct_point")
    hydro_point <- rbind(hydro_point, mar)
  }

  invisible(hydro_point)
}

#' Generates hydrographic polygon, line and point layers for a Sequoia project.
#'
#' This function is a convenience wrapper around [get_hydro_poly()],
#' [get_hydro_line()] and [get_hydro_point()], allowing the user to download
#' all products in one call and automatically write them to the project
#' directory using [seq_write()].
#'
#' @param dirname `character` Path to the directory. Defaults to the current
#' working directory.
#' @inheritParams seq_write
#'
#' @details
#' Each hydrographic layer is always written to disk using [seq_write()],
#' even when it contains no features (`nrow == 0`).
#'
#' Informational messages are displayed to indicate whether a layer
#' contains features or is empty.
#'
#' @return A named list of file paths written by [seq_write()],
#' one per hydrographic layer.
#'
#' @seealso
#' [get_hydro_poly()], [get_hydro_line()], [get_hydro_point()],
#' [seq_write()]
#'
seq_hydro <- function(
    dirname = ".",
    verbose = TRUE,
    overwrite = FALSE
) {

  # read PARCA
  f_parca <- read_sf(get_path("v.seq.parca.poly", dirname = dirname))
  f_id <- get_id(dirname)

  id <- seq_field("identifiant")$name

  # create empty path list
  path <- list()

  # hydro layer specifications
  layers <- list(
    poly  = list(fun = get_hydro_poly,  key = "v.hydro.poly"),
    line  = list(fun = get_hydro_line,  key = "v.hydro.line"),
    point = list(fun = get_hydro_point, key = "v.hydro.point")
  )

  for (k in names(layers)) {

    f <- layers[[k]]$fun(f_parca)

    if (nrow(f)>0){
      f[[id]]<- f_id
    }

    f_path <- seq_write(
      f,
      layers[[k]]$key,
      dirname = dirname,
      verbose = FALSE,
      overwrite = overwrite
    )

    path <- c(path, f_path)

    if (verbose) {
      if (nrow(f) == 0) {
        cli::cli_alert_info(
          c("i" = "Hydro {.field {k}} layer written (empty layer)")
        )
      } else {
        cli::cli_alert_success(
          "Hydro {.field {k}} layer written with {nrow(f)} feature{?s}"
        )
      }
    }
  }

  return(invisible(path))
}
