#' Retrieve hydrographic polygons around an area
#'
#' @param x An `sf` object used as the input area.
#' @param buffer `numeric`; Buffer around `x` (in **meters**) used to enlarge
#'
#' @return An `sf` object containing hydrographic polygons with two fields:
#'   * `TYPE` - hydrographic class
#'     - `RSO` = Reservoir or water tower
#'     - `SFP` = Permanent hydrographic surface
#'     - `SFI` = Intermittent hydrographic surface
#'   * `NATURE` - Original BDTOPO nature field
#'   * `NAME` - Official hydrographic name (when available)
#'
#' @details
#' The function retrieves BDTOPO layers within a convex buffer
#' around `x`, assigns the hydrographic types, and combines them into
#' a single layer.
#'
#' @export
get_hydro_poly <- function(x,
                           buffer = 1000){

  if (!inherits(x, c("sf", "sfc"))) {
    cli::cli_abort("{.arg x} must be {.cls sf} or {.cls sfc}, not {.cls {class(x)}}.")
  }

  # convex buffer
  crs <- 2154
  x <- sf::st_transform(x, crs)
  fetch_envelope <- seq_envelope(x, buffer)

  # empty sf
  hydro_poly <- create_empty_sf("POLYGON") |>
    seq_normalize("vct_poly")

  # standardized field names
  type <- seq_field("type")$name
  name <- seq_field("name")$name
  source <-  seq_field("source")$name

  # retrieve rso
  rso <- happign::get_wfs(
    x = fetch_envelope,
    layer = "BDTOPO_V3:reservoir",
    predicate = happign::intersects(),
    verbose = FALSE
  )

  if(nrow(rso)>0){
    rso <- sf::st_transform(rso, crs)
    rso[[type]] <- "RSO"
    rso[[source]] <- "IGNF_BDTOPO_V3"

    rso <- seq_normalize(rso, "vct_poly")
    hydro_poly <- rbind(hydro_poly, rso)
  }

  # surface
  sfo <- happign::get_wfs(
    x = fetch_envelope,
    layer = "BDTOPO_V3:surface_hydrographique",
    predicate = happign::intersects(),
    verbose = FALSE
  )

  if(nrow(sfo)>0){
    sfo <- sf::st_transform(sfo, crs)
    sfo[[type]] <- ifelse(sfo$persistance == "Permanent", "SFP", "SFI")
    sfo[[name]] <- sfo$cpx_toponyme_de_plan_d_eau
    sfo[[source]] <- "IGNF_BDTOPO_V3"

    sfo <- seq_normalize(sfo, "vct_poly")
    hydro_poly <- rbind(hydro_poly, sfo)
  }

  if (nrow(hydro_poly)==0) {
    cli::cli_warn("No hydrologic data found. Empty {.cls sf} is returned.")
  }

  return(invisible(hydro_poly))
}

#' Retrieve and assemble hydrographic lines around an area
#'
#' Builds a convex buffer around the input geometry, retrieves hydrographic
#' line features from BDTOPO, normalizes them, and returns a combined `sf`
#' linestring layer.
#'
#' @param x An `sf` object used as the input area.
#' @param buffer `numeric`; Buffer around `x` (in **meters**) used to enlarge
#'
#' @return An `sf` object containing hydrographic line features with four fields:
#'   * `TYPE` - hydrographic class
#'     - `RUP` = Permanent hydrographic line
#'     - `RUI` = Intermittent hydrographic line
#'   * `NATURE` - Original BDTOPO nature field
#'   * `NAME` - Official hydrographic name (when available)
#'   * `OFFSET` - Offset information
#'
#' @details
#' The function retrieves BDTOPO hydrographic line segments
#' (`troncon_hydrographique`) within a 1000 m convex buffer around `x`,
#' assigns the hydrographic types, normalizes the geometries,
#' and returns them as a single combined layer.
#'
#' @export
get_hydro_line <- function(x,
                           buffer = 1000){

  if (!inherits(x, c("sf", "sfc"))) {
    cli::cli_abort("{.arg x} must be {.cls sf} or {.cls sfc}, not {.cls {class(x)}}.")
  }

  # convex buffer
  crs <- 2154
  x <- sf::st_transform(x, crs)
  fetch_envelope <- seq_envelope(x, buffer)

  # empty sf
  hydro_line <- create_empty_sf("LINESTRING") |>
    seq_normalize("vct_line")

  # standardized field names
  type <- seq_field("type")$name
  name <- seq_field("name")$name
  source <-  seq_field("source")$name

  # troncon
  rui <- happign::get_wfs(
    x = fetch_envelope,
    layer = "BDTOPO_V3:troncon_hydrographique",
    predicate = happign::intersects(),
    verbose = FALSE
  )

  if(nrow(rui)){
    rui <- sf::st_transform(rui, crs)
    rui[[type]] = ifelse(rui$persistance == "Permanent", "RUP", "RUI")
    rui[[name]] = rui$cpx_toponyme_de_cours_d_eau
    rui[[source]] <- "IGNF_BDTOPO_V3"

    rui <- seq_normalize(rui, "vct_line")
    hydro_line <- rbind(hydro_line, rui)
  }

  if (nrow(hydro_line)==0) {
    cli::cli_warn("No hydrologic data found. Empty {.cls sf} is returned.")
  }

  return(invisible(hydro_line))
}

#' Retrieve and assemble hydrographic points around an area
#'
#' Builds a convex buffer around the input geometry, retrieves hydrographic
#' point features from BDTOPO, normalizes them, and returns a combined `sf`
#' point layer.
#'
#' @param x An `sf` object used as the input area.
#' @param buffer `numeric`; Buffer around `x` (in **meters**) used to enlarge
#'
#' @return An `sf` object containing hydrographic point features with four fields:
#'   * `TYPE` - hydrographic class
#'     - `MAR` = Pond
#'   * `NATURE` - Original BDTOPO nature field
#'   * `NAME` - Official hydrographic name (when available)
#'   * `ROTATION` - Rotation or orientation
#'
#' @details
#' The function retrieves BDTOPO hydrographic point details
#' (`detail_hydrographique`) within a 1000 m convex buffer around `x`,
#' assigns the hydrographic type, normalizes the geometries,
#' and returns them as a single combined layer.
#'
#'
#' @export
get_hydro_point <- function(x, buffer = 1000){

  if (!inherits(x, c("sf", "sfc"))) {
    cli::cli_abort("{.arg x} must be {.cls sf} or {.cls sfc}, not {.cls {class(x)}}.")
  }

  # convex buffer
  crs <- 2154
  x <- sf::st_transform(x, crs)
  fetch_envelope <- seq_envelope(x, buffer)

  # empty sf
  hydro_point <- create_empty_sf("POINT") |>
    seq_normalize("vct_point")

  # standardized field names
  type <- seq_field("type")$name
  name <- seq_field("name")$name
  source <-  seq_field("source")$name

  # mare
  mar <- happign::get_wfs(
    x = fetch_envelope,
    layer = "BDTOPO_V3:detail_hydrographique",
    predicate = happign::intersects(),
    verbose = FALSE
  )

  if(nrow(mar)>0){
    mar <- sf::st_transform(mar, crs)
    mar[[type]] <- "MAR"
    mar[[name]] <- mar$toponyme
    mar[[source]] <- "IGNF_BDTOPO_V3"

    mar <- seq_normalize(mar, "vct_point")
    hydro_point <- rbind(hydro_point, mar)
  }

  if (nrow(hydro_point)==0) {
    cli::cli_warn("No hydrologic data found. Empty {.cls sf} is returned.")
  }

  return(invisible(hydro_point))
}

#' Fetch hydrology  layers
#'
#' Internal worker
#'
#' @inheritParams get_hydro_poly
#' @param dirname `character`; Output directory.
#' @param id Optional Sequoia project identifier.
#'
#' @return Invisibly returns a named list of written file paths, or `NULL`
#'   if no infrastructure layer can be retrieved.
#'
#' @keywords internal
#' @noRd
.hydro_fetcher <- function(
    x,
    dirname,
    id = NULL,
    buffer = 1000,
    verbose = TRUE,
    overwrite = FALSE) {

  if (verbose) {
    cli::cli_h1("HYDRO")
  }

  layers <- list(
    "v.hydro.point" = get_hydro_point,
    "v.hydro.line"  = get_hydro_line,
    "v.hydro.poly"  = get_hydro_poly
  )

  pb <- NULL
  if (verbose) {
    pb <- cli::cli_progress_bar(
      format = paste0(
        "{cli::pb_spin} Searching HYDRO layer: {.val {k}} | ",
        "[{cli::pb_current}/{cli::pb_total}]"
      ),
      total = length(layers),
      auto_terminate = FALSE,
      clear = TRUE
    )
    on.exit(cli::cli_progress_done(id = pb, result = "clear"), add = TRUE)
  }

  paths <- lapply(names(layers), function(k) {

    if (verbose) {
      cli::cli_progress_update(id = pb, force = TRUE)
    }

    tryCatch({

      f <- layers[[k]](x, buffer = buffer)

      if (!is.null(id)) {
        identifier <- seq_field("identifier")$name
        # because there is empty sf, rep is used instead of `<- id `
        f[[identifier]] <- rep(id, nrow(f))
      }

      seq_write(
        f,
        key = k,
        dirname = dirname,
        id = id,
        verbose = verbose,
        overwrite = overwrite
      )

    }, error = function(e) {

      if (verbose) {
        cli::cli_alert_danger(
          "Failed HYDRO layer {.val {k}}: {conditionMessage(e)}"
        )
      }

      NULL
    })
  })

  names(paths) <- names(layers)
  paths <- Filter(Negate(is.null), paths)

  invisible(paths)
}

#' Fetch hydrology layers for an area
#'
#' Retrieves infrastructure polygon, line and point layers around `x` and
#' writes them to `dirname`.
#'
#' @inheritParams get_hydro_poly
#' @param dirname `character`; Output directory.
#'
#' @return Invisibly returns a named list of written file paths.
#'
#' @keywords internal
#' @noRd
fetch_hydro <- function(
    x,
    dirname,
    buffer = 1000,
    verbose = TRUE,
    overwrite = FALSE) {

  .hydro_fetcher(
    x = x,
    dirname = dirname,
    buffer = buffer,
    verbose = verbose,
    overwrite = overwrite
  )
}

#' Generate hydrographic polygon, line and point layers for a Sequoia project
#'
#' This function is a convenience wrapper around [get_hydro_poly()],
#' [get_hydro_line()] and [get_hydro_point()], allowing the user to download
#' all products in one call and automatically write them to the project
#' directory using [seq_write()].
#'
#' @inheritParams get_hydro_poly
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
    buffer = 1000,
    verbose = TRUE,
    overwrite = FALSE
){

  ctx <- .seq_context(dirname)

  .hydro_fetcher(
    x = ctx$parca,
    dirname = dirname,
    id = ctx$id,
    buffer = buffer,
    verbose = verbose,
    overwrite = overwrite
  )

}
