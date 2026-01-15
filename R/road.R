# TOPO tools ----
#' Retrieve road sections around an area
#'
#' @param x An `sf` object used as the input area.
#'
#' @return An `sf` object of type `LINESTRING` containing road sections
#'   with standardized fields, including:
#'   * `TYPE` — Road section type code, deduced from surface, numbering
#'     and importance attributes:
#'     - `RN` = National road (autoroutes, European roads, numbered roads
#'       starting with `A`, `E`, `N`, or departmental roads numbered > 900,
#'       as well as major slip roads)
#'     - `RD` = Departmental road (numbered roads starting with `D`)
#'     - `RC` = Communal road (other paved roads)
#'     - `RF` = Forest or gravel road (unpaved / empierrée)
#'     - `PN` = Natural path (tracks, trails, footpaths)
#'   * `NATURE` — Original BDTOPO road nature (surface / usage description)
#'   * `NAME` — Road identifier, taken from `cpx_numero` when available,
#'     otherwise from `cpx_toponyme_route_nommee`
#'   * `SOURCE` — Data source (`BDTOPO V3`)
#'
#' @details
#' The function retrieves road section layer from
#' the IGN BDTOPO V3 dataset within a 1000 m convex buffer around `x`.
#'
#' If no road section data are found, the function returns an empty
#' standardized `sf` object.
#'
#' @export
get_road <- function(x){

  ## convex buffer
  crs <- 2154
  x <- sf::st_transform(x, crs)
  fetch_envelope <- envelope(x, 1000)

  ## standardized field names
  type   <- seq_field("type")$name
  nature <- seq_field("nature")$name
  name   <- seq_field("name")$name
  source <- seq_field("source")$name

  ## retrieve troncon
  tr <- happign::get_wfs(
    fetch_envelope, "BDTOPO_V3:troncon_de_route", verbose = FALSE
  ) |> sf::st_transform(crs)

  if (!nrow(tr)) {
    return(NULL)
  }

  n <- nrow(tr)

  # DETECT SURFACE
  ## surface type
  revetue <- c(
    "Type autoroutier",
    "Bretelle",
    "Rond-point",
    "Route \u00e0 1 chauss\u00e9e",
    "Route \u00e0 2 chauss\u00e9es"
  )

  empierree <- "Route empierr\u00e9e"

  naturel <- c(
    "Piste cyclable",
    "Chemin",
    "Escalier",
    "Sentier"
  )

  revetement <- rep("naturel", n)
  revetement[tr$nature %in% revetue]   <- "revetue"
  revetement[tr$nature == empierree]   <- "empierree"
  revetement[tr$nature %in% naturel]   <- "naturel"

  # TYPE DEDUCTION

  type_synth <- rep(NA_character_, n)

  num <- tr$cpx_numero
  num[num == ""] <- NA

  ## RN = autoroutes + europeennes + D > 900
  is_RN <- revetement == "revetue" & (
    grepl("^(A|E|N)", num) |
      grepl("^D9[0-9]{2}", num)
  )

  ## RN = bretelles
  is_RN <- is_RN |
    (tr$nature == "Bretelle" & !is.na(tr$importance) & tr$importance >= 2)

  type_synth[is_RN] <- "RN"

  ## RD : routes départementales
  is_RD <- is.na(type_synth) &
    revetement == "revetue" &
    grepl("^D", num)

  ## RD = bretelles
  is_RD <- is_RD |
    (tr$nature == "Bretelle" & !is.na(tr$importance) & tr$importance >= 4 & tr$importance < 2)

  type_synth[is_RD] <- "RD"

  ## RC : other revetue
  is_RC <- is.na(type_synth) &
    revetement == "revetue"

  type_synth[is_RC] <- "RC"

  ## RF : empierrees
  type_synth[revetement == "empierree"] <- "RF"

  ## PN : naturel
  type_synth[revetement == "naturel"] <- "PN"

  # NAME

  nom <- tr$cpx_numero
  nom[nom == "" | is.na(nom)] <- tr$cpx_toponyme_route_nommee[nom == "" | is.na(nom)]
  nom[nom == ""] <- NA

  # OUT
  out <- seq_normalize(tr, "road_line")
  out[[type]]   <- type_synth
  out[[name]]   <- nom
  out[[source]] <- "BDTOPO V3"

  out
}

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
#' @noRd
road_buffer <- function(x, type, dist) {

  type_field <- seq_field("type")$name
  name_field <- seq_field("name")$name

  lines <- x[x[[type_field]] == type, ]
  if (nrow(lines) == 0) return(NULL)

  lines[[name_field]][is.na(lines[[name_field]])] <- "NA_TEMP"

  agg <- aggregate(
    lines,
    by = setNames(
      list(lines[[type_field]], lines[[name_field]]),
      c(type_field, name_field)
    ),
    FUN = mean,
    do_union = TRUE
  )

  agg[[name_field]][agg[[name_field]] == "NA_TEMP"] <- NA

  buf <- sf::st_simplify(
    sf::st_buffer(
      agg,
      dist,
      nQuadSegs = 5,
      joinStyle = "ROUND",
      endCapStyle = "ROUND"
    ),
    dTolerance = 0.1,
    preserveTopology = TRUE
  )

  polygon <- sf::st_sf(
    geometry = sf::st_union(sf::st_geometry(buf)),
    structure(list(type), names = type_field)
  )

  polygon
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
#' @noRd
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
#' @noRd
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
  ) |> sf::st_sf()

  result <- sf::st_union(union)
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
#' @noRd
line_to_poly <- function(x, dist = 3.5){
  type_field <- seq_field("type")$name

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

  road_poly
}

#' Generate road layers for a Sequoia project
#'
#' Retrieves road features intersecting and surrounding
#' the project area, classifies them by thematic type, and writes the
#' resulting layer to disk.
#'
#' @param dirname `character` Path to the project directory.
#'   Defaults to the current working directory.
#' @param verbose `logical`; whether to display informational messages.
#'   Defaults to `TRUE`.
#' @param overwrite `logical`; whether to overwrite existing files.
#'   Defaults to `FALSE`.
#'
#' @details
#' Road features are retrieved using [get_road()].
#'
#' If no road features are found, the function returns `NULL`
#' invisibly and no file is written.
#'
#' When features are present, the layer is written to disk using
#' [seq_write()] with the key `"v.road.topo.line"`.
#'
#' @return
#' Invisibly returns a named list of file paths written by [seq_write()].
#' Returns `NULL` invisibly when no road features are found.
#'
#' @seealso
#' [get_road()], [seq_write()]
#'
#' @export
seq_road <- function(
    dirname = ".",
    verbose = TRUE,
    overwrite = FALSE
) {

  parca <- seq_read("v.seq.parca.poly", dirname = dirname)
  id_field <- seq_field("identifier")$name
  id <- unique(parca[[id_field]])

  if (verbose){
    cli::cli_h1("ROAD")
  }

  # Retrieve toponyms
  road_line <- get_road(parca)

  # Exit early if nothing to write
  if (!nrow(road_line) || nrow(road_line) == 0) {
    if (verbose) {
      cli::cli_alert_info(
        "No road features found: road layer not written."
      )
    }
    return(invisible(NULL))
  }

  # Write road_line with id
  road_line[[id_field]] <- id

  line_path <- seq_write(
    road_line,
    "v.road.topo.line",
    dirname = dirname,
    id = id,
    verbose = verbose,
    overwrite = overwrite
  )

  # Write road_poly with id
  road_poly <- line_to_poly(road_line)
  road_poly[[id_field]] <- id

  poly_path <- seq_write(
    road_poly,
    "v.road.topo.poly",
    dirname = dirname,
    id = id,
    verbose = verbose,
    overwrite = overwrite
  )

  paths <- c(line_path, poly_path)

  return(invisible(paths))
}

# GRAPHIC tools ----
#' Retrieve cadastral gaps around the _PARCA_
#'
#' @param x An `sf` object used as the input area.
#'   It must contain a source field with `bdp` or `etalab` value.
#'
#' @return An `sf` object of type `POLYGON` containing cadastral gaps
#'   with standardized fields, including:
#'   * `TYPE` — Empty, to complete
#'   * `NATURE` — Empty
#'   * `NAME` — Empty, to complete
#'   * `SOURCE` — Empty
#'
#' @export
get_vides <- function(x){
  # source
  source_field <- seq_field("source")$name
  tab <- table(x[[source_field]])
  source <- names(tab)[which.max(tab)]

  # source check
  sources <- c("bdp", "etalab")
  if (length(source) != 1 || !source %in% sources) {
    cli::cli_abort(c(
      "x" = "Source must be one of {.val bdp} or {.val etalab}."
    ))
  }

  # convex buffer
  crs <- 2154
  x <- sf::st_transform(x, crs)
  fetch_envelope <- envelope(x, 1000)

  # parcels
  layer <- switch(source,
                  "bdp" = "BDPARCELLAIRE-VECTEUR_WLD_BDD_WGS84G:parcelle",
                  "etalab" = "CADASTRALPARCELS.PARCELLAIRE_EXPRESS:parcelle")

  parcels <- happign::get_wfs(fetch_envelope, layer, verbose = FALSE) |>
    sf::st_transform(crs)

  # mask
  mask <- sf::st_union(sf::st_geometry(parcels),
                       sf::st_geometry(x)) |>
    sf::st_union() |>
    quiet()

  # difference
  difference <- sf::st_difference(fetch_envelope, mask) |>
    quiet() |>
    sf::st_make_valid()

  return(seq_normalize(difference, "vct_poly"))
}

#' Generate cadastral gaps layer for a Sequoia project
#'
#' Retrieves cadastral gaps features intersecting and surrounding
#' the project area and writes the resulting layer to disk.
#'
#' @param dirname `character` Path to the project directory.
#'   Defaults to the current working directory.
#' @param verbose `logical`; whether to display informational messages.
#'   Defaults to `TRUE`.
#' @param overwrite `logical`; whether to overwrite existing files.
#'   Defaults to `FALSE`.
#'
#' @details
#' Cadastral gaps are retrieved using [get_vides()].
#'
#' If no features are found, the function returns `NULL`
#' invisibly and no file is written.
#'
#' When features are present, the layer is written to disk using
#' [seq_write()] with the key `"v.cad.vides.poly"`.
#'
#' @return
#' Invisibly returns a named list of file paths written by [seq_write()].
#' Returns `NULL` invisibly when no features are found.
#'
#' @seealso
#' [get_vides()], [seq_write()]
#'
#' @export
seq_vides <- function(
    dirname = ".",
    verbose = TRUE,
    overwrite = FALSE
) {

  parca <- seq_read("v.seq.parca.poly", dirname = dirname)
  id_field <- seq_field("identifier")$name
  id <- unique(parca[[id_field]])

  if (verbose){
    cli::cli_h1("VIDES")
  }

  # Retrieve toponyms
  gaps <- get_vides(parca)

  # Exit early if nothing to write
  if (!nrow(gaps) || nrow(gaps) == 0) {
    if (verbose) {
      cli::cli_alert_info(
        "No gaps found: gaps layer not written."
      )
    }
    return(invisible(NULL))
  }

  # Write road_line with id
  gaps[[id_field]] <- id

  gaps_path <- seq_write(
    gaps,
    "v.cad.vides.poly",
    dirname = dirname,
    id = id,
    verbose = verbose,
    overwrite = overwrite
  )

  return(invisible(gaps_path))
}

#' Get lines from the completed cadastral gaps layer
#'
#' Generates line features along polygons, using buffer difference
#' and convex hull splitting.
#'
#' @param x An `sf` polygon object from [Rsequoia2::get_vides()]
#'   completed by user
#'
#' @return An `sf` object with LINESTRING geometries normalized as road lines.
#'
#' @noRd
vides_to_line <- function(x){
  cast <- sf::st_cast(x, 'MULTILINESTRING', warn = F)
  difference <- sf::st_difference(cast, sf::st_buffer(sf::st_union(x), -0.0001))

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

#' Get polygons from the completed cadastral gaps layer
#'
#' Cleans polygon topology and normalizes the result as road polygons.
#'
#' @param x An `sf` polygon object from [Rsequoia2::get_vides()]
#'   completed by user
#'
#' @return An `sf` object with POLYGON or MULTIPOLYGON geometries normalized as
#'   road polygons.
#'
#' @noRd
vides_to_poly <- function(x){
  poly <- clean_topology(x)
  return(seq_normalize(poly, "road_poly"))
}

#' Generate graphic road layers for a Sequoia project
#'
#' Create graphic road layers from a completed cadastral gaps layer.
#'
#' @param dirname `character` Path to the project directory.
#'   Defaults to the current working directory.
#' @param verbose `logical`; whether to display informational messages.
#'   Defaults to `TRUE`.
#' @param overwrite `logical`; whether to overwrite existing files.
#'   Defaults to `FALSE`.
#'
#' @details
#' Features are created from a completed cadastral gaps layer obtained by using
#' `seq_vides()`.
#'
#' @return
#' Invisibly returns a named list of file paths written by [seq_write()].
#' Returns `NULL` invisibly when no features are created.
#'
#' @seealso
#' [seq_vides()], [seq_write()]
#'
#' @export
seq_graphic_road <- function(
    dirname = ".",
    verbose = TRUE,
    overwrite = FALSE
) {

  parca <- seq_read("parca", dirname = dirname)
  id_field <- seq_field("identifier")$name
  id <- unique(parca[[id_field]])

  gaps <- seq_read("v.cad.vides.poly", dirname = dirname)
  if (any(is.na(gaps[[type_field]]))) {
    cli::cli_abort(c(
      "x" = "Column {.val {type_field}} contains missing values (NA).",
      "i" = "Please remove or fill NA values before continuing."
    ))
  }

  if (verbose){
    cli::cli_h1("GRAPHIC ROAD")
  }

  # Exit early if nothing to write
  if (!nrow(gaps) || nrow(gaps) == 0) {
    if (verbose) {
      cli::cli_alert_info(
        "No features found: graphic road layer not written."
      )
    }
    return(invisible(NULL))
  }

  # Retrieve lines
  road_lines <- seq_read("v.road.topo.line", dirname = dirname)
  type_field <- seq_field("type")$name
  pn <- subset(road_lines, road_lines[[type_field]] %in% "PN")

  road_line <- rbind(vides_to_line(gaps), pn)

  # Write road_line with id
  road_line[[id_field]] <- id

  line_path <- seq_write(
    road_line,
    "v.road.graphic.line",
    dirname = dirname,
    id = id,
    verbose = verbose,
    overwrite = overwrite
  )

  # Write road_poly with id
  road_poly <- vides_to_poly(gaps)
  road_poly[[id_field]] <- id

  poly_path <- seq_write(
    road_poly,
    "v.road.graphic.poly",
    dirname = dirname,
    id = id,
    verbose = verbose,
    overwrite = overwrite
  )

  paths <- list(line_path, poly_path)

  return(invisible(paths))
}

