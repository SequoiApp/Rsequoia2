#' Retrieve infrastructure polygon features around an area
#'
#' @param x An `sf` object used as the input area.
#'
#' @return An `sf` object of type `POLYGON` containing infrastructure
#'   features with standardized fields, including:
#'   * `TYPE` — Infrastructure type code:
#'     - `BAT` = Building
#'     - `CIM` = Cemetery
#'     - `CST` = Surface construction
#'     - `AER` = Aerodrome runway
#'     - `SPO` = Sports ground
#'     - `VIL` = Urbanized area (importance 1–2)
#'     - `HAB` = Residential area
#'   * `NAME` — Toponym when available
#'   * `SOURCE` — Data source (`IGNF_BDTOPO_V3`)
#'
#' @details
#' The function retrieves several polygon infrastructure layers from
#' the IGN BDTOPO V3 dataset within a 1000 m convex buffer around `x`.
#'
#' Retrieved layers include buildings, cemeteries, surface constructions,
#' aerodrome runways, sports grounds, and residential or urbanized areas.
#'
#' If no infrastructure data are found, the function returns an empty
#' standardized `sf` object.
#'
#' @export
get_infra_poly <- function(x) {

  if (!inherits(x, c("sf", "sfc"))) {
    cli::cli_abort("{.arg x} must be {.cls sf} or {.cls sfc}, not {.cls {class(x)}}.")
  }

  crs <- 2154
  x <- sf::st_transform(x, crs)
  fetch_envelope <- envelope(x, 1000)

  # empty sf
  infra_poly <- create_empty_sf("POLYGON") |>
    seq_normalize("vct_poly")

  # standardized field names
  type <- seq_field("type")$name
  nature <- seq_field("nature")$name
  name <- seq_field("name")$name
  source <-  seq_field("source")$name

  # building
  building <- happign::get_wfs(
    x = fetch_envelope,
    layer = "BDTOPO_V3:batiment",
    predicate = happign::intersects(),
    verbose = FALSE
  )

  if(nrow(building)>0){
    building <- sf::st_transform(building, crs)
    building[[type]] <- "BAT"
    building[[source]] <- "IGNF_BDTOPO_V3"

    building <- seq_normalize(building, "vct_poly")

    infra_poly <- rbind(infra_poly, building)
  }

  # cimetiere
  cemetery <- happign::get_wfs(
    x = fetch_envelope,
    layer = "BDTOPO_V3:cimetiere",
    predicate = happign::intersects(),
    verbose = FALSE
  )

  if(nrow(cemetery)>0){
    cemetery <- sf::st_transform(cemetery, crs)
    cemetery[[type]] <- "CIM"
    cemetery[[name]] <- cemetery$toponyme
    cemetery[[source]] <- "IGNF_BDTOPO_V3"

    cemetery <- seq_normalize(cemetery, "vct_poly")

    infra_poly <- rbind(infra_poly, cemetery)
  }

  # construction
  construction <- happign::get_wfs(
    x = fetch_envelope,
    layer = "BDTOPO_V3:construction_surfacique",
    predicate = happign::intersects(),
    verbose = FALSE
  )

  if(nrow(construction)>0){
    construction <- sf::st_transform(construction, crs)
    construction[[type]] <- "CST"
    construction[[name]] <- construction$toponyme
    construction[[source]] <- "IGNF_BDTOPO_V3"

    construction <- seq_normalize(construction, "vct_poly")

    infra_poly <- rbind(infra_poly, construction)
  }

  # runway
  runway <- happign::get_wfs(
    x = fetch_envelope,
    layer = "BDTOPO_V3:piste_d_aerodrome",
    predicate = happign::intersects(),
    verbose = FALSE
  )

  if(nrow(runway)>0){
    runway <- sf::st_transform(runway, crs)
    runway[[type]] <- "AER"
    runway[[source]] <- "IGNF_BDTOPO_V3"

    runway <- seq_normalize(runway, "vct_poly")

    infra_poly <- rbind(infra_poly, runway)
  }

  # terrain de sport
  sport <- happign::get_wfs(
    x = fetch_envelope,
    layer = "BDTOPO_V3:terrain_de_sport",
    predicate = happign::intersects(),
    verbose = FALSE
  )

  if(nrow(sport)>0){
    sport <- sf::st_transform(sport, crs)
    sport[[type]] <- "SPO"
    sport[[source]] <- "IGNF_BDTOPO_V3"

    sport <- seq_normalize(sport, "vct_poly")

    infra_poly <- rbind(infra_poly, sport)
  }

  # habitation
  habitation <- happign::get_wfs(
    x = fetch_envelope,
    layer = "BDTOPO_V3:zone_d_habitation",
    predicate = happign::intersects(),
    verbose = FALSE
  )

  if(nrow(habitation)>0){
    habitation <- sf::st_transform(habitation, crs)
    habitation[[type]] <- ifelse(habitation$importance  %in% c(1, 2), "VIL", "HAB")
    habitation[[name]] <- habitation$toponyme
    habitation[[source]] <- "IGNF_BDTOPO_V3"

    habitation <- seq_normalize(habitation, "vct_poly")

    infra_poly <- rbind(infra_poly, habitation)
  }

  if (nrow(infra_poly)==0) {
    cli::cli_warn("No infrastructure data found. Empty {.cls sf} is returned.")
  }

  return(invisible(infra_poly))
}

#' Retrieve linear infrastructure features around an area
#'
#' @param x An `sf` object used as the input area.
#'
#' @return An `sf` object of type `LINESTRING` containing linear
#'   infrastructure features with standardized fields, including:
#'   * `TYPE` — Infrastructure type code:
#'     - `CST` = Linear construction
#'     - `LEL` = Power line
#'     - `ORO` = Orographic line
#'     - `VFE` = Railway line
#'   * `NAME` — Toponym when available
#'   * `NATURE` — Additional attribute (e.g. voltage for power lines)
#'   * `SOURCE` — Data source (`IGNF_BDTOPO_V3`)
#'
#' @details
#' The function retrieves linear infrastructure layers from the IGN
#' BDTOPO V3 dataset within a 1000 m convex buffer around `x`.
#'
#' Retrieved layers include linear constructions, power lines,
#' orographic lines, and railway segments.
#'
#' If no linear infrastructure data are found, the function returns
#' an empty standardized `sf` object.
#'
#' @export
get_infra_line <- function(x) {

  if (!inherits(x, c("sf", "sfc"))) {
    cli::cli_abort("{.arg x} must be {.cls sf} or {.cls sfc}, not {.cls {class(x)}}.")
  }

  # convex buffers
  crs <- 2154
  x <- sf::st_transform(x, crs)
  fetch_envelope <- envelope(x, 1000)

  # empty sf
  infra_line <- create_empty_sf("LINESTRING") |>
    seq_normalize("vct_line")

  # standardized field names
  type <- seq_field("type")$name
  nature <- seq_field("nature")$name
  name <- seq_field("name")$name
  source <-  seq_field("source")$name

  # construction lineaire
  construction <- happign::get_wfs(
    x = fetch_envelope,
    layer = "BDTOPO_V3:construction_lineaire",
    predicate = happign::intersects(),
    verbose = FALSE
  )

  if(nrow(construction)>0){
    construction <- sf::st_transform(construction, crs)
    construction[[type]] <- "CST"
    construction[[name]] <- construction$toponyme
    construction[[source]] <- "IGNF_BDTOPO_V3"

    construction <- seq_normalize(construction, "vct_line")

    infra_line <- rbind(infra_line, construction)
  }

  # ligne electrique
  electric <- happign::get_wfs(
    x = fetch_envelope,
    layer = "BDTOPO_V3:ligne_electrique",
    predicate = happign::intersects(),
    verbose = FALSE
  )

  if(nrow(electric)>0){
    electric <- sf::st_transform(electric, crs)
    electric[[type]] <- "LEL"
    electric[[nature]] <- electric$voltage
    electric[[source]] <- "IGNF_BDTOPO_V3"

    electric <- seq_normalize(electric, "vct_line")

    infra_line <- rbind(infra_line, electric)
  }

  # ligne orographique
  orography <- happign::get_wfs(
    x = fetch_envelope,
    layer = "BDTOPO_V3:ligne_orographique",
    predicate = happign::intersects(),
    verbose = FALSE
  )

  if(nrow(orography)>0){
    orography <- sf::st_transform(orography, crs)
    orography[[type]] <- "ORO"
    orography[[name]] <- orography$toponyme
    orography[[source]] <- "IGNF_BDTOPO_V3"

    orography <- seq_normalize(orography, "vct_line")

    infra_line <- rbind(infra_line, orography)
  }

  # rail
  rail <- happign::get_wfs(
    x = fetch_envelope,
    layer = "BDTOPO_V3:troncon_de_voie_ferree",
    predicate = happign::intersects(),
    verbose = FALSE
  )

  if(nrow(rail)>0){
    rail <- sf::st_transform(rail, crs)
    rail[[type]] <- "VFE"
    rail[[name]] <- rail$cpx_toponyme
    rail[[source]] <- "IGNF_BDTOPO_V3"

    rail <- seq_normalize(rail, "vct_line")

    infra_line <- rbind(infra_line, rail)
  }

  if (nrow(infra_line)==0) {
    cli::cli_warn("No infrastructure data found. Empty {.cls sf} is returned.")
  }

  return(invisible(infra_line))
}

#' Retrieve point infrastructure features around an area
#'
#' @param x An `sf` object used as the input area.
#'
#' @return An `sf` object of type `POINT` containing point infrastructure
#'   features with standardized fields, including:
#'   * `TYPE` — Infrastructure type code derived from BDTOPO nature values:
#'     - `PYL` = Pylon / antenna
#'     - `CLO` = Steeple
#'     - `CRX` = Cross or calvary
#'     - `EOL` = Wind turbine
#'     - `CST` = Other point construction
#'     - `GRO` = Cave
#'     - `GOU` = Sinkhole
#'     - `ORO` = Other orographic detail
#'   * `NAME` — Toponym when available
#'   * `SOURCE` — Data source (`IGNF_BDTOPO_V3`)
#'
#' @details
#' The function retrieves point infrastructure layers from the IGN
#' BDTOPO V3 dataset within a 1000 m convex buffer around `x`.
#'
#' Retrieved layers include point constructions, orographic details,
#' and pylons. Feature types are classified into standardized Sequoia
#' codes based on their original `nature` attribute.
#'
#' If no point infrastructure data are found, the function returns
#' an empty standardized `sf` object.
#'
#' @export
get_infra_point <- function(x) {

  if (!inherits(x, c("sf", "sfc"))) {
    cli::cli_abort("{.arg x} must be {.cls sf} or {.cls sfc}, not {.cls {class(x)}}.")
  }

  # convex buffers
  crs <- 2154
  x <- sf::st_transform(x, crs)
  fetch_envelope <- envelope(x, 1000)

  # empty sf
  infra_point <- create_empty_sf("POINT") |>
    seq_normalize("vct_point")

  # standardized field names
  type <- seq_field("type")$name
  nature <- seq_field("nature")$name
  name <- seq_field("name")$name
  source <-  seq_field("source")$name

  # construction
  construction <- happign::get_wfs(
    x = fetch_envelope,
    layer = "BDTOPO_V3:construction_ponctuelle",
    predicate = happign::intersects(),
    verbose = FALSE
  )

  if(nrow(construction)>0){
    construction <- sf::st_transform(construction, crs)
    construction[[type]] <- ifelse(construction$nature == "Antenne", "PYL",
                                   ifelse(construction$nature == "Clocher", "CLO",
                                          ifelse(construction$nature == "Croix", "CRX",
                                                 ifelse(construction$nature == "Calvaire", "CRX",
                                                        ifelse(construction$nature == "Eolienne", "EOL", "CST")))))
    construction[[name]] <- construction$toponyme
    construction[[source]] <- "IGNF_BDTOPO_V3"

    construction <- seq_normalize(construction, "vct_point")

    infra_point <- rbind(infra_point, construction)
  }

  # orography
  orography <- happign::get_wfs(
    x = fetch_envelope,
    layer = "BDTOPO_V3:detail_orographique",
    predicate = happign::intersects(),
    verbose = FALSE
  )

  if(nrow(orography)>0){
    orography <- sf::st_transform(orography, crs)
    orography[[type]] <- ifelse(orography$nature == "Grotte", "GRO",
                          ifelse(orography$nature == "Gouffre", "GOU", "ORO"))
    orography[[name]] <- orography$toponyme
    orography[[source]] <- "IGNF_BDTOPO_V3"

    orography <- seq_normalize(orography, "vct_point")

    infra_point <- rbind(infra_point, orography)
  }

  # pylon
  pylon <- happign::get_wfs(
    x = fetch_envelope,
    layer = "BDTOPO_V3:pylone",
    predicate = happign::intersects(),
    verbose = FALSE
  )

  if(nrow(pylon)>0){
    pylon <- sf::st_transform(pylon, crs)
    pylon[[type]] <- "PYL"
    pylon[[source]] <- "IGNF_BDTOPO_V3"

    pylon <- seq_normalize(pylon, "vct_point")

    infra_point <- rbind(infra_point, pylon)
  }

  if (nrow(infra_point)==0) {
    cli::cli_warn("No infrastructure data found. Empty {.cls sf} is returned.")
  } else {
    infra_point <- sf::st_zm(infra_point, drop = TRUE, what = "ZM")
  }

  return(invisible(infra_point))
}

#' Generates infrastructure polygon, line and point layers for a Sequoia project.
#'
#' This function is a convenience wrapper around [get_infra_poly()],
#' [get_infra_line()] and [get_infra_point()], allowing the user to download
#' all products in one call and automatically write them to the project
#' directory using [seq_write()].
#'
#' @param dirname `character` Path to the directory. Defaults to the current
#' working directory.
#' @inheritParams seq_write
#'
#' @details
#' Each infrastructure layer is always written to disk using [seq_write()],
#' even when it contains no features (`nrow == 0`).
#'
#' Informational messages are displayed to indicate whether a layer
#' contains features or is empty.
#'
#' @return A named list of file paths written by [seq_write()],
#' one per hydrographic layer.
#'
#' @seealso
#' [get_infra_poly()], [get_infra_line()], [get_infra_point()],
#' [seq_write()]
#'
seq_infra <- function(
    dirname = ".",
    verbose = TRUE,
    overwrite = FALSE
) {

  # read PARCA
  parca <- seq_read("v.seq.parca.poly", dirname = dirname)
  id_field <- seq_field("identifier")$name
  id <- unique(parca[[id_field]])

  if (verbose){
    cli::cli_h1("INFRA")
  }

  # create empty path list
  path <- list()

  # hydro layer specifications
  layers <- list(
    poly  = list(fun = get_infra_poly,  key = "v.infra.poly"),
    line  = list(fun = get_infra_line,  key = "v.infra.line"),
    point = list(fun = get_infra_point, key = "v.infra.point")
  )

  for (k in names(layers)) {

    f <- layers[[k]]$fun(parca)

    if (nrow(f) > 0){
      f[[id_field]] <- id
    }

    f_path <- seq_write(
      f,
      layers[[k]]$key,
      dirname = dirname,
      id      = id,
      verbose = verbose,
      overwrite = overwrite
    )

    path <- c(path, f_path)

    if (verbose) {
      if (nrow(f) == 0) {
        cli::cli_alert_info(
          c("i" = "Infra {.field {k}} layer written (empty layer)")
        )
      }
    }
  }

  return(invisible(path))
}
