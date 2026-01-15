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

  # batiment
  batiment <- happign::get_wfs(fetch_envelope, "BDTOPO_V3:batiment", verbose = FALSE) |>
    sf::st_transform(crs)

  if(nrow(batiment)){
    batiment[[type]] <- "BAT"
    batiment[[source]] <- "IGNF_BDTOPO_V3"

    batiment <- seq_normalize(batiment, "vct_poly")

    infra_poly <- rbind(infra_poly, batiment)
  }

  # cimetiere
  cimetiere <- happign::get_wfs(fetch_envelope, "BDTOPO_V3:cimetiere", verbose = FALSE) |>
    sf::st_transform(crs)

  if(nrow(cimetiere)){
    cimetiere[[type]] <- "CIM"
    cimetiere[[name]] <- cimetiere$toponyme
    cimetiere[[source]] <- "IGNF_BDTOPO_V3"

    cimetiere <- seq_normalize(cimetiere, "vct_poly")

    infra_poly <- rbind(infra_poly, cimetiere)
  }

  # construction
  construction <- happign::get_wfs(fetch_envelope, "BDTOPO_V3:construction_surfacique", verbose = FALSE) |>
    sf::st_transform(crs)

  if(nrow(construction)){
    construction[[type]] <- "CST"
    construction[[name]] <- construction$toponyme
    construction[[source]] <- "IGNF_BDTOPO_V3"

    construction <- seq_normalize(construction, "vct_poly")

    infra_poly <- rbind(infra_poly, construction)
  }

  # piste d aerodrome
  piste <- happign::get_wfs(fetch_envelope, "BDTOPO_V3:piste_d_aerodrome", verbose = FALSE) |>
    sf::st_transform(crs)

  if(nrow(piste)){
    piste[[type]] <- "AER"
    piste[[source]] <- "IGNF_BDTOPO_V3"

    piste <- seq_normalize(piste, "vct_poly")

    infra_poly <- rbind(infra_poly, piste)
  }

  # terrain de sport
  terrain <- happign::get_wfs(fetch_envelope, "BDTOPO_V3:terrain_de_sport", verbose = FALSE) |>
    sf::st_transform(crs)

  if(nrow(terrain)){
    terrain[[type]] <- "SPO"
    terrain[[source]] <- "IGNF_BDTOPO_V3"

    terrain <- seq_normalize(terrain, "vct_poly")

    infra_poly <- rbind(infra_poly, terrain)
  }

  # terrain de sport
  terrain <- happign::get_wfs(fetch_envelope, "BDTOPO_V3:terrain_de_sport", verbose = FALSE) |>
    sf::st_transform(crs)

  if(nrow(terrain)){
    terrain[[type]] <- "SPO"
    terrain[[source]] <- "IGNF_BDTOPO_V3"

    terrain <- seq_normalize(terrain, "vct_poly")

    infra_poly <- rbind(infra_poly, terrain)
  }

  # terrain de sport
  habitation <- happign::get_wfs(fetch_envelope, "BDTOPO_V3:zone_d_habitation", verbose = FALSE) |>
    sf::st_transform(crs)

  if(nrow(habitation)){
    habitation[[type]] <- ifelse(habitation$importance  %in% c(1, 2), "VIL", "HAB")
    habitation[[name]] <- habitation$toponyme
    habitation[[source]] <- "IGNF_BDTOPO_V3"

    habitation <- seq_normalize(habitation, "vct_poly")

    infra_poly <- rbind(infra_poly, habitation)
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
    fetch_envelope, "BDTOPO_V3:construction_lineaire", verbose = FALSE
  ) |> sf::st_transform(crs)

  if(nrow(construction)){
    construction[[type]] <- "CST"
    construction[[name]] <- construction$toponyme
    construction[[source]] <- "IGNF_BDTOPO_V3"

    construction <- seq_normalize(construction, "vct_line")

    infra_line <- rbind(infra_line, construction)
  }

  # ligne electrique
  ligne <- happign::get_wfs(
    fetch_envelope, "BDTOPO_V3:ligne_electrique", verbose = FALSE
  ) |> sf::st_transform(crs)

  if(nrow(ligne)){
    ligne[[type]] <- "LEL"
    ligne[[nature]] <- ligne$voltage
    ligne[[source]] <- "IGNF_BDTOPO_V3"

    ligne <- seq_normalize(ligne, "vct_line")

    infra_line <- rbind(infra_line, ligne)
  }

  # ligne orographique
  oro <- happign::get_wfs(
    fetch_envelope, "BDTOPO_V3:ligne_orographique", verbose = FALSE
  ) |> sf::st_transform(crs)

  if(nrow(oro)){
    oro[[type]] <- "ORO"
    oro[[name]] <- oro$toponyme
    oro[[source]] <- "IGNF_BDTOPO_V3"

    oro <- seq_normalize(oro, "vct_line")

    infra_line <- rbind(infra_line, oro)
  }

  # voie ferree
  vfe <- happign::get_wfs(
    fetch_envelope, "BDTOPO_V3:troncon_de_voie_ferree", verbose = FALSE
  ) |> sf::st_transform(crs)

  if(nrow(vfe)){
    vfe[[type]] <- "VFE"
    vfe[[name]] <- vfe$cpx_toponyme
    vfe[[source]] <- "IGNF_BDTOPO_V3"

    vfe <- seq_normalize(vfe, "vct_line")

    infra_line <- rbind(infra_line, vfe)
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

  # construction ponctuelle
  construction <- happign::get_wfs(
    fetch_envelope, "BDTOPO_V3:construction_ponctuelle", verbose = FALSE
  ) |> sf::st_transform(crs)

  if(nrow(construction)){
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

  # detail orographique
  oro <- happign::get_wfs(
    fetch_envelope, "BDTOPO_V3:detail_orographique", verbose = FALSE
  ) |> sf::st_transform(crs)

  if(nrow(oro)){
    oro[[type]] <- ifelse(oro$nature == "Grotte", "GRO",
                          ifelse(oro$nature == "Gouffre", "GOU", "ORO"))
    oro[[name]] <- oro$toponyme
    oro[[source]] <- "IGNF_BDTOPO_V3"

    oro <- seq_normalize(oro, "vct_point")

    infra_point <- rbind(infra_point, oro)
  }

  # pylone
  pylone <- happign::get_wfs(
    fetch_envelope, "BDTOPO_V3:pylone", verbose = FALSE
  ) |> sf::st_transform(crs)

  if(nrow(pylone)){
    pylone[[type]] <- "PYL"
    pylone[[source]] <- "IGNF_BDTOPO_V3"

    pylone <- seq_normalize(pylone, "vct_point")

    infra_point <- rbind(infra_point, pylone)
  }

  return(invisible(infra_point |> sf::st_zm(drop = TRUE, what = "ZM")))
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
