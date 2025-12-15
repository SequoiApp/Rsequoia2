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

  # convex buffers
  convex1000 <- buffer_to_convex(x, 1000)

  # empty sf
  infra_poly <-  create_empty_sf("POLYGON") |>
    seq_normalize("vct_poly")

  # standardized field names
  type <- seq_field("type")$name
  nature <- seq_field("nature")$name
  name <- seq_field("name")$name
  source <-  seq_field("source")$name

  # batiment
  batiment <- get_topo(convex1000, "BDTOPO_V3:batiment")

  if(!(is.null(batiment))){
    batiment[[type]] <- "BAT"
    batiment[[source]] <- "IGNF_BDTOPO_V3"

    batiment <- seq_normalize(batiment, "vct_poly")

    infra_poly <- rbind(infra_poly, batiment)
  }

  # cimetiere
  cimetiere <- get_topo(convex1000, "BDTOPO_V3:cimetiere")

  if(!(is.null(cimetiere))){
    cimetiere[[type]] <- "CIM"
    cimetiere[[name]] <- cimetiere$toponyme
    cimetiere[[source]] <- "IGNF_BDTOPO_V3"

    cimetiere <- seq_normalize(cimetiere, "vct_poly")

    infra_poly <- rbind(infra_poly, cimetiere)
  }

  # construction
  construction <- get_topo(convex1000, "BDTOPO_V3:construction_surfacique")

  if(!(is.null(construction))){
    construction[[type]] <- "CST"
    construction[[name]] <- construction$toponyme
    construction[[source]] <- "IGNF_BDTOPO_V3"

    construction <- seq_normalize(construction, "vct_poly")

    infra_poly <- rbind(infra_poly, construction)
  }

  # piste d aerodrome
  piste <- get_topo(convex1000, "BDTOPO_V3:piste_d_aerodrome")

  if(!(is.null(piste))){
    piste[[type]] <- "AER"
    piste[[source]] <- "IGNF_BDTOPO_V3"

    piste <- seq_normalize(piste, "vct_poly")

    infra_poly <- rbind(infra_poly, piste)
  }

  # terrain de sport
  terrain <- get_topo(convex1000, "BDTOPO_V3:terrain_de_sport")

  if(!(is.null(terrain))){
    terrain[[type]] <- "SPO"
    terrain[[source]] <- "IGNF_BDTOPO_V3"

    terrain <- seq_normalize(terrain, "vct_poly")

    infra_poly <- rbind(infra_poly, terrain)
  }

  # terrain de sport
  terrain <- get_topo(convex1000, "BDTOPO_V3:terrain_de_sport")

  if(!(is.null(terrain))){
    terrain[[type]] <- "SPO"
    terrain[[source]] <- "IGNF_BDTOPO_V3"

    terrain <- seq_normalize(terrain, "vct_poly")

    infra_poly <- rbind(infra_poly, terrain)
  }

  # terrain de sport
  habitation <- get_topo(convex1000, "BDTOPO_V3:zone_d_habitation")

  if(!(is.null(habitation))){
    habitation[[type]] <- ifelse(habitation$importance  %in% c(1, 2), "VIL", "HAB")
    habitation[[name]] <- habitation$toponyme
    habitation[[source]] <- "IGNF_BDTOPO_V3"

    habitation <- seq_normalize(habitation, "vct_poly")

    infra_poly <- rbind(infra_poly, habitation)
  }

  invisible(infra_poly)
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
  convex1000 <- buffer_to_convex(x, 1000)

  # empty sf
  infra_line <-  create_empty_sf("LINESTRING") |>
    seq_normalize("vct_line")

  # standardized field names
  type <- seq_field("type")$name
  nature <- seq_field("nature")$name
  name <- seq_field("name")$name
  source <-  seq_field("source")$name

  # construction lineaire
  construction <- get_topo(convex1000, "BDTOPO_V3:construction_lineaire")

  if(!(is.null(construction))){
    construction[[type]] <- "CST"
    construction[[name]] <- construction$toponyme
    construction[[source]] <- "IGNF_BDTOPO_V3"

    construction <- seq_normalize(construction, "vct_line")

    infra_line <- rbind(infra_line, construction)
  }

  # ligne electrique
  ligne <- get_topo(convex1000, "BDTOPO_V3:ligne_electrique")

  if(!(is.null(ligne))){
    ligne[[type]] <- "LEL"
    ligne[[nature]] <- ligne$voltage
    ligne[[source]] <- "IGNF_BDTOPO_V3"

    ligne <- seq_normalize(ligne, "vct_line")

    infra_line <- rbind(infra_line, ligne)
  }

  # ligne orographique
  oro <- get_topo(convex1000, "BDTOPO_V3:ligne_orographique")

  if(!(is.null(oro))){
    oro[[type]] <- "ORO"
    oro[[name]] <- oro$toponyme
    oro[[source]] <- "IGNF_BDTOPO_V3"

    oro <- seq_normalize(oro, "vct_line")

    infra_line <- rbind(infra_line, oro)
  }

  # voie ferree
  vfe <- get_topo(convex1000, "BDTOPO_V3:troncon_de_voie_ferree")

  if(!(is.null(vfe))){
    vfe[[type]] <- "VFE"
    vfe[[name]] <- vfe$cpx_toponyme
    vfe[[source]] <- "IGNF_BDTOPO_V3"

    vfe <- seq_normalize(vfe, "vct_line")

    infra_line <- rbind(infra_line, vfe)
  }

  invisible(infra_line)
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
  convex1000 <- buffer_to_convex(x, 1000)

  # empty sf
  infra_point <-  create_empty_sf("POINT") |>
    seq_normalize("vct_point")

  # standardized field names
  type <- seq_field("type")$name
  nature <- seq_field("nature")$name
  name <- seq_field("name")$name
  source <-  seq_field("source")$name

  # construction ponctuelle
  construction <- get_topo(convex1000, "BDTOPO_V3:construction_ponctuelle")

  if(!(is.null(construction))){
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
  oro <- get_topo(convex1000, "BDTOPO_V3:detail_orographique")

  if(!(is.null(oro))){
    oro[[type]] <- ifelse(oro$nature == "Grotte", "GRO",
                          ifelse(oro$nature == "Gouffre", "GOU", "ORO"))
    oro[[name]] <- oro$toponyme
    oro[[source]] <- "IGNF_BDTOPO_V3"

    oro <- seq_normalize(oro, "vct_point")

    infra_point <- rbind(infra_point, oro)
  }

  # pylone
  pylone <- get_topo(convex1000, "BDTOPO_V3:pylone")

  if(!(is.null(pylone))){
    pylone[[type]] <- "PYL"
    pylone[[source]] <- "IGNF_BDTOPO_V3"

    pylone <- seq_normalize(pylone, "vct_point")

    infra_point <- rbind(infra_point, pylone)
  }

  invisible(infra_point)
}
