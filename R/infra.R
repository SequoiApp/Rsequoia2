#' Retrieve infrastructure polygon features around an area
#'
#' @param x An `sf` object used as the input area.
#' @param buffer `numeric`; Buffer around `x` (in **meters**) used to enlarge
#'
#' @return An `sf` object of type `POLYGON` containing infrastructure
#'   features with standardized fields, including:
#'   * `TYPE` - Infrastructure type code:
#'     - `AER` = Aerodrome runway
#'     - `BAT` = Building
#'     - `CIM` = Cemetery
#'     - `CST` = Surface construction
#'     - `HAB` = Residential area
#'     - `SPO` = Sports ground
#'     - `VIL` = Urbanized area (importance 1-2)
#'   * `NAME` - Toponym when available
#'   * `SOURCE` - Data source (`IGNF_BDTOPO_V3`)
#'
#' @details
#' The function retrieves several polygon infrastructure layers from
#' the IGN BDTOPO V3 dataset within a convex buffer around `x`.
#'
#' Retrieved layers include buildings, cemeteries, surface constructions,
#' aerodrome runways, sports grounds, and residential or urbanized areas.
#'
#' If no infrastructure data are found, the function returns an empty
#' standardized `sf` object.
#'
#' @export
get_infra_poly <- function(x, buffer = 1000) {

  if (!inherits(x, c("sf", "sfc"))) {
    cli::cli_abort("{.arg x} must be {.cls sf} or {.cls sfc}, not {.cls {class(x)}}.")
  }

  crs <- 2154
  x <- sf::st_transform(x, crs)
  fetch_envelope <- seq_envelope(x, buffer)

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

  # cemetery
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
#' @param buffer `numeric`; Buffer around `x` (in **meters**) used to enlarge
#'
#' @return An `sf` object of type `LINESTRING` containing linear
#'   infrastructure features with standardized fields, including:
#'   * `TYPE` - Infrastructure type code:
#'     - `CST` = Linear construction
#'     - `LEL` = Power line
#'     - `ORO` = Orographic line
#'     - `VFE` = Railway line
#'   * `NAME` - Toponym when available
#'   * `NATURE` - Additional attribute (e.g. voltage for power lines)
#'   * `SOURCE` - Data source (`IGNF_BDTOPO_V3`)
#'
#' @details
#' The function retrieves linear infrastructure layers from the IGN
#' BDTOPO V3 dataset within a convex buffer around `x`.
#'
#' Retrieved layers include linear constructions, power lines,
#' orographic lines, and railway segments.
#'
#' If no linear infrastructure data are found, the function returns
#' an empty standardized `sf` object.
#'
#' @export
get_infra_line <- function(x, buffer = 1000) {

  if (!inherits(x, c("sf", "sfc"))) {
    cli::cli_abort("{.arg x} must be {.cls sf} or {.cls sfc}, not {.cls {class(x)}}.")
  }

  # convex buffers
  crs <- 2154
  x <- sf::st_transform(x, crs)
  fetch_envelope <- seq_envelope(x, buffer)

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
#' @param buffer `numeric`; Buffer around `x` (in **meters**) used to enlarge
#'
#' @return An `sf` object of type `POINT` containing point infrastructure
#'   features with standardized fields, including:
#'   * `TYPE` - Infrastructure type code derived from BDTOPO nature values:
#'     - `PYL` = Pylon / antenna
#'     - `CLO` = Steeple
#'     - `CRX` = Cross or calvary
#'     - `EOL` = Wind turbine
#'     - `CST` = Other point construction
#'     - `GRO` = Cave
#'     - `GOU` = Sinkhole
#'     - `ORO` = Other orographic detail
#'   * `NAME` - Toponym when available
#'   * `SOURCE` - Data source (`IGNF_BDTOPO_V3`)
#'
#' @details
#' The function retrieves point infrastructure layers from the IGN
#' BDTOPO V3 dataset within a convex buffer around `x`.
#'
#' Retrieved layers include point constructions, orographic details,
#' and pylons. Feature types are classified into standardized Sequoia
#' codes based on their original `nature` attribute.
#'
#' If no point infrastructure data are found, the function returns
#' an empty standardized `sf` object.
#'
#' @export
get_infra_point <- function(x, buffer = 1000) {

  if (!inherits(x, c("sf", "sfc"))) {
    cli::cli_abort("{.arg x} must be {.cls sf} or {.cls sfc}, not {.cls {class(x)}}.")
  }

  # convex buffers
  crs <- 2154
  x <- sf::st_transform(x, crs)
  fetch_envelope <- seq_envelope(x, buffer)

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

#' Fetch infrastructure layers
#'
#' Internal worker
#'
#' @inheritParams get_infra_poly
#' @param dirname `character`; Output directory.
#' @param id Optional Sequoia project identifier.
#'
#' @return Invisibly returns a named list of written file paths, or `NULL`
#'   if no infrastructure layer can be retrieved.
#'
#' @keywords internal
#' @noRd
.infra_fetcher <- function(
    x,
    dirname,
    id = NULL,
    buffer = 1000,
    verbose = TRUE,
    overwrite = FALSE) {

  if (verbose) {
    cli::cli_h1("INFRA")
  }

  layers <- list(
    "v.infra.point" = get_infra_point,
    "v.infra.line"  = get_infra_line,
    "v.infra.poly"  = get_infra_poly
  )

  pb <- NULL
  if (verbose) {
    pb <- cli::cli_progress_bar(
      format = paste0(
        "{cli::pb_spin} Searching INFRA layer: {.val {k}} | ",
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
          "Failed INFRA layer {.val {k}}: {conditionMessage(e)}"
        )
      }

      NULL
    })
  })

  names(paths) <- names(layers)
  paths <- Filter(Negate(is.null), paths)

  invisible(paths)
}

#' Fetch infrastructure layers for an area
#'
#' Retrieves infrastructure polygon, line and point layers around `x` and
#' writes them to `dirname`.
#'
#' @inheritParams get_infra_poly
#' @param dirname `character`; Output directory.
#'
#' @return Invisibly returns a named list of written file paths.
#'
#' @keywords internal
#' @noRd
fetch_infra <- function(
    x,
    dirname,
    buffer = 1000,
    verbose = TRUE,
    overwrite = FALSE) {

  .infra_fetcher(
    x = x,
    dirname = dirname,
    buffer = buffer,
    verbose = verbose,
    overwrite = overwrite
  )
}


#' Generate infrastructure layers for a Sequoia project
#'
#' Retrieves infrastructure polygon, line and point layers around the project
#' area and writes them to the Sequoia project directory.
#'
#' @inheritParams get_infra_poly
#' @inheritParams seq_write
#'
#' @return Invisibly returns a named list of written file paths.
#'
#' @seealso
#' [get_infra_poly()], [get_infra_line()], [get_infra_point()]
#'
#' @export
seq_infra <- function(
    dirname = ".",
    buffer = 1000,
    verbose = TRUE,
    overwrite = FALSE) {

  ctx <- .seq_context(dirname)

  .infra_fetcher(
    x = ctx$parca,
    dirname = dirname,
    id = ctx$id,
    buffer = buffer,
    verbose = verbose,
    overwrite = overwrite
  )
}
