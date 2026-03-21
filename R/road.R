#' Retrieve road sections around an area
#'
#' @param x An `sf` object used as the input area.
#' @param buffer `numeric`; Buffer around `x` (in **meters**) used to enlarge
#'
#' @return An `sf` object of type `LINESTRING` containing road sections
#'   with standardized fields, including:
#'   * `TYPE` - Road section type code, deduced from surface, numbering
#'     and importance attributes:
#'     - `RN` = National road (autoroutes, European roads, numbered roads
#'       starting with `A`, `E`, `N`, or departmental roads numbered > 900,
#'       as well as major slip roads)
#'     - `RD` = Departmental road (numbered roads starting with `D`)
#'     - `RC` = Communal road (other paved roads)
#'     - `RF` = Forest or gravel road (unpaved / empierree)
#'     - `PN` = Natural path (tracks, trails, footpaths)
#'     - `LY` = Tie ridge
#'   * `NATURE` - Original BDTOPO road nature (surface / usage description)
#'   * `IMPORTANCE` - Road importance, taken from `importance` when available,
#'   * `PRIVE` - Road status, taken from `prive` when available,
#'   * `RESTRICTION` - Road restriction, taken from `restriction_de_poids_total` when available,
#'   * `NOM` - Road identifier, taken from `cpx_numero` when available,
#'     otherwise from `cpx_toponyme_route_nommee`
#'   * `SOURCE` - Data source (`BDTOPO V3`)
#'
#' @details
#' The function retrieves road section layer from
#' the IGN BDTOPO V3 dataset within a convex buffer around `x`.
#'
#' If no road section data are found, the function returns an empty
#' standardized `sf` object.
#'
#' @export
get_road <- function(x,
                     buffer = 1000){

  ## convex buffer
  crs <- 2154
  x <- sf::st_transform(x, crs)
  fetch_envelope <- envelope(x, buffer)

  ## standardized field names
  type   <- seq_field("type")$name
  nature <- seq_field("nature")$name
  name   <- seq_field("name")$name
  source <- seq_field("source")$name

  ## retrieve troncon
  tr <- happign::get_wfs(
    fetch_envelope, "BDTOPO_V3:troncon_de_route", verbose = FALSE
  ) |>
    sf::st_transform(crs) |>
    sf::st_zm()

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

  ## RD : routes departementales
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

  unique(out)
}

#' Generate road section layer for a Sequoia project
#'
#' Retrieves road section line features intersecting and surrounding
#' the project area and writes the resulting layer to disk.
#'
#' @inheritParams get_road
#' @inheritParams seq_write
#'
#' @details
#' Road section line features are retrieved using [get_road()].
#'
#' If no features are found, the function returns `NULL` invisibly and no file
#' is written.
#'
#' When features are present, the layer is written to disk using
#' [seq_write()] with the key `"v.road.line"`.
#'
#' @return
#' Invisibly returns a named list of file paths written by [seq_write()].
#' Returns `NULL` invisibly when no features are found.
#'
#' @seealso
#' [get_road()], [seq_write()]
#'
#' @export
seq_road <- function(
    dirname = ".",
    buffer = 1000,
    verbose = TRUE,
    overwrite = FALSE
) {

  # Read project area (PARCA)
  parca <- seq_read("v.seq.parca.poly", dirname = dirname)
  id_field <- seq_field("identifier")$name
  id <- unique(parca[[id_field]])

  if (verbose){
    cli::cli_h1("ROAD LINES")
  }

  # Retrieve road section
  roads <- get_road(parca, buffer = buffer)

  # Exit early if nothing to write
  if (!is.null(roads) ) {
    roads[[id_field]] <- id

    roads <- seq_write(
      roads,
      "v.road.line",
      dirname = dirname,
      id = id,
      verbose = verbose,
      overwrite = overwrite
    )
  }

  return(invisible(c(roads) |> as.list()))
}
