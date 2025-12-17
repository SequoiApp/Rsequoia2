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

  # PREPARATION
  ## convex buffer
  convex <- buffer_to_convex(x, 1000)

  ## standardized field names
  type   <- seq_field("type")$name
  nature <- seq_field("nature")$name
  name   <- seq_field("name")$name
  source <- seq_field("source")$name

  ## retrieve troncon
  tr <- get_topo(convex, "BDTOPO_V3:troncon_de_route")

  if (is.null(tr)) {
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
