#' Retrieve and classify toponymic point features around an area
#'
#' Builds a convex buffer around the input geometry, retrieves toponymic
#' point features from the BDTOPO dataset, classifies them by thematic type
#' (hydrographic, vegetation, or other), normalizes the result, and returns
#' a standardized `sf` point layer.
#'
#' @param x An `sf` object defining the input area of interest.
#'
#' @return An `sf` object containing toponymic point features with standardized
#' attribute fields:
#'   * `TYPE` — Toponym class
#'     - `THYDR` = Hydrographic toponym
#'     - `TVEGE` = Vegetation-related toponym
#'     - `TYPON` = Other toponyms
#'   * `NATURE` — Original BDTOPO object nature
#'   * `NAME` — Official toponym name (when available)
#'   * `SOURCE` — Data source identifier (`IGNF_BDTOPO_V3`)
#'
#' @details
#' The function creates a 1000 m convex buffer around the input geometry `x`
#' and retrieves toponymic point features from the BDTOPO toponymy layer.
#' Retrieved features are classified into thematic types based on their
#' object class, attribute names are standardized, and geometries are
#' normalized before being returned as a single `sf` point layer.
#'
#' @export
get_toponyme <- function(x) {

  # convex buffer
  convex <- envelope(x, 1000)

  # retrieve toponymic point
  toponyme <- get_topo(convex, "BDTOPO_V3:toponymie")

  if (is.null(toponyme)) {
    return(NULL)
  }

  # normalise field
  type   <- seq_field("type")$name
  name   <- seq_field("name")$name
  nature <- seq_field("nature")$name
  source <- seq_field("source")$name

  # classes
  t_hydr <- c(
    "Cours d\u0027eau",
    "D\u00E9tails hydrographiques",
    "N\u0153uds hydrographiques",
    "Plans d\u0027eau",
    "Plan d\u0027eau",
    "R\u00E9servoirs",
    "Surfaces hydrographiques",
    "Tron\u00E7ons hydrographiques",
    "Bassins versants topographiques"
  )

  t_vege <- c(
    "Zones de v\u00E9g\u00E9tation",
    "Parcs et r\u00E9serves",
    "Haies",
    "For\u00EAts publiques"
  )

  # type
  toponyme[[type]] <- "TYPON"
  toponyme[toponyme$classe_de_l_objet %in% t_hydr, type] <- "THYDR"
  toponyme[toponyme$classe_de_l_objet %in% t_vege, type] <- "TVEGE"

  # autres champs
  toponyme[[nature]] <- toponyme$nature_de_l_objet
  toponyme[[name]]   <- toponyme$graphie_du_toponyme
  toponyme[[source]] <- "IGNF_BDTOPO_V3"

  # normalisation finale
  toponyme <- seq_normalize(toponyme, "vct_point")

  invisible(toponyme)
}
