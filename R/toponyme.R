#' Retrieve and classify toponymic point features around an area
#'
#' Builds a fetch_envelope buffer around the input geometry, retrieves toponymic
#' point features from the BDTOPO dataset, classifies them by thematic type
#' (hydrographic, vegetation, or other), normalizes the result, and returns
#' a standardized `sf` point layer.
#'
#' @param x An `sf` object defining the input area of interest.
#' @param verbose `logical` If `TRUE`, display messages.
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
#' The function creates a 1000 m fetch_envelope buffer around the input geometry `x`
#' and retrieves toponymic point features from the BDTOPO toponymy layer.
#' Retrieved features are classified into thematic types based on their
#' object class, attribute names are standardized, and geometries are
#' normalized before being returned as a single `sf` point layer.
#'
#' @export
get_toponyme <- function(x, verbose = verbose) {

  # fetch_envelope buffer
  crs <- 2154
  x <- sf::st_transform(x, crs)
  fetch_envelope <- envelope(x, 1000)

  if (verbose){
    cli::cli_alert_info("Downloading toponyme dataset...")
  }

  # retrieve toponymic point
  toponyme <- happign::get_wfs(
    fetch_envelope, "BDTOPO_V3:toponymie", verbose = FALSE
  ) |> sf::st_transform(crs)

  if (!nrow(toponyme)) {
    return(NULL)
  }

  # normalise field
  type <- seq_field("type")$name
  name <- seq_field("name")$name
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

#' Generate toponymic point layer for a Sequoia project
#'
#' Retrieves toponymic point features intersecting and surrounding
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
#' Toponymic point features are retrieved using [get_toponyme()].
#'
#' If no toponymic features are found, the function returns `NULL`
#' invisibly and no file is written.
#'
#' When features are present, the layer is written to disk using
#' [seq_write()] with the key `"v.toponyme.point"`.
#'
#' @return
#' Invisibly returns a named list of file paths written by [seq_write()].
#' Returns `NULL` invisibly when no toponymic features are found.
#'
#' @seealso
#' [get_toponyme()], [seq_write()]
#'
#' @export
seq_toponyme <- function(
    dirname = ".",
    verbose = TRUE,
    overwrite = FALSE
) {

  parca <- seq_read("v.seq.parca.poly", dirname = dirname)
  id_field <- seq_field("identifier")$name
  id <- unique(parca[[id_field]])

  if (verbose){
    cli::cli_h1("TOPONYME")
  }

  # Retrieve toponyms
  topo <- get_toponyme(parca, verbose = verbose)

  # Exit early if nothing to write
  if (!nrow(topo) || nrow(topo) == 0) {
    if (verbose) {
      cli::cli_alert_info(
        "No toponymic features found: toponymy layer not written."
      )
    }
    return(invisible(NULL))
  }

  # Add project identifier
  topo[[id_field]] <- id

  # Write layer
  topo_path <- seq_write(
    topo,
    "v.toponyme.point",
    dirname = dirname,
    verbose = verbose,
    overwrite = overwrite
  )

  return(invisible(topo_path))
}
