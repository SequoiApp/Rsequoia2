#' Retrieve and classify toponymic point features around an area
#'
#' Builds a fetch_envelope buffer around the input geometry, retrieves toponymic
#' point features from the BDTOPO dataset, classifies them by thematic type
#' (hydrographic, vegetation, or other), normalizes the result, and returns
#' a standardized `sf` point layer.
#'
#' @param x An `sf` object defining the input area of interest.
#' @param buffer `numeric`; Buffer around `x` (in **meters**) used to enlarge
#' @param verbose `logical`; If `TRUE`, display progress and informational messages.
#'
#' @return An `sf` object containing toponymic point features with standardized
#' attribute fields:
#'   * `TYPE` - Toponym class
#'     - `AER` = Aerodrome runway
#'     - `BAT` = Building
#'     - `CIM` = Cemetery
#'     - `CST` = Construction
#'     - `HAB` = Residential area
#'     - `HYD` = Hydrographic
#'     - `ORO` = Orographic
#'     - `VEG` = Vegetation-related
#'     - `AUT` = Other toponyms
#'   * `NATURE` - Original BDTOPO object nature
#'   * `NAME` - Official toponym name (when available)
#'   * `SOURCE` - Data source identifier (`IGNF_BDTOPO_V3`)
#'
#' @details
#' The function creates a 1000 m fetch_envelope buffer around the input geometry `x`
#' and retrieves toponymic point features from the BDTOPO toponymy layer.
#' Retrieved features are classified into thematic types based on their
#' object class, attribute names are standardized, and geometries are
#' normalized before being returned as a single `sf` point layer.
#'
#' @export
get_toponyme <- function(x, buffer = 1000, verbose = TRUE) {

  # fetch_envelope buffer
  crs <- 2154
  x <- sf::st_transform(x, crs)
  fetch_envelope <- seq_envelope(x, buffer)

  if (verbose){
    cli::cli_alert_info("Downloading toponyme dataset...")
  }

  # retrieve toponymic point
  toponyme <- happign::get_wfs(
    fetch_envelope,
    "BDTOPO_V3:toponymie",
    verbose = FALSE
  )

  if (!nrow(toponyme)) {
    return(NULL)
  }

  toponyme <- toponyme |> sf::st_transform(crs)

  # normalise field
  raw_names <- names(toponyme)
  id <- seq_field("identifier")$name
  type <- seq_field("type")$name
  nature <- seq_field("nature")$name
  name <- seq_field("name")$name
  source <- seq_field("source")$name
  rotation <- seq_field("rotation")$name

  # --- type ---
  toponyme[[id]] <- NA_character_
  toponyme$cleabs <- substr(toponyme$cleabs_de_l_objet, 1, 8)
  toponyme[[type]] <- "AUT"

  #Building
  t_bat <- c("SURFACTI")
  toponyme[toponyme$cleabs %in% t_bat, type] <- "BAT"

  # Cimetery
  t_cim <- c("CIMETIER")
  toponyme[toponyme$cleabs %in% t_cim, type] <- "CIM"

  # Construction
  t_cst <- c("CONSPONC", "CONSLINE", "CONSSURF")
  toponyme[toponyme$cleabs %in% t_cst, type] <- "CST"

  # Runway
  t_aer <- c("AERODROM")
  toponyme[toponyme$cleabs %in% t_aer, type] <- "AER"

  # Habitation
  t_hab <- c("PAIHABIT")
  toponyme[toponyme$cleabs %in% t_hab, type] <- "HAB"

  # Hydrographie
  t_hyd <- c("PAIHYDRO", "COURDEAU", "PLANDEAU")
  toponyme[toponyme$cleabs %in% t_hyd, type] <- "HYD"

  # Orographie
  t_oro <- c("PAIOROGR")
  toponyme[toponyme$cleabs %in% t_oro, type] <- "ORO"

  # Vegetation
  t_veg <- c(
    "FORETPUB",
    "SURFPARC",
    "Zones de v\u00E9g\u00E9tation",
    "Haies",
    "Bois"
  )
  toponyme[toponyme$cleabs %in% t_veg, type] <- "VEG"
  toponyme[toponyme$nature_de_l_objet %in% t_veg, type] <- "VEG"

  # autres champs
  toponyme[[nature]] <- toponyme$nature_de_l_objet
  toponyme[[name]]   <- toponyme$graphie_du_toponyme
  toponyme[[source]] <- "IGNF_BDTOPO_V3"
  toponyme[[rotation]] <- NA_real_

  # normalisation finale
  # toponyme <- seq_normalize(toponyme, "vct_point")
  cols <- c(id, type, nature, name, source, rotation, raw_names)
  toponyme <- subset(toponyme, select = cols)

  return(invisible(toponyme))
}

#' Fetch TOPONYME data
#'
#' @inheritParams get_toponyme
#' @param dirname `character`; Output directory.
#' @param id Optional Sequoia project identifier.
#' @param overwrite `logical`; If `TRUE`, overwrite an existing layer.
#'
#' @return Invisibly returns the written layer path, or `NULL`.
#'
#' @keywords internal
#' @noRd
.toponyme_fetcher <- function(
    x,
    dirname,
    id = NULL,
    buffer = 1000,
    verbose = TRUE,
    overwrite = FALSE){

  if (verbose) {
    cli::cli_h1("TOPONYME")
    cli::cli_progress_message("Downloading TOPONYME layer...", clear = TRUE)
  }

  toponyme <- get_toponyme(x, buffer = buffer, verbose = FALSE)

  if (is.null(toponyme)) {
    if (verbose) cli::cli_alert_warning("No TOPONYME found.")
    return(NULL)
  }

  if (!is.null(id)) {
    identifier <- seq_field("identifier")$name
    toponyme[[identifier]] <- id
  }

  path <- seq_write(
    toponyme,
    key = "v.toponyme.point",
    dirname = dirname,
    id = id,
    verbose = verbose,
    overwrite = overwrite
  )

  return(invisible(path))
}

#' Fetch TOPONYME layer for an area
#'
#' Retrieves TOPONYME points around `x` and writes the resulting layer to
#' `dirname`.
#'
#' @inheritParams toponyme
#' @param dirname `character`; Output directory.
#' @param overwrite `logical`; If `TRUE`, overwrite an existing layer.
#'
#' @return Invisibly returns the written layer path, or `NULL`.
#'
#' @keywords internal
#' @noRd
fetch_toponyme <- function(
    x,
    dirname,
    buffer = 1000,
    verbose = TRUE,
    overwrite = FALSE) {

  .toponyme_fetcher(
    x = x,
    dirname = dirname,
    buffer = buffer,
    verbose = verbose,
    overwrite = overwrite
  )
}

#' Generate toponymic point layer for a Sequoia project
#'
#' Retrieves toponymic point features intersecting and surrounding
#' the project area, classifies them by thematic type, and writes the
#' resulting layer to disk.
#'
#' @inheritParams get_toponyme
#' @inheritParams seq_write
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
    buffer = 1000,
    verbose = TRUE,
    overwrite = FALSE
) {

  ctx <- .seq_context(dirname)

  .toponyme_fetcher(
    x = ctx$parca,
    dirname = dirname,
    id = ctx$id,
    buffer = buffer,
    verbose = verbose,
    overwrite = overwrite
  )
}
