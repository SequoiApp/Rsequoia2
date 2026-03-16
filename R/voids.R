#' Retrieve cadastral gaps around the _PARCA_
#'
#' @param x An `sf` object used as the input area.
#'   It must contain a source field with `bdp` or `etalab` value.
#'
#' @return An `sf` object of type `POLYGON` containing cadastral gaps
#'   with standardized fields, including:
#'   * `TYPE` - Empty, to complete
#'   * `NATURE` - Empty
#'   * `NAME` - Empty, to complete
#'   * `SOURCE` - Empty
#'
#' @export
get_voids <- function(x){

  # source
  source_field <- seq_field("source")$name

  if (!source_field %in% names(x)) {
    cli::cli_abort("{.arg x} must have {.val {source_field} as field}.")
  }

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

  if (!nrow(parcels)) {
    cli::cli_warn(
      "No data retrieved for {.val {source}}"
    )
    return(fetch_envelope)
  }

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
#' Cadastral gaps are retrieved using `get_voids()`.
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
#' [get_voids()], [seq_write()]
#'
#' @export
seq_voids <- function(
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
  gaps <- get_voids(parca)

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
