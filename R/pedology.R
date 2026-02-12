#' Retrieve pedology polygon features around an area
#'
#' Retrieves pedological polygon features from the INRA soil map
#' intersecting an area of interest and computes surface attributes.
#'
#' @param x An `sf` object defining the input area of interest.
#'
#' @return An `sf` object containing pedology polygon features
#' intersecting the input geometry, with additional surface fields.
#'
#' @details
#' The function retrieves pedology polygon features from the
#' `INRA.CARTE.SOLS:geoportail_vf` layer intersecting the input geometry `x`.
#' The resulting geometries are intersected with `x`, cast to polygons,
#' and surface attributes are computed using `ua_generate_area()`.
#'
#' @seealso [ua_generate_area()]
#'
#' @export
get_pedology <- function(x) {

  crs <- 2154
  x <- sf::st_transform(x, crs)

  # retrieve toponymic point
  pedology <- happign::get_wfs(x, "INRA.CARTE.SOLS:geoportail_vf", verbose = FALSE) |>
    sf::st_transform(crs)

  if (!nrow(pedology)) {
    return(NULL)
  }

  # Intersection
  intersect <- sf::st_intersection(pedology, x) |>
    sf::st_cast("POLYGON") |>
    suppressWarnings()

  # Field names from configuration
  idu <- seq_field("idu")$name
  cad_area <- seq_field("cad_area")$name
  gis_area <- seq_field("gis_area")$name
  cor_area <- seq_field("cor_area")$name
  required_fields <- c(idu, cad_area, gis_area, cor_area)

  if (all(required_fields %in% names(intersect))) {
    pedology <- ua_generate_area(intersect, verbose = FALSE)
    pedology <- intersect[, c(names(pedology), idu, cad_area, gis_area, cor_area)]
  } else  {
    pedology <- intersect[, c(names(pedology))]
  }

  return(invisible(pedology))
}

#' Download pedology PDF reports from INRA soil maps
#'
#' Downloads pedological PDF documents associated with UCS identifiers
#' from the INRA soil map repository.
#'
#' @param id_ucs `character` used to identify pedology reports.
#' @param dirname `character`; directory where the PDF will be saved. Defaults to
#' [tools::R_user_dir()]
#' @param verbose `logical`. If `TRUE`, display progress messages.
#'
#' @return
#' Invisibly returns the normalized path to `out_dir`. Returns
#' `NULL` invisibly if no valid `id_ucs` is found.
#'
#' @details
#' The function extracts unique UCS identifiers from the `id_ucs`
#' field of `pedology`, builds download URLs pointing to the INRA
#' soil map repository, and downloads the corresponding PDF documents.
#'
#' Existing files are skipped unless `overwrite = TRUE`. All user
#' feedback is handled via the `cli` package and can be silenced by
#' setting `verbose = FALSE`.
#'
#' @seealso [get_pedology()]
#'
#' @export
get_pedology_pdf <- function(id_ucs, dirname = NULL, verbose = TRUE) {

  if (is.null(dirname)) {
    dirname <- tools::R_user_dir("Rsequoia2", which = "data")
  }
  dir.create(dirname, recursive = TRUE, showWarnings = FALSE)

  id_ucs <- unique(id_ucs)
  if (is.null(id_ucs) || length(id_ucs) == 0) {
    cli::cli_abort("{.arg id_ucs} must be a non-empty vector.")
  }

  base_url <- "https://data.geopf.fr/annexes/ressources/INRA_carte_des_sols/INRA"

  paths <- c()
  for (id in id_ucs) {

    filename <- sprintf("id_ucs_%s.pdf", id)

    url <- file.path(base_url, filename)
    filepath <- file.path(dirname, filename)

    tryCatch(
      {
        curl::curl_download(url, filepath, quiet = !verbose)
        paths <- c(paths, setNames(filepath, tools::file_path_sans_ext(filename)))
        if (verbose){
          cli::cli_alert_success("UCS {id} saved to: {.path {dirname}}")
        }
      },
      error = function(e) {
        cli::cli_alert_warning("Failed to download {.file {filename}}")
      }
    )
  }

  return(invisible(paths))
}

#' Generate pedology polygon layer and associated PDF reports
#'
#' Retrieves pedology polygon features intersecting the project area,
#' writes the resulting layer to disk and downloads associated pedology
#' PDF reports into the project directory.
#'
#' @param dirname `character` Path to the project directory.
#'   Defaults to the current working directory.
#' @param verbose `logical`; whether to display informational messages.
#'   Defaults to `TRUE`.
#' @param overwrite `logical`; whether to overwrite existing files.
#'   Defaults to `FALSE`.
#'
#' @details
#' Pedology polygon features are retrieved using [get_pedology()].
#'
#' If no pedology features intersect the project area, the function
#' returns `NULL` invisibly and no file is written.
#'
#' When pedology features are present, the polygon layer is written
#' to disk using [seq_write()] with the key `"v.sol.pedo.poly"`.
#' Associated UCS PDF reports are then downloaded into `dirname`
#' using [get_pedology_pdf()].
#'
#' @return
#' Invisibly returns a named list of file paths written by [seq_write()].
#' Returns `NULL` invisibly when no pedology features are found.
#'
#' @seealso
#' [get_pedology()], [get_pedology_pdf()], [seq_write()]
#'
#' @export
seq_pedology <- function(dirname = ".", verbose = TRUE, overwrite = FALSE){

  # Read project area (PARCA) ----
  parca <- seq_read("v.seq.parca.poly", dirname = dirname)
  id_field <- seq_field("identifier")$name
  id <- unique(parca[[id_field]])

  if (verbose){
    cli::cli_h1("PEDOLOGY")
  }

  # Retrieve pedology ----
  pedo <- get_pedology(parca)

  pedo_path <- NULL
  if (!is.null(pedo)){
    pedo[[id_field]] <- id

    pedo_path <- seq_write(
      pedo,
      "v.sol.pedo.poly",
      dirname = dirname,
      id = id,
      verbose = verbose,
      overwrite = overwrite
    )

    get_pedology_pdf(
      id_ucs = pedo$id_ucs,
      dirname = dirname(pedo_path),
      verbose = verbose
    )
  }

  return(invisible(pedo_path))
}

