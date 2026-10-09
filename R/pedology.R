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

  if (!inherits(x, c("sf", "sfc"))) {
    cli::cli_abort(
      "{.arg x} must be of class {.cls sf} or {.cls sfc}."
    )
  }

  crs <- 2154
  x <- sf::st_transform(x, crs)

  # retrieve toponymic point
  pedology <- happign::get_wfs(
    x,
    "INRA.CARTE.SOLS:geoportail_vf",
    predicate = happign::intersects(),
    verbose = FALSE)

  if (!nrow(pedology)) {
    return(NULL)
  }

  pedology <- sf::st_transform(pedology, crs)

  return(invisible(pedology))
}

#' Download pedology PDF reports from INRA soil maps
#'
#' Downloads pedological PDF documents associated with UCS identifiers
#' from the INRA soil map repository.
#'
#' @param id_ucs `character` used to identify pedology reports.
#'   It can be got by using `get_pedology()$id_ucs`.
#' @param dirname `character`; Output directory for downloaded PDF files.
#' @param verbose `logical`; If `TRUE`, display progress and informational
#'   messages.
#'
#' @return
#' Invisibly returns the normalized path to `out_dir`. Returns
#' `NULL` invisibly if no valid `id_ucs` is found.
#'
#' @details
#' The function needs unique UCS identifiers typically got from the `id_ucs`
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
get_pedology_pdf <- function(
    id_ucs,
    dirname,
    verbose = TRUE
) {

  id_ucs <- unique(id_ucs)

  if (!length(id_ucs)) {
    cli::cli_abort("{.arg id_ucs} must be a non-empty vector.")
  }

  if (anyNA(id_ucs)) {
    cli::cli_abort("{.arg id_ucs} must not contain NA values.")
  }

  dir.create(dirname, recursive = TRUE, showWarnings = FALSE)

  base_url <- "https://data.geopf.fr/annexes/ressources/INRA_carte_des_sols/INRA"

  paths <- c()
  for (id in id_ucs) {

    filename <- sprintf("id_ucs_%s.pdf", id)

    url <- file.path(base_url, filename)
    filepath <- file.path(dirname, filename)

    tryCatch(
      {
        curl::curl_download(url, filepath, quiet = TRUE)
        paths <- c(paths, setNames(filepath, tools::file_path_sans_ext(filename)))
        if (verbose){
          cli::cli_alert("{.file {filename}} saved")
        }
      },
      error = function(e) {
        if (verbose) {
          cli::cli_alert_warning("Failed to download {.file {filename}}")
        }
      }
    )
  }

  return(invisible(paths))
}


#' Clip pedology data to an area
#'
#' @param pedology `sf`; Raw pedology data.
#' @param x `sf` or `sfc`; Area used to clip pedology data.
#'
#' @return An `sf` polygon layer, or `NULL` if there is no intersection.
#'
#' @keywords internal
#' @noRd
.pedology_transformer <- function(pedology, x) {

  if (is.null(pedology) || !nrow(pedology)) {
    return(NULL)
  }

  pedology <- suppressWarnings(
    pedology |>
      sf::st_transform(sf::st_crs(x)) |>
      sf::st_intersection(x |> sf::st_geometry() |> sf::st_union()) |>
      sf::st_collection_extract("POLYGON") |>
      sf::st_cast("MULTIPOLYGON") |>
      sf::st_cast("POLYGON")
  )

  if (!nrow(pedology)) {
    return(NULL)
  }

  pedology
}

#' Fetch pedology data
#'
#'
#' @param x `sf` or `sfc`; Area of interest.
#' @param dirname `character`; Output directory.
#' @param id Optional Sequoia project identifier.
#' @param verbose `logical`; If `TRUE`, display messages.
#' @param overwrite `logical`; If `TRUE`, overwrite existing files.
#'
#' @return Invisibly returns the written pedology layer path, or `NULL`.
#'
#' @keywords internal
#' @noRd
.pedology_fetcher <- function(
    x,
    dirname,
    id = NULL,
    verbose = TRUE,
    overwrite = FALSE) {

  if (verbose) {
    cli::cli_h1("PEDOLOGY")
    cli::cli_progress_message("Downloading pedology layer...", clear = TRUE)
  }

  pedology <- get_pedology(x)
  pedology <- .pedology_transformer(pedology, x)

  if (is.null(pedology)) {
    if (verbose) {
      cli::cli_alert_info("No pedology layer found.")
    }
    return(invisible(NULL))
  }

  if (!is.null(id)) {
    identifier <- seq_field("identifier")$name
    pedology[[identifier]] <- id
  }

  path <- seq_write(
    pedology,
    key = "v.sol.pedo.poly",
    dirname = dirname,
    id = id,
    verbose = verbose,
    overwrite = overwrite
  )

  get_pedology_pdf(
    id_ucs = pedology$id_ucs,
    dirname = dirname(path),
    verbose = verbose
  )

  invisible(path)
}


#' Fetch pedology layer for an area
#'
#' Downloads pedology data for `x` and writes the resulting layer and
#' associated PDF reports to `dirname`.
#'
#' @inheritParams get_pedology
#' @param dirname `character`; Output directory.
#' @param verbose `logical`; If `TRUE`, display messages.
#' @param overwrite `logical`; If `TRUE`, overwrite the vector layer.
#'
#' @return Invisibly returns the written pedology layer path, or `NULL`.
#'
#' @keywords internal
#' @noRd
fetch_pedology <- function(
    x,
    dirname,
    verbose = TRUE,
    overwrite = FALSE) {

  .pedology_fetcher(
    x = x,
    dirname = dirname,
    verbose = verbose,
    overwrite = overwrite
  )
}


#' Generate pedology data for a Sequoia project
#'
#' Retrieves pedology features and associated PDF reports for the project area.
#'
#' @inheritParams seq_write
#'
#' @return Invisibly returns the written pedology layer path, or `NULL`.
#'
#' @seealso [get_pedology()], [get_pedology_pdf()]
#'
#' @export
seq_pedology <- function(
    dirname = ".",
    verbose = TRUE,
    overwrite = FALSE) {

  ctx <- .seq_context(dirname)

  .pedology_fetcher(
    x = ctx$parca,
    dirname = dirname,
    id = ctx$id,
    verbose = verbose,
    overwrite = overwrite
  )
}
