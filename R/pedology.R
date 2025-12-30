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

  # retrieve toponymic point
  pedology <- get_topo(x, "INRA.CARTE.SOLS:geoportail_vf")

  if (is.null(pedology)) {
    return(NULL)
  }

  # Intersection
  intersect <- sf::st_intersection(pedology, x) |>
    sf::st_cast("POLYGON") |>
    ua_generate_area(verbose = FALSE) |>
    quiet()

  # Field names from configuration
  idu <- seq_field("idu")$name
  surf_cad <- seq_field("surf_cad")$name
  surf_sig <- seq_field("surf_sig")$name
  surf_cor <- seq_field("surf_cor")$name

  pedology <- intersect[, c(names(pedology), idu, surf_cad, surf_sig, surf_cor)]

  invisible(pedology)
}

#' Download pedology PDF reports from INRA soil maps
#'
#' Downloads pedological PDF documents associated with UCS identifiers
#' from the INRA soil map repository.
#'
#' @param pedology An object (typically an `sf` object get by `get_pedology()`)
#' containing an `id_ucs` field used to identify pedology reports.
#' @param out_dir Output directory where PDF files are saved.
#' @param overwrite `logical`; whether to overwrite existing files.
#'   Defaults to `FALSE`.
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
get_pedology_pdf <- function(pedology,
                             out_dir   = "pedology_pdf",
                             overwrite = FALSE,
                             verbose   = TRUE) {

  # Input checks ----
  if (!"id_ucs" %in% names(pedology)) {
    cli::cli_abort("Field {.field id_ucs} is missing from the input object.")
  }

  id_ucs <- unique(na.omit(pedology$id_ucs))

  if (length(id_ucs) == 0) {
    if (verbose) {
      cli::cli_alert_warning("No valid {.field id_ucs} found.")
    }
    return(invisible(NULL))
  }

  # Output directory ----
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
    if (verbose) {
      cli::cli_alert_success("Created directory {.path {out_dir}}.")
    }
  }

  base_url <- "https://data.geopf.fr/annexes/ressources/INRA_carte_des_sols/INRA/"

  if (verbose) {
    cli::cli_h1("Downloading pedology PDFs")
  }

  # Download loop ----
  for (id in id_ucs) {

    file_name <- paste0("id_ucs_", id, ".pdf")
    url       <- paste0(base_url, file_name)
    destfile  <- file.path(out_dir, file_name)

    if (file.exists(destfile) && !overwrite) {
      if (verbose) {
        cli::cli_alert_info("Already exists: {.file {file_name}}")
      }
      next
    }

    if (verbose) {
      cli::cli_alert("Downloading {.file {file_name}}")
    }

    tryCatch(
      utils::download.file(
        url      = url,
        destfile = destfile,
        mode     = "wb",
        quiet    = TRUE
      ),
      error = function(e) {
        if (verbose) {
          cli::cli_alert_danger("Failed to download {.file {file_name}}")
        }
      }
    )
  }

  invisible(normalizePath(out_dir))
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
#' The pedology polygon layer is retrieved using [get_pedology()] and
#' always written to disk using [seq_write()], even when it contains
#' no features.
#'
#' When pedology features are present, associated UCS PDF reports are
#' downloaded using [get_pedology_pdf()] and saved into `dirname`.
#'
#' @return
#' Invisibly returns a named list of file paths written by [seq_write()].
#'
#' @seealso
#' [get_pedology()], [get_pedology_pdf()], [seq_write()]
#'
#' @export
seq_pedology <- function(
    dirname   = ".",
    verbose   = TRUE,
    overwrite = FALSE
) {

  # Read project area (PARCA) ----
  f_parca <- sf::read_sf(get_path("v.seq.parca.poly", dirname = dirname))
  f_id    <- get_id(dirname)

  id <- seq_field("identifiant")$name

  # Retrieve pedology ----
  pedo <- get_pedology(f_parca)

  if (!is.null(pedo) && nrow(pedo) > 0) {
    pedo[[id]] <- f_id
  }

  # Write pedology layer ----
  pedo_path <- quiet(seq_write(
    pedo,
    "v.sol.pedo.poly",
    dirname   = dirname,
    verbose   = FALSE,
    overwrite = overwrite
  ))

  if (verbose) {
    if (is.null(pedo) || nrow(pedo) == 0) {
      cli::cli_alert_info(
        "Pedology layer written (empty layer)"
      )
    } else {
      cli::cli_alert_success(
        "Pedology layer written with {nrow(pedo)} feature{?s}"
      )
    }
  }

  # Download associated PDFs ----
  if (!is.null(pedo) && nrow(pedo) > 0) {

    get_pedology_pdf(
      pedology  = pedo,
      out_dir   = dirname,
      overwrite = overwrite,
      verbose   = verbose
    )
  }

  invisible(pedo_path)
}
