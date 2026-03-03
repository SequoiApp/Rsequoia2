#' Retrieve IFN regional layers by intersecting an area
#'
#' Downloads (if necessary), caches and extracts IFN regional layers
#' intersecting a given area of interest.
#'
#' @param x An `sf` object defining the input area of interest.
#' @param key A `character` string specifying the IFN regional dataset to use.
#'   Must be one of `get_keys("ifn")`:
#'   \itemize{
#'     \item `"ser"`: sylvo-ecoregions
#'     \item `"ser_ar"`: sylvo-ecoregions (aggregated regions)
#'     \item `"rfn"`: region forestiere nationale
#'     \item `"rfd"`: region forestiere departementale
#'     \item `"zp"`: zones de production
#'   }
#' @param cache A character string defining the cache directory.
#'   Defaults to a package-specific cache directory created with
#'   `tools::R_user_dir("Rsequoia2", which = "cache")`.
#'
#' @return
#' An `sf` object containing the region features intersecting `x`.
#' Returns `NULL` if no region intersects the input geometry.
#'
#' @details
#' The function retrieves official IGN regional datasets provided as
#' shapefiles in Lambert-93 projection. Downloaded archives are stored
#' locally and reused on subsequent calls.
#'
#' The regional layer is automatically reprojected to match the
#' coordinate reference system of `x` before computing spatial
#' intersections.
#'
#' Only features intersecting the input geometry are returned. No
#' geometry modification (e.g. clipping) is applied.
#'
#' @export
get_ifn <- function(x,
                    key,
                    cache = NULL) {

  # key check
  if (length(key) != 1 || !key %in% get_keys("ifn")) {
    cli::cli_abort(c(
      "x" = "{.arg key} is equal to {.val {format(key)}}.",
      "i" = "{.arg key} must be one of {.val ser}, {.val ser_ar}, {.val rfn}, {.val rfd} or {.val zp}."
    ))
  }

  # cache dir
  if (is.null(cache)) {
    cache <- tools::R_user_dir("Rsequoia2", which = "cache") |>
      file.path("ifn")
  }
  dir.create(cache, showWarnings = FALSE, recursive = TRUE)

  # URLs
  urls <- c(
    ser    = "https://inventaire-forestier.ign.fr/IMG/zip/ser_l93.zip",
    ser_ar = "https://inventaire-forestier.ign.fr/IMG/zip/ser_ar_l93.zip",
    rfn    = "https://inventaire-forestier.ign.fr/IMG/zip/rn250_l93_shp-2.zip",
    rfd    = "https://inventaire-forestier.ign.fr/IMG/zip/rf250_l93_shp-2.zip",
    zp     = "https://inventaire-forestier.ign.fr/IMG/zip/zp250_shp-2.zip"
  )

  url <- urls[[key]]

  zipfile   <- file.path(cache, basename(url))
  unzip_dir <- tools::file_path_sans_ext(zipfile)

  # download / unzip
  if (!file.exists(zipfile)) {
    cli::cli_inform("Downloading {.val {key}} data")
    curl::curl_download(url, zipfile)
  }

  if (!dir.exists(unzip_dir)) {
    utils::unzip(zipfile, exdir = unzip_dir)
  }

  # read shapefile
  shp <- list.files(unzip_dir, pattern = "\\.shp$", full.names = TRUE)
  if (length(shp) == 0) {
    cli::cli_abort("No shapefile found in {.file {unzip_dir}}.")
  }

  ifn <- sf::st_read(shp[1], quiet = TRUE)

  # CRS handling
  if (is.na(sf::st_crs(ifn))) {
    sf::st_crs(ifn) <- sf::st_crs(x)
  }

  if (!sf::st_crs(ifn) == sf::st_crs(x)) {
    ifn <- sf::st_transform(ifn, sf::st_crs(x))
  }

  # spatial intersection
  idx <- sf::st_intersects(ifn, x, sparse = FALSE)

  if (!any(idx)) {
    return(NULL)
  }

  # subset only - no reprojection here
  ifn <- ifn[rowSums(idx) > 0, , drop = FALSE]

  ifn
}

#' Download sylvoecoregion PDF reports from INF repository
#'
#' Downloads PDF documents associated with SER identifiers
#' from the IFN repository.
#'
#' @param id_ser `character` used to identify pedology reports.
#'   It can be got by using `get_ifn("ser")$codeser`.
#' @param dirname Output directory where PDF files are saved.
#' @param overwrite `logical`; whether to overwrite existing files.
#'   Defaults to `FALSE`.
#' @param verbose `logical`. If `TRUE`, display progress messages.
#'
#' @return
#' Invisibly returns the normalized path to `out_dir`. Returns
#' `NULL` invisibly if no valid `id_ucs` is found.
#'
#' @details
#' The function needs unique SER identifiers typically got from the `codeser`
#' field of `ser`, builds download URLs pointing to the INRAE
#' soil map repository, and downloads the corresponding PDF documents.
#'
#' Existing files are skipped unless `overwrite = TRUE`. All user
#' feedback is handled via the `cli` package and can be silenced by
#' setting `verbose = FALSE`.
#'
#' @seealso [get_ifn()]
#'
#' @export
get_ser_pdf <- function(
    id_ser,
    dirname = NULL,
    overwrite = FALSE,
    verbose = TRUE) {

  id_ser <- unique(id_ser)
  if (is.null(id_ser) || length(id_ser) == 0 ) {
    cli::cli_abort("{.arg id_ser} must be a non-empty vector.")
  }
  if (any(is.na(id_ser))) {
    cli::cli_abort("{.arg id_ucs} must not contain NA values.")
  }

  if (is.null(dirname)) {
    dirname <- tools::R_user_dir("Rsequoia2", which = "data") |>
      file.path("ifn")
  }
  dir.create(dirname, recursive = TRUE, showWarnings = FALSE)

  base_url <- "https://inventaire-forestier.ign.fr/IMG/pdf/"

  if (verbose) {
    cli::cli_h1("Downloading ser PDFs")
  }

  # Download loop ----
  for (code in id_ser) {

    file_name <- paste0(substr(code, 1, 1),
                        "_",
                        substr(code, 2, 3),
                        ".pdf")
    url       <- paste0(base_url, file_name)
    destfile  <- file.path(dirname, file_name)

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

  invisible(normalizePath(dirname))
}

#' Generate regional layers for a Sequoia project
#'
#' Retrieves official IGN regional datasets intersecting the project
#' area and writes the resulting layers to disk.
#'
#' @param dirname `character` Path to the project directory.
#'   Defaults to the current working directory.
#' @param key `character`; List of ifn layer identifiers to download. If not
#'   provided, the function uses `get_keys("ifn")` to automatically select all
#'   MNHN layers defined in the Sequoia configuration (`inst/config/seq_layers.yaml`)
#' @param verbose `logical`; whether to display informational messages.
#'   Defaults to `TRUE`.
#' @param overwrite `logical`; whether to overwrite existing files.
#'   Defaults to `FALSE`.
#'
#' @details
#' Regional datasets are retrieved using [get_ifn()] based on the
#' project area defined by the PARCA polygon.
#'
#' Each regional layer is written to disk using [seq_write()] with a
#' dedicated output key corresponding to the requested region k.
#'
#' If no feature is found for a given k, the corresponding layer
#' is not written.
#'
#' @return
#' Invisibly returns a named list of file paths written by [seq_write()].
#' Returns `NULL` invisibly if no regional layer is written.
#'
#' @seealso
#' [get_ifn()], [seq_write()]
#'
#' @export
seq_ifn <- function(
    dirname = ".",
    key = get_keys("ifn"),
    verbose = TRUE,
    overwrite = FALSE
) {

  # valid key + output keys
  type_key <- c(
    ser    = "v.ifn.ser.poly",
    ser_ar = "v.ifn.ser_ar.poly",
    rfn    = "v.ifn.rfn.poly",
    rfd    = "v.ifn.rfd.poly",
    zp     = "v.ifn.zp.poly"
  )

  if (!all(key %in% names(type_key))) {
    cli::cli_abort(
      "{.arg key} must be one or more of {.val {names(type_key)}}."
    )
  }

  if (verbose){
    cli::cli_h1("IFN")
  }

  # read project area once
  parca <- seq_read("v.seq.parca.poly", dirname = dirname)
  identifier <- seq_field("identifier")$name
  id <- unique(parca[[identifier]])

  out <- vector("list", length(key))
  names(out) <- key

  # main loop
  for (k in key) {

    region <- get_ifn(parca, key = k)

    if (is.null(region) || nrow(region) == 0) {
      if (verbose) {
        cli::cli_alert_info(
          "No {.val {k}} features found: layer not written."
        )
      }
      next
    }

    out[[k]] <- seq_write(
      region,
      type_key[[k]],
      dirname   = dirname,
      id        = id,
      verbose   = verbose,
      overwrite = overwrite
    )

    if (k == "ser"){
      get_ser_pdf(
        region,
        dirname   = file.path(dirname, seq_layer("v.ifn.ser.poly")$path),
        overwrite = overwrite,
        verbose   = verbose
      )
    }

    if (verbose) {
      cli::cli_alert_success(
        "{.val {k}} layer written with {nrow(region)} feature{?s}."
      )
    }
  }

  out <- Filter(Negate(is.null), out)

  if (length(out) == 0) {
    return(invisible(NULL))
  }

  invisible(out)
}
