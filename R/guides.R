#' @title Download forestry guides dataset
#'
#' @description
#' Downloads the forestry guides GeoPackage and stores it in the Rsequoia2
#' cache. The downloaded file is reused on later calls unless
#' `overwrite = TRUE`.
#'
#' @param cache `character`; Directory where the GeoPackage is stored.
#'   Defaults to the guides cache directory, see [Rsequoia2::seq_cache()].
#' @param overwrite `logical`; If `TRUE`, re-download the file even when
#'   it already exists in `cache`.
#' @param verbose `logical`; If `TRUE`, display progress and informational messages.
#'
#' @details
#' Data source: [Forestry guides for forest stations in metropolitan France](https://www.data.gouv.fr/datasets/guides-des-stations-forestieres-en-france-metropolitaine).
#'
#' @return Invisibly returns a `character` scalar containing the local
#' GeoPackage file path.
#'
#' @export
download_guides <- function(
    cache = seq_cache("guides")$path,
    overwrite = FALSE,
    verbose = TRUE
){

  # --- Validate arguments ---
  if (is.null(cache)) {
    cache <- seq_cache("guides")$path
  }

  if (!is.character(cache) || length(cache) != 1L || is.na(cache) || !nzchar(cache)) {
    cli::cli_abort(
      "{.arg cache} must be a non-empty character scalar."
    )
  }

  if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
    cli::cli_abort(
      "{.arg overwrite} must be a single non-missing logical value."
    )
  }

  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) {
    cli::cli_abort(
      "{.arg verbose} must be a single non-missing logical value."
    )
  }

  # --- Create destination directory ---
  if (!dir.exists(cache)) {
    dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  }

  if (!dir.exists(cache)) {
    cli::cli_abort(
      "Cannot create destination directory {.path {cache}}."
    )
  }

  # --- URL ---
  url <- "https://www.data.gouv.fr/api/1/datasets/r/a875438e-6555-4f45-bb48-b8c92c783a29"

  # --- Determine destination file ---
  destfile <- file.path(cache, "gs.gpkg")

  if (file.exists(destfile) && !overwrite) {
    if (verbose) {
      cli::cli_alert_info(
        "File already exists: {.file {destfile}}"
      )
    }

    return(invisible(destfile))
  }

  # --- Download file ---
  if (verbose) {
    cli::cli_alert_info(
      "Downloading {.file {basename(destfile)}}..."
    )
  }

  status <- utils::download.file(
    url = url,
    destfile = destfile,
    mode = "wb",
    quiet = TRUE
  )

  if (
    status != 0L ||
    !file.exists(destfile) ||
    is.na(file.info(destfile)$size) ||
    file.info(destfile)$size == 0
  ) {
    unlink(destfile)

    cli::cli_abort(
      "Download failed or returned an empty file for {.url {url}}."
    )
  }

  if (verbose) {
    cli::cli_alert_success(
      "File downloaded to {.file {destfile}}."
    )
  }

  invisible(destfile)
}

#' @title Retrieve forestry guides for an area
#'
#' @description
#' Downloads and reads the forestry guides GeoPackage, then retrieves the
#' guides intersecting a buffered area around `x`, together with their
#' associated URLs and stations.
#'
#' @param x `sf` or `sfc`; Area used to retrieve forestry guides.
#' @param buffer `numeric`; Buffer distance, in meters, applied around `x`
#'   before spatial intersection. Default is `100`.
#' @param cache `character`; Directory where the GeoPackage is stored.
#'   Defaults to the guides cache directory, see [Rsequoia2::seq_cache()].
#' @param overwrite `logical`; If `TRUE`, re-download the GeoPackage even when
#'   it already exists in `cache`.
#' @param verbose `logical`; If `TRUE`, display progress and informational messages.
#'
#' @return A named `list` containing three `sf` objects:
#'   `guides`, `guides_urls`, and `stations`. Returns `NULL` if no guides
#'   intersect the buffered area around `x`.
#'
#' @export
get_guides <- function(
    x,
    buffer = 100,
    cache = NULL,
    overwrite = FALSE,
    verbose = TRUE
){

  # --- Validate arguments ---
  if (!inherits(x, c("sf", "sfc"))) {
    cli::cli_abort("{.arg x} must be of class {.cls sf} or {.cls sfc}.")
  }

  if (length(x) == 0L || any(sf::st_is_empty(x))) {
    cli::cli_abort(
      "{.arg x} must not contain empty geometries."
    )
  }

  # --- Prepare x ---
  x <- sf::st_transform(x, 2154)
  fetch_envelope <- seq_envelope(x, buffer)

  # --- Load gs ---
  file <- download_guides(
    cache = cache,
    overwrite = overwrite,
    verbose = verbose
  )

  layers <- c("guides", "guides_urls", "stations")

  datas <- lapply(layers, function(layer) {
    sf::read_sf(file, layer = layer)
  })

  names(datas) <- layers

  # --- Intersect ---
  idx <- sf::st_intersects(datas$guides, fetch_envelope)

  guides <- datas$guides[lengths(idx) > 0, ]

  if(!nrow(guides)){
    if (verbose){
      cli::cli_alert_warning("No guides available for {.arg x}")
    }

    return(invisible(NULL))
  }

  # --- Filter related data ---
  guide_ids <- guides$id

  guides_urls <- datas$guides_urls[datas$guides_urls$guide_id %in% guide_ids, ]

  stations <- datas$stations[datas$stations$guide_id %in% guide_ids, ]

  # --- Res
  res <- list(
    guides = guides,
    guides_urls = guides_urls,
    stations = stations
  )

  return(res)
}

#' @title Download forestry guide PDFs
#'
#' @description
#' Downloads the PDF documents associated with forestry guides.
#' Multiple URLs may be available for a guide; subsequent URLs are tried
#' only if the previous download fails.
#'
#' Existing files are reused unless `overwrite = TRUE`.
#'
#' @param guides_urls `data.frame`; Table containing guide identifiers
#'   (`guide_id`) and download URLs (`url`).
#' @param dirname `character`; Directory where the PDF files are stored.
#'   Defaults to the guides cache directory, see [Rsequoia2::seq_cache()].
#' @param overwrite `logical`; If `TRUE`, re-download existing PDF files.
#' @param verbose `logical`; If `TRUE`, display progress and informational messages.
#'
#' @return Invisibly returns a `character` scalar containing the normalized
#'   output directory path.
#'
#' @export
get_guides_pdf <- function(
    guides_urls,
    dirname = seq_cache("guides")$path,
    overwrite = FALSE,
    verbose = TRUE
){

  # --- Validate arguments ---
  if (!inherits(guides_urls, "data.frame")) {
    cli::cli_abort("{.arg guides_urls} must be {.cls data.frame}.")
  }

  guides_urls <- unique(guides_urls)

  if (!nrow(guides_urls)){
    cli::cli_abort("{.arg guides_urls} can't be empty.")
  }

  if (!is.character(dirname) || length(dirname) != 1L || is.na(dirname) || !nzchar(dirname)) {
    cli::cli_abort(
      "{.arg dirname} must be a non-empty character scalar."
    )
  }

  dir.create(dirname, recursive = TRUE, showWarnings = FALSE)

  if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
    cli::cli_abort(
      "{.arg overwrite} must be a single non-missing logical value."
    )
  }

  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) {
    cli::cli_abort(
      "{.arg verbose} must be a single non-missing logical value."
    )
  }

  # --- Download loop ----
  for (id in unique(guides_urls$guide_id)) {

    urls <- unique(guides_urls$url[guides_urls$guide_id == id])
    urls <- urls[!is.na(urls) & nzchar(urls)]

    for (url in urls) {

      filename <- basename(url)

      if (!grepl("\\.pdf$", filename, ignore.case = TRUE)) {
        if (grepl("cloud.cnpf.fr", url, ignore.case = TRUE)) {
          url <- sprintf(
            "https://cloud.cnpf.fr/public.php/dav/files/%s/",
            filename
          )
        }

        filename <- paste0("Guide_", id, ".pdf")
      }

      destfile <- file.path(dirname, filename)

      if (file.exists(destfile) && !overwrite) {
        if (verbose) {
          cli::cli_alert_info("Already exists: {.file {filename}}")
        }
        break
      }

      success <- tryCatch({
        status <- utils::download.file(
          url = url,
          destfile = destfile,
          mode = "wb",
          quiet = TRUE
        )

        identical(status, 0L) &&
          file.exists(destfile) &&
          file.info(destfile)$size > 0
      }, error = function(e) FALSE)

      if (success) {
        if (verbose) {
          cli::cli_alert_success("Downloaded {.file {filename}}.")
        }
        break
      }

      if (file.exists(destfile)) {
        unlink(destfile)
      }

      if (verbose) {
        cli::cli_alert_warning(
          "Download failed for {.url {url}}."
        )
      }
    }
  }

  invisible(normalizePath(dirname))
}

#' @title Fetch forestry guides for an area
#'
#' @description
#' Retrieves forestry guides intersecting `x`, writes the guides and their
#' associated URLs and stations to a GeoPackage, and downloads the
#' associated PDF documents.
#'
#' If `id` is supplied, it is added to the guide features and used to prefix
#' the output filename.
#'
#' @inheritParams get_guides
#' @inheritParams seq_write
#' @param dirname `character`; Output directory.
#' @param id Optional Sequoia project identifier.
#'
#' @return Invisibly returns the path of the written GeoPackage, or
#' `NULL` if no guides intersect the buffered area around `x`.
#'
#' @keywords internal
#' @noRd
.guides_fetcher <- function(
    x,
    buffer = 100,
    cache = NULL,
    dirname,
    id = NULL,
    overwrite = FALSE,
    verbose = TRUE
){

  if (verbose) {
    cli::cli_h1("GUIDES")
    cli::cli_progress_message(
      "Downloading GUIDES layers...",
      clear = TRUE
    )
  }

  # --- Fetch data ---
  guides <- get_guides(
    x = x,
    buffer = buffer,
    cache = cache,
    overwrite = overwrite,
    verbose = verbose
  )

  if (is.null(guides)) {
    if (verbose) {
      cli::cli_alert_warning("No guides found.")
    }

    return(invisible(NULL))
  }

  # --- Set identifier ---
  if (!is.null(id)) {
    identifier <- seq_field("identifier")$name
    guides$guides[[identifier]] <- id
  }

  # --- Determine output path ---
  path <- seq_write(
    guides$guides,
    key = "v.sol.guides.poly",
    dirname = dirname,
    id = id,
    verbose = FALSE,
    overwrite = overwrite
  )

  # --- Write related layers ---
  sf::write_sf(
    guides$guides_urls,
    dsn = path,
    layer = "guides_urls",
    delete_layer = TRUE
  )

  sf::write_sf(
    guides$stations,
    dsn = path,
    layer = "stations",
    delete_layer = TRUE
  )

  if (verbose) {
    cli::cli_alert_success(
      "GUIDES layers saved to {.file {path}}."
    )
  }

  # --- Download related pdfs ---
  get_guides_pdf(
    guides$guides_urls,
    dirname = dirname,
    overwrite = overwrite,
    verbose = verbose
  )

  invisible(path)
}

#' @title Fetch forestry guides for an area
#'
#' @description
#' Retrieves forestry guides intersecting `x`, writes the guides and their
#' associated URLs and stations to a GeoPackage, and downloads the
#' associated PDF documents.
#'
#' @inheritParams get_guides
#' @param dirname `character`; Output directory.
#'
#' @return Invisibly returns the path of the written GeoPackage, or
#' `NULL` if no guides intersect the buffered area around `x`.
#'
#' @export
fetch_guides <- function(
    x,
    buffer = 100,
    dirname,
    cache = NULL,
    overwrite = FALSE,
    verbose = TRUE
) {

  .guides_fetcher(
    x = x,
    buffer = buffer,
    cache = cache,
    dirname = dirname,
    id = NULL,
    overwrite = overwrite,
    verbose = verbose
  )
}

#' @title Create forestry guide layers for a Sequoia project
#'
#' @description
#' Uses the project PARCA layer as the area of interest, retrieves the
#' intersecting forestry guides, and writes the resulting layers and
#' associated PDF documents to the project directory.
#'
#' @inheritParams seq_write
#' @inheritParams get_guides
#'
#' @return Invisibly returns the path of the written GeoPackage, or
#' `NULL` if no guides intersect the buffered project area.
#'
#' @export
seq_guides <- function(
    dirname = ".",
    cache = NULL,
    buffer = 100,
    verbose = TRUE,
    overwrite = FALSE
) {

  ctx <- .seq_context(dirname)

  .guides_fetcher(
    x = ctx$parca,
    buffer = buffer,
    cache = cache,
    dirname = dirname,
    id = ctx$id,
    overwrite = overwrite,
    verbose = verbose
  )
}
