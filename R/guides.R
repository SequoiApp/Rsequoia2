#' Download forestry guides dataset
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
    verbose = TRUE,
    overwrite = FALSE
){

  if (!dir.exists(cache)) {
    dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  }

  url <- "https://www.data.gouv.fr/api/1/datasets/r/a875438e-6555-4f45-bb48-b8c92c783a29"
  gpkg_name <- "guides_stations_forestieres.gpkg"
  gpkg_path <- file.path(cache, gpkg_name)

  if (!file.exists(gpkg_path) || overwrite){
    if (verbose){
      cli::cli_progress_step(
        "Downloading {.file {gpkg_name}}",
        msg_done = "Downloading {.file {gpkg_name}}",
        spinner = TRUE
      )
    }

    curl::curl_download(
      url = url,
      destfile = gpkg_path,
      quiet = TRUE
    )

  } else if (verbose) {
    cli::cli_alert_success(
      "Using cached {.file {gpkg_name}}"
    )
  }

  invisible(gpkg_path)
}

#' Retrieve forestry guides for an area
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
    verbose = TRUE,
    overwrite = FALSE
){

  if (!inherits(x, c("sf", "sfc"))) {
    cli::cli_abort("{.arg x} must be of class {.cls sf} or {.cls sfc}.")
  }

  crs <- 2154
  x <- sf::st_transform(x, crs)
  buffered_envelope <- sf::st_buffer(x, buffer)

  if (is.null(cache)){
    cache <- seq_cache("guides")$path
  }

  file <- download_guides(
    cache = cache,
    verbose = verbose,
    overwrite = overwrite
  )

  layers <- c("guides", "guides_urls", "stations")
  guides_layers <- lapply(layers, function(layer) {
    sf::read_sf(file, layer = layer)
  }) |> setNames(layers)

  # --- Intersect ---
  guides <- sf::st_filter(guides_layers$guides, buffered_envelope)
  if(!nrow(guides)){
    if (verbose){
      cli::cli_alert_warning("No guides available for {.arg x}")
    }

    return(invisible(NULL))
  }

  # --- Filter related data ---
  guide_ids <- guides$id
  guides_urls <- guides_layers$guides_urls[guides_layers$guides_urls$guide_id %in% guide_ids, ]
  stations <- guides_layers$stations[guides_layers$stations$guide_id %in% guide_ids, ]

  return(list(guides = guides, guides_urls = guides_urls, stations = stations))
}

#' Download forestry guide PDFs
#'
#' Downloads PDF documents associated with forestry guides using their IDs.
#' Guide metadata and download URLs are retrieved from [download_guides()].
#' When multiple URLs are available for a guide, they are tried sequentially
#' until a download succeeds.
#'
#' Existing files are reused unless `overwrite = TRUE`.
#' PDF filenames are generated from normalized guide names.
#'
#' @param guide_id `character` or `numeric`; Vector of forestry guide IDs.
#' @param dirname `character`; Directory where PDF files are stored.
#'   Defaults to the guides cache directory, see [seq_cache()].
#' @param verbose `logical`; If `TRUE`, display download progress and messages.
#' @param overwrite `logical`; If `TRUE`, re-download existing PDF files.
#'
#' @return Invisibly returns a `character` scalar containing the normalized
#'   output directory path.
#'
#' @export
get_guide_pdf <- function(
    guide_id,
    dirname = seq_cache("guides")$path,
    verbose = TRUE,
    overwrite = FALSE
) {

  ids <- unique(guide_id)
  if (!length(ids) || anyNA(ids) || any(!nzchar(as.character(ids)))) {
    cli::cli_abort("{.arg guide_id} must contain valid IDs.")
  }

  # --- Read metadata ---
  gpkg <- download_guides(verbose = FALSE)
  all_urls <- sf::read_sf(gpkg, layer = "guides_urls")
  all_guides <- sf::read_sf(gpkg, layer = "guides") |>
    sf::st_drop_geometry()

  urls <- merge(
    all_urls[all_urls$guide_id %in% ids, c("guide_id", "url")],
    all_guides[, c("id", "nom")],
    by.x = "guide_id",
    by.y = "id",
    sort = FALSE
  )

  urls <- urls[!is.na(urls$url) & nzchar(urls$url), ]

  if (!nrow(urls)) {
    cli::cli_abort("No download URLs found for the supplied guide IDs.")
  }

  dir.create(dirname, recursive = TRUE, showWarnings = FALSE)

  # --- Download guides ---
  for (guide in split(urls, urls$guide_id)) {

    name <- tolower(iconv(guide$nom[1], to = "ASCII//TRANSLIT"))
    name <- gsub("^_|_$", "", gsub("[^a-z0-9]+", "_", name))

    filename <- paste0(name, ".pdf")
    destfile <- file.path(dirname, filename)

    if (file.exists(destfile) && !overwrite) {
      if (verbose) cli::cli_alert_success("Already exists: {.file {filename}}")
      next
    }

    if (verbose) {
      cli::cli_progress_step(
        "Downloading {guide$nom[1]}",
        msg_done = "Downloaded {.file {filename}}",
        msg_failed = "Failed to download {guide_name}"
      )
    }

    for (url in unique(guide$url)) {

      if (grepl("cloud.cnpf.fr", url, fixed = TRUE)) {
        token <- basename(url)
        url <- sprintf("https://cloud.cnpf.fr/public.php/dav/files/%s/", token)
      }

      result <- try(
        curl::curl_download(url, destfile, quiet = TRUE, mode = "wb"),
        silent = TRUE
      )

      if (!inherits(result, "try-error")) break

      unlink(destfile)
    }

    if (verbose) {
      cli::cli_progress_done(
        result = if (file.exists(destfile)) "done" else "failed"
      )
    }
  }

  invisible(normalizePath(dirname))
}

#' Fetch forestry guides for an area
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
    dirname,
    id = NULL,
    buffer = 100,
    cache = NULL,
    verbose = TRUE,
    overwrite = FALSE
) {

  if (verbose) cli::cli_h1("GUIDES")

  # --- Fetch data ---
  guides <- get_guides(
    x = x,
    buffer = buffer,
    cache = cache,
    overwrite = overwrite,
    verbose = verbose
  )

  if (is.null(guides)) {
    if (verbose) cli::cli_alert_warning("No guides found.")
    return(invisible(NULL))
  }

  # --- Set identifier ---
  if (!is.null(id)) {
    identifier <- seq_field("identifier")$name
    guides$guides[[identifier]] <- id
  }

  # --- Write main layer ---
  path <- seq_write(
    guides$guides,
    key = "v.sol.guides.poly",
    dirname = dirname,
    id = id,
    verbose = verbose,
    overwrite = overwrite
  )

  # --- Write related layers ---
  existing <- sf::st_layers(path)$name
  for (layer in setdiff(names(guides), "guides")) {
    if (!overwrite && layer %in% existing) next
    sf::write_sf(guides[[layer]], dsn = path, layer = layer)
  }

  # --- Download related PDFs ---
  get_guide_pdf(
    guide_id = guides$guides$id,
    dirname = dirname,
    overwrite = overwrite,
    verbose = verbose
  )

  invisible(path)
}

#' Fetch forestry guides for an area
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
    dirname,
    buffer = 100,
    cache = NULL,
    verbose = TRUE,
    overwrite = FALSE
) {

  .guides_fetcher(
    x = x,
    dirname = dirname,
    id = NULL,
    buffer = buffer,
    cache = cache,
    verbose = verbose,
    overwrite = overwrite
  )
}

#' Create forestry guide layers for a Sequoia project
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
    dirname = dirname,
    id = ctx$id,
    buffer = buffer,
    cache = cache,
    overwrite = overwrite,
    verbose = verbose
  )
}
