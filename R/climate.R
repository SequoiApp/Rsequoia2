#' @title Download french climate typology raster
#'
#' @description
#' Downloads the climate typology GeoTIFF and stores it in the Rsequoia2
#' cache. The downloaded file is reused on later calls unless
#' `overwrite = TRUE`.
#'
#' @param cache `character`; Directory where the GeoTIFF is stored.
#'   Defaults to the climate typology cache directory, see [Rsequoia2::seq_cache()].
#' @param overwrite `logical`; If `TRUE`, re-download the file even when
#'   it already exists in `cache`.
#' @param verbose `logical`; If `TRUE`, display progress and informational messages.
#'
#' @return Invisibly returns a `character` scalar containing the local
#' GeoTIFF file path.
#'
#' @export
download_climate <- function(
    cache = seq_cache("climat_typo")$path,
    overwrite = FALSE,
    verbose = TRUE
) {

  # --- Validate arguments ---
  if (is.null(cache)) {
    cache <- seq_cache("climat_typo")$path
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
  url <- "https://entrepot.recherche.data.gouv.fr/api/access/datafile/:persistentId?persistentId=doi:10.15454/98BHVH/NSWTE3"

  # --- Determine destination file ---
  destfile <- file.path(cache, "TYPO_RGF93.tif")

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

  if (status != 0L || !file.exists(destfile)) {
    cli::cli_abort(
      "Download failed for {.url {url}}."
    )
  }

  if (verbose) {
    cli::cli_alert_success(
      "File downloaded to {.file {destfile}}."
    )
  }

  invisible(destfile)
}

#' @title Read French climate typology raster for an area
#'
#' @description
#' Downloads and reads the French climate typology raster, then crops and masks
#' it to a buffered area around `x`.
#'
#' The raster contains nine climate classification categories and is returned
#' with their corresponding labels.
#'
#' @inheritParams download_climate
#' @param x `sf` or `sfc`; Area used to extract climate classification.
#' @param buffer `numeric`; Buffer distance, in meters, applied around `x`
#'   before raster extraction. Default is `100`.
#'
#' @return A `SpatRaster` containing the climate classification clipped to
#' the buffered area around `x`, with categorical labels.
#'
#' @export
get_climate <- function(
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

  # --- Prepare spatial extent ---
  x_clean <- x |>
    sf::st_geometry() |>
    sf::st_transform(2154) |>
    sf::st_make_valid()

  x_clean <- x_clean[!sf::st_is_empty(x_clean)]
  if (length(x_clean) == 0) {
    cli::cli_abort("{.arg x} has no valid geometry after cleaning.")
  }

  point_or_line <- sf::st_dimension(x_clean) < 2
  has_point_or_line <- any(point_or_line)
  if (buffer == 0 && has_point_or_line) {
    cli::cli_abort(c(
      "x" = "{.arg buffer} must be positive for points and lines.",
      "i" = "{cli::qty(sum(point_or_line))}Row{?s} {.val {which(point_or_line)}} {cli::qty(sum(point_or_line))}{?is/are} point or line."
    ))
  }

  x_env <- x_clean |>
    sf::st_buffer(buffer) |>
    sf::st_union() |>
    suppressWarnings(sf::st_collection_extract("POLYGON")) |>
    sf::st_cast("POLYGON", warn = FALSE)

  x_env <- x_env[!sf::st_is_empty(x_env)]
  if (length(x_env) == 0) {
    cli::cli_abort("{.arg x} has no valid polygon geometry after buffering.")
  }

  vector <- terra::vect(x_env)

  # --- Load raster ---
  file <- download_climate(
    cache = cache,
    overwrite = overwrite,
    verbose = verbose
  )

  metropole <- terra::rast(file)

  if (!inherits(metropole, "SpatRaster")) {
    cli::cli_abort("Climate raster must be a {.cls SpatRaster}.")
  }

  if (terra::nlyr(metropole) != 1L) {
    cli::cli_abort(
      "Climate raster must contain exactly one layer."
    )
  }

  if (!terra::same.crs(metropole, vector)) {
    cli::cli_abort(
      "Climate raster and input geometry must use the same CRS."
    )
  }

  # --- Check spatial overlap ---
  raster_ext <- terra::ext(metropole)
  vector_ext <- terra::ext(vector)

  if (
    terra::xmax(raster_ext) < terra::xmin(vector_ext) ||
    terra::xmin(raster_ext) > terra::xmax(vector_ext) ||
    terra::ymax(raster_ext) < terra::ymin(vector_ext) ||
    terra::ymin(raster_ext) > terra::ymax(vector_ext)
  ) {
    cli::cli_abort(
      "Input geometry does not overlap the climate raster."
    )
  }

  # --- Crop and mask ---
  if (verbose) {
    cli::cli_alert_info("Extracting climate classification...")
  }

  raster <- terra::crop(
    metropole,
    vector,
    snap = "out"
  )

  raster <- terra::mask(
    raster,
    vector
  )

  # --- Apply categorical labels ---
  raster <- terra::as.factor(raster)

  levels(raster) <- list(
    data.frame(
      value = 1:9,
      label = c(
        "climat de montagne",
        "climat semi-continental",
        "climat oc\u00E9anique d\u00E9grad\u00E9",
        "climat oc\u00E9anique alt\u00E9r\u00E9",
        "climat oc\u00E9anique",
        "climat m\u00E9diterran\u00E9en alt\u00E9r\u00E9",
        "climat du bassin du Sud-Ouest",
        "climat m\u00E9diterran\u00E9en",
        "non class\u00E9"
      )
    )
  )

  if (verbose) {
    cli::cli_alert_success("Climate classification extracted!")
  }

  return(raster)
}

#' @title Clip climate data to an area
#'
#' @description
#' Converts a climate classification raster to polygons, then clips the
#' resulting features to `x`.
#'
#' @param x `sf` or `sfc`; Area used to clip the climate data.
#' @param climate `SpatRaster`; Climate classification raster.
#'
#' @return An `sf` object containing climate classification polygons clipped
#'   to `x`, or `NULL` if no features intersect the area.
#'
#' @keywords internal
#' @noRd
.climate_transformer <- function(
    x,
    climate
) {

  # --- Validate arguments ---
  if (!inherits(x, c("sf", "sfc"))) {
    cli::cli_abort("{.arg x} must be of class {.cls sf} or {.cls sfc}.")
  }

  if (any(sf::st_is_empty(x))) {
    cli::cli_abort(
      "{.arg x} must not contain empty geometries."
    )
  }

  if (!inherits(climate, "SpatRaster")) {
    cli::cli_abort("Climate raster must be a {.cls SpatRaster}.")
  }

  # --- Vectorize climate ---
  climate_sf <- terra::as.polygons(
    climate,
    dissolve = TRUE,
    na.rm = TRUE
  ) |>
    sf::st_as_sf()

  if (!nrow(climate_sf) > 0 || !"label" %in% names(climate_sf)) {
    cli::cli_abort("Vectorization failed")
  }

  # --- Intersection ---
  climate_sf <- suppressWarnings(
    climate_sf |>
      sf::st_transform(sf::st_crs(x)) |>
      sf::st_intersection(
        x |> sf::st_geometry() |> sf::st_union()
      ) |>
      sf::st_collection_extract("POLYGON") |>
      sf::st_cast("MULTIPOLYGON") |>
      sf::st_cast("POLYGON")
  )

  if (!nrow(climate_sf)) {
    return(NULL)
  }

  return(climate_sf)
}

#' @title Fetch climate classification data
#'
#' @description
#' Retrieves climate classification data for an area, converts the raster
#' to polygons, clips the features to `x`, and writes the resulting layer
#' to disk.
#'
#' @inheritParams get_climate
#' @param dirname `character`; Output directory.
#' @param id Optional Sequoia project identifier.
#'
#' @return Invisibly returns the path of the written climate layer, or
#' `NULL` if no climate data intersect the area.
#'
#' @keywords internal
#' @noRd
.climate_fetcher <- function(
    x,
    buffer = 100,
    cache = NULL,
    dirname,
    id = NULL,
    overwrite = FALSE,
    verbose = TRUE
) {

  if (verbose) {
    cli::cli_h1("CLIMATE")
    cli::cli_progress_message("Downloading CLIMATE layer...", clear = TRUE)
  }

  climate <- get_climate(
    x = x,
    buffer = buffer,
    cache = cache,
    overwrite = overwrite,
    verbose = verbose
  )

  climate <- .climate_transformer(
    x = x,
    climate = climate
  )

  if (is.null(climate)) {
    if (verbose) cli::cli_alert_warning("No climate found.")
    return(NULL)
  }

  if (!is.null(id)) {
    identifier <- seq_field("identifier")$name
    climate[[identifier]] <- id
  }

  path <- seq_write(
    climate,
    key = "v.clim.type.poly",
    dirname = dirname,
    id = id,
    verbose = verbose,
    overwrite = overwrite
  )

  invisible(path)
}

#' @title Fetch climate data for an area
#'
#' @description
#' Downloads climate classification data for `x` and writes the resulting
#' polygon layer to `dirname`.
#'
#' @inheritParams get_climate
#' @param dirname `character`; Output directory.
#'
#' @return Invisibly returns the path of the written climate layer, or
#' `NULL` if no climate data intersect the area.
#'
#' @export
fetch_climate <- function(
    x,
    buffer = 100,
    dirname,
    cache = NULL,
    overwrite = FALSE,
    verbose = TRUE
) {

  .climate_fetcher(
    x = x,
    buffer = buffer,
    cache = cache,
    dirname = dirname,
    id = NULL,
    overwrite = overwrite,
    verbose = verbose
  )
}

#' @title Create climate layers for a Sequoia project
#'
#' @description
#' Uses the project PARCA layer as the area of interest, retrieves the
#' climate classification data, and writes the resulting polygon layer
#' to the project directory.
#'
#' @inheritParams seq_write
#' @inheritParams get_climate
#'
#' @return Invisibly returns the path of the written climate layer, or
#' `NULL` if no climate data intersect the project area.
#'
#' @export
seq_climate <- function(
    dirname = ".",
    cache = NULL,
    buffer = 100,
    verbose = TRUE,
    overwrite = FALSE
) {

  ctx <- .seq_context(dirname)

  .climate_fetcher(
    x = ctx$parca,
    buffer = buffer,
    cache = cache,
    dirname = dirname,
    id = ctx$id,
    overwrite = overwrite,
    verbose = verbose
  )
}
