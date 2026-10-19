download_climates <- function(
    cache = seq_cache("climat_metropole")$path,
    overwrite = FALSE,
    verbose = TRUE
) {

  # --- url ---
  url <- "https://entrepot.recherche.data.gouv.fr/api/access/datafile/:persistentId?persistentId=doi:10.15454/98BHVH/NSWTE3"

  # --- Validate arguments ---
  check_boolean(overwrite)
  check_boolean(verbose)

  # --- Create destination directory ---
  if (!dir.exists(cache)) {
    dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  }

  if (!dir.exists(cache)) {
    cli::cli_abort(
      "Cannot create destination directory {.path {cache}}."
    )
  }

  # --- Determine destination file ---
  filename <- "TYPO_RGF93.tif"

  destfile <- file.path(cache, filename)

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
      "Downloading {.file {filename}}..."
    )
  }

  tmpfile <- tempfile(
    pattern = "download_",
    tmpdir = cache,
    fileext = ".part"
  )

  on.exit(unlink(tmpfile), add = TRUE)

  status <- utils::download.file(
    url = url,
    destfile = tmpfile,
    mode = "wb",
    quiet = TRUE
  )

  if (status != 0L || !file.exists(tmpfile)) {
    cli::cli_abort(
      "Download failed for {.url {url}}."
    )
  }

  if (file.exists(destfile) && !overwrite) {
    cli::cli_abort(
      "File already exists: {.file {destfile}}."
    )
  }

  if (!file.rename(tmpfile, destfile)) {
    cli::cli_abort(
      "Cannot move downloaded file to {.file {destfile}}."
    )
  }

  if (verbose) {
    cli::cli_alert_success(
      "File downloaded to {.file {destfile}}."
    )
  }

  invisible(destfile)
}

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
  if (any(sf::st_is_empty(x))) {
    cli::cli_abort(
      "{.arg x} must not contain empty geometries."
    )
  }

  if (is.null(cache)) {
    cache <- seq_cache("climat_metropole")$path
  }

  check_numeric(buffer, min = 0, null_is_valid = FALSE)
  check_character(cache, unique = TRUE)
  check_boolean(overwrite, verbose, null_is_valid = FALSE)

  # --- Prepare spatial extent ---
  x <- seq_envelope(
    x = x,
    buffer = buffer,
    crs = 2154
  )

  vector <- terra::vect(x)

  # --- Load raster ---
  file <- file.path(cache, "TYPO_RGF93.tif")

  if (!dir.exists(cache)) {
    cli::cli_abort(
      "Cache directory does not exist: {.path {cache}}."
    )
  }

  if (!file.exists(file)) {
    cli::cli_abort(
      "Climate raster not found: {.file {file}}."
    )
  }

  if (file.access(file, mode = 4) != 0L) {
    cli::cli_abort(
      "Climate raster is not readable: {.file {file}}."
    )
  }

  metropole <- terra::rast(file)

  if (!inherits(metropole, "SpatRaster")) {
    cli::cli_abort("Climate raster must be a {.cls SpatRaster}.")
  }

  if (terra::nlyr(metropole) != 1L) {
    cli::cli_abort(
      "Climate raster must contain exactly one layer."
    )
  }

  if (!nzchar(terra::crs(metropole))) {
    cli::cli_abort(
      "Climate raster has no defined coordinate reference system."
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
    cli::cli_alert_info("Extracting climate classification.")
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
    cli::cli_alert_success("Climate classification extracted.")
  }

  return(raster)
}

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

  if (!inherits(metropole, "SpatRaster")) {
    cli::cli_abort("Climate raster must be a {.cls SpatRaster}.")
  }

  # --- Vectorize climate ---
  climate_sf <- terra::as.polygons(
    climate,
    dissolve = TRUE,
    na.rm = TRUE
  ) |>
    sf::st_as_sf()

  check_table(climate_sf, class = "sf", fields = c("label"))

  # --- Intersection ---
  suppressWarnings(
    climate_sf |>
      sf::st_transform(sf::st_crs(x)) |>
      sf::st_intersection(x |> sf::st_geometry() |> sf::st_union()) |>
      sf::st_collection_extract("POLYGON") |>
      sf::st_cast("MULTIPOLYGON") |>
      sf::st_cast("POLYGON")
  )

  if (!nrow(climate_sf)) {
    return(NULL)
  }

  return(climate_sf)
}

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
    prsf[[identifier]] <- id
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
