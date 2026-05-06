#' Download LIDAR HD tiles
#'
#' Downloads IGN LIDAR HD tiles intersecting an area of interest.
#'
#' @param x `sf` or `sfc`; Area used to select intersecting LIDAR tiles.
#' @param key `character`; LIDAR product to download. One of `"mnt"`,
#' `"mns"` or `"mnh"`.
#' @param cache `character`; Cache directory. If `NULL`, the appropriate
#' Rsequoia2 LIDAR cache is used, see [Rsequoia2::seq_cache()].
#' @param overwrite `logical`; If `TRUE`, re-download existing tiles.
#' @param verbose `logical`; If `TRUE`, display messages.
#' @param max_tries `integer`; Maximum number of download attempts.
#'
#' @return Invisibly returns a `character` vector of local tile paths.
#'
#' @keywords internal
download_lidar <- function(
    x,
    key = c("mnt", "mns", "mnh"),
    cache = NULL,
    overwrite = FALSE,
    verbose = TRUE,
    max_tries = 3
) {

  key <- match.arg(key)

  if (!inherits(x, c("sf", "sfc"))) {
    cli::cli_abort("{.arg x} must be an {.cls sf} or {.cls sfc} object.")
  }

  if (is.null(cache)) {
    cache <- seq_cache(sprintf("lidar_%s", key))$path
  }

  dir.create(cache, recursive = TRUE, showWarnings = FALSE)

  layer <- switch(
    key,
    mnt = "IGNF_MNT-LIDAR-HD:dalle",
    mns = "IGNF_MNS-LIDAR-HD:dalle",
    mnh = "IGNF_MNH-LIDAR-HD:dalle"
  )

  dalle <- happign::get_wfs(
    x = sf::st_make_valid(x),
    layer = layer,
    predicate = happign::intersects()
  )

  if (nrow(dalle) == 0) {
    cli::cli_abort("No LIDAR {toupper(key)} tile found for {.arg x}.")
  }

  urls <- dalle$url
  destfiles <- file.path(cache, dalle$name_download)

  if (verbose){
    cli::cli_alert_info("Downloading {toupper(key)} LIDAR tiles.")
  }

  seq_multi_download(
    urls = urls,
    destfiles = destfiles,
    overwrite = overwrite,
    verbose = verbose,
    max_tries = max_tries
  )

  invisible(files)
}

#' Download and extract LiDAR HD raster
#'
#' Downloads IGN LiDAR HD tiles for an area of interest, builds a VRT, masks
#' the raster to a buffered envelope around `x`, and returns it in `crs`.
#'
#' @inheritParams download_lidar
#' @param buffer `numeric`; Buffer distance, in meters, applied around `x`
#'   before masking.
#' @param crs `integer` or `character`; Target CRS of the returned raster.
#'   Defaults to EPSG:2154.
#'
#' @return Invisibly returns a `terra::SpatRaster`.
#' @export
get_lidar <- function(
    x,
    key = c("mnt", "mns", "mnh"),
    buffer = 200,
    crs = 2154,
    cache = NULL,
    overwrite = FALSE,
    verbose = TRUE
) {

  key <- match.arg(key)

  x_clean <- x |>
    sf::st_transform(crs) |>
    sf::st_make_valid() |>
    sf::st_collection_extract("POLYGON") |>
    suppressWarnings()

  x_env <- seq_envelope(
    x = x_clean,
    buffer = buffer,
    crs = crs
  )

  x_clean <- x_env[!sf::st_is_empty(x_env), ]

  if (nrow(x_clean) == 0) {
    cli::cli_abort("{.arg x} has no valid polygon geometry after cleaning.")
  }

  files <- download_lidar(
    x = x_clean,
    key = key,
    cache = cache,
    overwrite = overwrite,
    verbose = verbose
  )

  vrt <- terra::vrt(
    files,
    options = c("-hidenodata")
  )

  if (verbose) {
    cli::cli_alert_info("Raster size optimization...")
  }

  x_clean <- sf::st_transform(x_clean, terra::crs(vrt))
  x_vect <- terra::vect(x_clean)

  r <- terra::crop(vrt, x_vect)
  r <- terra::mask(r, x_vect)

  target_crs <- terra::crs(sf::st_crs(crs)$wkt)
  if (!terra::same.crs(r, target_crs)) {
    r <- terra::project(r, target_crs)
  }

  names(r) <- paste0(key, "_lidar")

  return(invisible(r))
}

#' Create LiDAR layers for a Sequoia project
#'
#' Uses the project's _PARCA_ layer to download, extract, and write LiDAR HD
#' altimetry rasters.
#'
#' @inheritParams get_lidar
#' @inheritParams seq_write
#' @param key `character`; LiDAR product(s) to create. One or more of `"mnt"`,
#' `"mns"` and `"mnh"`.
#'
#' @return Invisibly returns a named `list` of created raster file paths.
#'
#' @export
seq_lidar <- function(
    dirname = ".",
    key = c("mnt", "mns", "mnh"),
    buffer = 200,
    crs = 2154,
    cache = NULL,
    overwrite = FALSE,
    verbose = TRUE
) {

  key <- match.arg(key, several.ok = TRUE)

  if (verbose) {
    cli::cli_h1("LIDAR")
  }

  parca <- seq_read("v.seq.parca.poly", dirname = dirname)

  identifier <- seq_field("identifier")$name
  id <- unique(parca[[identifier]])

  key_map <- c(
    mnt = "r.alt.mnt.lidar",
    mns = "r.alt.mns.lidar",
    mnh = "r.alt.mnh.lidar"
  )

  paths <- lapply(key, function(one_key) {

    seq_key <- key_map[[one_key]]

    meta <- seq_layer(seq_key, verbose = FALSE)

    path <- file.path(
      dirname,
      meta$path,
      sprintf("%s_%s.%s", id, meta$name, meta$ext)
    )

    path <- normalizePath(path, winslash = "/", mustWork = FALSE)

    if (file.exists(path) && !overwrite) {
      cli::cli_warn(c(
        "{.file {basename(path)}} already exists.",
        "i" = "Use {.arg overwrite = TRUE} to replace it."
      ))

      return(path)
    }

    r <- get_lidar(
      x = parca,
      key = one_key,
      buffer = buffer,
      crs = crs,
      cache = cache,
      overwrite = overwrite,
      verbose = verbose
    )

    seq_write(
      r,
      key = seq_key,
      id = id,
      dirname = dirname,
      overwrite = overwrite,
      verbose = verbose
    )
  })

  names(paths) <- key

  invisible(paths)
}
