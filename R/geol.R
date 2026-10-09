#' Read BRGM geology data for an area
#'
#' Downloads and reads BRGM geology layers for the departments intersecting
#' `x`, then keeps only features intersecting a buffered envelope around `x`.
#'
#' Supported datasets are `"carhab"` and `"bdcharm50"`.
#'
#' @param x `sf` or `sfc`; Area used to determine departments and filter geology.
#' @param key `character`; Dataset to use. One of `"carhab"` or `"bdcharm50"`.
#' @param buffer `numeric`; Buffer distance, in meters, applied around `x`
#' before spatial filtering. Default is `100`.
#' @param cache `character`; Optional cache directory. If `NULL`, the
#' dataset-specific cache from [Rsequoia2::seq_cache()] is used.
#' @param verbose `logical`; If `TRUE`, display progress and informational messages.
#' @param overwrite `logical`; If `TRUE`, re-download archives even when
#' they already exist in `cache`.
#'
#' @return An `sf` object containing geology features intersecting the buffered
#'   envelope of `x`, returned in EPSG:2154.
#'
#' @export
get_geol <- function(
    x,
    key = c("carhab", "bdcharm50"),
    buffer = 100,
    cache = NULL,
    verbose = FALSE,
    overwrite = FALSE
) {
  key <- match.arg(key)

  if (!inherits(x, c("sf", "sfc"))) {
    cli::cli_abort("{.arg x} must be of class {.cls sf} or {.cls sfc}.")
  }

  crs <- 2154
  x <- sf::st_transform(x, crs)

  dep <- happign::get_wfs(
    x,
    layer = "BDCARTO_V5:departement",
    predicate = happign::intersects()
  )

  dep <- unique(dep[["code_insee"]])
  dep <- check_dep(dep)

  if (is.null(cache)) {
    cache <- seq_cache(key)$path
  }

  zip_path <- switch(
    key,
    carhab = download_carhab(
      dep = dep,
      cache = cache,
      verbose = verbose,
      overwrite = overwrite
    ),
    bdcharm50 = download_bdcharm50(
      dep = dep,
      cache = cache,
      verbose = verbose,
      overwrite = overwrite
    )
  )

  shp_pattern <- switch(
    key,
    carhab = "CarHab.*\\.shp$",
    bdcharm50 = "S_FGEOL.*\\.shp$"
  )

  geol <- lapply(zip_path, function(zip) {
    shp <- grep(
      shp_pattern,
      archive::archive(zip)$path,
      value = TRUE,
      ignore.case = TRUE
    )

    if (length(shp) == 0) {
      cli::cli_abort(c(
        "No geology shapefile found in archive.",
        "x" = "Archive: {.file {basename(zip)}}",
        "i" = "Expected pattern: {.val {shp_pattern}}"
      ))
    }

    if (length(shp) > 1) {
      cli::cli_abort(c(
        "Several geology shapefiles found in archive.",
        "x" = "Archive: {.file {basename(zip)}}",
        "i" = "Matches: {.vals {shp}}",
        "i" = "Please make the shapefile matching rule stricter."
      ))
    }

    sf::read_sf(file.path("/vsizip", zip, shp))
  })

  geol <- do.call(rbind, geol)
  geol <- sf::st_transform(geol, crs)
  idx <- sf::st_intersects(geol, x)

  geol <- geol[lengths(idx) > 0, ]

  return(geol)
}

#' Clip raw geology data to an area
#'
#' @param x `sf` or `sfc`; Area used to clip the geology data.
#' @param geol `sf`; Raw geology data.
#'
#' @return An `sf` object clipped to `x`.
#'
#' @keywords internal
#' @noRd
.geol_transformer <- function(x, geol) {

  if (is.null(geol) || !nrow(geol)) {
    return(NULL)
  }

  suppressWarnings(
    geol |>
      sf::st_transform(sf::st_crs(x)) |>
      sf::st_intersection(x |> sf::st_geometry() |> sf::st_union()) |>
      sf::st_collection_extract("POLYGON") |>
      sf::st_cast("MULTIPOLYGON") |>
      sf::st_cast("POLYGON")
  )

  if (!nrow(geol)) {
    return(NULL)
  }

  geol
}

#' Extract the BD Charm 50 QML style
#'
#' @param x `sf` or `sfc`; Area used to identify the relevant department.
#' @param layer_path `character`; Path of the BD Charm 50 layer. The QML file
#'   is written next to it with the same basename.
#' @param cache `character`; Optional BD Charm 50 cache directory.
#' @param verbose `logical`; If `TRUE`, display informational messages.
#' @param overwrite `logical`; If `TRUE`, overwrite an existing QML file.
#'
#' @return Invisibly returns the QML path, or `NULL` when no unique style is
#'   found.
#'
#' @keywords internal
#' @noRd
.extract_geol_qml <- function(
    x,
    layer_path,
    cache = NULL,
    verbose = TRUE,
    overwrite = FALSE) {

  qml_path <- paste0(tools::file_path_sans_ext(layer_path), ".qml")

  if (file.exists(qml_path) && !overwrite) {
    return(invisible(qml_path))
  }

  dep <- happign::get_wfs(
    sf::st_transform(x, 2154),
    layer = "BDCARTO_V5:departement",
    predicate = happign::intersects()
  )

  dep <- check_dep(unique(dep[["code_insee"]]))

  if (is.null(cache)) {
    cache <- seq_cache("bdcharm50")$path
  }

  zip_path <- download_bdcharm50(
    dep = dep,
    cache = cache,
    verbose = FALSE,
    overwrite = FALSE
  )

  qml_zip <- grep(
    "S_FGEOL.*\\.qml$",
    archive::archive(zip_path[[1]])$path,
    value = TRUE,
    ignore.case = TRUE
  )

  if (length(qml_zip) != 1) {
    if (verbose) {
      cli::cli_warn("Could not find a unique BD Charm 50 QML style in archive.")
    }

    return(invisible(NULL))
  }

  archive::archive_extract(
    zip_path[[1]],
    dir = dirname(layer_path),
    files = qml_zip
  )

  extracted_path <- file.path(dirname(layer_path), qml_zip)

  if (file.exists(qml_path) && overwrite) {
    file.remove(qml_path)
  }

  file.rename(extracted_path, qml_path) |> invisible()

  return(invisible(qml_path))
}

#' Fetch geology layers
#'
#' @inheritParams get_geol
#' @param dirname `character`; Output directory.
#' @param id Optional Sequoia project identifier.
#'
#' @return Invisibly returns a named list of written file paths.
#'
#' @keywords internal
#' @noRd
.geol_fetcher <- function(
    x,
    dirname,
    id = NULL,
    key = c("carhab", "bdcharm50"),
    buffer = 100,
    cache = NULL,
    verbose = TRUE,
    overwrite = FALSE) {

  allowed <- c("carhab", "bdcharm50")
  key <- tryCatch(
    match.arg(key, allowed, several.ok = TRUE),
    error = function(e) {
      cli::cli_abort("Invalid {.arg key}. Allowed values are: {.vals {allowed}}.")
    }
  )

  if (verbose) {
    cli::cli_h1("GEOLOGY")
  }

  outputs <- list()

  for (k in key) {

    geol <- get_geol(
      x = x,
      key = k,
      buffer = buffer,
      cache = cache,
      verbose = verbose,
      overwrite = FALSE
    )

    geol <- .geol_transformer(x, geol)
    if (is.null(geol)) {
      if (verbose) {
        cli::cli_alert_info("No geology layer found.")
      }
      return(invisible(NULL))
    }

    if (!is.null(id)) {
      identifier <- seq_field("identifier")$name
      geol[[identifier]] <- id
    }

    path <- seq_write(
      geol,
      key = k,
      dirname = dirname,
      id = id,
      verbose = verbose,
      overwrite = overwrite
    )

    outputs[[k]] <- path

    if (identical(k, "bdcharm50")) {
      .extract_geol_qml(
        x = x,
        layer_path = path,
        cache = cache,
        verbose = verbose,
        overwrite = overwrite
      )
    }
  }

  invisible(outputs)
}


#' Fetch geology layers for an area
#'
#' Downloads the requested BRGM geology layers for `x` and writes them to
#' `dirname`.
#'
#' @inheritParams get_geol
#' @param dirname `character`; Output directory.
#' @param key `character`; Geology layer identifier(s).
#'
#' @return Invisibly returns a named list of written file paths.
#'
#' @keywords internal
#' @noRd
fetch_geol <- function(
    x,
    dirname,
    key = c("carhab", "bdcharm50"),
    buffer = 100,
    cache = NULL,
    verbose = TRUE,
    overwrite = FALSE) {

  .geol_fetcher(
    x = x,
    dirname = dirname,
    id = NULL,
    key = key,
    buffer = buffer,
    cache = cache,
    verbose = verbose,
    overwrite = overwrite
  )
}


#' Create geology layers for a Sequoia project
#'
#' Uses the project PARCA layer as the area of interest, downloads the
#' requested BRGM geology layers, and writes them to the project directory.
#'
#' @inheritParams seq_write
#' @inheritParams get_geol
#'
#' @param key `character`; Geology layer identifier(s).
#' @return Invisibly returns a named list of written file paths.
#'
#' @export
seq_geol <- function(
    dirname = ".",
    key = c("carhab", "bdcharm50"),
    cache = NULL,
    buffer = 100,
    verbose = TRUE,
    overwrite = FALSE) {

  ctx <- .seq_context(dirname)

  .geol_fetcher(
    x = ctx$parca,
    dirname = dirname,
    id = ctx$id,
    key = key,
    buffer = buffer,
    cache = cache,
    verbose = verbose,
    overwrite = overwrite
  )
}
