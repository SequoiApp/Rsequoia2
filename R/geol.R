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
#' @param verbose `logical`; If `TRUE`, display progress messages.
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

#' Create geology layers for a Sequoia project from BRGM data
#'
#' Uses the project's _PARCA_ layer as the area of interest, downloads the
#' requested BRGM geology dataset(s), clips them to the parcel geometry, and
#' writes the resulting layers to the project directory with [seq_write()].
#'
#' @inheritParams seq_write
#' @inheritParams get_geol
#' @param key `character`. Optional geology layer identifier(s). If `NULL`,
#'   all available geology layers are created. Available layers are
#'   `"v.sol.carhab.poly"` and `"v.sol.bdcharm50.poly"`. Partial matching is
#'   supported through [seq_key()].
#'
#' @details
#' **BD Charm 50** contains harmonised 1:50,000 geological map data from BRGM.
#' It is detailed and may contain finely split geological units.
#'
#' **CarHab** is a simplified and harmonised reinterpretation of geological
#' formations into broader lithological classes, designed for ecological
#' modelling.
#'
#' More info: <https://infoterre.brgm.fr/page/carhab-donnees-geologiques>
#'
#' @return Invisibly returns a named `list` of file paths to the created layers.
#'
#' @export
seq_geol <- function(
    dirname = ".",
    key = NULL,
    cache = NULL,
    buffer = 100,
    verbose = TRUE,
    overwrite = FALSE
) {

  if (verbose) cli::cli_h1("GEOLOGY")

  # KEY CHECK ----
  allowed <- c("v.sol.carhab.poly", "v.sol.bdcharm50.poly")

  if (is.null(key)) {
    key <- allowed
  } else {
    key <- lapply(key, seq_key, allow_multiple = TRUE) |>
      unlist(use.names = FALSE)

    key <- intersect(key, allowed)

    if (length(key) == 0) {
      cli::cli_abort(c(
        "Invalid {.arg key}.",
        "x" = "No valid geology layer selected.",
        "i" = "Allowed geology keys are: {.vals {allowed}}."
      ))
    }
  }

  # BASE INFO ----
  parca <- seq_read("v.seq.parca.poly", dirname = dirname)

  identifier <- seq_field("identifier")$name
  id <- unique(parca[[identifier]])

  outputs <- list()

  # CREATE LAYERS ----
  for (layer_key in key) {

    geol_key <- switch(
      layer_key,
      "v.sol.carhab.poly" = "carhab",
      "v.sol.bdcharm50.poly" = "bdcharm50"
    )

    if (verbose) {
      cli::cli_h2("{geol_key}")
    }

    geol <- get_geol(
      x = parca,
      key = geol_key,
      buffer = buffer,
      cache = cache,
      verbose = verbose,
      overwrite = FALSE
    )

    geol <- geol |>
      sf::st_transform(sf::st_crs(parca)) |>
      sf::st_intersection(parca) |>
      suppressWarnings()

    geol[[identifier]] <- id

    path <- seq_write(
      geol,
      layer_key,
      dirname = dirname,
      id = id,
      verbose = verbose,
      overwrite = overwrite
    )

    outputs[[layer_key]] <- path

    # QML handling for BD Charm 50 ----
    if (identical(layer_key, "v.sol.bdcharm50.poly")) {

      dep <- unique(parca[[seq_field("dep_code")$name]])

      qml_cache <- cache
      if (is.null(qml_cache)) {
        qml_cache <- seq_cache("bdcharm50")$path
      }

      zip_path <- download_bdcharm50(
        dep = dep,
        cache = qml_cache,
        verbose = FALSE,
        overwrite = FALSE
      )

      qml_zip <- grep(
        "S_FGEOL.*\\.qml$",
        archive::archive(zip_path[[1]])$path,
        value = TRUE,
        ignore.case = TRUE
      )

      if (length(qml_zip) == 1) {
        archive::archive_extract(
          zip_path[[1]],
          dir = dirname,
          files = qml_zip
        )

        qml_path <- paste0(
          tools::file_path_sans_ext(path),
          ".qml"
        )

        file.rename(file.path(dirname, qml_zip), qml_path) |>
          invisible()
      } else if (verbose) {
        cli::cli_warn(
          "Could not find a unique BD Charm 50 QML style in archive."
        )
      }
    }
  }

  return(invisible(outputs))
}
