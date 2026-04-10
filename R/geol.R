#' Download BRGM "BD Charm 50" harmonised geology ZIP files
#'
#' Downloads BRGM ZIP archives for one french department. Files are stored
#' in a persistent cache directory (`R_user_dir("Rsequoia2", "cache")`).
#'
#' Existing ZIP files are never re-downloaded.
#'
#' @param dep `character` or `numeric`; Department code (see [Rsequoia2::get_cog()]).
#' @param key `character` Source use to download geology from BRGM. Must be one of:
#'   - `"carhab"` : geological data used to created the `CarHab` dataset ;
#'   - `"bdcharm50"` : geological maps, vectorized and harmonised at 1:50,000 scale.
#' @param cache `character`; Storage directory. Defaults to the user cache
#' directory (see [tools::R_user_dir()]).
#' @param verbose `logical` If `TRUE`, display messages.
#' @param overwrite `logical` If `TRUE`, overwrite zipfile.
#'
#' @return Character vector of directories where ZIPs are stored.
#'
#' @export
download_brgm <- function(dep, key = "carhab", cache = NULL, verbose = FALSE, overwrite = FALSE){

  if (length(dep) != 1) {
    cli::cli_abort(c(
      "x" = "{.arg dep} must contain exactly one element.",
      "i" = "You supplied {length(dep)}: {.val {dep}}"
    ))
  }

  if (length(key) != 1) {
    cli::cli_abort(c(
      "x" = "{.arg key} must contain exactly one element.",
      "i" = "You supplied {length(key)}: {.val {key}}"
    ))
  }

  if (!all(key %in% c("carhab", "bdcharm50"))) {
    cli::cli_abort(c(
      "x" = "{.arg key} is equal to {.val {format(key)}}.",
      "i" = "{.arg key} must be equal to {.val carhab} or {.val bdcharm50}."
    ))
  }

  dep <- pad_left(dep, 2)
  all_dep <- get_cog(verbose = FALSE)$dep
  if (length(dep)) {
    valid_dep <- dep %in% all_dep$DEP
    if (!all(valid_dep)) {
      cli::cli_abort(c(
        "x" = "Invalid department code: {.val {dep}}",
        "i" = "See {.run get_cog()$dep} for valid department code."
      ))
    }
  }

  if (is.null(cache)){
    cache <- tools::R_user_dir("Rsequoia2", which = "cache") |> file.path("geology")
    dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  }

  zip_name <- sprintf("GEO050K_HARM_%s.zip", pad_left(dep, 3))
  if (key == "carhab"){
    dep_name <- all_dep[all_dep$DEP == dep, c("DEP", "NCC_DEP")]
    dep_name <- gsub("\\s", "-", dep_name)
    zip_name <- sprintf("CARHAB_%s.zip", paste(dep_name, collapse = "_"))
  }

  zip_url <- paste0("http://infoterre.brgm.fr/telechargements/BDCharm50/", zip_name)
  zip_local <- file.path(cache, zip_name)

  is_download <- length(list.files(cache, pattern = zip_name, recursive = TRUE)) > 0
  if (!is_download || overwrite) {
    if (verbose){
      cli::cli_progress_step(
        "Downloading BRGM dataset for dep {.val {dep}}...",
        msg_done = "Downloaded"
      )
    }
    curl::curl_download(zip_url, destfile = zip_local, quiet = !verbose)
  }

  return(zip_local)
}

#' Download and read BRGM "BD Charm 50" harmonised geology layers
#'
#' Downloads and reads the geology layer (`S_FGEOL_*.shp`) from BRGM
#' "BD Charm 50" ZIP archives for one or several French
#' departments.
#'
#' Multiple departments are combined into a single `sf` object.
#'
#' @inheritParams download_brgm
#' @param deps `character` or `numeric`; One or sevral french
#' department code (see [Rsequoia2::get_cog()]).
#'
#' @return An `sf` object containing the geology features for all requested
#' departments.
#'
#' @details
#' For each department:
#' - The corresponding BRGM ZIP archive is obtained via [download_brgm()] ;
#' - The geology shapefile (`S_FGEOL_*.shp`) is imported ;
#' - The layer is read directly from the ZIP using `sf::read_sf("/vsizip/...")`
#' without extracting files ;
#'
#' The resulting layers are row-bound into a single `sf` object. All
#' geometries are returned in the CRS provided by BRGM (typically EPSG:2154).
#'
#' @return `sf`
#'
#' @export
get_brgm <- function(deps, key = "carhab", cache = NULL, verbose = FALSE, overwrite = FALSE){

  zip_path <- lapply(
    deps, download_brgm,
    key = key, cache = cache, verbose = verbose, overwrite = overwrite
  )

  geol <- lapply(zip_path, function(x){
    pattern <- ifelse(key == "carhab", "CarHab.*shp$", "S_FGEOL.*shp")
    name <- grep(pattern, archive::archive(x)$path, value = TRUE)
    geol <- sf::read_sf(file.path("/vsizip", x, name))
  })

  geol <- do.call(rbind, geol)

  return(invisible(geol))
}

#' Download BRGM geology data to a geometry
#'
#' Downloads BRGM geology layers for the departments intersecting `x`
#' (or for specified departments) and spatially filters them using a
#' buffered envelope around `x`.
#'
#' @param x `sf` or `sfc`; Geometry used to determine relevant departments
#' and to spatially filter geology data.
#' @param buffer `numeric`; Buffer distance (in meters) applied to `x`
#' before spatial filtering. Default is `100`.
#' @inheritParams get_brgm
#'
#' @details
#' The function:
#' - Identifies departments intersecting `x` (if `deps` is not provided) ;
#' - Downloads geology layers using [get_brgm()] ;
#' - Filters features intersecting a buffered envelope around `x`.
#'
#' All geometries are returned in EPSG:2154.
#'
#' @return An `sf` object containing clipped geology features.
#'
#' @export
get_geol <- function(
    x,
    key = "carhab",
    buffer = 100,
    deps = NULL,
    cache = NULL,
    verbose = FALSE,
    overwrite = FALSE){

  if (!inherits(x, c("sf", "sfc"))) {
    cli::cli_abort("{.arg x} must be of class {.cls sf} or {.cls sfc}.")
  }

  crs <- 2154
  x <- sf::st_transform(x, crs)
  if (is.null(deps)){
    deps <- happign::get_wfs(x, "BDCARTO_V5:departement", predicate = happign::intersects())
    deps <- unique(deps[["code_insee"]])
  }

  geol <- get_brgm(
    deps = deps,
    key = key,
    cache = cache,
    verbose = verbose,
    overwrite = overwrite
  )
  geol <- sf::st_transform(geol, crs)

  fetch_envelope <- seq_envelope(x = x, buffer = buffer, crs = crs)

  idx <- sf::st_intersects(geol, fetch_envelope)
  geol_intersects <- geol[lengths(idx) > 0, ]

  return(geol_intersects)
}

#' Create geology layers for a Sequoia project from BRGM data
#'
#' Uses the project's _PARCA_ layer to download geology datasets and clip
#' them to its geometry.
#'
#' Layers and their associated *QML style files* are written to the
#' project directory using [seq_write()].
#'
#' @inheritParams get_brgm
#' @inheritParams seq_write
#' @param key `character`. Optional geology layer identifier(s). If `NULL`
#' (default), all available geology layers are created. Available layers
#' are `"bdcharm50"` and `"carhab"`. Partial matching is supported
#' (see [seq_key()]).
#'
#' @details
#' **Difference between BRGM and CARHAB data**
#'
#' The BRGM 1:50000 geological maps are detailed, heterogeneous products
#' created for geological interpretation. They include many finely split
#' geological units and may show inconsistencies between neighbouring map
#' sheets due to varying survey dates, methods, and levels of detail.
#'
#' CARHAB data are a simplified, harmonised reinterpretation of these maps.
#' Geological formations are recoded into broader lithological classes tailored
#' for ecological modelling. The goal is to provide consistent, comparable,
#' and ecologically relevant information across departments.
#'
#'
#' More info at [infoterre](https://infoterre.brgm.fr/page/carhab-donnees-geologiques)
#'
#' @return An invisible named `list` of file paths to the created layers.
#'
#' @export
seq_geol <- function(
    dirname = ".",
    key = NULL,
    cache = NULL,
    verbose = TRUE,
    overwrite = FALSE
  ){

  if (verbose) cli::cli_h1("GEOLOGY")

  # KEY CHECK ----
  allowed <- c("v.sol.carhab.poly", "v.sol.bdcharm50.poly")
  if (is.null(key)) {
    key <- allowed
  } else {
    key <- lapply(key, seq_key, allow_multiple = TRUE) |> unlist()
    key <- intersect(key, allowed)

    if (length(key) == 0) {
      cli::cli_abort(c(
        "x" = "Invalid {.arg key}.",
        "i" = "Allowed geology keys are {.val {allowed}}."
      ))
    }
  }

  # BASE INFO ----
  parca <- seq_read("v.seq.parca.poly", dirname = dirname)
  identifier <- seq_field("identifier")$name
  id <- unique(parca[[identifier]])

  dep <- unique(parca[[seq_field("dep_code")$name]])

  outputs <- list()

  # CARHAB ----
  if ("v.sol.carhab.poly" %in% key) {

    carhab <- get_brgm(
      dep,
      key = "carhab",
      cache = cache,
      verbose = verbose,
      overwrite = FALSE
    )

    carhab <- carhab |>
      sf::st_transform(sf::st_crs(parca)) |>
      sf::st_intersection(parca) |>
      suppressWarnings()

    carhab[[identifier]] <- id

    path <- seq_write(
      carhab,
      "v.sol.carhab.poly",
      dirname = dirname,
      id = id,
      verbose = verbose,
      overwrite = overwrite
    )

    outputs <- c(outputs, path)
  }

  # BDCHARM50 ----
  if ("v.sol.bdcharm50.poly" %in% key) {

    bdcharm50 <- get_brgm(
      dep,
      key = "bdcharm50",
      cache = cache,
      verbose = verbose,
      overwrite = FALSE
    )

    bdcharm50 <- bdcharm50 |>
      sf::st_transform(sf::st_crs(parca)) |>
      sf::st_intersection(parca) |>
      suppressWarnings()

    bdcharm50[[identifier]] <- id

    path <- seq_write(
      bdcharm50,
      "v.sol.bdcharm50.poly",
      dirname = dirname,
      id = id,
      verbose = verbose,
      overwrite = overwrite
    )

    outputs <- c(outputs, path)

    # ---- QML handling ----
    cache_local <- cache
    if (is.null(cache_local)) {
      cache_local <- file.path(
        tools::R_user_dir("Rsequoia2", which = "cache"),
        "geology"
      )
      dir.create(cache_local, recursive = TRUE, showWarnings = FALSE)
    }

    zip_name <- sprintf("GEO050K_HARM_%s.zip", pad_left(dep[1], 3))
    zip_path <- file.path(cache_local, zip_name)

    qml_zip <- grep(
      "S_FGEOL.*qml",
      archive::archive(zip_path)$path,
      value = TRUE
    )

    archive::archive_extract(zip_path, dir = dirname, files = qml_zip)

    qml_path <- paste0(
      tools::file_path_sans_ext(path),
      ".qml"
    )

    file.rename(file.path(dirname, qml_zip), qml_path) |> invisible()
  }

  return(invisible(outputs))
}

