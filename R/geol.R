#' Download BRGM "BD Charm 50" harmonised geology ZIP files
#'
#' Downloads BRGM ZIP archives for one french department. Files are stored
#' in a persistent cache directory (`R_user_dir("Rsequoia2", "cache")`).
#'
#' Existing ZIP files are never re-downloaded.
#'
#' @param dep `character` or `numeric`; Department code (see [Rsequoia2::get_cog()]).
#' @param source `character` Source use to download geology from BRGM. Must be one of:
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
download_brgm <- function(
    dep,
    source = "carhab",
    cache = NULL,
    verbose = FALSE,
    overwrite = FALSE
){

  if (length(dep) != 1) {
    cli::cli_abort(c(
      "x" = "{.arg dep} must contain exactly one element.",
      "i" = "You supplied {length(dep)}: {.val {dep}}"
    ))
  }

  if (length(source) != 1) {
    cli::cli_abort(c(
      "x" = "{.arg source} must contain exactly one element.",
      "i" = "You supplied {length(source)}: {.val {source}}"
    ))
  }

  if (!all(source %in% c("carhab", "bdcharm50"))) {
    cli::cli_abort(c(
      "x" = "{.arg source} is equal to {.val {format(source)}}.",
      "i" = "{.arg source} must be equal to {.val carhab} or {.val bdcharm50}."
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
    cache <- tools::R_user_dir("Rsequoia2", which = "cache") |>
      file.path("geology")
    dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  }

  zip_name <- sprintf("GEO050K_HARM_%s.zip", pad_left(dep, 3))
  if (source == "carhab"){
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
get_brgm <- function(
    deps,
    source = "carhab",
    cache = NULL,
    verbose = FALSE,
    overwrite = FALSE
){

  zip_path <- lapply(
    deps, download_brgm,
    source = source, cache = cache, verbose = verbose, overwrite = overwrite
  )

  geol <- lapply(zip_path, function(x){
    pattern <- ifelse(source == "carhab", "CarHab.*shp$", "S_FGEOL.*shp")
    name <- grep(pattern, archive::archive(x)$path, value = TRUE)
    geol <- sf::read_sf(file.path("/vsizip", x, name))
  })

  geol <- do.call(rbind, geol)

  return(invisible(geol))
}

#' Create a geology layer for a Sequoia project from BRGM data
#'
#' Uses the _PARCA_ from Sequoia to determine which French departments are
#' involved, downloads the corresponding BRGM geology datasets, and
#' builds a single geology layer for the project.
#'
#' Created layer and its *QML style file* are automatically written to the
#' project directory using [seq_write()].
#'
#' @inheritParams get_brgm
#' @inheritParams seq_write
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
#' In short: **BRGM layers describe geology in detail; CARHAB layers provide a
#' simplified lithology better suited for habitat modelling.**
#'
#' More info at [infoterre](https://infoterre.brgm.fr/page/carhab-donnees-geologiques)
#'
#' @return An invisible `sf` object containing the merged geology layer.
#'
#' @keywords internal
#' @export
seq_geol <- function(dirname = ".", cache = NULL, verbose = TRUE, overwrite = FALSE){
  seq_write2 <- function(x, key, id) {
    seq_write(x, key, dirname = dirname, id = id, verbose = verbose, overwrite = overwrite)
  }

  parca <- seq_read("v.seq.parca.poly", dirname = dirname)
  identifier <- seq_field("identifier")$name
  id <- unique(parca[[identifier]])

  fetch_envelope <- envelope(parca, 5000)

  if (verbose){
    cli::cli_h1("GEOLOGY")
  }

  dep <- unique(parca[[seq_field("dep_code")$name]])

  # CARHAB ----
  carhab <- get_brgm(dep, source = "carhab", cache = cache, verbose = verbose, overwrite = FALSE)
  carhab_mask <- carhab[sf::st_intersects(carhab, fetch_envelope, sparse = FALSE), ]
  carhab_mask[[identifier]] <- id
  carhab_path <- seq_write2(carhab_mask, "v.sol.carhab.poly", id)

  # BDCHARM50 ----
  bdcharm50 <- get_brgm(dep, source = "bdcharm50", cache = cache, verbose = verbose, overwrite = FALSE)
  bdcharm50_mask <- bdcharm50[sf::st_intersects(bdcharm50, fetch_envelope, sparse = FALSE), ]
  bdcharm50_mask[[identifier]] <- id
  bdcharm50_path <- seq_write2(bdcharm50_mask, "v.sol.bdcharm50.poly", id)

  # BDCHARM50 QML ----
  if (is.null(cache)){
    cache <- tools::R_user_dir("Rsequoia2", which = "cache") |>
      file.path("geology")
    dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  }

  zip_name <- sprintf("GEO050K_HARM_%s.zip", pad_left(dep[1], 3))
  zip_path <- file.path(cache, zip_name)
  qml_zip <- grep("S_FGEOL.*qml", archive::archive(zip_path[[1]])$path, value = TRUE)
  archive::archive_extract(zip_path, dir = dirname, files = qml_zip)

  qml_path <- paste0(tools::file_path_sans_ext(bdcharm50_path), ".qml")
  file.rename(file.path(dirname, qml_zip), qml_path) |> invisible()

  return(invisible(c(carhab_path, bdcharm50_path) |> as.list()))
}

