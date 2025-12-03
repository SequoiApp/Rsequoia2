#' Download BRGM "BD Charm 50" harmonised geology ZIP files
#'
#' Downloads BRGM ZIP archives for one french department. Files are stored
#' in a persistent cache directory (`R_user_dir("Rsequoia2", "cache")`).
#'
#' Existing ZIP files are never re-downloaded.
#'
#' @param dep `character` or `numeric`; Department code (see [happign::dep_2025]).
#' @param cache `character`; Storage directory. Defaults to the user cache
#' directory (see [tools::R_user_dir()]).
#' @param verbose `logical` If `TRUE`, display messages.
#' @param overwrite `logical` If `TRUE`, overwrite zipfile.
#'
#' @return Character vector of directories where ZIPs are stored.
#'
#' @export
download_brgm <- function(dep, cache = NULL, verbose = FALSE, overwrite = FALSE){

  if (length(dep) != 1) {
    cli::cli_abort(c(
      "x" = "{.arg dep} must contain exactly one element.",
      "i" = "You supplied {length(dep)}."
    ))
  }

  dep <- pad_left(dep, 2)
  if (length(dep)) {
    all_dep <- happign::dep_2025$DEP
    valid_dep <- dep %in% all_dep
    if (!all(valid_dep)) {
      cli::cli_abort(c(
        "x" = "Invalid department code: {.val {dep}}",
        "i" = "See {.run happign::dep_2025$DEP} for valid department code."
      ))
    }
  }

  if (is.null(cache)){
    cache <- tools::R_user_dir("Rsequoia2", which = "cache")
    dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  }

  zip_name <- sprintf("GEO050K_HARM_%s.zip", pad_left(dep, 3))
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
#' department code (see [happign::dep_2025]).
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
get_brgm <- function(deps, cache = NULL, verbose = FALSE, overwrite = FALSE){

  zip_path <- lapply(deps, download_brgm, cache = cache, verbose = verbose)

  brgm <- lapply(zip_path, function(x){
    name <- grep("S_FGEOL.*shp", archive::archive(x)$path, value = TRUE)
    sf::read_sf(file.path("/vsizip", x, name))
  })

  brgm <- do.call(rbind, brgm)

  return(invisible(brgm))
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
#' @return An invisible `sf` object containing the merged geology layer.
#'
#' @keywords internal
#' @export
seq_brgm <- function(dirname = ".", cache = NULL, verbose = FALSE, overwrite = FALSE){
  parca <- seq_read("v.seq.parca.poly", dirname = dirname, verbose = FALSE)

  dep <- unique(parca[[seq_field("dep_num")$name]])
  zip_path <- lapply(dep, download_brgm, cache = cache, verbose = verbose)

  brgm <- lapply(zip_path, function(x){
    name <- grep("S_FGEOL.*shp", archive::archive(x)$path, value = TRUE)
    sf::read_sf(file.path("/vsizip", x, name)) |> sf::st_transform(sf::st_crs(parca))
  })

  brgm <- do.call(rbind, brgm)
  parca_buff <- sf::st_buffer(sf::st_as_sfc(sf::st_bbox(parca)), 5000)

  brgm_mask <- brgm[sf::st_intersects(brgm, parca_buff, sparse = FALSE), ]
  brgm_path <- seq_write(brgm_mask, "v.sol.geol.poly", dirname = dirname, verbose = verbose, overwrite = overwrite)

  qml_zip <- grep("S_FGEOL.*qml", archive::archive(zip_path[[1]])$path, value = TRUE)
  archive::archive_extract(zip_path[[1]], dir = dirname, files = qml_zip)

  qml_path <- paste0(tools::file_path_sans_ext(brgm_path), ".qml")
  file.rename(file.path(dirname, qml_zip), qml_path) |> invisible()

  return(invisible(brgm))
}

