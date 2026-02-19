
#' Get nearest Meteo-France stations
#'
#' Finds the nearest meteorological stations from Meteo-France to the provided
#' spatial geometry.
#'
#' @param x `sf` or `sfc`.
#' @param n `integer`; number of nearest stations to return. Default is 1.
#' @param verbose `logical` If `TRUE`, display progress messages.
#'
#' @return An `sf` object containing the nearest meteorological stations.
#'
#' @export
mf_get_nearest_station <- function(x, n = 1, verbose = TRUE){
  stations_url <- "https://www.data.gouv.fr/api/1/datasets/r/596f6898-3698-4aca-add4-49f38de03009"

  if (verbose) cli::cli_alert_info("Downloading meteorological station dataset...")
  stations <- sf::read_sf(stations_url) |> sf::st_transform(sf::st_crs(x))

  pt <- sf::st_centroid(sf::st_union(x)) |> suppressWarnings()
  dist <- units::set_units(sf::st_distance(pt, stations), "km")

  nearest_idx <- order(as.numeric(dist))[1:n]
  station <- stations[nearest_idx, ]
  dist_n <- round(as.numeric(dist[nearest_idx]), 1)  # numeric km

  if (verbose) {
    cli::cli_alert_success(
      "Selected {n} nearest stations: {paste0(station$nom, ' (', dist_n, ' km)', collapse = ', ')}"
    )
  }

  return(invisible(station))
}

#' Download climatological station fiche
#'
#' Downloads the climatological fiche (PDF) of the nearest Meteo-France station
#' to the provided geometry.
#'
#' The fiche contains station metadata and long-term climatological summaries.
#'
#' @param x `sf` or `sfc`
#' @param dirname `character`; directory where the PDF will be saved. Defaults to
#' [tools::R_user_dir()]
#' @param verbose `logical` If `TRUE`, display progress messages.
#'
#' @return Path to the downloaded PDF
#'
#' @examples
#' \dontrun{
#' mf_get_climate_fiche(parca, dirname = "METEO")
#' }
#'
#' @export
mf_get_climate_fiche <- function(x, dirname = NULL, verbose = TRUE) {
  # 1. Prepare dirname folder (R_user_dir by default)
  if (is.null(dirname)) {
    dirname <- tools::R_user_dir("Rsequoia2", which = "data")
  }
  dir.create(dirname, recursive = TRUE, showWarnings = FALSE)

  station <- mf_get_nearest_station(x, verbose = verbose)

  # 4. Build output file path
  pdf_url <- station$chemin
  dirname <- file.path(dirname, basename(pdf_url))

  # 5. Download PDF
  if (verbose) cli::cli_alert_info("Downloading climate fiche PDF to {.path {dirname}}")
  curl::curl_download(pdf_url, dirname)

  # 6. Return useful information
  return(dirname |> setNames("fiche.meteo") |> invisible())
}

#' Get Météo-France Dataset Metadata
#'
#' Downloads and parses the metadata describing variables from the
#' Météo-France monthly climate dataset ("MENSQ").
#'
#' The function retrieves the `descriptif_champs` resource from the
#' corresponding data.gouv.fr dataset and returns a structured table
#' containing variable names and their descriptions.
#'
#' @return A `data.frame` with two columns
#'
#' @examples
#' \dontrun{
#' meta <- mf_get_metadata()
#' head(meta)
#' }
#'
#' @export
mf_get_metadata <- function(){

  all_resource <- dg_get_dataset("6569b3d7d193b4daf2b43edc")$resource
  url <- all_resource$url[grepl("descriptif_champs", all_resource$title)][[1]]

  txt <- readLines(url, encoding = "UTF-8", warn = FALSE)
  stop_line <- grep("^NBJBROU", txt)
  txt_clean <- txt[seq_len(stop_line)]

  mf_metadata <- do.call(rbind, strsplit(txt_clean, " : ", fixed = TRUE)) |>
    as.data.frame(stringsAsFactors = FALSE) |>
    setNames(c("variable", "description"))

  return(mf_metadata)

}

#' Download monthly climatology from Meteo-France
#'
#' Downloads monthly climatological data for the *3 nearest meteorological stations*.
#'
#' @param x `sf` or `sfc`.
#' @param cache `character`; Storage directory. Defaults to the user cache
#' directory (see [tools::R_user_dir()])
#' @param verbose `logical` If `TRUE`, display progress messages.
#'
#' @return
#' A `data.frame` containing monthly climatology records for the nearest stations.
#'
#' @details
#' Cached files are reused to avoid redundant downloads.
#'
#' @examples
#' \dontrun{
#' clim <- mf_get_climatology(parca)
#' }
#'
#' @export
mf_get_climatology <- function(x, cache = NULL, verbose = TRUE){

  if (!inherits(x, c("sf", "sfc"))) {
    cli::cli_abort("{.arg x} must be {.cls sf} or {.cls sfc}, not {.cls {class(x)}}.")
  }

  nearest <- mf_get_nearest_station(x, n = 3, verbose = verbose)
  dep <- substr(nearest$num, 1, 2) |> unique()

  if (is.null(cache)){
    cache <- tools::R_user_dir("Rsequoia2", which = "cache") |>
      file.path("meteo_france")
    dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  }

  all_resource <- dg_get_dataset("6569b3d7d193b4daf2b43edc")$resource
  dep <- pad_left(dep, 2)
  pattern <- sprintf("MENS_departement_%s_periode", dep) |>
    paste(collapse = "|")

  resource <- all_resource[grepl(pattern, all_resource$title),]
  if (nrow(resource) == 0) {
    cli::cli_abort("No climatology resource found for department {dep}")
  }

  climatology <- lapply(resource$url, function(url) {
    filename <- basename(url)
    cache_file <- file.path(cache, filename)
    already_download <- file.exists(cache_file)
    if (already_download){
      if (verbose) {cli::cli_alert_info("Reading from cache: {.path {filename}}")}
      clim <- read.csv2(cache_file)
    }else{
      if (verbose) {cli::cli_alert_info("Downloading {.path {filename}}")}
      curl::curl_download(url, cache_file)
      clim <- read.csv2(cache_file)
    }

    periode <- regmatches(filename, regexpr("\\d{4}-\\d{4}", filename))
    cbind(PERIODE = periode, clim)

  })

  climatology <- do.call(rbind, climatology)
  climatology <- climatology[climatology$NUM_POSTE %in% as.numeric(nearest$num), ]

  return(climatology)

}

#' Compute ombrothermic climatology summaries
#'
#' Computes monthly ombrothermic summaries from raw climatology data over
#' specified rolling periods.
#'
#' @param clim `data.frame` Raw climatology data from [mf_get_climatology()]
#' @param periods `integer` Vector of periods (in years) used to compute averages.
#' Default is `c(30, 5)`.
#'
#' @return
#' A `data.frame` containing monthly averages of temperature and precipitation
#' for each period.
#'
#' @examples
#' \dontrun{
#' ombro <- mf_ombro(clim)
#' }
#'
#' @export
mf_ombro <- function(clim, periods = c(30, 5)){

  # p: precipitation
  # nei: neige
  # nbj: nombre de jour
  year_month <- "AAAAMM"
  year <- "year"
  month <- "month"
  tmoy <- "TM"
  tmin <- "TN"
  tmax <- "TX"
  p_mm <- "RR"

  clim[[year]] <- substr(clim[[year_month]], 1, 4) |> as.integer()
  clim[[month]] <- substr(clim[[year_month]], 5, 6) |> as.numeric()

  cols <- c(tmoy, tmin, tmax, p_mm)
  clim[cols] <- clim[cols] |> lapply(as.numeric)

  current <- Sys.Date() |> format("%Y") |> as.numeric() - 1

  ombro <- lapply(periods, function(x){
    clim_subset <- clim[clim[[year]] >= current - x, ]
    ombro <- aggregate(
      clim_subset[, c(tmoy, tmin, tmax, p_mm)],
      by = list(MONTH = clim_subset[[month]]),
      FUN = mean,
      na.rm = TRUE
    )
    ombro$PERIODE <- sprintf("%d-%d", current - x, current)
    ombro <- ombro[, c("PERIODE", "MONTH", tmoy, tmin, tmax, p_mm)]

  })

  ombro <- do.call(rbind, ombro)

  return(ombro)
}

#' Compute annual precipitation summaries
#'
#' Computes annual precipitation averages from raw climatology data.
#'
#' @param clim `data.frame` Raw climatology data from [mf_get_climatology()].
#'
#' @return
#' A `data.frame` containing annual precipitation averages.
#'
#' @details
#' Precipitation values are computed from monthly records and aggregated by year.
#'
#' @examples
#' \dontrun{
#' precip <- mf_precipitation(clim)
#' }
#'
#' @export
mf_precipitation <- function(clim){

  # p: precipitation
  # nei: neige
  # nbj: nombre de jour
  year_month <- "AAAAMM"
  year <- "year"
  month <- "month"
  p_mm <- "RR"

  clim[[year]] <- substr(clim[[year_month]], 1, 4) |> as.integer()
  clim[[month]] <- substr(clim[[year_month]], 5, 6) |> as.integer()
  clim[[p_mm]] <- clim[[p_mm]] |> as.numeric()

  current <- Sys.Date() |> format("%Y") |> as.numeric() - 1
  clim_subset <- clim[clim[[year]] <= current, ]

  precipitation <- aggregate(
    clim_subset[p_mm],
    by = list(YEAR = clim_subset[[year]]),
    FUN = mean,
    na.rm = TRUE
  )

  return(precipitation)
}

#' Download Meteo-France meteorological data for a sequoia process
#'
#' Downloads and formats meteorological data from Meteo-France.
#' This includes download of the nearest climatological station fiche (PDF),
#' ombrothermic summaries, precipitation statistics. Results are written
#' to an Excel workbook based on the internal template `CLIMAT_MF.xlsx`.
#'
#' @inheritParams seq_write
#' @inheritParams mf_get_climatology
#'
#' @return
#' A named `list` of file paths :
#' - `"fiche.meteo"``: Path to the downloaded climatological station PDF
#' - `"meteofrance"``: Path to the generated Excel workbook
#'
#' @export
seq_meteo_france <- function(
    dirname = ".",
    cache = NULL,
    verbose = TRUE,
    overwrite = FALSE
    ){

  parca <- seq_read("v.seq.parca.poly", dirname = dirname)
  identifier <- seq_field("identifier")$name
  id <- unique(parca[[identifier]])

  if (verbose){
    cli::cli_h1("METEO FRANCE")
  }

  path <- list()

  meteo_dir <- file.path(dirname, "4_METEO")
  dir.create(meteo_dir, showWarnings = FALSE, recursive = TRUE)
  filepath <- file.path(meteo_dir, sprintf("%s_CLIMAT_MF.xlsx", id))

  # Fiche climatologique
  pdf_path <- mf_get_climate_fiche(parca, dirname = meteo_dir, verbose = verbose)
  path <- c(path, pdf_path)

  # Meteo france
  raw_clim <- mf_get_climatology(parca, cache = cache, verbose = verbose)
  ombro <- mf_ombro(raw_clim, periods = c(30, 5))
  precipitation <- mf_precipitation(raw_clim)

  # Metadat
  mf_metadata <- mf_get_metadata()

  wb <- openxlsx2::wb_load(system.file("xlsx/CLIMAT_MF.xlsx", package = "Rsequoia2"))

  if (verbose) {cli_alert_info("Writing Meteo-France data to: {.path {filepath}}")}

  wb <- openxlsx2::wb_clean_sheet(wb, sheet = 1, styles = FALSE) |>
    openxlsx2::wb_add_data(sheet = 1, mf_metadata)
  wb <- openxlsx2::wb_clean_sheet(wb, sheet = 2, styles = FALSE) |>
    openxlsx2::wb_add_data(sheet = 2, raw_clim)
  wb <- openxlsx2::wb_clean_sheet(wb, sheet = 3, dims = "A1:F30", styles = FALSE) |>
    openxlsx2::wb_add_data(sheet = 3, ombro)
  wb <- openxlsx2::wb_clean_sheet(wb, sheet = 4, dims = "A1:B2000", styles = FALSE) |>
    openxlsx2::wb_add_data(sheet = 4, precipitation)

  openxlsx2::wb_save(wb, file = filepath,  overwrite = overwrite)
  path <- c(path, filepath |> setNames("meteofrance"))

  return(invisible(path))
}
