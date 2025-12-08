
get_nearest_station <- function(x, n = 1, verbose = TRUE){
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

  return(station)
}

get_climate_fiche <- function(x, dirname = NULL, verbose = TRUE) {
  # 1. Prepare dirname folder (R_user_dir by default)
  if (is.null(dirname)) {
    dirname <- tools::R_user_dir("Rsequoia2", which = "data")
  }
  dir.create(dirname, recursive = TRUE, showWarnings = FALSE)

  station <- get_nearest_station(x, verbose = TRUE)

  # 4. Build output file path
  pdf_url <- station$chemin
  dirname <- file.path(dirname, basename(pdf_url))

  # 5. Download PDF
  curl::curl_download(pdf_url, dirname, quiet = )
  if (verbose) cli::cli_alert_info("Downloading climate fiche PDF to {.path {dirname}}")

  # 6. Return useful information
  list(
    pdf = dirname,
    station = station
  )
}

get_climatology <- function(dep, cache = NULL){

  if (is.null(cache)){
    cache <- tools::R_user_dir("Rsequoia2", which = "cache")
  }

  all_resource <- dg_get_dataset("6569b3d7d193b4daf2b43edc")$resource
  dep <- pad_left(dep, 2)
  pattern <- sprintf("MENS_departement_%s_periode", dep)

  resource <- all_resource[grepl(pattern, all_resource$title),]

  climatology <- lapply(resource$url, function(url){
    filename <- basename(url)
    res <- curl::curl_download(url, file.path(cache, filename))
    read.csv2(res)
  })
  climatology <- do.call(rbind, climatology)

  return(climatology)

}
