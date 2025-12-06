get_climate_fiche <- function(x, dirname = NULL, verbose = TRUE) {
  # 1. Prepare dirname folder (R_user_dir by default)
  if (is.null(dirname)) {
    dirname <- tools::R_user_dir("Rsequoia2", which = "data")
  }
  dir.create(dirname, recursive = TRUE, showWarnings = FALSE)

  # 2. Download station dataset (points)
  stations_url <- "https://www.data.gouv.fr/api/1/datasets/r/596f6898-3698-4aca-add4-49f38de03009"

  if (verbose) cli::cli_alert_info("Downloading meteorological station dataset...")
  stations <- sf::read_sf(stations_url) |> sf::st_transform(sf::st_crs(x))

  # 3. Nearest station to x
  pt <- sf::st_centroid(sf::st_union(x)) |> suppressWarnings()
  nearest_idx <- sf::st_nearest_feature(pt, stations)
  station <- stations[nearest_idx, ]

  dist <- units::set_units(sf::st_distance(pt, station), "km")
  if (verbose) {
    cli::cli_alert_success("Nearest station is at {round(dist, 0)} km ({station$nom}).")
  }

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

