#' Open the toolbox menu
#'
#' Internal menu used to access standalone sequoia2 tools.
#'
#' @keywords internal
#' @noRd
menu_toolbox <- function() {
  path <- rstudioapi::selectDirectory(
    caption = "Selectionner un dossier de destination",
    path = getOption("seq_dir_path", getwd())
  )

  if (is.null(path) || !nzchar(path)) {
    return(invisible(NULL))
  }

  options(seq_dir_path = path)
  info <- cli::format_inline("Dossier selectionne : {.path {path}}")

  download_data <- function() {
    file <- rstudioapi::selectFile(
      caption = "Selectionner une couche SIG",
      path = path,
      filter = "Couches SIG (*.gpkg *.shp *.geojson *.json *.kml)"
    )

    if (is.null(file) || !nzchar(file)) {
      return(invisible(NULL))
    }

    zone <- sf::read_sf(file)
    menu_toolbox_data(zone)
  }

  actions <- list(
    "RP PDF -> Excel" = menu_rp,
    "Rechercher une personne morale" = menu_pm,
    "Télécharger PARCA depuis des IDU" = function() {
      cli::cli_alert_info("Fonctionnalité à implémenter.")
    },
    "Télécharger des données sur une zone" = download_data
  )

  seq_run_menu(
    actions = actions,
    title = "Boîte à outils",
    info = info,
    is_sub = TRUE
  )

}
