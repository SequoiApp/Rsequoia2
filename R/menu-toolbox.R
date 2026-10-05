#' Open the toolbox menu
#'
#' Internal menu used to access standalone sequoia2 tools.
#'
#' @keywords internal
#' @noRd
menu_toolbox <- function() {
  old_path <- getOption("seq_dir_path", NULL)
  on.exit(options(seq_dir_path = old_path), add = TRUE)

  path <- rstudioapi::selectDirectory(
    caption = "S\u00E9lectionner un dossier de destination",
    path = getOption("seq_dir_path", getwd())
  )

  if (is.null(path) || !nzchar(path)) {
    return(invisible(NULL))
  }

  options(seq_dir_path = path)

  info <- cli::format_inline("Dossier s\u00E9lectionn\u00E9 : {.path {path}}")

  download_data <- function() {
    file <- rstudioapi::selectFile(
      caption = "S\u00E9lectionner une couche SIG",
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
    "T\u00E9l\u00E9charger PARCA depuis des IDU" = function() {
      cli::cli_alert_info("Fonctionnalit\u00E9 \u00E0 impl\u00E9menter.")
    },
    "T\u00E9l\u00E9charger des donn\u00E9es sur une zone" = download_data
  )

  seq_run_menu(
    actions = actions,
    title = "Bo\u00EEte \u00E0 outils",
    info = info,
    is_sub = TRUE
  )

}
