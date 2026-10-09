#' Create selection state for a menu session
#'
#' @noRd
seq_menu_state <- function() {
  list2env(
    list(path = NULL, zone = NULL, zone_file = NULL, pdf_path = NULL),
    parent = emptyenv()
  )
}

#' Select a folder without replacing the current selection on cancellation
#'
#' @param state Menu selection state.
#' @param caption Folder picker caption.
#' @noRd
seq_select_folder <- function(state, caption) {
  path <- rstudioapi::selectDirectory(
    caption = caption,
    path = if (is.null(state$path)) getwd() else state$path
  )

  if (is.null(path) || !nzchar(path)) {
    return(invisible(NULL))
  }

  state$path <- path
  invisible(NULL)
}

#' Select and read a geographic zone for the menu session
#'
#' @param state Menu selection state.
#' @noRd
seq_select_zone <- function(state) {
  path <- if (!is.null(state$zone_file)) {
    dirname(state$zone_file)
  } else if (!is.null(state$path)) {
    state$path
  } else {
    getwd()
  }

  file <- rstudioapi::selectFile(
    caption = "S\u00E9lectionner une emprise",
    path = path,
    filter = "Couches SIG (*.gpkg *.shp *.geojson *.json *.kml)"
  )

  if (is.null(file) || !nzchar(file)) {
    return(invisible(NULL))
  }

  zone <- sf::read_sf(file)
  state$zone <- zone
  state$zone_file <- file
  invisible(NULL)
}

#' Display the status of a folder or zone selection
#'
#' @param value Selected path, or NULL if nothing is selected.
#' @param label Label for a selected path.
#' @param missing Message when nothing is selected.
#' @noRd
seq_show_selection <- function(
    value,
    label = "Dossier s\u00E9lectionn\u00E9",
    missing = "Aucun dossier s\u00E9lectionn\u00E9."
) {
  if (is.null(value) || !nzchar(value)) {
    cli::cli_alert_info("{missing}")
  } else {
    cli::cli_alert_success("{label} : {.path {value}}")
  }
  invisible(NULL)
}

#' Get the selected directory from menu state
#'
#' @param state Menu selection state.
#' @return A character path. Errors if no directory has been selected.
#' @noRd
seq_get_path <- function(state) {
  path <- state$path
  if (is.null(path) || !nzchar(path)) {
    cli::cli_abort("Veuillez d'abord s\u00E9lectionner un dossier dans le menu.")
  }
  path
}
