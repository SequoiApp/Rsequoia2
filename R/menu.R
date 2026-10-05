# Main menu ----

#' Launch the main sequoia2 menu
#'
#' Opens the interactive command-line menu for the sequoia2 workflow.
#'
#' The selected directory is stored in the R option `seq_dir_path` and reused by
#' the other menus and processing functions.
#'
#' @export
sequoia2 <- function() {

  ask_help <- function() {
    utils::browseURL("https://github.com/SequoiApp/Rsequoia2/issues")
  }

  website <- function() {
    utils::browseURL("https://sequoiapp.github.io/Rsequoia2/index.html")
  }

  actions <- list(
    "Projet Sequoia" = menu_sequoia,
    "Boite \u00E0 outils" = menu_toolbox,
    "Gestion des dossiers" = menu_folders,
    "Documentation" = website,
    "Signaler un probl\u00E8me" = ask_help
  )

  seq_run_menu(actions = actions)

}
