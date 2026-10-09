# Main menu ----

#' Launch the main sequoia2 menu
#'
#' Opens the interactive command-line menu for the sequoia2 workflow.
#'
#' Project and toolbox selections are kept separately for the menu session and
#' passed explicitly to their submenus and processing functions.
#'
#' @export
sequoia2 <- function() {

  project_state <- seq_menu_state()
  toolbox_state <- seq_menu_state()

  ask_help <- function() {
    utils::browseURL("https://github.com/SequoiApp/Rsequoia2/issues")
  }

  website <- function() {
    utils::browseURL("https://sequoiapp.github.io/Rsequoia2/index.html")
  }

  actions <- list(
    "Projet Sequoia" = function() menu_sequoia(project_state),
    "Bo\u00EEte \u00E0 outils" = function() menu_toolbox(toolbox_state),
    "Gestion des dossiers" = menu_folders,
    "Documentation" = website,
    "Signaler un probl\u00E8me" = ask_help
  )

  seq_run_menu(actions = actions)

}
