#' Open the toolbox menu
#'
#' Internal menu used to access standalone sequoia2 tools.
#'
#' @param state Menu selection state shared with toolbox submenus.
#' @keywords internal
#' @noRd
menu_toolbox <- function(state = seq_menu_state()) {

  select_folder <- function() {
    seq_select_folder(
      state,
      caption = "S\u00E9lectionner un dossier de sortie"
    )
  }

  info <- function() {
    seq_show_selection(
      state$path,
      label = "Dossier de sortie",
      missing = "Aucun dossier de sortie s\u00E9lectionn\u00E9."
    )
    seq_show_selection(
      state$zone_file,
      label = "Zone g\u00E9ographique",
      missing = "Aucune zone g\u00E9ographique s\u00E9lectionn\u00E9e."
    )
  }

  actions <- list(
    "S\u00E9lectionner un dossier de sortie" = select_folder,
    "S\u00E9lectionner une zone g\u00E9ographique" = function() seq_select_zone(state),
    "RP PDF -> Excel" = function() menu_rp(state),
    "Rechercher une personne morale" = function() menu_pm(state),
    "T\u00E9l\u00E9charger PARCA depuis des IDU" = function() {
      cli::cli_alert_info("Fonctionnalit\u00E9 \u00E0 impl\u00E9menter.")
    },
    "T\u00E9l\u00E9charger des donn\u00E9es sur une zone" = function() menu_toolbox_data(state)
  )

  seq_run_menu(
    actions = actions,
    title = "Bo\u00EEte \u00E0 outils",
    info = info,
    is_sub = TRUE
  )

}
