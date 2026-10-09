#' Open the toolbox menu
#'
#' Internal menu used to access standalone sequoia2 tools.
#'
#' @param state Menu selection state shared with toolbox submenus.
#' @keywords internal
#' @noRd
menu_toolbox <- function(state = seq_menu_state()) {

  actions <- list(
    "T\u00E9l\u00E9charger des donn\u00E9es depuis une emprise" = function() {
      menu_toolbox_setup(
        state, menu_toolbox_data,
        title = "T\u00E9l\u00E9charger des donn\u00E9es depuis une emprise",
        launch = "Choisir les donn\u00E9es \u00E0 t\u00E9l\u00E9charger",
        needs_zone = TRUE
      )
    },
    "Chercher une personne morale" = function() {
      menu_toolbox_setup(
        state, menu_pm,
        title = "Chercher une personne morale",
        launch = "Lancer la recherche"
      )
    },
    "Convertir un relev\u00E9 de propri\u00E9t\u00E9 -> Excel" = function() {
      menu_toolbox_setup(
        state, menu_rp,
        title = "Convertir un relev\u00E9 de propri\u00E9t\u00E9 -> Excel",
        launch = "Lancer la conversion"
      )
    }
  )

  seq_run_menu(
    actions = actions,
    title = "Bo\u00EEte \u00E0 outils",
    is_sub = TRUE
  )

}

#' Configure and run a standalone toolbox tool
#'
#' @param state Menu selection state shared by toolbox tools.
#' @param action Tool function taking the selection state as its argument.
#' @param title Tool menu title.
#' @param launch Label for the action that runs the tool.
#' @param needs_zone Whether the tool requires a geographic extent.
#' @noRd
menu_toolbox_setup <- function(state, action, title, launch, needs_zone = FALSE) {

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
    if (needs_zone) {
      seq_show_selection(
        state$zone_file,
        label = "Emprise",
        missing = "Aucune emprise s\u00E9lectionn\u00E9e."
      )
    }
  }

  run <- function() {
    if (needs_zone && is.null(state$zone)) {
      seq_select_zone(state)
      if (is.null(state$zone)) {
        return(invisible(NULL))
      }
    }

    if (is.null(state$path) || !nzchar(state$path)) {
      select_folder()
      if (is.null(state$path) || !nzchar(state$path)) {
        return(invisible(NULL))
      }
    }

    action(state)
  }

  actions <- list()
  if (needs_zone) {
    actions[["S\u00E9lectionner une emprise"]] <- function() seq_select_zone(state)
  }
  actions[["Choisir un dossier de sortie"]] <- select_folder
  actions[[launch]] <- run

  seq_run_menu(
    actions = actions,
    title = title,
    info = info,
    is_sub = TRUE
  )

}
