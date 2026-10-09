#' Open the main project menu
#'
#' Internal menu used to run the main sequoia2 processing workflow for the
#' selected directory.
#'
#' @param state Menu selection state shared with project submenus.
#' @keywords internal
#' @noRd
menu_sequoia <- function(state = seq_menu_state()) {

  select_folder <- function() {
    seq_select_folder(
      state,
      caption = "S\u00E9lectionner un dossier Sequoia2"
    )
  }

  info <- function() {
    seq_show_selection(
      state$path,
      missing = "Aucun dossier Sequoia s\u00E9lectionn\u00E9."
    )
  }

  aggregate_ua <- function() {

    path <- seq_get_path(state)

    cli::cli_alert_warning(
      "Cette op\u00E9ration peut \u00E9craser des fichiers existants."
    )

    answer <- readline(
      cli::format_inline(
        "Voulez-vous continuer ? [{.strong o/N}] "
      )
    )

    overwrite <- tolower(trimws(answer)) %in% c("o", "oui", "y", "yes")

    if (!overwrite) {
      cli::cli_alert_info("Op\u00E9ration annul\u00E9e. Aucun fichier n'a \u00E9t\u00E9 \u00E9cras\u00E9.")
      return(invisible(FALSE))
    }

    seq_boundaries(path, overwrite = overwrite)
    seq_parcels(path, overwrite = overwrite)
    seq_occupation(path, overwrite = overwrite)

    invisible(TRUE)
  }

  actions <- list(
    "S\u00E9lectionner un dossier Sequoia" = select_folder,
    "G\u00E9n\u00E9rer une MATRICE CADASTRALE" = function() menu_matrice(state),
    "T\u00E9l\u00E9charger PARCA" = function() seq_parca(seq_get_path(state)),
    "T\u00E9l\u00E9charger DONNEES" = function() menu_data(state),
    "G\u00E9n\u00E9rer les UA" = function() seq_parca_to_ua(seq_get_path(state)),
    "Corriger les UA" = function() seq_ua(seq_get_path(state)),
    "Aggr\u00E9ger les UA" = aggregate_ua,
    "Synth\u00E9tiser les UA" = function() seq_summary(seq_get_path(state))
  )

  seq_run_menu(
    actions = actions,
    title = "Projet Sequoia2",
    info = info,
    is_sub = TRUE
  )

}
