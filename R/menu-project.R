#' Open the main project menu
#'
#' Internal menu used to run the main sequoia2 processing workflow for the
#' selected directory.
#'
#' @keywords internal
#' @noRd
menu_sequoia <- function() {

  path <- rstudioapi::selectDirectory(
    caption = "S\u00E9lectionner un dossier Sequoia2",
    path = getOption("seq_dir_path", getwd())
  )

  if (is.null(path) || !nzchar(path)) {
    return(invisible(NULL))
  }

  options(seq_dir_path = path)
  info <- cli::format_inline("Dossier s\u00E9lectionn\u00E9 : {.path {path}}")

  download_parca <- function() seq_parca(seq_get_path())

  create_ua <- function() seq_parca_to_ua(seq_get_path())

  correct_ua <- function() seq_ua(seq_get_path())

  aggregate_ua <- function() {

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

    path <- seq_get_path()

    seq_boundaries(path, overwrite = overwrite)
    seq_parcels(path, overwrite = overwrite)
    seq_occupation(path, overwrite = overwrite)

    invisible(TRUE)
  }

  sumarize_ua <- function() seq_summary(seq_get_path())

  actions <- list(
    "G\u00E9n\u00E9rer une MATRICE CADASTRALE" = menu_matrice,
    "T\u00E9l\u00E9charger PARCA" = download_parca,
    "T\u00E9l\u00E9charger DONNEES" = menu_data,
    "G\u00E9n\u00E9rer les UA" = create_ua,
    "Corriger les UA" = correct_ua,
    "Aggr\u00E9ger les UA" = aggregate_ua,
    "Synth\u00E9tiser les UA" = sumarize_ua
  )

  seq_run_menu(
    actions = actions,
    title = "Projet Sequoia2",
    info = info,
    is_sub = TRUE
  )

}
