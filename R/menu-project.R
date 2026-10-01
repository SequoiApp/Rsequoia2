#' Open the main project menu
#'
#' Internal menu used to run the main sequoia2 processing workflow for the
#' selected directory.
#'
#' @keywords internal
#' @noRd
menu_sequoia <- function() {

  path <- rstudioapi::selectDirectory(
    caption = "Sélectionner un dossier Sequoia2",
    path = getOption("seq_dir_path", getwd())
  )

  if (is.null(path) || !nzchar(path)) {
    return(invisible(NULL))
  }

  options(seq_dir_path = path)
  info <- cli::format_inline("Dossier sélectionné : {.path {path}}")

  download_parca <- function() seq_parca(seq_get_path())

  create_ua <- function() seq_parca_to_ua(seq_get_path())

  correct_ua <- function() seq_ua(seq_get_path())

  aggregate_ua <- function() {

    cli::cli_alert_warning(
      "Cette opération peut écraser des fichiers existants."
    )

    answer <- readline(
      cli::format_inline(
        "Voulez-vous continuer ? [{.strong o/N}] "
      )
    )

    overwrite <- tolower(trimws(answer)) %in% c("o", "oui", "y", "yes")

    if (!overwrite) {
      cli::cli_alert_info("Opération annulée. Aucun fichier n'a été écrasé.")
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
    "Générer une MATRICE CADASTRALE" = menu_matrice,
    "Télécharger PARCA" = download_parca,
    "Télécharger DONNEES" = menu_data,
    "Générer les UA" = create_ua,
    "Corriger les UA" = correct_ua,
    "Aggréger les UA" = aggregate_ua,
    "Synthétiser les UA" = sumarize_ua
  )

  seq_run_menu(
    actions = actions,
    title = "Projet Sequoia2",
    info = info,
    is_sub = TRUE
  )

}
