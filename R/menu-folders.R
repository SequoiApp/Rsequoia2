#' Open the folder management menu
#'
#' Internal menu used to migrate or rename sequoia2 directories.
#'
#' @keywords internal
#' @noRd
menu_folders <- function() {

  update_folder <- function() {
    path <- rstudioapi::selectDirectory(
      caption = "S\u00E9lectionner ancien dossier sequoia"
    )

    if (nzchar(path)) {
      seq1_update(path)
    }
  }

  rename_folder <- function() {
    path <- rstudioapi::selectDirectory(
      caption = "S\u00E9lectionner ancien dossier sequoia"
    )

    old_id <- readline("Ancien identifiant : ")
    new_id <- readline("Nouvel identifiant : ")

    if (nzchar(path) && nzchar(old_id) && nzchar(new_id)) {
      seq_dir_rename(path, old_id, new_id)
    }
  }

  actions <- list(
    "Migrer un ancien dossier" = update_folder,
    "Renommer un dossier" = rename_folder
  )

  seq_run_menu(
    actions = actions,
    title = "Gestion des dossiers",
    is_sub = TRUE
  )

}
