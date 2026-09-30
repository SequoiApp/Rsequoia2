#' Open the cadastral matrix menu
#'
#' Internal menu used to create or import cadastral matrices for the selected
#' sequoia2 directory.
#'
#' @keywords internal
#' @noRd
menu_matrice <- function() {

  path <- seq_get_path()
  info <- cli::format_inline("Dossier selectionne : {.path {path}}")

  blank_matrice <- function() {
    id <- readline("Identifiant de la foret : ")
    create_matrice(dirname = path, id = id, overwrite = FALSE)
  }

  actions <- list(
    "Matrice vierge" = blank_matrice,
    "Matrice releve de propriete" = menu_rp,
    "Matrice personne morale" = menu_pm
  )

  seq_run_menu(
    actions = actions,
    info = info,
    is_sub = TRUE
  )

}
