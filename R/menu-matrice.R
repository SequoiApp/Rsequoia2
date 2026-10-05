#' Open the cadastral matrix menu
#'
#' Internal menu used to create or import cadastral matrices for the selected
#' sequoia2 directory.
#'
#' @keywords internal
#' @noRd
menu_matrice <- function() {

  path <- seq_get_path()
  info <- cli::format_inline("Dossier s\u00E9lectionn\u00E9 : {.path {path}}")

  blank_matrice <- function() {
    id <- readline("Identifiant de la for\u00EAt : ")
    create_matrice(dirname = path, id = id, overwrite = FALSE)
  }

  actions <- list(
    "Matrice vierge" = blank_matrice,
    "Matrice relev\u00E9 de propri\u00E9t\u00E9" = menu_rp,
    "Matrice personne morale" = menu_pm
  )

  seq_run_menu(
    actions = actions,
    info = info,
    is_sub = TRUE
  )

}
