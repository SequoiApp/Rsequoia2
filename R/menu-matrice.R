#' Open the cadastral matrix menu
#'
#' Internal menu used to create or import cadastral matrices for the selected
#' sequoia2 directory.
#'
#' @param state Menu selection state shared with matrix submenus.
#' @keywords internal
#' @noRd
menu_matrice <- function(state) {

  path <- seq_get_path(state)
  info <- function() seq_show_selection(state$path)

  blank_matrice <- function() {
    id <- readline("Identifiant de la for\u00EAt : ")
    create_matrice(dirname = path, id = id, overwrite = FALSE)
  }

  actions <- list(
    "Matrice vierge" = blank_matrice,
    "Matrice relev\u00E9 de propri\u00E9t\u00E9" = function() menu_rp(state),
    "Matrice personne morale" = function() menu_pm(state)
  )

  seq_run_menu(
    actions = actions,
    info = info,
    is_sub = TRUE
  )

}
