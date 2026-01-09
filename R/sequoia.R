#' Rsequoia2 Main Interactive Workflow
#'
#' Launches the main interactive workflow for Rsequoia2, allowing the user to
#' select a project folder and choose among map layer creation or cartographic tools.
#'
#' @param path Character. Optional. Path to the project folder. If not provided,
#'   the user will be prompted to enter it interactively.
#'
#' @details
#' The function opens interactive menus to guide the user through tasks such as:
#' \itemize{
#'   \item Creating map layers (MATRICE, PARCA, UA, UA finalization)
#'   \item Cartographic tools (currently not implemented)
#' }
#' The user must make selections at each step; cancelling or leaving a selection
#' empty will stop the function.
#'
#' @return Invisibly returns `path`.
#' The function primarily calls other Rsequoia2 functions.
#'
#' @export
sequoia <- function(path = NULL) {

  if (is.null(path)){
    path <- if (rstudioapi::isAvailable()) {
      rstudioapi::selectDirectory()
    } else {
      readline("Enter directory path: ")
    }
  }

  if (is.null(path) | path == "") cli_abort("No selection.")

  cli_alert_info("You choose folder: {.path {normalizePath(path)}}")

  # MAIN MENU ---
  actions <- list(
    "Create MATRICE" = function() {
      identifiant <- readline("Choose the forest identifiant: ")
      create_matrice(dirname = path, id = identifiant)
    },
    "Create MATRICE (legal entity)" = function() menu_legal_entity(path),
    "Download PARCA" = function() seq_parca(path),
    "Create UA" = function() seq_parca_to_ua(path),
    "Correct UA" = function() seq_ua(path)
  )

  choice <- menu(names(actions))
  actions[[choice]]()

  return(invisible(path))
}

#' Interactive legal-entity menu
#'
#' Prompts the user for INSEE codes and owner name patterns, retrieves matching
#' legal-entity parcels, displays a short summary (area, owners, number of
#' parcels), and asks for confirmation before creating the matrice.
#'
#' @details
#' This function is interactive and intended for manual use only.
#'
#' @noRd
menu_legal_entity <- function(path){
  x <- readline("Enter DEP/INSEE codes (comma-separated): ")
  y <- readline("Enter proprietaire search pattern (comma-separated): ")
  insee <- trimws(strsplit(x, ",")[[1]])
  prop <- trimws(strsplit(y, ",")[[1]])

  m <- get_legal_entity(insee)
  ms <- search_legal_entity(m, prop = prop)

  s <- sum(ms$SURF_CA / 10000)
  p <- unique(ms$PROPRIETAIRE)

  cli::cli_h2("Summary")
  cli::cli_bullets(c(
    "Number of parcels: {nrow(ms)}",
    "Total area: {format(round(s, 2), nsmall = 2)} ha",
    "{length(p)} owners : {paste(p, collapse = ', ')}"
  ))

  switch(menu(c("Confirm and continue", "Cancel")),
         {
           identifiant <- readline("Choose the forest identifiant: ")
           create_matrice(dirname = path, id = identifiant, overwrite = TRUE)
         }
  )

  return(NULL)

}
