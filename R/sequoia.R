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
#' @return Invisibly returns NULL. The function primarily calls other Rsequoia2 functions.
#'
#' @export
sequoia <- function(path = NULL) {

  if (is.null(path)) {
    path <- readline("Choose the project folder: ")
  }

  if (path == "") stop("No selection.", call. = FALSE)

  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  message("Your project folder path is: ", path)

  choose_option <- function(options, title) {
    res <- select.list(
      options,
      multiple = FALSE,
      title = title,
      graphics = TRUE
    )
    if (length(res) == 0 || res == "") stop("No selection.", call. = FALSE)
    res
  }

  main_menu <- c("Creating map layers", "Cartographic tools")
  choice <- choose_option(main_menu, "Rsequoia2")
  message("You chose: ", choice)

  if (choice == "Creating map layers") {
    sub_menu <- c(
      "1 MATRICE creation",
      "2 PARCA creation",
      "3 UA creation",
      "4 UA finalization"
    )

    sub_choice <- choose_option(sub_menu, "Creating map layers")
    message("You chose: ", sub_choice)

    switch(sub_choice,
           "1 MATRICE creation" = {
             identifiant <- readline("Choose the forest project name: ")
             create_matrice(path, identifiant)
           },
           "2 PARCA creation" = seq_parca(path),
           "3 UA creation" = seq_parca_to_ua(path),
           "4 UA finalization" = seq_ua(path)
    )
  }

  if (choice == "Cartographic tools") {
    message("Not ready yet")
  }
}
