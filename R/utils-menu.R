# Select ----

#' Select values from a console menu
#'
#' @param choices Character vector of choices.
#' @param title Menu title.
#' @param info Optional character vector of information messages, or a function
#'   that displays status messages below the title.
#' @param multi If `TRUE`, multiple selections are allowed.
#' @param offset Integer used for the first displayed choice.
#'
#' @return Integer vector of selected positions, or `0L` to quit.
#'
#' @noRd
seq_select <- function(
    choices,
    title = "Menu",
    info = NULL,
    is_sub = FALSE,
    multi = FALSE,
    offset = 1L
) {
  if (!length(choices)) {
    cli::cli_abort("{.arg choices} must not be empty.")
  }

  msg <- NULL

  repeat {
    cli::cli_h1(title)
    cli::cli_text("")

    if (!is.null(info)) {
      if (is.function(info)) {
        info()
      } else {
        for (message in info) {
          cli::cli_alert_info(message)
        }
      }
      cli::cli_text("")
    }

    displayed_idx <- seq_along(choices) - 1L + offset

    for (i in seq_along(choices)) {
      cli::cli_text("{.val {displayed_idx[[i]]}}. {choices[[i]]}")
    }

    cli::cli_text("")
    cli::cli_rule(if (is_sub) "{.key q} quitter | {.key r} retour" else "{.key q} quitter")

    if (!is.null(msg)) {
      cli::cli_text("")
      cli::cli_alert_warning(msg)
      msg <- NULL
    }

    prompt <- if (multi) {
      "Selection(s), separees par des virgules : "
    } else {
      "Selection : "
    }

    ans <- trimws(readline(prompt))

    if (!nzchar(ans)) {
      return(invisible(if (is_sub) 0L else -1L))
    }

    if (is_sub && identical(tolower(ans), "r")) {
      return(invisible(0L))
    }

    if (identical(tolower(ans), "q")) {
      return(invisible(-1L))
    }

    idx <- strsplit(ans, ",", fixed = TRUE)[[1]]
    idx <- trimws(idx)
    idx <- suppressWarnings(as.integer(idx))

    if (anyNA(idx) || any(!idx %in% displayed_idx)) {
      msg <- paste0(
        "Selection invalide. Utilisez un nombre entre ",
        min(displayed_idx),
        " et ",
        max(displayed_idx),
        "."
      )
      next
    }

    if (!multi && length(idx) > 1) {
      msg <- "Veuillez selectionner une seule option."
      next
    }

    return(unique(idx - offset + 1L))
  }
}

# Internal helpers ----

#' Run a menu action safely
#'
#' Executes a menu action while catching errors and warnings. Errors are printed
#' with `cli::cli_alert_danger()`; warning formatting is preserved with
#' `cli::cli_verbatim()`.
#'
#' @param action A function to execute.
#'
#' @return The result of `action()`, or invisibly `NULL` if an error occurs.
#'
#' @keywords internal
#' @noRd
seq_run_action <- function(action) {
  withCallingHandlers(
    tryCatch(
      action(),
      error = function(e) {
        message <- conditionMessage(e)
        cli::cli_alert_danger("{message}")
        invisible(NULL)
      }
    ),
    warning = function(w) {
      cli::cli_verbatim(conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
}

#' Run an interactive sequoia2 menu
#'
#' Displays an interactive menu, executes the selected actions, and repeats until
#' the user chooses to go back or quit.
#'
#' Return values from are interpreted as follows:
#'
#' - `-1L`: quit all menus
#' - `0L`: go back
#' - positive integer(s): selected action index or indices
#'
#' @param actions A named list of functions. Names are displayed as menu choices.
#' @param title Menu title.
#' @param info Optional information displayed above the menu. Can be a character
#'   vector or a function that displays status messages.
#' @param is_sub Logical. Whether the menu is a submenu.
#' @param multi Logical. Whether multiple choices can be selected.
#'
#' @return Invisibly returns `-1L` to quit all menus, `0L` to go back, or keeps
#'   running until one of those values is selected.
#'
#' @keywords internal
#' @noRd
seq_run_menu <- function(
    actions,
    title = sprintf("Sequoia2 (%s)", utils::packageVersion("Rsequoia2")),
    info = NULL,
    is_sub = FALSE,
    multi = FALSE
) {
  repeat {

    idx <- seq_select(
      choices = names(actions),
      title = title,
      info = info,
      is_sub = is_sub,
      multi = multi,
      offset = 1L
    )

    if (identical(idx, -1L)) {
      return(invisible(-1L)) # quit all
    }

    if (identical(idx, 0L)) {
      return(invisible(0L)) # back
    }

    for (i in idx) {
      out <- seq_run_action(actions[[i]])

      if (identical(out, -1L)) {
        return(invisible(-1L)) # propagate quit
      }
    }
  }
}
