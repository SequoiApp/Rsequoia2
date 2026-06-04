#' Select multiple values from a console menu
#'
#' Display numbered choices in the console and let the user select one or more
#' values by entering comma-separated numbers.
#'
#' @param choices Character vector of choices.
#' @param title Menu title.
#'
#' @return A character vector of selected choices.
#'
#' @noRd
multi_menu <- function(choices, title = "Choose one or more options") {

  if (length(choices) == 0) {
    cli::cli_abort("{.arg choices} must not be empty.")
  }

  cli::cli_h2(title)

  for (i in seq_along(choices)) {
    cli::cli_text("{.val {i}}. {choices[[i]]}")
  }

  cli::cli_rule()

  ans <- readline("Enter numbers separated by comma: ")

  idx <- strsplit(ans, ",")[[1]]
  idx <- trimws(idx)
  idx <- suppressWarnings(as.integer(idx))

  if (anyNA(idx) || any(!idx %in% seq_along(choices))) {
    cli::cli_abort("Invalid selection. Use numbers between 1 and {length(choices)}.")
  }

  idx
}
