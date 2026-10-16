#' Aggregate _UA_ surfaces by treatment
#'
#' Aggregates corrected surface areas from a _UA_ layer by grouping rows
#' according to treatment
#'
#' @param ua `sf` object containing analysis units.
#' @param verbose `logical`; If `TRUE`, display progress and informational messages.
#'
#' @return An `sf` object aggregated by occupation status.
#'
#' @seealso [seq_ame()], [seq_field()], [seq_normalize()]
#'
#' @export
ua_to_ame <- function(ua, verbose = TRUE) {
  ua <- seq_normalize(ua, "ua")

  treatment <- seq_field("treatment")$name
  cor_area <- seq_field("cor_area")$name

  if (all(is.na(ua[[treatment]]))) {
    cli::cli_abort(c(
      "x" = "Failed to generate treatment layer from UA.",
      "!" = "Field {.field {treatment}} in UA is missing or is empty."
    ))
  }

  by <- list(addNA(factor(ua[[treatment]]))) |> setNames(treatment)

  ame <- aggregate(
    x = ua[cor_area],
    by = by,
    FUN = sum,
    na.rm = TRUE
  )

  return(ame)
}

#' Create treatment layer from _UA_ for a Sequoia project
#'
#' Reads the UA polygon layer, aggregates surfaces by treatment status, and
#' writes the derived treatment layer to the project directory.
#'
#' @inheritParams seq_write
#'
#' @return An invisible file path to the exported occupation layer.
#'
#' @seealso [ua_to_ame()], [seq_read()], [seq_write()]
#'
#' @export
seq_ame <- function(dirname = ".", verbose = TRUE, overwrite = FALSE) {

  ctx <- .seq_context(dirname)

  ua <- seq_read("v.seq.ua.poly", dirname = dirname, verbose = FALSE)

  ame <- ua_to_ame(ua, verbose = verbose)

  ame_path <- seq_write(
    ame,
    "v.seq.ame.poly",
    dirname = dirname,
    id = ctx$id,
    verbose = verbose,
    overwrite = overwrite
  )

  invisible(ame)
}
