#' Download patrimony vector layer
#'
#' Downloads a vector layer with `frheritage` for the area covering `x`
#' expanded with a buffer.
#'
#' @param x `sf` or `sfc`; Geometry located in France.
#' @param key `character`; Layer to download.
#'   Must be one of from `get_keys("pat")`
#' @param buffer `numeric`; Buffer around `x` (in **meters**) used to enlarge
#'   the download area.
#' @param verbose `logical`; If `TRUE`, display progress and informational messages.
#'
#' @return `sf` object from `sf` package
#'
#' @export
#'
get_patrimony <- function(
    x,
    key,
    buffer = 500,
    verbose = TRUE){

  if (!inherits(x, c("sf", "sfc"))){
    cli::cli_abort(c(
      "x" = "{.arg x} is of class {.cls {class(x)}}.",
      "i" = "{.arg x} should be of class {.cls sf} or {.cls sfc}."
    ))
  }

  if (length(key) != 1) {
    cli::cli_abort(c(
      "x" = "{.arg key} must contain exactly one element.",
      "i" = "You supplied {length(key)}."
    ))
  }

  if (!key %in% get_keys("pat")){
    cli::cli_abort(c(
      "x" = "{.arg key} {.val {key}} isn't valid.",
      "i" = "Run {.run Rsequoia2::get_keys(\"pat\")} for available layers."
    ))
  }

  data_code <- toupper(key)

  f <- tryCatch(
    frheritage::get_heritage(
      x,
      data_code,
      buffer = buffer,
      crs = 2154,
      verbose = FALSE
    ),
    error = function(e) {

      if (grepl("Atlas service is not available", e$message, fixed = TRUE)) {
        if (verbose) {
          cli::cli_alert_warning(c(
            "x" = "Atlas service is not available.",
            "i" = "Please try again later."
          ))
        }
        return(invisible(NULL))
      }

      stop(e)
    }
  )

  if (is.null(f) || !nrow(f)) {
    if (verbose){
      cli::cli_alert_warning("Layer {.field {key}}: no intersecting features")
    }
    return(invisible(NULL))
  }

  return(invisible(f))
}

#' Download patrimony vector layers for a Sequoia project
#'
#' Downloads one or several vector layers with `frheritage`
#' for `pat` layer(s) of a Sequoia project.
#'
#' This function is a convenience wrapper looping over [get_patrimony()], allowing
#' the user to download all products in one call and automatically write them
#' to the project directory using [seq_write()].
#'
#' @inheritParams get_patrimony
#' @inheritParams seq_write
#'
#' @param key `character`; List of layer identifiers to download. If not
#' provided, the function uses `get_keys("pat")` to automatically select all
#' patrimony layers defined in the Sequoia configuration (`inst/config/seq_layers.yaml`)
#'
#' @details
#' For each value in `key`, the function attempts to query the corresponding
#' MNHN layer using [get_patrimony()].
#'
#' - If the layer contains features, it is written to the project directory
#'   via [seq_write()] and recorded as a successful download.
#' - If the layer contains no features, it is skipped and marked
#'   as empty.
#'
#' @return A named list of file paths written by [seq_write()], one per layer.
#'
#' @seealso [get_patrimony()], [seq_write()]
#'
seq_patrimony <- function(
    dirname = ".",
    buffer = 500,
    key = get_keys("pat"),
    verbose = TRUE,
    overwrite = FALSE){

  # read matrice
  parca <- seq_read("v.seq.parca.poly", dirname = dirname)
  identifier <- seq_field("identifier")$name
  id <- unique(parca[[identifier]])

  if (verbose){
    cli::cli_h1("PATRIMONY")
  }

  pb <- NULL
  if (verbose) {
    pb <- cli::cli_progress_bar(
      format = paste0(
        "{cli::pb_spin} Searching Patrimony layer: {.val {k}} | ",
        "[{cli::pb_current}/{cli::pb_total}]"
      ),
      total = length(key),
      auto_terminate = FALSE
    )
  }

  path <- list()
  for (k in key) {

    if (verbose) {
      cli::cli_progress_update(id = pb, force = TRUE)
    }

    f_path <- tryCatch({
      f <- get_patrimony(parca, k, buffer = buffer, verbose = FALSE)

      if (is.null(f) || nrow(f) == 0) {
        NULL
      } else {
        f[[identifier]] <- id

        seq_write(
          f,
          sprintf("v.pat.%s.poly", k),
          dirname,
          id,
          verbose = verbose,
          overwrite = overwrite
        )
      }
    }, error = function(e) NULL)

    if (!is.null(f_path)) {
      path <- c(path, f_path)
    }

  }

  if (!length(path)) {
    if (verbose){
      cli::cli_alert_info("No Patrimony layer found.")
    }
    return(invisible(NULL))
  }

  invisible(path)
}
