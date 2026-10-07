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
    buffer = 1000,
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

#' Fetch patrimony layers
#'
#' @inheritParams get_patrimony
#' @param dirname `character`; Output directory.
#' @param id Optional Sequoia project identifier.
#' @param key `character`; Patrimony layer identifiers.
#' @param overwrite `logical`; If `TRUE`, overwrite existing layers.
#'
#' @return Invisibly returns a named list of written paths.
#'
#' @keywords internal
#' @noRd
.patrimony_fetcher <- function(
    x,
    dirname,
    id = NULL,
    buffer = 1000,
    key = get_keys("pat"),
    verbose = TRUE,
    overwrite = FALSE) {

  if (!all(key %in% get_keys("pat"))) {
    cli::cli_abort("{.arg key} must be one or more of {.val {get_keys(\"pat\")}}.")
  }

  if (verbose) cli::cli_h1("PATRIMONY")

  pb <- NULL
  if (verbose) {
    pb <- cli::cli_progress_bar(
      format = paste0(
        "{cli::pb_spin} Searching PATRIMONY layer: {.val {k}} | ",
        "[{cli::pb_current}/{cli::pb_total}]"
      ),
      total = length(key)
    )
  }

  paths <- lapply(key, function(k) {
    if (verbose) {
      cli::cli_progress_update(id = pb, set = list(k = k), force = TRUE)
    }

    tryCatch({
      f <- get_patrimony(x, k, buffer = buffer, verbose = FALSE)
      if (is.null(f) || !nrow(f)){
        return(NULL)
      }

      if (!is.null(id)){
        identifier <- seq_field("identifier")$name
        f[[identifier]] <- id
      }

      seq_write(
        f,
        key = sprintf("v.pat.%s.poly", k),
        dirname = dirname,
        id = id,
        verbose = verbose,
        overwrite = overwrite
      )

    }, error = function(e) {
      if (verbose) {
        cli::cli_alert_danger(
          "Failed PATRIMONY layer {.val {k}}: {conditionMessage(e)}"
        )
      }
      NULL
    })
  })

  names(paths) <- sprintf("v.pat.%s.poly", key)
  invisible(Filter(Negate(is.null), paths))
}


#' Fetch patrimony layers for an area
#'
#' Retrieves patrimony layers around `x` and writes them to `dirname`.
#'
#' @inheritParams get_patrimony
#' @param dirname `character`; Output directory.
#' @param key `character`; Patrimony layer identifiers.
#' @param overwrite `logical`; If `TRUE`, overwrite existing layers.
#'
#' @return Invisibly returns a named list of written paths.
#'
#' @keywords internal
#' @noRd
fetch_patrimony <- function(
    x,
    dirname,
    buffer = 1000,
    key = get_keys("pat"),
    verbose = TRUE,
    overwrite = FALSE) {

  .patrimony_fetcher(
    x = x,
    dirname = dirname,
    buffer = buffer,
    key = key,
    verbose = verbose,
    overwrite = overwrite
  )
}


#' Generate patrimony layers for a Sequoia project
#'
#' Retrieves patrimony layers around the project area and writes them to the
#' Sequoia project directory.
#'
#' @inheritParams get_patrimony
#' @inheritParams seq_write
#' @param key `character`; Patrimony layer identifiers. Defaults to
#'   `get_keys("pat")`.
#'
#' @return Invisibly returns a named list of written paths.
#'
#' @seealso [get_patrimony()], [seq_write()]
#'
#' @export
seq_patrimony <- function(
    dirname = ".",
    buffer = 1000,
    key = get_keys("pat"),
    verbose = TRUE,
    overwrite = FALSE) {

  ctx <- .seq_context(dirname)

  .patrimony_fetcher(
    x = ctx$parca,
    dirname = dirname,
    id = ctx$id,
    buffer = buffer,
    key = key,
    verbose = verbose,
    overwrite = overwrite
  )
}
