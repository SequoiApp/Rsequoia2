#' Download patrimony vector layer
#'
#' Downloads a vector layer with `frheritage` for the area covering `x`
#' expanded with a buffer.
#'
#' @param x `sf` or `sfc`; Geometry located in France.
#' @param key `character`; Layer to download.
#' Must be one of from `get_keys("pat")`
#' @param buffer `numeric`; Buffer around `x` (in **meters**) used to enlarge
#' the download area.
#' @param overwrite `logical`; If `TRUE`, file is overwritten.
#' @param verbose `logical`; If `TRUE`, display messages.
#'
#' @return `sf` object from `sf` package
#'
#' @export
#'
get_patrimony <- function(
    x,
    key,
    buffer = 500,
    overwrite = FALSE,
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

  f <- frheritage::get_heritage(x,
                                data_code,
                                buffer = buffer,
                                crs = 2154,
                                spatial_filter = "intersects",
                                verbose = FALSE)

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
    verbose = TRUE,
    overwrite = FALSE){

  # read matrice
  parca <- read_sf(get_path("v.seq.parca.poly"))
  keys <- get_pat_keys()

  pb <- cli::cli_progress_bar(
    format = "{cli::pb_bar} {.val {key}} | [{pb_current}/{pb_total}]",
    total = length(keys)
  )

  time_start <- Sys.time()
  res <- lapply(keys, function(key){
    cli::cli_progress_update(id = pb)
    get_pat(parca, key, buffer = buffer)
  })
  cli::cli_progress_done(id = pb)
  cli::cli_alert_success(
    "Downloaded {length(keys)} PAT layers in {Sys.time() - time_start}."
  )

}
