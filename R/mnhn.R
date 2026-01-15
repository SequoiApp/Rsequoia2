#' Download MNHN vector layer from IGN WFS (RGB or IRC)
#'
#' Downloads a vector layer from the IGN WFS service for the
#' area covering `x` expanded with a buffer.
#' Available layer are from MNHN (Nationale Museum of Natural History)
#'
#' @param x `sf` or `sfc`; Geometry located in France.
#' @param key `character`; Layer to download. Must be one of from `get_keys("mnhn")`
#' @param buffer `numeric`; Buffer around `x` (in **meters**) used to enlarge
#' the download area.
#' @param verbose `logical`; If `TRUE`, display messages.
#'
#' @return `sf` object from `sf` package
#'
#' @seealso [happign::get_wfs()]
#'
#' @examples
#' \dontrun{
#' library(sf)
#'
#' x <- st_sfc(st_point(c(-4.372746579180652, 47.79820761331345)), crs = 4326)
#'
#' ospar <- get_mnhn(x, key = "ospar", buffer = 5000)
#' x <- st_transform(x, st_crs(ospar))
#'
#' plot(st_geometry(ospar), col = "lightgrey")
#' plot(st_geometry(st_buffer(x, 5000)), col = "#FFB3B3", add = TRUE)
#' plot(st_geometry(x), col = "red", lwd =2, add = TRUE, pch = 19, cex = 2)
#'
#' }
get_mnhn <- function(
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

  if (!key %in% get_keys("mnhn")){
    cli::cli_abort(c(
      "x" = "{.arg key} {.val {key}} isn't valid.",
      "i" = "Run {.run Rsequoia2::get_keys(\"mnhn\")} for available layers."
    ))
  }

  x <- sf::st_transform(x, 2154)
  x_buff <- sf::st_buffer(x, buffer)

  layer <- switch(key,
                  "apb" = "patrinat_apb:apb",
                  "apg" = "patrinat_apg:apg",
                  "aphn" = "patrinat_aphn:aire_protection_habitats_naturels",
                  "aplg" = "patrinat_aplg:apg",
                  "bios" = "patrinat_bios:bios",
                  "unesco" = "patrinat_bpm:Bien_patrimoine_mondial_UNESCO",
                  "cdl" = "patrinat_cdl:conservatoire_littoral",
                  "cen" = "patrinat_cen:cen",
                  "geoparc" = "patrinat_geoparc:geoparc",
                  "gsf" = "patrinat_gsf:gsf",
                  "inpg" = "patrinat_inpg:inpg",
                  "ospar" = "patrinat_ospar:pgsf",
                  "pgsf" = "patrinat_pgsf:pgsf",
                  "pn" = "patrinat_pn:parc_national",
                  "pn2" = "patrinat_pn2:pn",
                  "pnm" = "patrinat_pnm:pnm",
                  "pnr" = "patrinat_pnr:pnr",
                  "pprnn" = "patrinat_pprnn:pprnn",
                  "ramsar" = "patrinat_ramsar:pnm",
                  "rb" = "patrinat_rb:reserve_biologique",
                  "ripn" = "patrinat_ripn:ripn",
                  "rnc" = "patrinat_rnc:pnm",
                  "rncfs" = "patrinat_rncfs:rncfs",
                  "rnn" = "patrinat_rnn:rnn",
                  "rnr" = "patrinat_rnr:rnr",
                  "sc" = "patrinat_sc:sc",
                  "sic" = "patrinat_sic:sic",
                  "znieff1" = "patrinat_znieff1:znieff1",
                  "znieffmer1" = "patrinat_znieff1_mer:znieff1_mer",
                  "znieff2" = "patrinat_znieff2:znieff2",
                  "znieffmer2" = "patrinat_znieff2_mer:znieff2_mer",
                  "zpr" = "patrinat_zpr:zpr",
                  "zps" = "patrinat_zps:zps"
  )

  f <- happign::get_wfs(
    x_buff,
    layer = layer,
    predicate = happign::intersects(),
    verbose = FALSE
  )

  if (!(is.null(f))){
    f <- sf::st_transform(f, 2154)
  }

  return(invisible(f))
}

#' Download MNHN vector layers for a Sequoia project
#'
#' Downloads one or several vector layers from the IGN WFS service
#' for `mnhn` layer(s) of a Sequoia project.
#'
#' This function is a convenience wrapper looping over [get_mnhn()], allowing
#' the user to download all products in one call and automatically write them
#' to the project directory using [seq_write()].
#'
#' @inheritParams get_mnhn
#' @inheritParams seq_write
#'
#' @param key `character`; List of MNHN layer identifiers to download. If not
#' provided, the function uses `get_keys("mnhn")` to automatically select all
#' MNHN layers defined in the Sequoia configuration (`inst/config/seq_layers.yaml`)
#'
#' @details
#' For each value in `key`, the function attempts to query the corresponding
#' MNHN layer using [get_mnhn()].
#'
#' - If the layer contains features, it is written to the project directory
#'   via [seq_write()] and recorded as a successful download.
#' - If the layer contains no features, it is skipped and marked
#'   as empty.
#'
#' @return A named list of file paths written by [seq_write()], one per layer.
#'
#' @seealso [get_mnhn()], [seq_write()]
#'
seq_mnhn <- function(
    dirname = ".",
    buffer = 500,
    key = get_keys("mnhn"),
    verbose = TRUE,
    overwrite = FALSE){

  # read matrice
  parca <- seq_read("v.seq.parca.poly", dirname = dirname)
  identifier <- seq_field("identifier")$name
  id <- unique(parca[[identifier]])

  if (verbose){
    cli::cli_h1("MNHN")
  }

  pb <- cli::cli_progress_bar(
    format = "{cli::pb_spin} Querying MNHN layer: {.val {k}} | [{cli::pb_current}/{cli::pb_total}]",
    total = length(key)
  )

  valid <- character()
  empty <- character()
  path <- list()
  for (k in key) {

    if (verbose) {cli::cli_progress_update(id = pb)}

    # f mean feature in this context
    f <- get_mnhn(parca, k, buffer = buffer)
    if (nrow(f) > 0) {
      f[[identifier]] <- id
      valid <- c(valid, k)
      seq_key <- sprintf("v.mnhn.%s.poly", k)
      f_path <- seq_write(f, seq_key, dirname, id, verbose = FALSE, overwrite = overwrite)
      path <- c(path, f_path)
    } else {
      empty <- c(empty, k)
    }
  }
  cli::cli_progress_done(id = pb)

  if (verbose){
    if (length(valid) > 0) {
      cli::cli_alert_success(
        "{length(valid)} non-empty layer{?s} found: {.val {valid}}"
      )
    } else {
      cli::cli_warn("All layers are empty.")
    }
  }

  return(path)
}
