#' Download GPU vector layer
#'
#' Downloads a vector layer with `hhapign` for the area covering `x`
#' expanded with a buffer.
#'
#' @param x `sf` or `sfc`; Geometry located in France.
#' @param key `character`; Layer to download.
#'   Must be one of from `get_keys("pat")`
#' @param buffer `numeric`; Buffer around `x` (in **meters**) used to enlarge
#'   the download area.
#' @param verbose `logical`; If `TRUE`, display messages.
#'
#' @return `sf` object from `sf` package
#'
#' @export
#'
get_gpu <- function(x,
                    key,
                    buffer = 0,
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

  if (!key %in% get_keys("gpu", reduce = FALSE)){
    cli::cli_abort(c(
      "x" = "{.arg key} {.val {key}} isn't valid.",
      "i" = "Run {.run Rsequoia2::get_keys(\"gpu\", reduce = FALSE)} for available layers."
    ))
  }

  layers <- switch(
    key,
    "v.gpu.municipality.poly" = "municipality",
    "v.gpu.document.poly"     = "document",
    "v.gpu.zone.poly"         = "zone-urba",
    "v.gpu.prescription.poly" = "prescription-surf",
    "v.gpu.prescription.line" = "prescription-lin",
    "v.gpu.prescription.point"= "prescription-pct",
    "v.gpu.supa.poly"         = c("assiette-sup-s", "assiette-sup-l", "assiette-sup-p"),
    "v.gpu.supg.poly"         = "generateur-sup-s",
    "v.gpu.supg.line"         = "generateur-sup-l",
    "v.gpu.supg.point"        = "generateur-sup-p"
  )

  if (length(layers) > 1) {

    res <- lapply(layers, function(k) {
      quiet(happign::get_apicarto_gpu(x, k))
    })

    res <- res[!vapply(res, is.null, logical(1))]

    if (!length(res) || !any(vapply(res, nrow, integer(1)))) {
      if (verbose){
        cli::cli_alert_warning("Layer {.field {key}}: no intersecting features")
      }
      return(invisible(NULL))
    }

    g <- do.call(rbind, res)

  } else {

    g <- quiet(happign::get_apicarto_gpu(x, layers))

    if (is.null(g) || !nrow(g)) {
      if (verbose){
        cli::cli_alert_warning("Layer {.field {key}}: no intersecting features")
      }
      return(invisible(NULL))
    }
  }

  return(invisible(g))
}

#' Generate GPU layers for a Sequoia project
#'
#' Retrieves applicable GPU (Geoportail de l'Urbanisme) layers intersecting
#' and surrounding the project area, and writes the resulting layer to disk.
#'
#' @param dirname Character. Root directory of the project.
#'   Defaults to the current directory.
#' @param verbose Logical. Whether to display progress messages.
#'   Defaults to `TRUE`.
#' @param overwrite Logical. Whether to overwrite existing output files.
#'   Defaults to `FALSE`.
#'
#' @return
#' Invisibly returns a named list of file paths corresponding to the
#' GPU layers written to disk. Layers with no intersecting features
#' are not included.
#'
#' @details
#' The function queries the GPU API via the `happign` package for the
#' following thematic layers:
#'
#' * Municipality boundaries
#' * Urban planning documents
#' * Urban zones
#' * Surface, linear and point prescriptions
#' * SUP assiettes (SUPA)
#' * SUP generateurs (SUPG)
#'
#' Only layers intersecting the project area are written. Empty or unavailable
#' layers are silently skipped.
#'
#' Output file names, formats and locations are fully driven by the
#' project configuration (see `files_structure.yaml`).
#'
#' @seealso [seq_write()], [happign::get_apicarto_gpu()]
#'
#' @export
seq_gpu <- function(dirname = ".", verbose = TRUE, overwrite = FALSE) {

  # area of interest
  parca <- seq_read("v.seq.parca.poly", dirname = dirname)
  identifier <- seq_field("identifier")$name
  id <- unique(parca[[identifier]])

  geom <- sf::st_union(parca)

  if (verbose){
    cli::cli_h1("GPU")
  }

  key <- get_keys("gpu", reduce = FALSE)

  paths <- vector("list", length(key))
  names(paths) <- key

  for (k in key) {

    f <- get_gpu(
      x = geom,
      key = k,
      verbose = FALSE
    )

    if (is.null(f)) {
      if (verbose) {
        cli::cli_alert_info("GPU {.field {k}}: no features found")
      }
      next
    }

    f[[identifier]] <- id

    paths[[k]] <- seq_write(
      x         = sf::st_transform(f, 2154),
      key       = k,
      dirname   = dirname,
      id        = id,
      verbose   = verbose,
      overwrite = overwrite
    )
  }

  paths <- paths[!vapply(paths, is.null, logical(1))]

  if (!length(paths)) {
    return(invisible(NULL))
  }

  invisible(paths)
}
