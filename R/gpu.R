#' Generate GPU layers for a Sequoia project
#'
#' Retrievesapplicable GPU (Geoportail de l’Urbanisme) layers intersecting
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
  geom <- seq_read("v.seq.parca.poly", dirname = dirname) |>
    sf::st_union()

  if (verbose){
    cli::cli_h1("GPU")
  }

  id_field <- seq_field("identifiant")$name
  id_value <- get_id(dirname)

  # GPU layers specification
  layers <- list(
    municipality_s = list("v.gpu.municipality.poly",  "municipality"),
    document_s     = list("v.gpu.document.poly",      "document"),
    zone_s         = list("v.gpu.zone.poly",          "zone-urba"),
    prescription_s = list("v.gpu.prescription.poly",  "prescription-surf"),
    prescription_l = list("v.gpu.prescription.line",  "prescription-lin"),
    prescription_p = list("v.gpu.prescription.point", "prescription-pct"),
    supa_s = list(
      "v.gpu.supa.poly",
      c("assiette-sup-s", "assiette-sup-l", "assiette-sup-p")
    ),
    supg_s = list("v.gpu.supg.poly",  "generateur-sup-s"),
    supg_l = list("v.gpu.supg.line",  "generateur-sup-l"),
    supg_p = list("v.gpu.supg.point", "generateur-sup-p")
  )

  # results
  paths <- vector("list", length(layers))
  names(paths) <- names(layers)

  # main loop
  for (k in names(layers)) {

    spec <- layers[[k]]
    layerspec <- spec[[2]]

    # GPU retrieval (single vs multiple layers)
    if (length(layerspec) > 1) {

      res <- lapply(layerspec, \(x) {quiet(happign::get_apicarto_gpu(geom, x))})

      res <- res[!vapply(res, is.null, logical(1))]
      f <- if (length(res)) do.call(rbind, res) else NULL

    } else {
      f <- quiet(happign::get_apicarto_gpu(geom, layerspec))
    }

    # skip empty layers
    if (is.null(f) || !nrow(f)) {
      if (verbose) {
        cli::cli_alert_info(
          "GPU {.field {k}}: no features found"
        )
      }
      next
    }

    # add project identifier
    f[[id_field]] <- id_value

    # write output
    paths[[k]] <- seq_write(
      x         = sf::st_transform(f, 2154),
      key       = spec[[1]],
      dirname   = dirname,
      verbose   = verbose,
      overwrite = overwrite
    )

  }

  # drop empty entries
  paths <- paths[!vapply(paths, is.null, logical(1))]

  invisible(paths)
}
