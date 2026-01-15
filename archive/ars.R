#' Download ARS vector layer from IGN WFS
#'
#' ARS layers are typically used to assess sanitary constraints, ensure
#' compliance with water-protection regulations, and understand environmental
#' risks near captages in a given study area.
#'
#' @param x `sf` or `sfc`; Geometry located in France.
#' @param key `character`; key identifying layer to download. Must be one of
#' `"captage"`, `"ppe"`, `"ppi"` or `"ppr"`.
#' @param buffer `numeric`; Buffer around `x` (in **meters**) used to enlarge
#' the download area.
#'
#' @details
#' Four ARS layers are currently supported:
#'
#' - **`captage`**: Drinking-water abstraction points declared by partner
#' organisations (collectivités, services d’eau). These represent the physical
#' points where water is captured for human consumption.
#' - **`ppi`** : The most restrictive protection zone, directly surrounding the
#' captage, usually fenced and strictly controlled("Périmètre de Protection Immédiate")
#' - **`ppr`** : An intermediate protection zone between the PPI and PPE,
#' with moderate regulatory constraints ("Périmètre de Protection Rapprochée")
#' - **`ppe`**: A broad protection zone around a captage where activities may
#' be regulated to safeguard water quality ("Périmètre de Protection Élargi")
#'
get_ars <- function(
    x,
    key = c("captage", "ppi", "ppr", "ppe"),
    buffer = 500){

  if (!inherits(x, c("sf", "sfc"))){
    cli::cli_abort(c(
      "x" = "{.arg x} is of class {.cls {class(x)}}.",
      "i" = "{.arg x} should be of class {.cls sf} or {.cls sfc}."
    ))
  }

  if (length(key) != 1) {
    cli::cli_abort(c(
      "x" = "{.arg key} must contain exactly one element.",
      "i" = "You supplied {length(key)} : {.val {key}}"
    ))
  }

  available_key <- c("captage", "ppi", "ppr", "ppe")
  if (!key %in% available_key){
    cli::cli_abort(c(
      "x" = "{.arg key} {.val {key}} isn't valid.",
      "i" = "Available keys are : {.val {available_key}}"
    ))
  }

  x <- sf::st_transform(x, 2154)
  x_buff <- sf::st_buffer(x, buffer)

  layer <- switch(
    key,
    "captage" = "ars_zip_wfs:ars_carteaux_captages_partenaires_j",
    "ppi" = "ars_zip_wfs:dgs_carteaux_ppi_partenaire_j",
    "ppr" = "ars_zip_wfs:dgs_carteaux_ppr_partenaire_j",
    "ppe" = "ars_zip_wfs:dgs_carteaux_ppe_partenaire_j"
  )

  f <- happign::get_wfs(
    x_buff,
    layer = layer,
    spatial_filter = "intersects"
  ) |> suppressWarnings() |> suppressMessages()

  if (!(is.null(f))){
    f <- sf::st_transform(f, 2154)
    f$type <- key
  }

  return(invisible(f))
}

#' Download ARS vector layers for a Sequoia project
#'
#' Downloads one or several vector layers from the IGN WFS service
#' for `ars` layer(s) of a Sequoia project.
#'
#' This function is a convenience wrapper looping over ars layers (`"captage"`,
#' `"ppi"`, `"ppr"`, `"ppe"`), allowing the user to download all products in
#' one call and automatically write them to the project directory using
#' [seq_write()].
#'
#' @inheritParams get_ars
#' @inheritParams seq_write
#'
#' @param key `character`; List of ARS layer identifiers to download. If not
#' provided, the function automatically select all ARS layers defined in the
#' Sequoia configuration (`inst/config/seq_layers.yaml`)
#'
#' @return A named list of file paths written by [seq_write()], one per layer.
#'
#' @seealso [get_ars()], [seq_write()]
#'
seq_ars <- function(
    dirname = ".",
    buffer = 500,
    key = c("captage", "ppi", "ppr", "ppe"),
    verbose = TRUE,
    overwrite = FALSE){

  # read matrice
  parca <- seq_read("parca", dirname = dirname)

  pb <- cli::cli_progress_bar(
    format = "{cli::pb_spin} Querying MNHN layer: {.val {key}} | [{cli::pb_current}/{cli::pb_total}]",
    total = length(key)
  )

  ars <- lapply(key, function(key) {
    if (verbose) cli::cli_progress_update(id = pb)
    quiet(get_ars(parca, key, buffer = buffer))
  }) |> stats::setNames(key)
  cli::cli_progress_done(id = pb)

  has_no_layers <- all(sapply(ars, is.null))
  if (has_no_layers){
    cli::cli_warn("All ARS layers are empty.")
    return(NULL)
  }

  ars_path <- list()

  captage <- ars[["captage"]]
  has_captage <- !is.null(captage)
  if (has_captage) {
    captage_path <- seq_write(
      captage,
      "v.ars.captage.point",
      dirname = dirname,
      verbose = verbose,
      overwrite = overwrite
    )
    ars_path <- c(ars_path, captage_path)
  }

  all_perimetre <- Filter(Negate(is.null), ars[c("ppi", "ppr", "ppe")])
  has_perimetre <- length(all_perimetre) > 0
  if (has_perimetre){
    perimetre <- do.call(rbind, all_perimetre)
    perimetre_path <- seq_write(
      perimetre,
      "v.ars.perimetre.poly",
      dirname = dirname,
      verbose = verbose,
      overwrite = overwrite
    )
    ars_path <- c(ars_path, perimetre_path)
  }

  return(ars_path)
}

