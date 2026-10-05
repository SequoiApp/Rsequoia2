#' Open the data download menu
#'
#' Internal menu used to download or generate geographic and environmental data
#' for the currently selected sequoia2 directory.
#'
#' @keywords internal
#' @noRd
menu_data <- function() {

  path <- seq_get_path()
  info <- cli::format_inline("Dossier s\u00E9lectionn\u00E9 : {.path {path}}")

  seq_altimetry <- function(dirname = ".", overwrite = FALSE, verbose = TRUE, ...) {

    tryCatch(
      seq_lidar(dirname = dirname, overwrite = overwrite, verbose = verbose, ...),
      error = function(e) {
        cli::cli_alert_warning("LiDAR unavailable. Falling back to RGE ALTI.")
        cli::cli_alert_info(conditionMessage(e))
        seq_rgealti(dirname = dirname, overwrite = overwrite, verbose = verbose, ...)
      }
    )
    seq_terrain(dirname = dirname, unit = "percent", overwrite = overwrite, verbose = verbose)
  }

  base_actions <- list(
    "Communes"       = function() seq_com(path),
    "MNHN"           = function() seq_mnhn(path),
    "G\u00E9ologie"       = function() seq_geol(path),
    "P\u00E9dologie"      = function() seq_pedology(path),
    "Infra"          = function() seq_infra(path),
    "Route"          = function() seq_road(path),
    "Route cad."     = function() seq_roadway(path),
    "PRSF"           = function() seq_prsf(path),
    "OLD"            = function() seq_old(path),
    "Toponyme"       = function() seq_toponyme(path),
    "Hydrologie"     = function() seq_hydro(path),
    "V\u00E9g\u00E9tation"     = function() seq_vege(path),
    "Accessibilit\u00E9"  = function() seq_access(path),
    "Meteo-France"   = function() seq_meteo_france(path),
    "Drias"          = function() seq_drias(path),
    "Courbes niveau" = function() seq_curves(path),
    "IFN"            = function() seq_ifn(path),
    "GPU"            = function() seq_gpu(path),
    "Patrimoine"     = function() seq_patrimony(path),
    "Altim\u00E9trie"     = function() seq_altimetry(path),
    "Orthophoto"     = function() seq_ortho(path)
  )

  actions <- c(
    "Toutes les donn\u00E9es" = function() invisible(lapply(base_actions, seq_run_action)),
    base_actions
  )

  seq_run_menu(
    actions = actions,
    info = info,
    is_sub = TRUE,
    multi = TRUE
  )

}

#' Open the toolbox data download menu
#'
#' Internal menu used to download or generate geographic and environmental data
#' for the currently selected sequoia2 directory.
#'
#' @keywords internal
#' @noRd
menu_toolbox_data <- function(x) {

  path <- seq_get_path()
  info <- cli::format_inline("Dossier s\u00E9lectionn\u00E9 : {.path {path}}")

  base_actions <- list(
    "Communes"       = function() fetch_com(x, path),
    "MNHN"           = function() fetch_mnhn(x, path)
    # "Geologie"       = function() seq_geol(path),
    # "Pedologie"      = function() seq_pedology(path),
    # "Infra"          = function() seq_infra(path),
    # "Route"          = function() seq_road(path),
    # "Route cad."     = function() seq_roadway(path),
    # "PRSF"           = function() seq_prsf(path),
    # "OLD"            = function() seq_old(path),
    # "Toponyme"       = function() seq_toponyme(path),
    # "Hydrologie"     = function() seq_hydro(path),
    # "Vegetation"     = function() seq_vege(path),
    # "Accessibilite"  = function() seq_access(path),
    # "Meteo-France"   = function() seq_meteo_france(path),
    # "Drias"          = function() seq_drias(path),
    # "Courbes niveau" = function() seq_curves(path),
    # "IFN"            = function() seq_ifn(path),
    # "GPU"            = function() seq_gpu(path),
    # "Patrimoine"     = function() seq_patrimony(path),
    # "Altimetrie"     = function() seq_altimetry(path),
    # "Orthophoto"     = function() seq_ortho(path)
  )

  actions <- c(
    "Toutes les donn\u00E9es" = function() invisible(lapply(base_actions, seq_run_action)),
    base_actions
  )

  seq_run_menu(
    actions = actions,
    title = "T\u00E9l\u00E9charger des donn\u00E9es",
    info = info,
    is_sub = TRUE,
    multi = TRUE
  )

}
