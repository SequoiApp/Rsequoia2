#' Open the data download menu
#'
#' Internal menu used to download or generate geographic and environmental data
#' for the currently selected sequoia2 directory.
#'
#' @param state Project menu selection state.
#' @keywords internal
#' @noRd
menu_data <- function(state) {

  path <- seq_get_path(state)
  info <- function() seq_show_selection(state$path)

  base_actions <- list(
    "Communes"             = function() seq_com(path),
    "MNHN"                 = function() seq_mnhn(path),
    "G\u00E9ologie"        = function() seq_geol(path),
    "P\u00E9dologie"       = function() seq_pedology(path),
    "Infra"                = function() seq_infra(path),
    "Route"                = function() seq_road(path),
    # "Route cad."           = function() seq_roadway(path),
    "PRSF"                 = function() seq_prsf(path),
    "OLD"                  = function() seq_old(path),
    "Toponyme"             = function() seq_toponyme(path),
    "Hydrologie"           = function() seq_hydro(path),
    "V\u00E9g\u00E9tation" = function() seq_vege(path),
    "Accessibilit\u00E9"   = function() seq_access(path),
    "Meteo-France"         = function() seq_meteo_france(path),
    "Drias"                = function() seq_drias(path),
    "Courbes niveau"       = function() seq_curves(path),
    "IFN"                  = function() seq_ifn(path),
    "GPU"                  = function() seq_gpu(path),
    "Patrimoine"           = function() seq_patrimony(path),
    "Altim\u00E9trie"      = function() seq_altimetry(path),
    "Orthophoto"           = function() seq_ortho(path)
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
#' for the selected geographic zone and output directory.
#'
#' @param state Toolbox menu selection state, including the geographic zone.
#' @keywords internal
#' @noRd
menu_toolbox_data <- function(state) {

  path <- seq_get_path(state)
  x <- state$zone
  if (is.null(x)) {
    cli::cli_abort("Veuillez d'abord s\u00E9lectionner une emprise.")
  }
  info <- function() seq_show_selection(state$path)
  base_actions <- list(
    "Communes"             = function() fetch_com(x, path),
    "MNHN"                 = function() fetch_mnhn(x, path),
    "G\u00e9ologie"        = function() fetch_geol(x, path),
    "P\u00E9dologie"       = function() fetch_pedology(x, path),
    "Infra"                = function() fetch_infra(x, path),
    "Route"                = function() fetch_road(x, path),
    "PRSF"                 = function() fetch_prsf(x, path),
    "OLD"                  = function() fetch_old(x, path),
    "Toponyme"             = function() fetch_toponyme(x, path),
    "Hydrologie"           = function() fetch_hydro(x, path),
    "V\u00E9g\u00E9tation" = function() fetch_vege(x, path),
    "Accessibilit\u00E9"   = function() fetch_access(x, path),
    "Meteo-France"         = function() fetch_meteo_france(x, path),
    # "Drias"                = function() seq_drias(path),
    "Courbes niveau"       = function() fetch_curves(x, path),
    "IFN"                  = function() fetch_ifn(x, path),
    "GPU"                  = function() fetch_gpu(x, path),
    "Patrimoine"           = function() fetch_patrimony(x, path),
    "Altim\u00E9trie"      = function() fetch_altimetry(x, path),
    "Orthophoto"           = function() fetch_ortho(x, path)
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
