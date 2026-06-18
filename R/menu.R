# Main menu ----

#' Launch the main sequoia2 menu
#'
#' Opens the interactive command-line menu for the sequoia2 workflow.
#'
#' The selected directory is stored in the R option `seq_dir_path` and reused by
#' the other menus and processing functions.
#'
#'
#' @export
sequoia2 <- function() {

  seq_path_info <- function() {
    path <- seq_get_path()

    if (is.null(path)) {
      return("Aucun dossier selectionne.")
    }

    cli::format_inline("Dossier selectionne : {.path {path}}")
  }

  select_seq_path <- function() {
    path <- rstudioapi::selectDirectory(
      caption = "Selectionner un dossier sequoia2",
      path = getOption("seq_dir_path", getwd())
    )

    if (is.null(path) || !nzchar(path)) {
      return(invisible(NULL))
    }

    options(seq_dir_path = path)
  }

  update_folder <- function() {
    p <- rstudioapi::selectDirectory(
      caption = "Selectionner ancien dossier sequoia"
    )

    if (nzchar(p)) {
      seq1_update(p)
    }
  }

  rename_folder <- function() {
    p <- rstudioapi::selectDirectory(
      caption = "Selectionner ancien dossier sequoia"
    )

    old_id <- readline("Ancien identifiant : ")
    new_id <- readline("Nouvel identifiant : ")

    if (nzchar(p) && nzchar(old_id) && nzchar(new_id)) {
      seq_dir_rename(p, old_id, new_id)
    }

  }

  ask_help <- function() {
    utils::browseURL("https://github.com/SequoiApp/Rsequoia2/issues")
  }

  website <- function() {
    utils::browseURL("https://sequoiapp.github.io/Rsequoia2/index.html")
  }

  actions <- list(
    "Choisir un dossier" = select_seq_path,
    "Travailler sur le dossier" = menu_sequoia,
    "Migrer un ancien dossier" = update_folder,
    "Renommer un dossier" = rename_folder,
    "Signaler un probleme" = ask_help,
    "Site web / Documentation" = website
  )

  seq_run_menu(
    actions = actions,
    info = seq_path_info
  )

}

# Other menus ----*

#' Open the data download menu
#'
#' Internal menu used to download or generate geographic and environmental data
#' for the currently selected sequoia2 directory.
#'
#' @keywords internal
#' @noRd
menu_data <- function(){

  path <- seq_get_path()
  info <- cli::format_inline("Dossier selectionne : {.path {path}}")

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
    "Geologie"       = function() seq_geol(path),
    "Pedologie"      = function() seq_pedology(path),
    "Infra"          = function() seq_infra(path),
    "Route"          = function() seq_road(path),
    "Route cad."     = function() seq_roadway(path),
    "PRSF"           = function() seq_prsf(path),
    "OLD"            = function() seq_old(path),
    "Toponyme"       = function() seq_toponyme(path),
    "Hydrologie"     = function() seq_hydro(path),
    "Vegetation"     = function() seq_vege(path),
    "Accessibilite"  = function() seq_access(path),
    "Meteo-France"   = function() seq_meteo_france(path),
    "Drias"          = function() seq_drias(path),
    "Courbes niveau" = function() seq_curves(path),
    "IFN"            = function() seq_ifn(path),
    "GPU"            = function() seq_gpu(path),
    "Patrimoine"     = function() seq_patrimony(path),
    "Altimetrie"     = function() seq_altimetry(path),
    "Orthophoto"     = function() seq_ortho(path)
  )

  actions <- c(
    "All" = function() invisible(lapply(base_actions, seq_run_action)),
    base_actions
  )

  seq_run_menu(
    actions = actions,
    info = info,
    is_sub = TRUE,
    multi = TRUE
  )

}

#' Open the cadastral matrix menu
#'
#' Internal menu used to create or import cadastral matrices for the selected
#' sequoia2 directory.
#'
#' @keywords internal
#' @noRd
menu_matrice <- function(){

  path <- seq_get_path()
  info <- cli::format_inline("Dossier selectionne : {.path {path}}")

  blank_matrice <- function() {
    id <- readline("Identifiant de la foret : ")
    create_matrice(dirname = path, id = id, overwrite = FALSE)
  }

  actions <- list(
    "Matrice vierge" = blank_matrice,
    "Matrice releve de propriete" = menu_rp,
    "Matrice personne morale" = menu_pm
  )

  seq_run_menu(
    actions = actions,
    info = info,
    is_sub = TRUE
  )

}

#' Open the main project menu
#'
#' Internal menu used to run the main sequoia2 processing workflow for the
#' selected directory.
#'
#' directory is selected.
#'
#' @keywords internal
#' @noRd
menu_sequoia <- function() {

  path <- seq_get_path()
  if (is.null(path) || !nzchar(path)) {
    cli::cli_warn("Aucun dossier sequoia2 selectionne.")
    return(invisible(NULL))
  }

  info <- cli::format_inline("Dossier selectionne : {.path {path}}")

  download_parca <- function() seq_parca(seq_get_path())
  create_ua <- function() seq_parca_to_ua(seq_get_path())
  correct_ua <- function() seq_ua(seq_get_path())
  aggregate_ua <- function() {

    cli::cli_alert_warning(
      "Cette operation peut ecraser des fichiers existants."
    )

    answer <- readline(
      cli::format_inline(
        "Voulez-vous continuer ? [{.strong o/N}] "
      )
    )

    overwrite <- tolower(trimws(answer)) %in% c("o", "oui", "y", "yes")

    if (!overwrite) {
      cli::cli_alert_info("Operation annulee. Aucun fichier n'a ete ecrase.")
      return(invisible(FALSE))
    }

    path <- seq_get_path()

    seq_boundaries(path, overwrite = overwrite)
    seq_parcels(path, overwrite = overwrite)
    seq_occupation(path, overwrite = overwrite)

    invisible(TRUE)
  }
  sumarize_ua <- function() seq_summary(seq_get_path())

  actions <- list(
    "Generer une MATRICE CADASTRALE" = menu_matrice,
    "Telecharger PARCA" = download_parca,
    "Telecharger DONNEES" = menu_data,
    "Generer les UA" = create_ua,
    "Corriger les UA" = correct_ua,
    "Aggreger les UA" = aggregate_ua,
    "Synthetiser les UA" = sumarize_ua
  )

  seq_run_menu(
    actions = actions,
    info = info,
    is_sub = TRUE
  )

}

