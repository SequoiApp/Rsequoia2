#' Open the cadastral matrix menu
#'
#' Internal menu used to create or import cadastral matrices for the selected
#' sequoia2 directory.
#'
#' @param state Menu selection state shared with matrix submenus.
#' @keywords internal
#' @noRd
menu_matrice <- function(state) {

  path <- seq_get_path(state)
  info <- function() seq_show_selection(state$path)

  blank_matrice <- function() {
    id <- readline("Identifiant de la for\u00EAt : ")
    create_matrice(dirname = path, id = id, overwrite = FALSE)
  }

  from_aoi <- function(state){

    x <- seq_select_zone(state)
    if (is.null(x)) {
      return(invisible(NULL))
    }

    cli::cli_alert_info("T\u00E9l\u00E9chargement PARCA...")

    parca_geom <- tryCatch({
      # get_wfs instead of get_apicarto_cadastre to avoid body char limit
      #`Error: required resulting string length 43260 is greater than maximal 8192`
      parca <- happign::get_wfs(
        x = x,
        layer = "CADASTRALPARCELS.PARCELLAIRE_EXPRESS:parcelle",
        predicate = happign::intersects()
      )
      get_parca(parca$idu, lieu_dit = TRUE, verbose = TRUE)
      },
      error = \(e) {
        cli::cli_alert_danger("Failed to retrieve PARCA geometry: {conditionMessage(e)}")
        NULL
      }
    )

    if (!is.null(parca_geom)) {
      suppressMessages(suppressWarnings(tmap::tmap_mode("view")))
      print(tmap::qtm(parca_geom))
    }

    identifiant <- readline("Choisir un IDENTIFIANT: ")
    owner <- readline("Choisir un PROPRIETAIRE: ")

    m <- parca_geom |> sf::st_drop_geometry()
    m[[seq_field("identifier")$name]] <- identifiant
    m[[seq_field("owner")$name]] <- owner

    seq_xlsx(
      MATRICE = m,
      filename = file.path(path, paste0(identifiant, "_matrice.xlsx")),
      overwrite = FALSE,
      verbose = TRUE
    )

  }

  actions <- list(
    "Matrice vierge" = blank_matrice,
    "Matrice relev\u00E9 de propri\u00E9t\u00E9" = function() menu_rp(state),
    "Matrice personne morale" = function() menu_pm(state),
    "Matrice depuis une emprise" = function() from_aoi(state)
  )

  seq_run_menu(
    actions = actions,
    info = info,
    is_sub = TRUE
  )

}
