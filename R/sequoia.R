#' Rsequoia2 Main Interactive Workflow
#'
#' Launches the main interactive workflow for Rsequoia2, allowing the user to
#' select a project folder and choose among map layer creation or cartographic tools.
#'
#' @param overwrite `logical` If `TRUE`, file is overwritten.
#'
#' @return Invisibly returns `path`.
#' The function primarily calls other Rsequoia2 functions.
#'
#' @export
sequoia <- function(overwrite = FALSE) {

  actions <- list(
    "Selectionner dossier sequoia" = function() {

      p <- rstudioapi::selectDirectory(
        caption = "Selectionner dossier sequoia",
        path = getOption("seq_dir_path", getwd())
      )

      options(seq_dir_path = p)
    },
    "Creer MATRICE (vierge)" = function() {

      path <- getOption("seq_dir_path", getwd())
      if (is.null(path))
        cli::cli_abort("Veuillez d'abord selectionner un dossier sequoia.")

      id <- readline("Identifiant de la foret : ")

      create_matrice(dirname = path, id = id, overwrite = overwrite)

    },
    "Creer MATRICE (depuis PDF RP)" = function() menu_rp(path, overwrite),
    "Creer MATRICE (personne morale)" = function() menu_legal_entity(path, overwrite),
    "Telecharger PARCA" = function() seq_parca(path, overwrite = overwrite),
    "Creer UA & LIMITES" = function() {
      seq_parca_to_ua(path, overwrite = overwrite)
      seq_boundaries(path, overwrite = overwrite)
    },
    "Corriger UA & PARCELLES" = function() {
      seq_ua(path, overwrite = TRUE)
      seq_parcels(path, overwrite = TRUE)
    },
    "Telecharger DONNeES" = function() menu_data(path, overwrite),
    "Generer une synthese" = function() seq_summary(path, overwrite = overwrite)
  )

  repeat {

    cli::cli_h1("Menu Sequoia")

    path <- getOption("seq_dir_path")
    if (is.null(path)) {
      cli::cli_alert_warning("Aucun dossier selectionne.")
    } else {
      cli::cli_alert_info("Dossier selectionne: {.file {normalizePath(path)}}"
      )
    }

    choice <- utils::menu(names(actions))

    withCallingHandlers(

      tryCatch(
        actions[[choice]](),

        error = function(e) {
          cli::cli_alert_danger(e$message)
        }

      ),

      warning = function(w) {
        cli::cli_alert_warning(w$message)
        invokeRestart("muffleWarning")
      }

    )

  }
}

#' Interactive legal-entity menu
#'
#' Prompts the user for INSEE codes and owner name patterns, retrieves matching
#' legal-entity parcels, displays a short summary (area, owners, number of
#' parcels), and asks for confirmation before creating the matrice.
#'
#' @details
#' This function is interactive and intended for manual use only.
#'
#' @noRd
menu_rp <- function(path, overwrite){
  files <- character()
  repeat {
    last_path <- path
    f <- rstudioapi::selectFile(
      caption = "Selectionner Releve de propriete",
      path = getOption("last_pdf_path", path),
      filter = "PDF files (*.pdf)"
    )

    if (!nzchar(f)) break

    files <- c(files, f)
    base::options(last_pdf_path = dirname(f))

    another <- rstudioapi::showQuestion(
      title = "Select files",
      message = "Select another file?",
      cancel = "No"
    )

    if (!another) break
  }

  rp <- lapply(files, parse_rp)
  m <- do.call(rbind, lapply(rp, `[[`, "m"))
  m_all <- do.call(rbind, lapply(rp, `[[`, "m_all"))

  s <- sum(m$SURF_CA)

  cli::cli_h2("Summary")
  cli::cli_bullets(c(
    "Number of parcels: {nrow(m)}",
    "Total area: {format(round(s, 2), nsmall = 2)} ha"
  ))

  parca_geom <- get_parca(m$IDU, verbose = TRUE)
  parca <- merge(parca_geom[ , "IDU"], m) |> seq_normalize("parca")

  cli::cli_alert_info("Plotting parca...")

  tmap::tmap_mode("view")
  print(tmap::qtm(parca))

  switch(utils::menu(c("Confirm and continue", "Cancel")),
         {
           identifiant <- readline("Choose the forest identifiant: ")
           owner <- readline("Choose the forest owner: ")
           m$IDENTIFIANT <- identifiant
           m$OWNER <- owner
           seq_xlsx(
             x = list("MATRICE" = m),
             filename = file.path(path, paste0(identifiant, "_matrice.xlsx")),
             overwrite = overwrite
           )

           m_all$IDENTIFIANT <- identifiant
           m_all$OWNER <- owner
           seq_xlsx(
             x = list("MATRICE" = m_all),
             filename = file.path(path, paste0(identifiant, "_matrice_detail.xlsx")),
             overwrite = overwrite
           )
         }
  )

  return(NULL)

}

#' Interactive legal-entity menu
#'
#' Prompts the user for INSEE codes and owner name patterns, retrieves matching
#' legal-entity parcels, displays a short summary (area, owners, number of
#' parcels), and asks for confirmation before creating the matrice.
#'
#' @details
#' This function is interactive and intended for manual use only.
#'
#' @noRd
menu_legal_entity <- function(path, overwrite){
  x <- readline("Enter DEP/INSEE codes (comma-separated): ")
  y <- readline("Enter proprietaire search pattern (comma-separated): ")
  insee <- trimws(strsplit(x, ",")[[1]])
  prop <- trimws(strsplit(y, ",")[[1]])

  m <- get_legal_entity(insee)
  ms <- search_legal_entity(m, prop = prop)

  s <- sum(ms$SURF_CA)
  p <- unique(ms$PROPRIETAIRE)

  cli::cli_h2("Summary")
  cli::cli_bullets(c(
    "Number of parcels: {nrow(ms)}",
    "Total area: {format(round(s, 2), nsmall = 2)} ha",
    "{length(p)} owners : {paste(p, collapse = ', ')}"
  ))

  parca_geom <- get_parca(ms$IDU, verbose = TRUE)
  parca <- merge(parca_geom[ , "IDU"], ms) |> seq_normalize("parca")

  tmap::tmap_mode("view")
  print(tmap::qtm(parca))

  switch(utils::menu(c("Confirm and continue", "Cancel")),
         {
           identifiant <- readline("Choose the forest identifiant: ")
           ms$IDENTIFIANT <- identifiant
           seq_xlsx(
             x = list("MATRICE" = ms),
             filename = file.path(path, paste0(identifiant, "_matrice.xlsx")),
             overwrite = overwrite
           )
         }
  )

  return(NULL)

}

#' Interactive data menu
#'
#' Prompts the user for available data.
#'
#' @details
#' This function is interactive and intended for manual use only.
#'
#' @noRd
menu_data <- function(path, overwrite){

  cli::cli_h1("Menu data")

  seq_functions <- list(
    "Communes"      = function() try(seq_com(path, overwrite = overwrite)),
    "MNHN"          = function() try(seq_mnhn(path, overwrite = overwrite)),
    "Geology"       = function() try(seq_geol(path, overwrite = overwrite)),
    "Pedology"      = function() try(seq_pedology(path, overwrite = overwrite)),
    "Infra"         = function() try(seq_infra(path, overwrite = overwrite)),
    "Routes"        = function() try(seq_road(path, overwrite = overwrite)),
    "PRSF"          = function() try(seq_prsf(path, overwrite = overwrite)),
    "OLD"           = function() try(seq_old(path, overwrite = overwrite)),
    "Toponyme"      = function() try(seq_toponyme(path, overwrite = overwrite)),
    "Hydrology"     = function() try(seq_hydro(path, overwrite = overwrite)),
    "Vegetation"    = function() try(seq_vege(path, overwrite = overwrite)),
    "Climate"       = function() try(seq_climate(path, overwrite = overwrite)),
    "Contour lines" = function() try(seq_curves(path, overwrite = overwrite)),
    "IFN"           = function() try(seq_ifn(path, overwrite = overwrite)),
    "GPU"           = function() try(seq_gpu(path, overwrite = overwrite)),
    "Patrimony"     = function() try(seq_patrimony(path, overwrite = overwrite)),
    "Elevation"     = function() try(seq_elevation(path, overwrite = overwrite)),
    "Orthophoto"    = function() try(seq_ortho(path, overwrite = overwrite)),
    "Scan"          = function() try(seq_scan(path, overwrite = overwrite))
  )

  # 2. Actions reuse seq_functions
  actions <- c(
    list(
      "All" = function() {
        lapply(seq_functions, function(f) try(f()))
      }
    ),
    seq_functions
  )

  # 3. Menu + execution
  choice <- utils::menu(names(actions))
  actions[[choice]]()

}
