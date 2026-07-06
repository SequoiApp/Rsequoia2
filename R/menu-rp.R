#' Interactive RP menu
#'
#' Prompts the user to select one or several RP PDF files, parses them,
#' writes matrice outputs, retrieves PARCA geometry and previews it.
#'
#' @details
#' This function is interactive and intended for manual use only.
#'
#' @noRd
menu_rp <- function() {

  identifiant <- readline("Choose the forest identifiant: ")
  owner <- readline("Choose the forest owner: ")

  path <- seq_get_path()
  files <- select_pdf_files(path)

  if (length(files) == 0) {
    cli::cli_alert_info("Aucun fichier selectionne.")
    return(invisible(NULL))
  }

  rp <- lapply(files, parse_rp)
  m <- do.call(rbind, lapply(rp, `[[`, "m"))
  m_all <- do.call(rbind, lapply(rp, `[[`, "m_all"))

  m[[seq_field("identifier")$name]] <- identifiant
  m[[seq_field("owner")$name]] <- owner
  m <- seq_normalize(m, "parca")

  cli::cli_h2("Summary")
  cli::cli_bullets(c(
    "Number of parcels: {nrow(m)}",
    "Total area: {format(round(sum(m$SURF_CA), 2), nsmall = 2)} ha"
  ))

  cli::cli_text("")
  cli::cli_alert_info("Affichage PARCA...")

  parca_geom <- tryCatch(
    get_parca(m$IDU, verbose = TRUE),
    error = \(e) {
      cli::cli_alert_danger("Failed to retrieve PARCA geometry: {conditionMessage(e)}")
      NULL
    }
  )

  if (!is.null(parca_geom)) {
    suppressMessages(suppressWarnings(tmap::tmap_mode("view")))
    print(tmap::qtm(parca_geom))
  }

  number <- seq_field("number")$name
  section <- seq_field("section")$name
  prefix <- seq_field("prefix")$name
  insee <- seq_field("insee")$name

  m <- m[order(m[[insee]], m[[prefix]], m[[section]], m[[number]]), ]
  m_all <- m_all[order(m_all[[insee]], m_all[[prefix]], m_all[[section]], m_all[[number]]), ]

  seq_xlsx(
    MATRICE = m,
    filename = file.path(path, paste0(identifiant, "_matrice.xlsx")),
    overwrite = FALSE,
    verbose = TRUE
  )

  seq_xlsx(
    MATRICE_DETAIL = m_all,
    filename = file.path(path, paste0(identifiant, "_matrice_detail.xlsx")),
    overwrite = FALSE,
    verbose = TRUE
  )

}

#' Ask user to select several PDF files
#' @noRd
select_pdf_files <- function(path) {
  files <- character()

  repeat {
    f <- rstudioapi::selectFile(
      caption = "Selectionner un releve de propriete",
      path = getOption("last_pdf_path", path),
      filter = "PDF files (*.pdf)"
    )

    if (!nzchar(f)) {
      break
    }

    if (f %in% files) {
      rstudioapi::showDialog(
        title = "Fichier deja selectionne",
        message = paste0(
          "Ce fichier est deja selectionne : ",
          basename(f)
        )
      )
      next
    }

    files <- c(files, f)
    options(last_pdf_path = dirname(f))

    n <- length(files)
    msg <- paste0(
      n, " fichier", if (n > 1) "s" else "", " selectionne", if (n > 1) "s" else "", " : \n",
      paste("-", basename(files), collapse = "\n")
    )

    another <- rstudioapi::showQuestion(
      title = "Selection des fichiers",
      message = paste0(msg, "\n\nVoulez-vous selectionner un autre fichier ?"),
      ok = "Oui",
      cancel = "Lancer la conversion"
    )

    if (!another) {
      break
    }
  }

  files
}
