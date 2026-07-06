#' Interactive PM menu
#'
#' Prompts the user to found and download matrice from legal entity
#'
#' @details
#' This function is interactive and intended for manual use only.
#'
#' @noRd
menu_pm <- function() {

  path <- seq_get_path()

  read_csv <- function(prompt) {
    x <- readline(prompt)
    x <- strsplit(x, ",", fixed = TRUE)[[1]]
    x <- trimws(x)
    x[nzchar(x)]
  }

  search_by <- seq_select(
    c("Communale", "Departementale", "Regionale"),
    info = "Rechercher une personne morale a l'echelle :"
  )

  dep <- NULL
  insee <- NULL

  if (search_by == 1) {
    insee <- read_csv("Code(s) INSEE, separes par des virgules : ")
  }

  if (search_by == 2) {
    dep <- read_csv("Code(s) departement, separes par des virgules : ")
  }

  if (search_by == 3) {
    cog <- get_cog(verbose = FALSE)

    selected_reg <- seq_select(cog$reg$NCC_REG, multi = TRUE)

    reg_code <- cog$reg$REG[selected_reg]
    dep <- cog$dep$DEP[cog$dep$REG %in% reg_code]
  }

  cli::cli_alert_info("Plusieurs mots peuvent etre combines, par exemple : GF, ETANGS")
  cli::cli_alert_info("La casse et les caractere speciaux sont ignores pour la recherche")
  pattern <- read_csv("Nom(s), separes par des virgules : ")

  if (length(pattern) == 0) {
    cli::cli_alert_warning("Aucun nom saisi.")
    return(invisible(NULL))
  }

  found <- search_pm(pattern = pattern, dep = dep, insee = insee)
  if (nrow(found) == 0) {
    cli::cli_alert_warning("Aucune personne morale trouvee.")
    return(invisible(NULL))
  }

  selected_pm <- select_pm(found)
  if (length(selected_pm) == 0) {
    cli::cli_alert_warning("Aucune personne morale selectionnee.")
    return(invisible(NULL))
  }

  pm <- get_pm(selected_pm, dep = dep, insee = insee) |>
    normalize_pm()

  m <- pm$m
  m_detail <- pm$m_detail

  cli::cli_alert_success("Personne morale recuperee.")
  cli::cli_alert_info("Veuillez indiquer l'identifiant du dossier.")
  identifiant <- readline("Identifiant du dossier : ")

  id_field <- seq_field("identifier")$name
  m[[id_field]] <- identifiant
  m_detail[[id_field]] <- identifiant

  cli::cli_text("")
  cli::cli_alert_info("Affichage PARCA...")
  cli::cli_text("")

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

  seq_xlsx(
    MATRICE = m,
    filename = file.path(path, paste0(identifiant, "_matrice.xlsx")),
    overwrite = FALSE,
    verbose = TRUE
  )

  seq_xlsx(
    MATRICE_DETAIL = m_detail,
    filename = file.path(path, paste0(identifiant, "_matrice_detail.xlsx")),
    overwrite = FALSE,
    verbose = TRUE
  )
}
