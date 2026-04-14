# Helpers ----

#' Get current Sequoia folder
#' @noRd
get_seq_path <- function() {
  getOption("seq_dir_path", NULL)
}

#' Require current Sequoia folder
#' @noRd
require_seq_path <- function() {
  path <- get_seq_path()

  if (is.null(path) || !nzchar(path)) {
    cli::cli_abort("Veuillez d'abord selectionner un dossier sequoia.")
  }

  path
}

#' Select and store Sequoia folder
#' @noRd
select_seq_path <- function() {
  p <- rstudioapi::selectDirectory(
    caption = "Selectionner dossier sequoia",
    path = getOption("seq_dir_path", getwd())
  )

  if (nzchar(p)) {
    options(seq_dir_path = p)
  }

  invisible(p)
}

#' Run an interactive action with consistent error/warning handling
#' @noRd
run_interactive <- function(expr) {
  withCallingHandlers(
    tryCatch(
      force(expr),
      error = function(e) {
        cat(conditionMessage(e), "\n")
        invisible(NULL)
      }
    ),
    warning = function(w) {
      cat(conditionMessage(w), "\n")
      invokeRestart("muffleWarning")
    }
  )
}

#' Generic menu runner
#' @noRd
run_menu <- function(actions,
                     title = "Menu",
                     show_path = FALSE,
                     repeat_menu = FALSE) {
  repeat {
    cli::cli_h1(title)

    if (repeat_menu) {
      cli::cli_alert_info("Appuyez sur {.key Esc} pour quitter le menu.")
    }

    if (show_path) {
      path <- get_seq_path()

      if (is.null(path) || !nzchar(path)) {
        cli::cli_alert_warning("Aucun dossier selectionne.")
      } else {
        cli::cli_alert_info(
          "Dossier selectionne: {.file {normalizePath(path)}}"
        )
      }
    }

    choice <- utils::menu(names(actions))

    if (choice == 0) {
      break
    }

    run_interactive(actions[[choice]]())

    if (!repeat_menu) {
      break
    }
  }

  invisible(NULL)
}

#' Parse comma-separated user input
#' @noRd
read_comma_separated_input <- function(prompt) {
  x <- readline(prompt)
  x <- trimws(strsplit(x, ",", fixed = TRUE)[[1]])
  x[nzchar(x)]
}

#' Print matrice summary
#' @noRd
print_matrice_summary <- function(x, owners = NULL) {
  s <- sum(x$SURF_CA)

  bullets <- c(
    "Number of parcels: {nrow(x)}",
    "Total area: {format(round(s, 2), nsmall = 2)} ha"
  )

  if (!is.null(owners)) {
    bullets <- c(
      bullets,
      "{length(owners)} owners : {paste(owners, collapse = ', ')}"
    )
  }

  cli::cli_h2("Summary")
  cli::cli_bullets(bullets)

  invisible(NULL)
}

#' Write matrice xlsx
#' @noRd
write_matrice_xlsx <- function(x,
                               path,
                               identifiant,
                               overwrite = FALSE,
                               owner = NULL,
                               filename = NULL) {
  if (is.null(filename)) {
    filename <- paste0(identifiant, "_matrice.xlsx")
  }

  x$IDENTIFIANT <- identifiant

  if (!is.null(owner)) {
    x$OWNER <- owner
  }

  out <- file.path(path, filename)

  seq_xlsx(
    x = list("MATRICE" = x),
    filename = out,
    overwrite = overwrite
  )

  invisible(out)
}

#' Preview PARCA in tmap
#' @noRd
preview_parca <- function(parca) {
  cli::cli_alert_info("Affichage PARCA...")
  tmap::tmap_mode("view")
  print(tmap::qtm(parca))
  invisible(parca)
}

#' Ask user to select several PDF files
#' @noRd
select_pdf_files <- function(path) {
  files <- character()

  repeat {
    f <- rstudioapi::selectFile(
      caption = "Selectionner Releve de propriete",
      path = getOption("last_pdf_path", path),
      filter = "PDF files (*.pdf)"
    )

    if (!nzchar(f)) {
      break
    }

    files <- c(files, f)
    options(last_pdf_path = dirname(f))

    another <- rstudioapi::showQuestion(
      title = "Selection des fichiers",
      message = "Selectionner un autre fichier ?",
      cancel = "Termine"
    )

    if (!another) {
      break
    }
  }

  unique(files)
}

#' Ask for multiple numbered choices
#' @noRd
read_multi_selection <- function(labels) {
  cat(
    paste(sprintf("%2d. %s", seq_along(labels), labels), collapse = "\n"),
    "\n"
  )

  input <- readline("Selection : ")

  idx <- suppressWarnings(
    as.integer(trimws(strsplit(input, ",", fixed = TRUE)[[1]]))
  )

  idx <- idx[!is.na(idx)]
  idx <- idx[idx >= 1 & idx <= length(labels)]
  unique(idx)
}


# Main menu ----

#' Build main Sequoia actions
#' @noRd
make_sequoia_actions <- function(overwrite = FALSE) {
  list(
    "Selectionner dossier sequoia" = function() {
      select_seq_path()
    },

    "Creer MATRICE (vierge)" = function() {
      path <- require_seq_path()
      id <- readline("Identifiant de la foret : ")
      create_matrice(dirname = path, id = id, overwrite = overwrite)
    },

    "Creer MATRICE (depuis PDF RP)" = function() {
      menu_rp(require_seq_path(), overwrite = overwrite)
    },

    "Creer MATRICE (personne morale)" = function() {
      menu_legal_entity(require_seq_path(), overwrite = overwrite)
    },

    "Telecharger PARCA" = function() {
      seq_parca(require_seq_path(), overwrite = overwrite)
    },

    "Creer UA & LIMITES" = function() {
      path <- require_seq_path()
      seq_parca_to_ua(path, overwrite = overwrite)
      seq_boundaries(path, overwrite = overwrite)
    },

    "Corriger UA & PARCELLES" = function() {
      path <- require_seq_path()
      seq_ua(path, overwrite = TRUE)
      seq_parcels(path, overwrite = TRUE)
    },

    "Telecharger DONNEES" = function() {
      menu_data(require_seq_path(), overwrite = overwrite)
    },

    "Generer synthese UA" = function() {
      seq_summary(require_seq_path())
    },

    "Mettre a jour un ancien dossier" = function() {
      p <- rstudioapi::selectDirectory(
        caption = "Selectionner ancien dossier sequoia"
      )

      if (nzchar(p)) {
        seq1_update(p, overwrite = overwrite)
      }
    }
  )
}

#' Rsequoia2 Main Interactive Workflow
#'
#' Launches the main interactive workflow for Rsequoia2, allowing the user to
#' select a project folder and choose among map layer creation or cartographic tools.
#'
#' @param overwrite `logical` If `TRUE`, file is overwritten.
#'
#' @return Invisibly returns current Sequoia path.
#' The function primarily calls other Rsequoia2 functions.
#'
#' @export
sequoia2 <- function(overwrite = FALSE) {
  run_menu(
    actions = make_sequoia_actions(overwrite = overwrite),
    title = "Menu Sequoia",
    show_path = TRUE,
    repeat_menu = TRUE
  )

  invisible(get_seq_path())
}


# RP menu ----

#' Interactive RP menu
#'
#' Prompts the user to select one or several RP PDF files, parses them,
#' writes matrice outputs, retrieves PARCA geometry and previews it.
#'
#' @details
#' This function is interactive and intended for manual use only.
#'
#' @noRd
menu_rp <- function(path, overwrite = FALSE) {
  files <- select_pdf_files(path)

  if (length(files) == 0) {
    cli::cli_alert_info("Aucun fichier selectionne.")
    return(invisible(NULL))
  }

  rp <- lapply(files, parse_rp)
  m <- do.call(rbind, lapply(rp, `[[`, "m"))
  m_all <- do.call(rbind, lapply(rp, `[[`, "m_all"))

  print_matrice_summary(m)

  identifiant <- readline("Choose the forest identifiant: ")
  owner <- readline("Choose the forest owner: ")

  m_path <- write_matrice_xlsx(
    x = m,
    path = path,
    identifiant = identifiant,
    owner = owner,
    overwrite = overwrite
  )

  write_matrice_xlsx(
    x = m_all,
    path = path,
    identifiant = identifiant,
    owner = owner,
    filename = paste0(identifiant, "_matrice_detail.xlsx"),
    overwrite = overwrite
  )

  parca_geom <- tryCatch(
    get_parca(m$IDU, verbose = TRUE),
    error = function(e) {
      cli::cli_abort(c(
        "x" = "Failed to retrieve PARCA geometry: {conditionMessage(e)}",
        "i" = "Please correct those IDU in: {.path {m_path}}"
      ))
    }
  )

  parca <- merge(parca_geom[, "IDU"], m) |> seq_normalize("parca")
  preview_parca(parca)

  invisible(NULL)
}


# Legal entity menu ----

#' Interactive legal-entity menu
#'
#' Prompts the user for INSEE codes and owner name patterns, retrieves matching
#' legal-entity parcels, displays a short summary and asks for confirmation
#' before creating the matrice.
#'
#' @details
#' This function is interactive and intended for manual use only.
#'
#' @noRd
menu_legal_entity <- function(path, overwrite = FALSE) {
  code <- read_comma_separated_input("Enter DEP/INSEE codes (comma-separated): ")
  prop <- read_comma_separated_input("Enter proprietaire search pattern (comma-separated): ")

  m <- get_legal_entity(code)
  ms <- search_legal_entity(m, prop = prop)

  print_matrice_summary(ms, owners = unique(ms$PROPRIETAIRE))

  parca_geom <- get_parca(ms$IDU, verbose = TRUE)
  parca <- merge(parca_geom[, "IDU"], ms) |> seq_normalize("parca")
  preview_parca(parca)

  choice <- utils::menu(c("Confirm and continue", "Cancel"))

  if (choice == 1) {
    identifiant <- readline("Choose the forest identifiant: ")

    write_matrice_xlsx(
      x = ms,
      path = path,
      identifiant = identifiant,
      overwrite = overwrite
    )
  }

  invisible(NULL)
}


# Data menu ----

#' Build data actions
#' @noRd
make_data_actions <- function(path, overwrite = FALSE) {
  fns <- list(
    "Communes"       = seq_com,
    "MNHN"           = seq_mnhn,
    "Geologie"       = seq_geol,
    "Pedologie"      = seq_pedology,
    "Infra"          = seq_infra,
    "Route"          = seq_road,
    "Route cad."     = seq_roadway,
    "PRSF"           = seq_prsf,
    "OLD"            = seq_old,
    "Toponyme"       = seq_toponyme,
    "Hydrologie"     = seq_hydro,
    "Vegetation"     = seq_vege,
    "Accessibilite"  = seq_access,
    "Meteo-France"   = seq_meteo_france,
    "Drias"          = seq_drias,
    "Courbes niveau" = seq_curves,
    "IFN"            = seq_ifn,
    "GPU"            = seq_gpu,
    "Patrimoine"     = seq_patrimony,
    "Altimetrie"     = seq_elevation,
    "Orthophoto"     = seq_ortho,
    "Scan"           = seq_scan
  )

  actions <- lapply(fns, function(fn) {
    force(fn)
    function() fn(path, overwrite = overwrite)
  })

  names(actions) <- names(fns)
  actions
}

#' Interactive data menu
#'
#' Prompts the user for available data.
#'
#' @details
#' This function is interactive and intended for manual use only.
#'
#' @noRd
menu_data <- function(path, overwrite = FALSE) {
  seq_actions <- make_data_actions(path, overwrite = overwrite)
  labels <- names(seq_actions)

  actions <- c(
    list(
      "All" = function() {
        for (f in seq_actions) {
          run_interactive(f())
        }
      },

      "Multi" = function() {
        cli::cli_alert_info(
          "Entrez les numeros separes par des virgules (ex: {.val 1,3,5})"
        )

        idx <- read_multi_selection(labels)

        if (length(idx) == 0) {
          cli::cli_alert_warning("Aucune selection valide.")
          return(invisible(NULL))
        }

        for (i in idx) {
          run_interactive(seq_actions[[i]]())
        }
      }
    ),
    seq_actions
  )

  run_menu(
    actions = actions,
    title = "Menu data",
    repeat_menu = FALSE
  )

  invisible(NULL)
}
