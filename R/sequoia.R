#' Rsequoia2 Main Interactive Workflow
#'
#' Launches the main interactive workflow for Rsequoia2, allowing the user to
#' select a project folder and choose among map layer creation or cartographic tools.
#'
#' @param path `character` Optional. Path to the project folder. If not
#' provided, the user will be prompted to enter it interactively.
#' @param overwrite `logical` If `TRUE`, file is overwritten.
#'
#' @details
#' The function opens interactive menus to guide the user through tasks such as:
#' \itemize{
#'   \item Creating map layers (MATRICE, PARCA, UA, UA finalization)
#'   \item Cartographic tools (currently not implemented)
#' }
#' The user must make selections at each step; cancelling or leaving a selection
#' empty will stop the function.
#'
#' @return Invisibly returns `path`.
#' The function primarily calls other Rsequoia2 functions.
#'
#' @export
sequoia <- function(path = NULL, overwrite = FALSE) {

  if (is.null(path)){
    path <- if (rstudioapi::isAvailable()) {
      rstudioapi::selectDirectory()
    } else {
      readline("Enter directory path: ")
    }
  }

  if (is.null(path) | path == "") cli_abort("No selection.")

  cli_alert_info("You choose folder: {.path {normalizePath(path)}}")

  # MAIN MENU ---
  actions <- list(
    "Create MATRICE" = function() {
      identifiant <- readline("Choose the forest identifiant: ")
      create_matrice(dirname = path, id = identifiant, overwrite = overwrite)
    },
    "Create MATRICE (legal entity)" = function() menu_legal_entity(path, overwrite),
    "Download PARCA" = function() seq_parca(path, overwrite = overwrite),
    "Create UA & BOUNDARIES" = function() {
      seq_parca_to_ua(path, overwrite = overwrite)
      seq_boundaries(path, overwrite = overwrite)
    },
    "Correct UA & PARCELS" = function() {
      seq_ua(path, overwrite = TRUE)
      seq_parcels(path, overwrite = TRUE)
    },
    "Download DATA" = function() menu_data(path, overwrite = overwrite)
  )

  choice <- utils::menu(names(actions))
  actions[[choice]]()

  return(invisible(path))
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

  s <- sum(ms$SURF_CA / 10000)
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

menu_data <- function(path, overwrite){

  seq_functions <- list(
    "Communes"      = function() seq_com(path, overwrite = overwrite),
    "MNHN"          = function() seq_mnhn(path, overwrite = overwrite),
    "Geology"       = function() seq_geol(path, overwrite = overwrite),
    "Pedology"      = function() seq_pedology(path, overwrite = overwrite),
    "Infra"         = function() seq_infra(path, overwrite = overwrite),
    "Routes"        = function() seq_road(path, overwrite = overwrite),
    "PRSF"          = function() seq_prsf(path, overwrite = overwrite),
    "Toponyme"      = function() seq_toponyme(path, overwrite = overwrite),
    "Hydrology"     = function() seq_hydro(path, overwrite = overwrite),
    "Vegetation"    = function() seq_vege(path, overwrite = overwrite),
    "Contour lines" = function() seq_curves(path, overwrite = overwrite),
    "IFN"           = function() seq_ifn(path, overwrite = overwrite),
    "GPU"           = function() seq_gpu(path, overwrite = overwrite),
    "Patrimony"     = function() seq_patrimony(path, overwrite = overwrite),
    "Elevation"     = function() seq_elevation(path, overwrite = overwrite),
    "Orthophoto"    = function() seq_ortho(path, overwrite = overwrite),
    "Scan"          = function() seq_scan(path, overwrite = overwrite)
  )

  # 2. Actions reuse seq_functions
  actions <- c(
    list(
      "All" = function() {
        lapply(seq_functions, function(f) f())
      }
    ),
    seq_functions
  )

  # 3. Menu + execution
  choice <- utils::menu(names(actions))
  actions[[choice]]()

}
