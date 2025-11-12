#' Download and cache cadastral data for legal entities ("Personnes Morales”)
#'
#' This function downloads the official "Personnes Morales” datasets published
#' by the French Ministry of Economy and Finance. These datasets contain
#' cadastral and property information (parcels and buildings) owned by
#' legal entities. The data are automatically cached locally to avoid
#' repeated downloads.
#'
#' @param dep `character`; Department code(s) (see [happign::dep_2025]).
#' @param cache `character`; Storage directory. Defaults to the user cache
#' directory (see [tools::R_user_dir()]).
#' @param verbose `logical` If `TRUE`, display messages.
#'
#' @importFrom archive archive_extract
#' @importFrom tools R_user_dir
#' @importFrom cli cli_alert_info cli_alert_success
#'
#' @return `character`;
#'
#' @examples
#' \dontrun{
#' download_legal_entity(c(29, "08"))
#' }
#'
#' @export
download_legal_entity <- function(dep, cache = NULL, verbose = TRUE) {
  # Helpers ----
  pad_right <- function(x, width) gsub(" ", "0", sprintf(paste0("%-", width, "s"), x))

  if (is.null(cache)){
    cache <- tools::R_user_dir("Rsequoia2", which = "cache")
    dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  }

  year <- 2025
  base_url <- paste0(
    "https://data.economie.gouv.fr/api/datasets/1.0/",
    "fichiers-des-locaux-et-des-parcelles-des-personnes-morales/attachments"
  )

  urls <- sprintf(
    "%s/fichier_des_parcelles_situation_%d_dpts_%s_zip",
    base_url, year, c("01_a_56", "57_a_976")
  )

  dep_chr <- pad_right(dep, 3)
  pattern <- paste("PM", format(Sys.Date(), "%y"), "NB", dep_chr, sep = "_")
  is_download <- length(list.files(cache, pattern = pattern, recursive = TRUE)) > 0

  if (!is_download) {
    if (verbose) cli::cli_alert_info("Téléchargement des données Personnes Morales...")
    invisible(lapply(urls, \(x) archive::archive_extract(x, dir = cache)))
    cli::cli_alert_success("Données disponibles dans {.path {normalizePath(cache)}}")
  }

  return(invisible(cache))
}

#' Create a forest matrice for legal entity from insee code
#'
#' Generates a forest matrice used to store general forest
#' information (e.g., `IDENTIFIANT`, `PROPRIETAIRE`) and cadastral attributes
#' (`CODE_INSEE`, `PREFIXE`, `SECTION`, `NUMERO`, `LIEU_DIT`) for legal
#' entity.
#'
#' @inheritParams download_legal_entity
#'
#' @param x `character`; Code(s) INSEE or code(s) department (see
#' [happign::com_2025] or [happign::dep_2025])
#'
#' @details
#' Matrice from `get_legal_entity` add `tx_boisee` information which is the
#' percentage of land considered as forest :
#' - `"L"`:  Landes
#' - `"LB"`: Landes Boisées
#' - `"B"`:  Bois
#' - `"BF"`: Futaies Feuillues
#' - `"BM"`: Futaies Mixtes
#' - `"BO"`: Oseraies
#' - `"BP"`: Peupleraies
#' - `"BR"`: Futaies Résineuses
#' - `"BS"`: Taillis sous Futaies
#' - `"BT"`: Taillis Simples
#'
#' @importFrom cli cli_alert_info cli_abort cli_alert_success cli_alert_warning
#' @importFrom utils read.csv2
#'
#' @examples
#' \dontrun{
#' pm <- get_legal_entity(c("29158", "08185"))
#' }
#'
#' @export
get_legal_entity <- function(
    x,
    cache = NULL,
    verbose = TRUE){
  # Helpers ----
  pad_left <- function(x, width) gsub(" ", "0", sprintf(paste0("%", width, "s"), as.character(x)))

  # Check input ---
  x <- as.character(x)
  code_insee <- pad_left(x[nchar(x) > 3], 5)
  code_dep <- pad_left(x[nchar(x) <= 3], 2)
  if (verbose && length(code_dep)) {
    cli::cli_alert_warning("Department-level queries may be slower. For better performance, use {.field code_insee} instead.")
  }

  if (length(code_insee)) {
    all_insee <- happign::com_2025$COM
    valid_insee <- code_insee %in% all_insee
    if (!all(valid_insee)) {
      bad_vals <- code_insee[!valid_insee]
      cli_abort("Invalid INSEE code(s): {paste(bad_vals, collapse = ', ')}",)
    }
  }

  if (length(code_dep)) {
    all_dep <- happign::dep_2025$DEP
    valid_dep <- code_dep %in% all_dep
    if (!all(valid_dep)) {
      bad_vals <- code_dep[!valid_dep]
      cli::cli_abort("Invalid department code(s): {paste(bad_vals, collapse = ', ')}")
    }
  }

  deps <- c(code_dep, substr(code_insee, 1, 2)) |> unique()
  cache <- download_legal_entity(dep = deps, cache = cache)

  files <- grep(
    paste(deps, collapse = "|"),
    list.files(cache, recursive = TRUE, full.names = TRUE),
    value = TRUE
  )

  boisee <- c("L", "LB", "B", "BF", "BM", "BO", "BP", "BR", "BS", "BT")

  if (verbose) cli_alert_info("Reading CSV files...")
  col_classes <- replace(rep("NULL", 24), c(1, 3, 5, 6, 7, 13, 14, 16, 17, 18, 24), NA)
  raw <- do.call(rbind, lapply(files, read.csv2, colClasses = col_classes))
  names(raw) <- c(
    "dep", "com", "prefix", "section", "numero", "lieu_dit",
    "surf_tot", "nature", "contenance", "type", "prop"
  )

  if (length(code_insee)) raw <- raw[paste0(raw$dep, raw$com) %in% code_insee, ] # Keep insee code
  raw <- raw[startsWith(raw$type, "P"), ] # Keep propriétaire

  if (verbose) cli_alert_info("Preparing CSV files...")
  legal_entity <- raw |>
    transform(
      idu = paste0(
        pad_left(dep, 2),
        pad_left(com, 3),
        ifelse(is.na(prefix), "000", pad_left(prefix, 3)),
        pad_left(section, 2),
        pad_left(numero, 4)
      ),
      is_boisee = sub(" -.*", "", nature) %in% boisee
    )

  legal_entity_boisee <- aggregate(contenance ~ idu, data = legal_entity, subset = is_boisee, FUN = sum, na.rm = TRUE)
  legal_entity_prop <- aggregate(prop ~ idu, data = legal_entity, FUN = \(x) paste(unique(x), collapse = " \\ "))
  legal_entity_lieu_dit <- aggregate(lieu_dit ~ idu, data = legal_entity, FUN = \(x) paste(unique(x), collapse = " \\ "))

  if (verbose) cli_alert_info("Generating matrice...")
  legal_entity_clean <- legal_entity |>
    subset(select = c("idu", "surf_tot")) |>
    unique() |>
    merge(legal_entity_boisee, all.x = TRUE) |>
    merge(legal_entity_prop, all.x = TRUE) |>
    merge(legal_entity_lieu_dit, all.x = TRUE) |>
    transform(tx_boisee = surf_tot / contenance)

  matrice_legal_entity <- data.frame(
    "IDENTIFIANT" = "",
    "PROPRIETAIRE" = legal_entity_clean$prop,
    "CODE_INSEE" = substr(legal_entity_clean$idu, 1, 5),
    "PREFIXE" = substr(legal_entity_clean$idu, 6, 8),
    "SECTION" = substr(legal_entity_clean$idu, 9, 10),
    "NUMERO" = substr(legal_entity_clean$idu, 11, 14),
    "LIEU_DIT" = legal_entity_clean$lieu_dit,
    "TX_BOISEE" = legal_entity_clean$tx_boisee
  )

  if (verbose) {
    cli::cli_alert_success("Matrix successfully generated ({.val {nrow(legal_entity_clean)}} rows).")
  }
  return(matrice_legal_entity)
}

#' Helpers to search within a forest matrix
#'
#' This function allows searching for parcels in a forest matrix based on
#' owner names or location (lieu-dit).
#'
#' @param matrice `data.frame`; The matrix containing parcel information in a
#' format readable by SEQUOIA.
#' @param prop `character`; Owner name(s) used to further filter results. Defaults to `NULL`.
#' Multiple owners can be specified.
#' @param lieu_dit `character`; Locality name(s) used to further filter results. Defaults to `NULL`.
#' Multiple locality names can be specified.
#'
#' @importFrom cli cli_abort
#'
#' @return A `data.frame`.
#'
#' @examples
#' \dontrun{
#' # Recherche des parcelles dans la matrice pour le département 01
#' pm29158 <- get_pm("29", "29158")
#'
#' # Recherche des parcelles pour une commune spécifique et un propriétaire
#' cad <- search_pm(pm29158, prop = c("COMMUNE"))
#' cad <- search_pm(pm29158, lieu_dit = "ECKMUHL")
#' cad <- search_pm(pm29158, prop = c("COMMUNE"), lieu_dit = "ECKMUHL")
#' }
#'
#' @export
search_matrice <- function(matrice, prop = NULL, lieu_dit = NULL) {
  normalize_txt <- function(x) {
    x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
    x <- toupper(x)
    x <- gsub("[^A-Z0-9]", "", x)  # remove all non-alphanumeric chars
    trimws(x)
  }

  matrice$prop_norm <- normalize_txt(matrice$PROPRIETAIRE)
  matrice$lieu_dit_norm <- normalize_txt(matrice$LIEU_DIT)

  res <- matrice
  if (!is.null(prop)) {
    prop_pattern <- paste(normalize_txt(prop), collapse = "|")
    res <- subset(res, grepl(prop_pattern, prop_norm))
  }

  if (!is.null(lieu_dit)) {
    lieu_dit_pattern <- paste(normalize_txt(lieu_dit), collapse = "|")
    res <- subset(res, grepl(lieu_dit_pattern, lieu_dit_norm))
  }

  return(res)
}


