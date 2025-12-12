#' Download and cache cadastral data for legal entities ("Personnes Morales")
#'
#' This function downloads the official "Personnes Morales" datasets published
#' by the French Ministry of Economy and Finance. These datasets contain
#' cadastral and property information (parcels and buildings) owned by
#' legal entities. The data are automatically cached locally to avoid
#' repeated downloads.
#'
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
download_legal_entity <- function(cache = NULL, verbose = TRUE) {

    if (is.null(cache)){
    cache <- tools::R_user_dir("Rsequoia2", which = "cache")
    dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  }

  # dgfip_id <- "534fff8ea3a7292c64a77f02"
  dataset_id <- "605d268f4661cf23272817c3"
  resource <- dg_get_dataset(dataset_id)$resource

  # Find lates year
  years <- regmatches(resource$title, gregexpr("\\b\\d{4}\\b", resource$title))
  latest_year <- max(as.numeric(unlist(years)), na.rm = TRUE)

  parcelles_latest <- resource[grepl(paste0("parcelle.*", latest_year), resource$title, ignore.case = TRUE), ]
  zip <- parcelles_latest[parcelles_latest$format == "zip", ]

  urls <- unlist(zip$url)
  title <- unlist(zip$title) |> tools::file_path_sans_ext()

  is_download <- all(title %in% dir(cache))
  if (!is_download) {
    if (verbose) cli::cli_alert_info("Downloading legal entity datasets...")
    invisible(lapply(urls, \(x) archive::archive_extract(x, dir = cache)))
    cli::cli_alert_success("Data available at: {.path {normalizePath(cache)}}")
  }

  return(invisible(cache))
}

globalVariables(c(
  "dep", "com", "prefix", "section", "numero", "nature", "is_boisee", "surf_tot",
  "contenance", "prop_norm", "lieu_dit_norm"
))
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
#' - `"LB"`: Landes Boisees
#' - `"B"`:  Bois
#' - `"BF"`: Futaies Feuillues
#' - `"BM"`: Futaies Mixtes
#' - `"BO"`: Oseraies
#' - `"BP"`: Peupleraies
#' - `"BR"`: Futaies Resineuses
#' - `"BS"`: Taillis sous Futaies
#' - `"BT"`: Taillis Simples
#'
#' @importFrom cli cli_alert_info cli_abort cli_alert_success cli_alert_warning
#' @importFrom utils read.csv2
#' @importFrom stats aggregate
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
  cache <- download_legal_entity(cache = cache, verbose = verbose)

  pattern <- paste0(deps, ".*\\.csv$")
  files <- list.files(cache, pattern = pattern, recursive = TRUE, full.names = TRUE)

  boisee <- c("L", "LB", "B", "BF", "BM", "BO", "BP", "BR", "BS", "BT")

  if (verbose) cli_alert_info("Reading CSV files...")
  col_classes <- replace(rep("NULL", 24), c(1, 3, 5, 6, 7, 13, 14, 16, 17, 18, 24), NA)
  raw <- do.call(rbind, lapply(files, read.csv2, colClasses = col_classes))
  names(raw) <- c(
    "dep", "com", "prefix", "section", "numero", "lieu_dit",
    "surf_tot", "nature", "contenance", "type", "prop"
  )

  if (length(code_insee)) raw <- raw[paste0(raw$dep, raw$com) %in% code_insee, ] # Keep insee code
  raw <- raw[startsWith(raw$type, "P"), ] # Keep proprietaire only

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
    "identifiant" = "",
    "proprietaire" = legal_entity_clean$prop,
    "insee" = substr(legal_entity_clean$idu, 1, 5),
    "prefix" = substr(legal_entity_clean$idu, 6, 8),
    "section" = substr(legal_entity_clean$idu, 9, 10),
    "numero" = substr(legal_entity_clean$idu, 11, 14),
    "lieu_dit" = legal_entity_clean$lieu_dit,
    "tx_boisee" = legal_entity_clean$tx_boisee
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
#' @param x `data.frame`; The matrix containing parcel legal entity information
#' in a format readable by SEQUOIA.
#' @param prop `character`; Owner name(s) used to further filter results. Defaults to `NULL`.
#' Multiple owners can be specified.
#' @param lieu_dit `character`; Locality name(s) used to further filter results. Defaults to `NULL`.
#' Multiple locality names can be specified.
#'
#' @importFrom cli cli_abort
#'
#' @details
#' The search relies on a text–normalization step applied to both the inputs and
#' the corresponding columns of the matrice. This makes the search robust
#' to accents, punctuation, spacing irregularities and case differences.
#'
#' **Examples of normalization:**
#'   - `"État / Forêts"` → `"ETATFORETS"`
#'   - `"  Le Bois-de l'Orme  "` → `"LEBOISDELORME"`
#'   - `"Société du Chêne"` → `"SOCIETEDUCHENE"`
#'
#' @return A `data.frame`.
#'
#' @examples
#' \dontrun{
#' legal_entity <- get_legal_entity(29158)
#' search_mat <- search_legal_entity(legal_entity, c("penmarch", "guenole"))
#' }
#'
#' @export
search_legal_entity <- function(x, prop = NULL, lieu_dit = NULL) {
  normalize_txt <- function(x) {
    x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
    x <- toupper(x)
    x <- gsub("[^A-Z0-9]", "", x)  # remove all non-alphanumeric chars
    trimws(x)
  }

  x$prop_norm <- normalize_txt(x$PROPRIETAIRE)
  x$lieu_dit_norm <- normalize_txt(x$LIEU_DIT)

  res <- x
  if (!is.null(prop)) {
    prop_pattern <- paste(normalize_txt(prop), collapse = "|")
    res <- subset(res, grepl(prop_pattern, prop_norm))
  }

  if (!is.null(lieu_dit)) {
    lieu_dit_pattern <- paste(normalize_txt(lieu_dit), collapse = "|")
    res <- subset(res, grepl(lieu_dit_pattern, lieu_dit_norm))
  }

  res$prop_norm <- NULL
  res$lieu_dit_norm <- NULL

  return(res)
}


