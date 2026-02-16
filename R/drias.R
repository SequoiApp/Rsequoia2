#' Parse DRIAS metadata from a text file
#'
#' Extracts structured metadata from a DRIAS text file, including
#' extraction information, climate scenario, horizons and indices.
#'
#' @param txt `character` string. Path to a DRIAS `.txt` file.
#'
#' @return A `list` with the following elements:
#' \describe{
#'   \item{date_extraction}{`character` string. Extraction date.}
#'   \item{producteur}{`character` string. Data producer.}
#'   \item{experience}{`character` string. Climate experiment.}
#'   \item{modele}{`character` string. Climate model.}
#'   \item{scenario}{`list` with `code` and `description`.}
#'   \item{horizons}{`data.frame` with columns `code` and `libelle`.}
#'   \item{indices}{`data.frame` with columns `code` and `description`.}
#' }
#'
#' @export
drias_read_metadata <- function(txt) {

  # Check txt input ----
  if (!is.character(txt)) {
    cli::cli_abort("{.arg txt} must be a character string (path to a DRIAS file).")
  }

  if (length(txt) != 1L) {
    cli::cli_abort("{.arg txt} must be a single file path (length 1).")
  }

  if (is.na(txt) || !nzchar(txt)) {
    cli::cli_abort("{.arg txt} must be a non-empty character string.")
  }

  if (!file.exists(txt)) {
    cli::cli_abort("File not found: {.file {txt}}")
  }

  lines <- readLines(txt, encoding = "ISO-8859-1", warn = FALSE)

  # Helpers ----
  clean <- \(x) trimws(sub("^#+", "", x))

  extract_value <- \(key) {
    x <- grep(paste0("^#\\s*", key), lines, value = TRUE)
    clean(sub(".*:\\s*", "", x))
  }

  extract_block <- \(start, end) {
    i <- grep(start, lines)[1]
    j <- grep(end, lines)[1]
    if (is.na(i) || is.na(j)) return(character())
    x <- clean(lines[(i+1):(j-1)])
    x[nzchar(x) & !grepl("^-{3,}$", x)]
  }

  split_keyval <- \(x) {
    data.frame(
      code = sub(" *:.*$", "", x),
      description = sub("^.*: *", "", x),
      stringsAsFactors = FALSE
    )
  }

  horizon_metadata <- data.frame(
    code = c("H1", "H2", "H3"),
    start = c(2021, 2041, 2071),
    end = c(2050, 2070, 2100)
  )

  horizon <- extract_block("Horizons", "Type d'indice") |>
    split_keyval() |>
    merge(horizon_metadata)

  # Extraction ----
  list(
    date_extraction = extract_value("Date d'extraction"),
    producteur = extract_value("Producteur"),
    experience = extract_value("Experience"),
    modele = extract_value("Modele"),
    scenario = split_keyval(extract_block("Scenario", "Horizons")),
    horizon = horizon,
    indices = split_keyval(extract_block("Indices", "Format des enregistrements"))
  )
}

#' Parse DRIAS raw data from a text file
#'
#' Extracts raw data from a DRIAS text file.
#'
#' @param txt `character` string. Path to a DRIAS `.txt` file.
#'
#' @return A `data.frame` containing raw data.
#'
#' @export
drias_read_table <- function(txt) {

  # Check txt input ----
  if (!is.character(txt)) {
    cli::cli_abort("{.arg txt} must be a character string (path to a DRIAS file).")
  }

  if (length(txt) != 1L) {
    cli::cli_abort("{.arg txt} must be a single file path (length 1).")
  }

  if (is.na(txt) || !nzchar(txt)) {
    cli::cli_abort("{.arg txt} must be a non-empty character string.")
  }

  if (!file.exists(txt)) {
    cli::cli_abort("File not found: {.file {txt}}")
  }

  # Read drias data ----
  drias <- utils::read.table(
    txt,
    sep = ";",
    header = FALSE,
    stringsAsFactors = FALSE,
    fileEncoding = "ISO-8859-1"
  )

  # Find colnames ----
  lines <- readLines(txt, encoding = "ISO-8859-1", warn = FALSE)
  header_idx <- grep("^# Point;", lines)

  if (length(header_idx) == 0){
    cli::cli_abort("No colname line starting with '# Point;' found.")
  }

  colnames <- strsplit(sub("^#\\s*", "", lines[header_idx]), ";")[[1]] |>
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT") |>
    toupper()
  names(drias) <- colnames

  # Format drias ----
  drias <- drias[, colSums(!is.na(drias)) > 0, drop = FALSE]

  return(drias)
}

#' Compute ombrothermic climatology summaries
#'
#' Computes monthly ombrothermic summaries from DRIAS extraction
#'
#' @param txt `character` string. Path to a DRIAS `.txt` file.
#'
#' @return
#' A `data.frame` containing monthly averages of temperature and precipitation
#' for each period.
#'
#' @examples
#' \dontrun{
#' ombro <- drias_ombro(clim)
#' }
#'
#' @export
drias_ombro <- function(txt){

  drias_meta <- drias_read_metadata(txt)
  drias <- drias_read_table(txt) |>
    merge(drias_meta$horizon, by.x = "PERIODE", by.y = "code")
  drias$PERIODE <- sprintf("%s-%s", drias$start, drias$end)

  # p: precipitation
  month <- "MOIS"
  tmoy <- "NORTAV"
  tmin <- "NORTNAV"
  tmax <- "NORTXAV"
  p_mm <- "NORRR"

  # remove vars all equal to zero to avoid bad mean
  vars <- c(tmoy, tmin, tmax, p_mm)
  keep <- sapply(
    split(drias[vars], drias$POINT),
    function(df_point) all(colSums(df_point != 0, na.rm = TRUE) > 0)
  )
  drias <- drias[drias$POINT %in% names(keep)[keep], ]

  ombro <- aggregate(
    drias[vars],
    by = drias[c(month, "PERIODE")],
    FUN = mean,
    na.rm = TRUE
  )

  ombro <- ombro[, c("PERIODE", month, tmoy, tmin, tmax, p_mm)]

  return(ombro)
}

#' Compute monthly precipitation, evapotranspiration and water balance from DRIAS projections
#'
#' Reads a DRIAS climate projection `.txt` file and computes the mean monthly
#' precipitation (P), potential evapotranspiration (ETP), and water balance (P - ETP)
#' for each projection period.
#'
#' Invalid climate points (where precipitation or evapotranspiration are entirely
#' equal to zero across all months) are automatically excluded to avoid biased averages.
#'
#' @param txt `character(1)`. Path to a DRIAS `.txt` file downloaded from the
#' [DRIAS portal](https://www.drias-climat.fr/)
#'
#' @return A `data.frame` with one row per month and period
#' @seealso
#' [drias_read_table()], [drias_read_metadata()], [drias_ombro()]
#'
#' @examples
#' \dontrun{
#' txt <- "indicesRACMO22E_CNRM-CM5.txt"
#' etp <- drias_etp(txt)
#' head(etp)
#' }
#'
#' @export
drias_etp <- function(txt){

  drias_meta <- drias_read_metadata(txt)
  drias <- drias_read_table(txt) |>
    merge(drias_meta$horizon, by.x = "PERIODE", by.y = "code")
  drias$PERIODE <- sprintf("%s-%s", drias$start, drias$end)

  # p: precipitation
  month <- "MOIS"
  etp_mm <- "NORETPC"
  p_mm <- "NORRR"
  p_etp <- "P-ETP"
  # remove vars all equal to zero to avoid bad mean
  vars <- c(etp_mm, p_mm)

  keep <- sapply(
    split(drias[vars], drias$POINT),
    function(df_point) all(colSums(df_point != 0, na.rm = TRUE) > 0)
  )

  drias <- drias[drias$POINT %in% names(keep)[keep], ]

  etp <- aggregate(
    drias[vars],
    by = drias[c(month, "PERIODE")],
    FUN = mean,
    na.rm = TRUE
  )

  etp[p_etp] <- etp[p_mm] - etp[etp_mm]
  etp <- etp[, c("PERIODE", month, p_mm, etp_mm, p_etp)]

  return(etp)
}

#' Generate DRIAS climate projection outputs
#'
#' Reads, formats, and exports climate projection data from DRIAS.
#' The function expects a DRIAS projection `.txt` file located in the
#' `4_METEO` directory of the Sequoia project. It extracts metadata,
#' raw climatology, ombrothermic summaries, and evapotranspiration (ETP)
#' indicators. Results are written to an Excel workbook based on the
#' internal template `CLIMAT_DRIAS.xlsx`.
#'
#' @inheritParams seq_write
#'
#' @details
#' The function looks for a single `.txt` file in the `4_METEO` directory.
#' This file must be downloaded beforehand from the DRIAS portal
#' (https://drias-climat.fr/).
#'
#' @return
#' A named `character` vector:
#' - `"drias"`: Path to the generated Excel workbook
#'
#' @export
seq_drias <- function(dirname = ".", verbose = TRUE, overwrite = FALSE){

  parca <- seq_read("v.seq.parca.poly", dirname = dirname)
  identifier <- seq_field("identifier")$name
  id <- unique(parca[[identifier]])

  if (verbose){
    cli::cli_h1("CLIMATE DRIAS")
  }

  meteo_dir <- file.path(dirname, "4_METEO")
  dir.create(meteo_dir, showWarnings = FALSE, recursive = TRUE)
  filepath <- file.path(meteo_dir, sprintf("%s_CLIMAT_DRIAS.xlsx", id)) |>
    setNames("drias")

  wb <- openxlsx2::wb_load(system.file("xlsx/CLIMAT_DRIAS.xlsx", package = "Rsequoia2"))

  # Drias
  txt <- list.files(meteo_dir, pattern = ".txt", full.names = TRUE)
  if (length(txt) == 0){
    cli::cli_abort(
      "No DRIAS {.file .txt} file found in: {.path {meteo_dir}}."
    )
  }

  if (verbose) {cli_alert_info("Writing DRIAS data to: {.path {filepath}}")}

  txt <- txt[1]  # ensure single file
  drias_metadata <- drias_read_metadata(txt)
  drias_raw <- drias_read_table(txt)
  drias_ombro <- drias_ombro(txt)
  drias_etp <- drias_etp(txt)

  wb <- openxlsx2::wb_clean_sheet(wb, sheet = 1, styles = FALSE) |>
    openxlsx2::wb_add_data(sheet = 1, drias_metadata$indices)

  wb <- openxlsx2::wb_clean_sheet(wb, sheet = 2, styles = FALSE) |>
    openxlsx2::wb_add_data(sheet = 2, drias_raw)

  wb <- openxlsx2::wb_clean_sheet(wb, sheet = 3, dims = "A1:F60", styles = FALSE) |>
    openxlsx2::wb_add_data(sheet = 3, drias_ombro)

  wb <- openxlsx2::wb_clean_sheet(wb, sheet = 4, dims = "A1:E60", styles = FALSE) |>
    openxlsx2::wb_add_data(sheet = 4, drias_etp)

  openxlsx2::wb_save(wb, file = filepath,  overwrite = overwrite)

  return(invisible(filepath))
}

