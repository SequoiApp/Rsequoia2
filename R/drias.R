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
get_drias_metadata <- function(txt) {

  # Check txt
  if (!is.character(txt)) {
    cli::cli_abort(
      "{.arg txt} must be a character string (path to a DRIAS file).",
      class = "drias_invalid_txt"
    )
  }

  if (length(txt) != 1L) {
    cli::cli_abort(
      "{.arg txt} must be a single file path (length 1).",
      class = "drias_invalid_txt"
    )
  }

  if (is.na(txt) || !nzchar(txt)) {
    cli::cli_abort(
      "{.arg txt} must be a non-empty character string.",
      class = "drias_invalid_txt"
    )
  }

  if (!file.exists(txt)) {
    cli::cli_abort(
      "File not found: {.file {txt}}",
      class = "drias_file_not_found"
    )
  }

  # Read txt
  lines <- readLines(txt, encoding = "UTF-8", warn = FALSE)

  # Helper functions
  # Remove leading '#' and trim whitespace
  clean_line <- function(x) trimws(sub("^#+", "", x))

  # Identify separator lines (e.g. '-----')
  is_separator <- function(x) grepl("^-{3,}$", x)

  # Extract a metadata value from a key
  extract_value <- function(key) {
    line <- lines[grepl(paste0("^#\\s*", key), lines)]
    clean_line(sub(paste0(".*", key, "\\s*:\\s*"), "", line))
  }

  # Extract a block of lines between two section headers
  extract_block <- function(start, end) {
    idx_start <- which(grepl(start, lines))
    idx_end   <- which(grepl(end, lines))
    if (!length(idx_start) || !length(idx_end)) return(character(0))
    block <- lines[(idx_start[1] + 1):(idx_end[1] - 1)]
    block <- clean_line(block)
    block[nzchar(block) & !is_separator(block)]
  }

  # Simple metadata

  date_extraction <- extract_value("Date d'extraction")
  producteur      <- extract_value("Producteur")
  experience      <- extract_value("Experience")
  modele          <- extract_value("Modele")

  # Scenario

  scen_lines <- extract_block("Scenario", "Horizons")

  scenario <- list(
    code = sub(" *:.*$", "", scen_lines),
    description = sub("^.*: *", "", scen_lines)
  )

  # Horizons

  h_lines <- extract_block("Horizons", "Type d'indice")

  horizons <- data.frame(
    code = sub(" *:.*$", "", h_lines),
    libelle = sub("^.*: *", "", h_lines),
    stringsAsFactors = FALSE
  )

  # Indices

  i_lines <- extract_block("Indices", "Format des enregistrements")

  indices <- data.frame(
    code = sub(" *:.*$", "", i_lines),
    description = sub("^.*: *", "", i_lines),
    stringsAsFactors = FALSE
  )

  # Output

  list(
    date_extraction = date_extraction,
    producteur = producteur,
    experience = experience,
    modele = modele,
    scenario = scenario,
    horizons = horizons,
    indices = indices
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
get_drias_table <- function(txt) {

  # Check txt
  if (!is.character(txt)) {
    cli::cli_abort(
      "{.arg txt} must be a character string (path to a DRIAS file).",
      class = "drias_invalid_txt"
    )
  }

  if (length(txt) != 1L) {
    cli::cli_abort(
      "{.arg txt} must be a single file path (length 1).",
      class = "drias_invalid_txt"
    )
  }

  if (is.na(txt) || !nzchar(txt)) {
    cli::cli_abort(
      "{.arg txt} must be a non-empty character string.",
      class = "drias_invalid_txt"
    )
  }

  if (!file.exists(txt)) {
    cli::cli_abort(
      "File not found: {.file {txt}}",
      class = "drias_file_not_found"
    )
  }

  # Read metadata and extract expected column names
  meta <- get_drias_metadata(txt)
  indices <- meta$indices

  if (!is.data.frame(indices) || !"code" %in% names(indices)) {
    cli::cli_abort(
      "Invalid metadata structure: {.field indices} must contain a {.field code} column.",
      class = "drias_invalid_metadata"
    )
  }

  colnames_expected <- c("Point", "Latitude", "Longitude",
                         "Contexte", "Periode", "Mois",
                         indices$code)

  if (anyNA(colnames_expected) || any(!nzchar(colnames_expected))) {
    cli::cli_abort(
      "Invalid index codes detected in metadata.",
      class = "drias_invalid_metadata"
    )
  }

  if (anyDuplicated(colnames_expected)) {
    cli::cli_abort(
      "Duplicate index codes detected in metadata.",
      class = "drias_invalid_metadata"
    )
  }

  # Read data table (comments ignored by read.table)
  tables <- utils::read.table(
    txt,
    sep = ";",
    header = FALSE,
    stringsAsFactors = FALSE
  )
  tables <- tables[, !sapply(tables, function(x) all(is.na(x)))]

  # Structural validation
  if (ncol(tables) != length(colnames_expected)) {
    cli::cli_abort(
      "Column count mismatch: data has {ncol(tables)} columns, metadata defines {length(colnames_expected)} indices.",
      class = "drias_table_mismatch"
    )
  }

  names(tables) <- colnames_expected

  tables
}

#' Summarize a DRIAS index by period and month
#'
#' Compute a summary (mean, sum, etc.) of a specific DRIAS index
#' for two periods (H1 and H3) by month and calculate the difference.
#'
#' @param tables A data.frame returned by `get_drias_table()`.
#'   Must contain columns `Periode`, `Mois` (between 1 and 12) and the target index.
#' @param field Character. Name of the index to summarize (e.g., "NORTAV").
#' @param fun Function to summarize the values (default: `mean`).
#'
#' @return A data.frame with columns:
#' \describe{
#'   \item{Mois}{Month number (between 1 and 12).}
#'   \item{H1_FIELD}{Summary of H1 period for the selected index.}
#'   \item{H3_FIELD}{Summary of H3 period for the selected index.}
#'   \item{EC_FIELD}{Difference H3 - H1 for the selected index.}
#' }
#' @export
get_drias_abstract <- function(tables, field, fun = mean) {
  # Check that the field exists
  if (!field %in% names(tables)) {
    cli::cli_abort("Field {.val {field}} not found in tables", class = "drias_missing_field")
  }
  if (!"Periode" %in% names(tables)) {
    cli::cli_abort("tables must have column 'Periode'", class = "drias_missing_col")
  }
  if (!"Mois" %in% names(tables)) {
    cli::cli_abort("tables must have column 'Mois'", class = "drias_missing_col")
  }

  # Aggregate values by Periode and Mois
  res <- aggregate(
    tables[[field]],
    by = list(Periode = tables$Periode, Mois = tables$Mois),
    FUN = fun,
    na.rm = TRUE
  )
  names(res)[3] <- "Value"

  # Extract H1 and H3 values
  H1_vals <- res$Value[res$Periode == "H1"]
  H3_vals <- res$Value[res$Periode == "H3"]

  # Check that H1 and H3 have the same length
  if (length(H1_vals) != length(H3_vals)) {
    cli::cli_abort(
      "Mismatch between H1 and H3 periods: H1 length = {length(H1_vals)}, H3 length = {length(H3_vals)}",
      class = "drias_period_mismatch"
    )
  }

  # Create final data.frame with field suffixes
  df <- data.frame(
    Mois = unique(res$Mois)
  )
  df[[paste0("H1_", field)]] <- H1_vals
  df[[paste0("H3_", field)]] <- H3_vals
  df[[paste0("EC_", field)]] <- H3_vals - H1_vals

  return(df)
}
