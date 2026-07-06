#' Build a SQL pattern filter
#'
#' Normalizes one or more search patterns and builds a SQL condition matching
#' `denomination_par` after accent stripping and punctuation removal.
#'
#' @param con A DBI connection, used to quote SQL strings.
#' @param pattern Character vector of search patterns.
#'
#' @return A single SQL condition as a character string.
#'
#' @noRd
sql_pattern <- function(con, pattern) {

  pattern <- iconv(pattern, to = "ASCII//TRANSLIT")
  pattern <- tolower(pattern)
  pattern <- gsub("[^a-z0-9]+", "", pattern)
  pattern <- pattern[nzchar(pattern)]

  if (length(pattern) == 0) {
    cli::cli_abort("{.arg pattern} must contain at least one searchable character.")
  }

  pattern <- DBI::dbQuoteString(con, paste0("%", pattern, "%"))

  expr <- "
    regexp_replace(
      lower(strip_accents(denomination_par)),
      '[^a-z0-9]+',
      '',
      'g'
    )
  "

  sprintf(
    "(%s)",
    paste(
      sprintf("%s LIKE %s", expr, pattern),
      collapse = " AND "
    )
  )
}

#' Build SQL filters for department and INSEE codes
#'
#' Validates department and/or INSEE codes and builds SQL `IN` conditions.
#'
#' @param con A DBI connection, used to quote SQL strings.
#' @param dep Optional department code vector.
#' @param insee Optional INSEE code vector.
#'
#' @return A character vector of SQL conditions. Empty if both filters are `NULL`.
#'
#' @noRd
sql_dep_insee <- function(con, dep = NULL, insee = NULL) {
  sql <- character()

  if (!is.null(dep)) {
    dep <- check_dep(dep)
    sql <- c(sql, sprintf(
      "departement IN (%s)",
      paste(DBI::dbQuoteString(con, dep), collapse = ", ")
    ))
  }

  if (!is.null(insee)) {
    insee <- check_insee(insee)
    sql <- c(sql, sprintf(
      "code_insee IN (%s)",
      paste(DBI::dbQuoteString(con, insee), collapse = ", ")
    ))
  }

  sql
}

#' Search legal entities
#'
#' Search legal entity parcels by denomination pattern, optionally restricted
#' to departments or INSEE codes.
#'
#' @param pattern Character vector of patterns to search in legal entity names.
#' @param dep Optional department code vector.
#' @param insee Optional INSEE code vector.
#' @param path Path to the legal entity parquet file.
#'
#' @return A data frame with matching `idu`, `denomination_par`, and `contenance`.
#'
#' @export
search_pm <- function(
    pattern,
    dep = NULL,
    insee = NULL,
    path = pm_download()
) {
  if (missing(pattern) || is.null(pattern) || length(pattern) == 0) {
    cli::cli_abort("{.arg pattern} must be a non-empty vector.")
  }

  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  where <- c(
    sql_pattern(con, pattern),
    "starts_with(code_droit_par, 'P')",
    sql_dep_insee(con, dep = dep, insee = insee)
  )

  sql <- sprintf(
    "
    SELECT
      idu,
      denomination_par,
      contenance
    FROM read_parquet(%s)
    WHERE %s
    GROUP BY idu, denomination_par, contenance
    ORDER BY denomination_par, idu
    ",
    DBI::dbQuoteString(con, path),
    paste(where, collapse = " AND ")
  )

  DBI::dbGetQuery(con, sql)
}

#' Select legal entities interactively
#'
#' Build a summary by legal entity and prompt the user to select one or more
#' legal entities.
#'
#' @param pm A data frame returned by [search_pm()].
#'
#' @return A character vector of selected legal entity names.
#'
#' @export
select_pm <- function(pm) {
  if (nrow(pm) == 0) {
    cli::cli_abort("No legal entity found.")
  }

  pm$contenance <- as.numeric(pm$contenance)

  n_prf <- aggregate(
    x = list(n_prf = pm$idu),
    by = list(denomination_par = pm$denomination_par),
    FUN = function(z) length(unique(z))
  )

  surface <- aggregate(
    list(surface = pm$contenance),
    list(denomination_par = pm$denomination_par),
    sum,
    na.rm = TRUE
  )

  report <- merge(n_prf, surface, by = "denomination_par")
  report <- report[order(-report$surface), ]

  choices <- sprintf(
    "%s ha / %s prf : %s",
    report$surface / 10000,
    report$n_prf,
    report$denomination_par
  )

  selected <- seq_select(
    choices = choices,
    title = "Select legal entities",
    multi = TRUE
  )

  report[selected, "denomination_par"]
}

#' Get legal entity parcels
#'
#' Retrieve all parcel records for one or more legal entity denominations,
#' optionally restricted to departments or INSEE codes.
#'
#' @param denomination Character vector of legal entity names.
#' @param dep Optional department code vector.
#' @param insee Optional INSEE code vector.
#' @param path Path to the legal entity parquet file.
#'
#' @return A data frame with matching legal entity parcel records.
#'
#' @export
get_pm <- function(
    denomination,
    dep = NULL,
    insee = NULL,
    path = pm_download()
) {
  if (missing(denomination) || is.null(denomination) || length(denomination) == 0) {
    cli::cli_abort("{.arg denomination} must be a non-empty vector.")
  }

  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  denomination <- unique(as.character(denomination))
  denomination <- denomination[nzchar(denomination)]

  where <- c(
    sprintf(
      "denomination_par IN (%s)",
      paste(DBI::dbQuoteString(con, denomination), collapse = ", ")
    ),
    "starts_with(coalesce(code_droit_par, ''), 'P')",
    sql_dep_insee(con, dep = dep, insee = insee)
  )

  sql <- sprintf(
    "SELECT * FROM read_parquet(%s) WHERE %s",
    DBI::dbQuoteString(con, path),
    paste(where, collapse = " AND ")
  )

  res = DBI::dbGetQuery(con, sql)
}

#' Normalize legal entity parcels
#'
#' Convert legal entity parcel data to the Sequoia parcel format and build a
#' detailed parcel table.
#'
#' @param pm A legal entity parcel data frame returned by [get_pm()].
#' @param verbose If `TRUE`, display progress messages.
#'
#' @return A list with:
#' \describe{
#'   \item{m}{Unique normalized parcel table.}
#'   \item{m_detail}{Detailed normalized parcel table with culture information.}
#' }
#'
#' @export
normalize_pm <- function(pm, verbose = FALSE){

  le_insee <- "code_insee"
  le_idu <- "idu"
  le_cad_area <- "contenance"

  if (verbose) cli_alert_info("Generating matrice...")

  cog <- get_cog(verbose = FALSE)

  m_format <- pm |>
    transform(
      "idu" = pm[[le_idu]],
      "com" = substr(idu, 3, 5),
      "section" = substr(idu, 9, 10),
      "numero" = substr(idu, 11, 14),
      "contenance" = as.numeric(pm[[le_cad_area]]) / 10000,
      "lieu_dit" = pm[["nom_voie"]],
      "source" = "https://data.economie.gouv.fr/api/v2/catalog/datasets/fichiers-des-locaux-et-des-parcelles-des-personnes-morales"
    ) |>
    merge(cog$com[, c("COM", "NCC_COM", "DEP")], by.x = le_insee, by.y = "COM") |>
    merge(cog$dep[, c("DEP", "NCC_DEP", "REG")], all.x = TRUE) |>
    merge(cog$reg[, c("REG", "NCC_REG")], all.x = TRUE) |>
    seq_normalize("parca")

  m_detail <- m_format
  m_detail[["DETAIL"]] <- pm$contenance_1
  m_detail[["NATURE"]] <- pm$nature_culture

  m <- unique(m_format)

  number <- seq_field("number")$name
  section <- seq_field("section")$name
  prefix <- seq_field("prefix")$name
  insee <- seq_field("insee")$name

  m <- m[order(m[[insee]], m[[prefix]], m[[section]], m[[number]]), ]
  m_detail <- m_detail[order(m_detail[[insee]], m_detail[[prefix]], m_detail[[section]], m_detail[[number]]), ]

  return(list(m = m, m_detail = m_detail))
}
