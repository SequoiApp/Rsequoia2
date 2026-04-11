#' Get SIREN from pattern
#'
#' Search SIREN identifiers by matching normalized patterns against company names.
#'
#' @param pattern Character vector of search patterns.
#' @param verbose Logical; print messages and results.
#'
#' @return A data.frame of matching records (invisible).
#'
#' @importFrom DBI dbConnect dbDisconnect dbExecute dbGetQuery dbQuoteString
#' @importFrom duckdb duckdb
#' @importFrom cli cli_abort cli_alert_info cli_alert_success cli_alert_warning cli_ul
#'
#' @examples
#' \dontrun{
#' get_siren("TOTAL")
#' get_siren(c("BANQUE", "POSTALE"))
#' }
#'
get_siren <- function(pattern, verbose = TRUE){

  # Normalize ---
  pattern <- iconv(pattern, to = "ASCII//TRANSLIT")
  pattern <- toupper(pattern)
  pattern <- gsub("[^A-Z0-9]", "", pattern)
  pattern <- trimws(pattern)

  pattern <- pattern[!is.na(pattern) & nzchar(pattern)]
  if (!length(pattern)) {
    cli::cli_abort("{.arg pattern} must contain at least one non-empty value.")
  }

  # Code used to find the dataset id
  # siren <- dg_get_dataset("5b7ffc618b4c4169d30727e0")
  # siren_r <- siren$resources
  # url <- siren_r[grepl("UniteLegale", siren_r$title) & siren_r$format == "parquet",]

  url <- "https://www.data.gouv.fr/api/1/datasets/r/350182c9-148a-46e0-8389-76c2ec1374a3"

  # Set database connection ----
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  DBI::dbExecute(con, "SET enable_progress_bar=false;")
  DBI::dbExecute(con, "LOAD httpfs;")

  if (verbose) cli::cli_alert_info("Recherche SIREN en cours...")

  # Query build (normalize db and combine pattern)
  db_norm <- "regexp_replace(coalesce(denominationUniteLegale,''), '[^A-Z0-9]','','g')"

  where_pattern <- paste(rep(paste0(db_norm, " LIKE ?"), length(pattern)), collapse = " AND ")

  sql <- sprintf(
    "SELECT siren, denominationUniteLegale, activitePrincipaleNAF25UniteLegale
    FROM read_parquet(%s)
    WHERE (%s)",
    DBI::dbQuoteString(con, url),
    where_pattern
  )

  params <- as.list(paste0("%", pattern, "%"))
  res <- DBI::dbGetQuery(con, sql, params = params)

  if (nrow(res) == 0) {
    cli::cli_alert_warning("Aucun résultat trouvé.")
    return(invisible(res))
  }

  if (verbose){
    cli::cli_alert_success("{nrow(res)} résultat(s) trouvé(s) :")
    cli::cli_ul(paste0(res$denominationUniteLegale, ": ", res$siren))
    cli::cli_alert_info(paste("All SIREN:", paste0(res$siren, collapse = ", ")))
  }

  return(invisible(res))
}
