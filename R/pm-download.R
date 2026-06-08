#' Get latest legal entity parcel resource
#'
#' Find the most recent available year and corresponding ZIP resource for legal
#' entity parcel data.
#'
#' @return A list with:
#' \describe{
#'   \item{year}{Latest available data year.}
#'   \item{zip}{ZIP resource metadata for that year.}
#' }
#'
#' @noRd
pm_latest_data <- function() {
  dataset_id <- "605d268f4661cf23272817c3"
  resource <- dg_get_dataset(dataset_id)$resource

  years <- regmatches(resource$title, gregexpr("\\d{4}", resource$title))
  years <- as.numeric(unlist(years))

  if (!length(years)) {
    cli::cli_abort("No valid year found in dataset for legal entity parcels.")
  }

  year <- max(years, na.rm = TRUE)

  zip <- resource[
    grepl(paste0("parcelle.*", year), resource$title, ignore.case = TRUE) &
      tolower(resource$format) == "zip",
  ]

  if (!nrow(zip)) {
    cli::cli_abort("No ZIP resource found for legal entity parcels year {.val {year}}.")
  }

  list(year = year, zip = zip)
}

#' Download and extract legal entity CSV files
#'
#' Download ZIP resources and extract contained CSV files to a temporary
#' directory.
#'
#' @param zip ZIP resource metadata returned by `pm_latest_data()`.
#' @param verbose If `TRUE`, display progress messages.
#'
#' @return A list with:
#' \describe{
#'   \item{tmp}{Temporary extraction directory.}
#'   \item{files}{Extracted CSV file paths.}
#' }
#'
#' @noRd
pm_extract_csv <- function(zip, verbose = TRUE) {
  tmp <- tempfile("pm")
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)

  if (verbose) {
    cli::cli_alert_info("Downloading and extracting legal entity datasets...")
  }

  invisible(lapply(unlist(zip$url), archive::archive_extract, dir = tmp))

  files <- list.files(tmp, "\\.csv$", recursive = TRUE, full.names = TRUE)

  if (!length(files)) {
    unlink(tmp, recursive = TRUE, force = TRUE)
    cli::cli_abort("No CSV files found after extraction.")
  }

  list(tmp = tmp, files = files)
}

#' Convert legal entity CSV files to parquet
#'
#' Read extracted legal entity CSV files, normalize column names, build parcel
#' identifiers, and write a compressed parquet file.
#'
#' @param files Character vector of CSV file paths.
#' @param parquet Output parquet file path.
#' @param verbose If `TRUE`, display progress messages.
#'
#' @return Invisibly returns the parquet file path.
#'
#' @noRd
pm_csv_to_parquet <- function(files, parquet, verbose = TRUE) {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  q <- function(x) {
    x <- normalizePath(x, winslash = "/", mustWork = FALSE)
    as.character(DBI::dbQuoteString(con, x))
  }

  cols <- names(DBI::dbGetQuery(
    con,
    sprintf(
      "
      SELECT *
      FROM read_csv(%s, delim = ';', header = true, all_varchar = true, quote = '')
      LIMIT 0
      ",
      q(files[1])
    )
  ))

  select_sql <- paste(
    sprintf(
      "%s AS %s",
      DBI::dbQuoteIdentifier(con, cols),
      DBI::dbQuoteIdentifier(con, clean_names(cols))
    ),
    collapse = ",\n        "
  )

  sql <- sprintf("
    COPY (
      WITH src AS (
        SELECT
          %s,
          filename AS source_file
        FROM read_csv(
          [%s],
          delim = ';',
          header = true,
          union_by_name = true,
          filename = true,
          all_varchar = true,
          quote = ''
        )
      ),

      cleaned AS (
        SELECT
          * REPLACE (
            lpad(coalesce(nullif(trim(departement), ''), '00'), 2, '0') AS departement,
            lpad(coalesce(nullif(trim(code_commune), ''), '000'), 3, '0') AS code_commune,
            lpad(coalesce(nullif(trim(prefixe), ''), '000'), 3, '0') AS prefixe,
            lpad(coalesce(nullif(trim(section), ''), '00'), 2, '0') AS section,
            lpad(coalesce(nullif(trim(n_plan), ''), '0000'), 4, '0') AS n_plan
          )
        FROM src
      ),

      final AS (
        SELECT
          *,
          departement || code_commune AS code_insee,
          departement || code_commune || prefixe || section || n_plan AS idu
        FROM cleaned
      )

      SELECT *
      FROM final
    )
    TO %s
    (FORMAT parquet, COMPRESSION zstd);
    ",
    select_sql,
    paste(q(files), collapse = ", "),
    q(parquet)
  )

  if (verbose) {
    cli::cli_alert_info("Writing {.path {basename(parquet)}} dataset to cache...")
  }

  DBI::dbExecute(con, sql) |> invisible()

  invisible(parquet)
}

#' Download legal entity parcel data
#'
#' Download the latest legal entity parcel dataset and cache it as a parquet
#' file. If the parquet file already exists, it is reused.
#'
#' @param cache Cache directory where the parquet file is stored.
#' @param verbose If `TRUE`, display progress messages.
#'
#' @return Invisibly returns the normalized parquet file path.
#'
#' @noRd
pm_download <- function(cache = seq_cache("pm")$path, verbose = FALSE) {
  old_timeout <- getOption("timeout")
  options(timeout = max(300, old_timeout))
  on.exit(options(timeout = old_timeout), add = TRUE)

  dir.create(cache, recursive = TRUE, showWarnings = FALSE)

  resource <- pm_latest_data()

  parquet <- file.path(
    cache,
    paste0("legal_entity_", resource$year, ".parquet")
  )

  if (file.exists(parquet)) {
    if (verbose) {
      cli::cli_alert_success("Legal entity parquet already available: {.path {parquet}}")
    }

    return(invisible(normalizePath(parquet, winslash = "/", mustWork = TRUE)))
  }

  extracted <- pm_extract_csv(
    zip = resource$zip,
    verbose = verbose
  )

  on.exit(unlink(extracted$tmp, recursive = TRUE, force = TRUE), add = TRUE)

  pm_csv_to_parquet(
    files = extracted$files,
    parquet = parquet,
    verbose = verbose
  )

  if (verbose) {
    cli::cli_alert_success("Legal entity parquet written to: {.path {parquet}}")
  }

  invisible(normalizePath(parquet, winslash = "/", mustWork = TRUE))
}
