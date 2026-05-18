#' Rename a Sequoia directory
#'
#' Rename files in a Sequoia directory by replacing an old forest identifier with
#' a new one, then update the Sequoia identifier field inside all GeoPackage
#' layers found in the directory.
#'
#' The function is fault-tolerant: if one GeoPackage cannot be read or updated,
#' the error is reported but the remaining files are still processed.
#'
#' @param path Character. Path to the Sequoia directory.
#' @param old_id Character. Identifier to replace in file names.
#' @param new_id Character. New identifier to use in file names and GeoPackage attributes.
#' @param verbose Logical. If `TRUE`, print a console report with `cli`.
#'
#' @return Invisibly returns a data frame with one row per GeoPackage and the
#'   columns `file`, `status`, and `message`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' seq_dir_rename(
#'   path = "C:/Users/PaulCarteron/Desktop/3233B_LA MOUSSAYE",
#'   old_id = "3233B",
#'   new_id = "LA MOUSSAYE"
#' )
#' }
seq_dir_rename <- function(path, old_id, new_id, verbose = TRUE) {
  if (!is.character(path) || length(path) != 1 || is.na(path) || path == "") {
    cli::cli_abort("{.arg path} must be a single non-empty character string.")
  }

  if (!is.character(old_id) || length(old_id) != 1 || is.na(old_id) || old_id == "") {
    cli::cli_abort("{.arg old_id} must be a single non-empty character string.")
  }

  if (!is.character(new_id) || length(new_id) != 1 || is.na(new_id) || new_id == "") {
    cli::cli_abort("{.arg new_id} must be a single non-empty character string.")
  }

  if (!dir.exists(path)) {
    cli::cli_abort("Directory does not exist: {.path {path}}")
  }

  if (verbose) {
    cli::cli_h1("Rename Sequoia forest")
    cli::cli_alert_info("{.val {old_id}} -> {.val {new_id}}")
  }

  renamed <- seq_files_rename(path, old_id, new_id)

  gpkg_files <- list.files(path, pattern = "\\.gpkg$", recursive = TRUE, full.names = TRUE)
  out <- lapply(gpkg_files, seq_gpkg_id_rename, new_id = new_id)
  report <- do.call(rbind, out) |> as.data.frame()

  if (verbose) {
    seq_dir_rename_report(renamed, report)
  }

  invisible(report)
}

#' Rename matching files
#'
#' Rename files recursively by replacing `old_id` with `new_id` in file names.
#'
#' @param path Character. Directory path.
#' @param old_id Character. Identifier to replace.
#' @param new_id Character. Replacement identifier.
#'
#' @return Logical vector returned by [file.rename()].
#'
#' @keywords internal
seq_files_rename <- function(path, old_id, new_id) {
  old_files <- list.files(
    path,
    pattern = old_id,
    recursive = TRUE,
    full.names = TRUE
  )

  if (length(old_files) == 0) {
    return(NULL)
  }

  new_files <- file.path(
    dirname(old_files),
    gsub(old_id, new_id, basename(old_files))
  )

  file.rename(old_files, new_files)
}

#' Rename GeoPackage identifier
#'
#' Update the Sequoia identifier field in one GeoPackage.
#'
#' @param path Character. Path to a GeoPackage.
#' @param new_id Character. New identifier value.
#'
#' @return A list with `file`, `status`, and `message`.
#'
#' @keywords internal
seq_gpkg_id_rename <- function(path, new_id) {
  id_field <- seq_field("identifier")$name

  tryCatch({
    x <- suppressWarnings(sf::st_read(path, quiet = TRUE))

    if (nrow(x) == 0) {
      return(list(
        file = basename(path),
        status = "skipped",
        message = "empty layer"
      ))
    }

    if (!id_field %in% names(x)) {
        return(list(
          file = basename(path),
          status = "skipped",
          message = paste("field", id_field, "not found")
        ))
    }

    x[[id_field]] <- new_id

    suppressWarnings(sf::st_write(
      x,
      path,
      delete_dsn = TRUE,
      quiet = TRUE
    ))

    return(list(
      file = basename(path),
      status = "updated",
      message = ""
    ))

    gpkg_result(path, "updated", "")
  }, error = function(e) {
    return(list(
      file = basename(path),
      status = "error",
      message = conditionMessage(e)
    ))
  })
}

#' Print Sequoia directory rename report
#'
#' Print a compact `cli` report after renaming files and updating GeoPackages.
#'
#' @param renamed Logical vector returned by `seq_files_rename()`.
#' @param report Data frame returned by `seq_dir_rename()`.
#'
#' @return Invisibly returns `NULL`.
#'
#' @keywords internal
seq_dir_rename_report <- function(renamed, report) {
  cli::cli_alert_success("{sum(renamed)} file{?s} renamed.")

  if (nrow(report) == 0) {
    cli::cli_alert_warning("No GeoPackage found.")
    return(invisible(NULL))
  }

  n_updated <- sum(report$status == "updated")
  n_skipped <- sum(report$status == "skipped")
  n_error <- sum(report$status == "error")

  cli::cli_alert_success("{n_updated} GeoPackage{?s} updated.")
  cli::cli_alert_warning("{n_skipped} GeoPackage{?s} skipped.")
  cli::cli_alert_danger("{n_error} GeoPackage{?s} failed.")

  problems <- report[report$status != "updated", ]

  if (nrow(problems) > 0) {
    cli::cli_h2("Details")
    for (i in seq_len(nrow(problems))) {
      if (problems$status[i] == "error") {
        cli::cli_alert_danger("{problems$file[i]}: {problems$message[i]}")
      } else {
        cli::cli_alert_warning("{problems$file[i]}: {problems$message[i]}")
      }
    }
  }

  invisible(NULL)
}
