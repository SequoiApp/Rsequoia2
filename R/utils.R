#' Left-pad a string with zeros
#'
#' Internal helper used to format fixed-width codes.
#'
#' @param x `character` String to pad
#' @param width `integer` Desired total width
#' @param fill `character` Filler to pad with
#'
#' @return A zero-padded character string.
#' @keywords internal
#'
pad_left <- function(x, width, fill = "0") {
  x <- trimws(x)
  x <- ifelse(is.na(x), "", x)
  gsub(" ", fill, sprintf(paste0("%", width, "s"), as.character(x)))
}

#' Right-pad a string with zeros
#'
#' Internal helper used to format fixed-width codes.
#'
#' @param x `character` String to pad
#' @param width `integer` Desired total width
#' @param fill `character` Filler to pad with
#'
#' @return A zero-padded character string.
#' @keywords internal
#'
pad_right <- function(x, width, fill = "0") {
  x <- trimws(x)
  x <- ifelse(is.na(x), "", x)
  gsub(" ", fill, sprintf(paste0("%-", width, "s"), as.character(x)))
}

#' Clean names
#'
#' Internal helper used to clean df names
#'
#' @param x `character` String to clean
#'
#' @return A cleaned string.
#' @keywords internal
#'
clean_names <- function(x) {
  x <- iconv(x, to = "ASCII//TRANSLIT")
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_|_$", "", x)
  make.unique(x, sep = "_")
}

#' Split IDU
#'
#' Internal helper used to split idu
#'
#' @param idu `character` IDU(s) to pad
#'
#' @keywords internal
#'
idu_split <- function(idu) {

  code_dep <- substr(idu, 1, 2)
  code_com <- substr(idu, 3, 5)
  insee <- paste0(code_dep, code_com)

  data.frame(
    idu     = idu,
    code_dep = code_dep,
    code_com = code_com,
    prefix   = substr(idu, 6, 8),
    section  = substr(idu, 9, 10),
    numero   = substr(idu, 11, 14),
    insee    = insee,
    stringsAsFactors = FALSE
  )
}

#' Build IDU
#'
#' Internal helper used to build idu
#'
#' @param dep `character` dep code
#' @param com `character` com code
#' @param prefix `character` prefic code
#' @param section `character` section code
#' @param numero `character` numero code
#'
#' @keywords internal
#'
idu_build <- function(dep, com, prefix, section, numero) {

  dep <- pad_left(dep, 2)
  com <- pad_left(com, 3)
  prefix <- pad_left(prefix, 3)
  section <- pad_left(section, 2)
  numero <- pad_left(numero, 4)

  return(paste0(dep, com, prefix, section, numero))
}

#' Force silent function
#'
#' Internal helper used to silent function
#'
#' @param expr Code to capture
#'
#' @keywords internal
#'
quiet <- function(expr) {
  utils::capture.output(result <- suppressMessages(suppressWarnings(expr)))
  result
}

#' Add retry capability to seq_* function
#'
#' Internal helper used to retry download when failing
#'
#' @param expr Code to capture
#' @param times Number of retry
#' @param wait Time to wait between retry
#' @param verbose `logical` If `TRUE`, display messages.
#'
#' @export
seq_retry <- function(expr, times = 3, wait = 0.5, verbose = TRUE) {

  last_error <- NULL

  suppressWarnings({

    for (i in seq_len(times)) {
      res <- try(expr, silent = TRUE)

      not_an_error <- !inherits(res, "try-error")
      if (not_an_error) {
        return(res)
      }

      last_error <- res
      if (verbose) {
        cli::cli_alert_info(
          "Attempt {i}/{times} failed, retrying..."
        )
      }

      if (i < times) {
        Sys.sleep(wait)
      }
    }

  })

  cli::cli_warn(c(
    "!" = "Operation failed after {times} attempt{?s}.",
    "x" = conditionMessage(attr(last_error, "condition"))
  ))

  return(invisible(NULL))

}

#' Validate department codes
#'
#' Checks that department codes are non-empty and exist in the COG reference.
#' Values are converted to character, padded to two digits, and deduplicated.
#'
#' @param dep Department code vector.
#'
#' @return A normalized `character` vector of valid department codes.
#'
#' @keywords internal
check_dep <- function(dep) {
  if (missing(dep) || is.null(dep) || length(dep) == 0) {
    cli::cli_abort("{.arg dep} must be a non-empty department code vector.")
  }

  dep <- unique(pad_left(as.character(dep), 2))

  valid_dep <- get_cog(verbose = FALSE)$dep$DEP
  invalid_dep <- dep[!dep %in% valid_dep]

  if (length(invalid_dep) > 0) {
    cli::cli_abort(c(
      "Invalid department code.",
      "x" = "Invalid department code(s): {.vals {invalid_dep}}",
      "i" = "See {.run get_cog()$dep} for valid department codes."
    ))
  }

  return(dep)
}

#' Validate insee codes
#'
#' Checks that insee codes are non-empty and exist in the COG reference.
#' Values are converted to character, padded to two digits, and deduplicated.
#'
#' @param insee insee code vector.
#'
#' @return A normalized `character` vector of valid insee codes.
#'
#' @keywords internal
check_insee <- function(insee) {
  if (missing(insee) || is.null(insee) || length(insee) == 0) {
    cli::cli_abort("{.arg insee} must be a non-empty insee code vector.")
  }

  insee <- unique(pad_left(as.character(insee), 5))

  valid_insee <- get_cog(verbose = FALSE)$com$COM
  invalid_insee <- insee[!insee %in% valid_insee]

  if (length(invalid_insee) > 0) {
    cli::cli_abort(c(
      "Invalid insee code.",
      "x" = "Invalid insee code(s): {.vals {invalid_insee}}",
      "i" = "See {.run get_cog()$com} for valid insee codes."
    ))
  }

  return(insee)
}

#' Download multiple files with retry
#'
#' Internal wrapper around [curl::multi_download()] used to download cached
#' resources. Existing files are skipped unless `overwrite = TRUE`; failed
#' downloads are retried up to `max_tries`.
#'
#' @param urls `character`; Remote file URLs.
#' @param destfiles `character`; Local destination paths.
#' @param overwrite `logical(1)`; If `TRUE`, re-download existing files.
#' @param verbose `logical(1)`; If `TRUE`, show download progress and messages.
#' @param max_tries `integer(1)`; Maximum number of download attempts.
#'
#' @return Invisibly returns `TRUE` if all required files are available.
#'
#' @keywords internal
seq_multi_download <- function(
    urls,
    destfiles,
    overwrite = FALSE,
    verbose = TRUE,
    max_tries = 3
) {

  to_download <- overwrite | !file.exists(destfiles)

  urls <- urls[to_download]
  destfiles <- destfiles[to_download]

  if (!length(destfiles)) {
    return(invisible(TRUE))
  }

  dir.create(unique(dirname(destfiles)), recursive = TRUE, showWarnings = FALSE)

  download_once <- function(urls, destfiles) {
    # multiplex = FALSE is better than multiplex = TRUE after benchmarking

    res <- curl::multi_download(
      urls = urls,
      destfiles = destfiles,
      resume = FALSE,
      progress = verbose,
      multiplex = FALSE,
      connecttimeout = 60,
      timeout = 600
    )

    ok <- !is.na(res$success) & res$success & res$status_code == 200
    failed <- res[!ok, , drop = FALSE]

    # Avoid keeping partial/corrupt files
    unlink(failed$destfile)

    list(
      urls = failed$url,
      destfiles = failed$destfile
    )
  }

  state <- list(urls = urls, destfiles = destfiles)

  for (attempt in seq_len(max_tries)) {
    state <- download_once(state$urls, state$destfiles)

    if (!length(state$destfiles)) {
      return(invisible(TRUE))
    }

    if (verbose && attempt < max_tries) {
      cli::cli_alert_warning(
        "{length(state$destfiles)} file(s) failed. Retrying in 5 seconds..."
      )

      Sys.sleep(5)
    }
  }

  cli::cli_abort(c(
    "Some files could not be downloaded.",
    "x" = "{length(state$destfiles)} file(s) still missing after {max_tries} attempt(s).",
    "i" = "Missing file(s): {.file {basename(state$destfiles)}}"
  ))
}
