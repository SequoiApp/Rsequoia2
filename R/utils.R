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

#' Normalize text
#'
#' Internal helper used to normalize string for query
#'
#' @param x `character` String to clean
#'
#' @return A cleaned string.
#' @keywords internal
#'
normalize_txt <- function(x) {
  x <- enc2utf8(x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- tolower(x)
  gsub("[^a-z0-9]+", "", x)
}

#' Clean data frame names
#'
#' Internal helper used to clean df names
#'
#' @param x `character` String to clean
#'
#' @return A cleaned string.
#' @keywords internal
#'
clean_names <- function(x) {
  x <- enc2utf8(x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
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

#' Check IDU
#'
#' Internal helper used to split idu
#'
#' @param idu `character` IDU(s) to pad
#'
#' @keywords internal
#'
check_idu <- function(idu) {
  if (is.null(idu) || length(idu) == 0) {
    cli::cli_abort("{.arg idu} must be a non-empty vector.")
  }

  if (!is.character(idu)) {
    cli::cli_abort(
      "{.arg idu} must be {.cls character}, not {.cls {class(idu)[1]}}."
    )
  }

  unique(idu)
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
#' @param verbose `logical`; If `TRUE`, display progress and informational messages.
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
#' @param overwrite `logical`; If `TRUE`, overwrite existing files.
#' @param verbose `logical`; If `TRUE`, display progress and informational messages.
#' @param max_tries `integer`; Maximum number of download attempts.
#'
#' @return Invisibly returns `TRUE` if all required files are available.
#'
#' @keywords internal
seq_multi_download <- function(
    urls,
    destfiles,
    overwrite = FALSE,
    verbose = TRUE,
    max_tries = 10
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

#' Get Sequoia project context
#'
#' Reads the project PARCA layer and retrieves its identifier.
#'
#' @param dirname `character`; Sequoia project directory.
#'
#' @return A list containing `parca`, `identifier`, and `id`.
#'
#' @keywords internal
#' @noRd
.seq_context <- function(dirname = ".") {

  parca <- seq_read("v.seq.parca.poly", dirname = dirname)
  identifier <- seq_field("identifier")$name
  id <- unique(stats::na.omit(parca[[identifier]]))

  if (length(id) != 1) {
    cli::cli_abort(
      "Could not determine a unique Sequoia project identifier."
    )
  }

  return(list(parca = parca, id = id))

}

#' @title Validate logical scalar(s)
#'
#' @description
#' Validates that each provided argument is a logical scalar (`TRUE` or `FALSE`).
#' It is designed for internal argument validation in functions that expect
#' boolean flags.
#'
#' @param ... Objects to validate. Each must be:
#'   * a `logical` vector,
#'   * of length 1,
#'   * not `NA`.
#'   If `null_is_valid = TRUE`, `NULL` values are also accepted.
#' @param null_is_valid A `logical`. If `TRUE`, allows `NULL` values.
#'   If `FALSE` (default), `NULL` is rejected.
#'
#' @return
#' Invisibly returns `TRUE` if all inputs are valid.
#'
#' @details
#' The function supports validating multiple arguments at once.
#' Each argument is checked independently and triggers an error immediately if invalid.
#'
#' @keywords internal
check_boolean <- function(..., null_is_valid = FALSE) {

  old_warn <- getOption("warn")
  on.exit(options(warn = old_warn), add = TRUE)
  options(warn = -1)

  dots  <- list(...)
  calls <- as.list(substitute(list(...)))[-1]
  nms   <- names(dots)

  for (i in seq_along(dots)) {

    x <- dots[[i]]

    nm <- if (!is.null(nms) && nzchar(nms[i])) {
      nms[i]
    } else {
      deparse(calls[[i]])
    }

    # NULL handling
    if (is.null(x)) {
      if (isTRUE(null_is_valid)) {
        next
      } else {
        cli::cli_abort(
          "{.arg {nm}} must be a single TRUE or FALSE (NULL not allowed)"
        )
      }
    }

    # Type check
    if (!is.logical(x)) {
      cli::cli_abort(
        "{.arg {nm}} must be a logical (TRUE or FALSE)"
      )
    }

    # Length check
    if (length(x) != 1L) {
      cli::cli_abort(
        "{.arg {nm}} must be a single TRUE or FALSE"
      )
    }

    # NA check
    if (is.na(x)) {
      cli::cli_abort(
        "{.arg {nm}} must be TRUE or FALSE (not NA)"
      )
    }
  }

  invisible(TRUE)
}

#' @title Validate non-empty character scalar(s)
#'
#' @description
#' Validates that each provided argument is a non-empty character scalar.
#' It is designed for internal argument validation in functions that expect
#' single string inputs (e.g. keys, field names, paths).
#'
#' @param ... Objects to validate. Each must be:
#'   * a `character` vector,
#'   * of length 1,
#'   * not `NA`,
#'   * not an empty string.
#' @param unique A `logical`. If `TRUE` (default), validates that each argument
#'   is of length 1. If `FALSE`, validates that each argument is a non-empty
#'   character vector (any length ≥ 1).
#'
#' @return
#' Invisibly returns `TRUE` if all inputs are valid.
#'
#' @details
#' The function supports validating multiple arguments at once.
#' Each argument is checked independently and errors immediately if invalid.
#'
#' @keywords internal
check_character <- function(..., unique = TRUE) {

  old_warn <- getOption("warn")
  on.exit(options(warn = old_warn), add = TRUE)
  options(warn = -1)

  dots  <- list(...)
  calls <- as.list(substitute(list(...)))[-1]
  nms   <- names(dots)

  for (i in seq_along(dots)) {

    x <- dots[[i]]

    nm <- if (!is.null(nms) && nzchar(nms[i])) {
      nms[i]
    } else {
      deparse(calls[[i]])
    }

    # Type verification
    if (!is.character(x)) {
      cli::cli_abort(
        "{.arg {nm}} must be a character vector"
      )
    }

    # Length check according to 'unique'
    if (unique) {
      if (length(x) != 1L) {
        cli::cli_abort(
          "{.arg {nm}} must be a single non-empty character"
        )
      }
      # For a scalar, check NA and empty string
      if (is.na(x) || !nzchar(x)) {
        cli::cli_abort(
          "{.arg {nm}} must be a single non-empty character"
        )
      }
    } else {
      # For unique = FALSE, check that there are no empty or NA elements
      if (length(x) == 0L) {
        cli::cli_abort(
          "{.arg {nm}} must be a non-empty character vector"
        )
      }
      if (any(is.na(x)) || any(!nzchar(x))) {
        cli::cli_abort(
          "{.arg {nm}} must be a non-empty character vector (no NA or empty strings)"
        )
      }
    }
  }

  invisible(TRUE)
}

#' @title Validate data.frame / sf inputs and required structure
#'
#' @description
#' Validates that provided objects are tabular structures (e.g. `data.frame` or `sf`)
#' and optionally enforce required column presence.
#' This function is intended for internal argument validation in data-processing
#' pipelines relying on structured spatial or non-spatial tabular data.
#'
#' @param ... Objects to validate. Each must inherit from the class specified
#'   in `class` and contain at least one row.
#' @param class A `character` vector specifying allowed classes (e.g.
#'   `c("data.frame", "sf")`). Objects must inherit from at least one of them.
#' @param fields A `character` vector of required column names.
#'   If `NULL` (default), column validation is skipped.
#'   Must be non-empty if provided.
#'
#' @return
#' Invisibly returns `TRUE` if all inputs satisfy the validation rules.
#'
#' @details
#' Each input object is validated independently.
#' The function checks:
#' \itemize{
#'   \item class inheritance (via `inherits()`),
#'   \item non-zero number of rows,
#'   \item presence of required columns (if provided).
#' }
#'
#' Validation fails immediately on the first detected inconsistency.
#'
#' @keywords internal
check_table <- function(..., class, fields = NULL) {

  # --- Silence warnings locally ---
  old_warn <- getOption("warn")
  on.exit(options(warn = old_warn), add = TRUE)
  options(warn = -1)

  # --- Capture arguments ---
  dots  <- list(...)
  calls <- as.list(substitute(list(...)))[-1]
  nms   <- names(dots)

  # --- Validate 'class' ---
  if (!is.character(class) || length(class) == 0L) {
    cli::cli_abort("{.arg class} must be a non-empty character vector")
  }

  # --- Validate 'fields' only if provided ---
  if (!is.null(fields)) {

    if (!is.character(fields) || length(fields) == 0L) {
      cli::cli_abort("{.arg fields} must be NULL or a non-empty character vector")
    }

    if (any(is.na(fields)) || any(!nzchar(fields))) {
      cli::cli_abort("{.arg fields} must not contain NA or empty strings")
    }
  }

  # --- Loop over inputs ---
  for (i in seq_along(dots)) {

    x <- dots[[i]]

    nm <- if (!is.null(nms) && nzchar(nms[i])) {
      nms[i]
    } else {
      deparse(calls[[i]])
    }

    # --- Class check ---
    if (!inherits(x, class)) {
      cli::cli_abort(
        "{.arg {nm}} must inherit from {.cls {class}}"
      )
    }

    # --- Empty check ---
    if (nrow(x) == 0) {
      cli::cli_abort(
        "{.arg {nm}} is empty"
      )
    }

    # --- Fields check (only if provided) ---
    if (!is.null(fields)) {
      missing_fields <- setdiff(fields, names(x))

      if (length(missing_fields) > 0) {
        cli::cli_abort(
          "{.val {length(missing_fields)}} missing column{?s} in {.arg {nm}}: {.val {missing_fields}}"
        )
      }
    }
  }

  invisible(TRUE)
}

#' @title Validate scalar numeric values
#'
#' @description
#' Checks that each supplied argument is a scalar numeric value and
#' optionally validates minimum and maximum bounds.
#'
#' @param ... Named numeric values to validate.
#' @param min Optional minimum allowed value.
#' @param max Optional maximum allowed value.
#' @param null_is_valid Logical. Whether `NULL` values are accepted.
#'
#' @return Invisibly returns `TRUE`.
#'
#' @importFrom cli cli_abort
#'
#' @keywords internal
check_numeric <- function(
    ...,
    min = NULL,
    max = NULL,
    null_is_valid = FALSE
) {

  args <- list(...)

  if (length(args) == 0) {
    cli::cli_abort("At least one argument must be supplied.")
  }

  for (name in names(args)) {

    x <- args[[name]]

    if (is.null(x)) {

      if (null_is_valid) {
        next
      }

      cli::cli_abort(
        "{.arg {name}} cannot be {.val NULL}."
      )
    }

    if (!is.numeric(x) || length(x) != 1 || is.na(x)) {
      cli::cli_abort(
        "{.arg {name}} must be a scalar numeric value."
      )
    }

    if (!is.null(min) && x < min) {
      cli::cli_abort(
        "{.arg {name}} must be greater than or equal to {.val {min}}."
      )
    }

    if (!is.null(max) && x > max) {
      cli::cli_abort(
        "{.arg {name}} must be less than or equal to {.val {max}}."
      )
    }

  }

  invisible(TRUE)

}
