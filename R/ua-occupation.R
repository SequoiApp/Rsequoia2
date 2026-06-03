#' Repair DGD submitted status
#'
#' Initializes DGD submitted status to TRUE when the field is empty.
#'
#' @param ua `sf` object containing UA polygons.
#' @param verbose Logical. Should messages be printed?
#'
#' @return The repaired `ua` object.
ua_repair_dgd <- function(ua, verbose = TRUE) {
  is_dgd <- seq_field("is_dgd")$name

  if (!is_dgd %in% names(ua) || all(is.na(ua[[is_dgd]]))) {
    if (verbose) {
      cli::cli_alert_warning(
        "{.field {is_dgd}} is empty and was initialized to {.val TRUE}."
      )
    }

    ua[[is_dgd]] <- TRUE
  }

  ua
}


#' Repair wooded status from DGD status
#'
#' Forces wooded status to FALSE when a surface is not submitted to DGD.
#'
#' @param ua `sf` object containing UA polygons.
#' @param verbose Logical. Should messages be printed?
#'
#' @return The repaired `ua` object.
ua_repair_wooded <- function(ua, verbose = TRUE) {
  is_dgd <- seq_field("is_dgd")$name
  is_wooded <- seq_field("is_wooded")$name
  mgmt_code <- seq_field("mgmt_code")$name

  bad <- !ua[[is_dgd]] & ua[[is_wooded]]

  if (any(bad, na.rm = TRUE)) {
    n_bad <- sum(bad, na.rm = TRUE)
    bad_mgmt_code <- unique(stats::na.omit(ua[[mgmt_code]][bad]))

    if (verbose) {
      cli::cli_alert_info(
        paste(
          "{.val {n_bad}} wooded status{?es} corrected:",
          "{.field {is_wooded}} set to {.val FALSE} because",
          "{.field {is_dgd}} is {.val FALSE} for {.field {mgmt_code}}",
          "{.val {bad_mgmt_code}}."
        ),
        wrap = TRUE
      )
    }

    ua[[is_wooded]][bad] <- FALSE
  }

  return(ua)
}

#' Check non-wooded surface threshold
#'
#' Checks that non-wooded submitted surfaces do not exceed 10% of submitted
#' DGD surfaces.
#'
#' @param ua `sf` object containing UA polygons.
#' @param threshold Maximum allowed non-wooded ratio. Defaults to `0.10`.
#' @param verbose Logical. Should messages be printed?
#'
#' @return `TRUE` if the threshold is respected, otherwise `FALSE`.
ua_check_non_wooded_threshold <- function(ua, threshold = 0.10, verbose = TRUE) {
  is_dgd <- seq_field("is_dgd")$name
  is_wooded <- seq_field("is_wooded")$name
  cor_area <- seq_field("cor_area")$name

  dgd <- ua[[is_dgd]]
  wooded <- ua[[is_wooded]]
  area <- ua[[cor_area]]

  total_area <- sum(area[dgd], na.rm = TRUE)
  non_wooded_area <- sum(area[dgd & !wooded], na.rm = TRUE)

  if (is.na(total_area) || total_area <= 0) {
    if (verbose) {
      cli::cli_alert_warning(
        "No submitted DGD surface found. Non-wooded threshold was not checked."
      )
    }

    return(FALSE)
  }

  ratio <- non_wooded_area / total_area
  pct <- round(ratio * 100, 2)
  threshold_pct <- threshold * 100

  if (ratio > threshold) {
    if (verbose) {
      cli::cli_alert_warning(
        "Non-wooded submitted surface exceeds the {threshold_pct}% threshold: {pct}%."
      )
    }

    return(FALSE)
  }

  if (verbose) {
    cli::cli_alert_success(
      "Non-wooded submitted surface is {pct}%, below the {threshold_pct}% threshold."
    )
  }

  return(TRUE)
}

#' Aggregate _UA_ surfaces by occupation status
#'
#' Aggregates corrected surface areas from a _UA_ layer by grouping rows
#' according to DGD-submitted and wooded status.
#'
#' @param ua `sf` object containing analysis units.
#' @param verbose `boolean` if `TRUE` print messages
#'
#' @return An `sf` object aggregated by occupation status.
#'
#' @seealso [seq_occupation()], [seq_field()], [seq_normalize()]
#'
#' @export
ua_to_occupation <- function(ua, verbose = TRUE) {
  ua <- seq_normalize(ua, "ua")

  is_dgd <- seq_field("is_dgd")$name
  is_wooded <- seq_field("is_wooded")$name
  cor_area <- seq_field("cor_area")$name

  if (all(is.na(ua[[is_dgd]]))) {
    cli::cli_abort(c(
      "x" = "Failed to generate occupation layer from UA.",
      "!" = "Field {.field {is_dgd}} in UA is missing or is empty."
    ))
  }

  if (all(is.na(ua[[is_wooded]]))) {
    cli::cli_abort(c(
      "x" = "Failed to generate occupation layer from UA.",
      "!" = "Field {.field {is_wooded}} in UA is missing or is empty."
    ))
  }

  ua <- ua_repair_wooded(ua, verbose = verbose)

  dgd <- ua[[is_dgd]]
  wooded <- ua[[is_wooded]]

  ua[["OCCUPATION"]] <- c(
    "NON_SOUMIS",
    "SOUMIS_NON_BOISE",
    "SOUMIS_BOISE"
  )[dgd + wooded + 1L]

  by <- list(
    ua[["OCCUPATION"]],
    ua[[is_dgd]],
    ua[[is_wooded]]
  ) |>
    setNames(c("OCCUPATION", is_dgd, is_wooded))

  aggregate(
    x = ua[cor_area],
    by = by,
    FUN = sum,
    na.rm = TRUE
  )
}

#' Create occupation layer from _UA_ for a Sequoia project
#'
#' Reads the UA polygon layer, aggregates surfaces by occupation status, and
#' writes the derived occupation layer to the project directory.
#'
#' @inheritParams seq_write
#'
#' @return An invisible file path to the exported occupation layer.
#'
#' @seealso [ua_to_occupation()], [seq_read()], [seq_write()]
#'
#' @export
seq_occupation <- function(dirname = ".", verbose = TRUE, overwrite = FALSE) {
  parca <- seq_read("v.seq.parca.poly", dirname = dirname, verbose = FALSE)
  identifier <- seq_field("identifier")$name
  id <- unique(parca[[identifier]])

  ua <- seq_read("v.seq.ua.poly", dirname = dirname, verbose = FALSE)

  occupation <- ua_to_occupation(ua, verbose = verbose)

  occupation_path <- seq_write(
    occupation,
    "v.seq.occup.poly",
    dirname = dirname,
    id = id,
    verbose = verbose,
    overwrite = overwrite
  )

  invisible(occupation_path)
}
