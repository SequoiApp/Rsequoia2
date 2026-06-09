#' Check and update _UA_ consistency with cadastral _PARCA_ data
#'
#' This function verifies and updates the consistency of analysis units (UA)
#' using cadastral parcel data (PARCA). It checks matching IDUs, validates areas,
#' generates management units (UG), computes corrected areas, and ensures the
#' internal consistency of the resulting UA object.
#'
#' @param ua `sf` object containing analysis units.
#' @param parca `sf` object, typically produced by [Rsequoia2::seq_parca()],
#'   containing cadastral parcels.
#' @param verbose `logical` If `TRUE`, display progress messages.
#' @param check `logical` If `TRUE`, ask user.
#'
#' @return An updated `sf` object identical to `ua`, but with:
#' - IDUs checked against PARCA,
#' - cadastral areas checked and corrected,
#' - management unit fields generated,
#' - corrected cadastral areas added,
#' - management unit consistency checked and corrected.
#'
#' @export
ua_to_ua <- function(ua, parca, verbose = TRUE, check = interactive()) {

  if (check && interactive()) {
    cli::cli_alert_warning(
      "{.var ua} should be topologically valid before generating UG."
    )

    if (utils::menu(c("Confirm and continue", "Cancel")) != 1) {
      cli::cli_abort("UG generation aborted by user.")
    }
  }

  ua <- seq_normalize(ua, "ua")

  # Check and repair cadastral consistency
  ua <- ua_check_coverage(ua, parca)
  ua <- ua_repair_idu(ua, parca, verbose)

  if (!ua_check_idu(ua, parca, verbose)) {
    cli::cli_abort("UA and PARCA IDU values are inconsistent after repair.")
  }

  ua <- ua_update_parca_fields(ua, parca)

  # Generate UG
  ua <- ua_generate_ug(ua, verbose = verbose)
  ua <- ua_generate_area(ua, verbose = verbose)

  # Repair and check occupation consistency
  ua <- ua_repair_dgd(ua, verbose = verbose)
  ua <- ua_repair_wooded(ua, verbose = verbose)
  if (!ua_check_non_wooded_threshold(ua, verbose = verbose)) {
    cli::cli_alert_danger(
      "You need to correct the non-wooded submitted surfaces in the UA layer."
    )
  }

  ua <- ua_clean_ug(ua)

  if (!ua_check_ug(ua, verbose = verbose)) {
    cli::cli_abort(
      "You need to correct the inconsistent units in the UA layer."
    )
  }

  return(ua)
}

#' Check and update the analysis units layer
#'
#' This function reads the analysis units layer file from a project directory,
#' check and udapte attributes and overwrite it.
#'
#' The resulting object is returned invisibly as an `sf` polygons layer.
#' The output file is automatically written into the working directory defined
#' by `dirname`.
#'
#' @inheritParams seq_write
#' @param secure `logical`. If `TRUE`, also writes a timestamped secure copy
#' of the file.
#'
#' @return An `sf` object
#'
#' @export
seq_ua <- function(
    dirname = ".",
    secure = TRUE,
    verbose = TRUE,
    overwrite = TRUE){

  # read
  parca <- seq_read("v.seq.parca.poly", dirname = dirname, verbose = FALSE)
  identifier <- seq_field("identifier")$name
  id <- unique(parca[[identifier]])

  ua <- seq_read("v.seq.ua.poly", dirname = dirname, verbose = FALSE)

  # ua treatment
  seq_ua <- ua_to_ua(ua, parca, verbose = verbose)

  # write ua
  ua_path <- seq_write(
    seq_ua,
    "v.seq.ua.poly",
    dirname = dirname,
    id = id,
    verbose = verbose,
    overwrite = overwrite
  )

  # secure write
  if (secure) {
    date_str <- format(Sys.time(), "%Y%m%dT%H%M%S")
    secure_ua_path <- sprintf(
      "%s_%s.%s",
      tools::file_path_sans_ext(ua_path),
      date_str,
      tools::file_ext(ua_path)
    )

    sf::write_sf(seq_ua, secure_ua_path)

    if (verbose) {
      cli::cli_alert_success(
        "UA also saved as {.file {basename(secure_ua_path)}} for safety."
      )
    }
  }

  return(invisible(ua_path))
}
