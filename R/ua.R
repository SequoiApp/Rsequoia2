#' Create _UA_ sf object from _PARCA_ sf object
#'
#' @param parca `sf` Object from [Rsequoia2::seq_parca()] containing cadastral
#' parcels.
#'
#' @return An `sf` object containing the analysis units.
#'
#' @export
parca_to_ua <- function(parca) {
  ua <- seq_normalize(parca, "ua")
}

#' Create analysis units layer
#'
#' This function reads the cadastral layer file from a project directory
#' and create the analysis units layer.
#'
#' The resulting object is returned invisibly as an `sf` polygons layer.
#' The output file is automatically written into the working directory defined
#' by `dirname`.
#'
#' @inheritParams create_matrice
#'
#' @return An `sf` object
#'
#' @export
seq_parca_to_ua <- function(
    dirname = ".",
    verbose = TRUE,
    overwrite = FALSE){

  # create ua
  parca <- seq_read("v.seq.parca.poly", dirname = dirname, verbose = verbose)
  ua <- parca_to_ua(parca)

  # write ua
  ua_path <- seq_write(
    seq_ua,
    "v.seq.ua.poly",
    dirname = dirname,
    verbose = verbose,
    overwrite = overwrite
  )

  return(invisible(ua_path |> as.list()))
}

#' Check cadastral IDU consistency between _UA_ and _PARCA_ sf objects
#'
#' @param ua `sf` Object from [Rsequoia2::parca_to_ua()] containing analysis
#' units
#' @inheritParams parca_to_ua
#' @param verbose `logical` If `TRUE`, display progress messages.
#'
#' @return `TRUE` if all `parca` idu values are found in `ua`;
#'         `FALSE` otherwise (with CLI messages if `verbose = TRUE`).
#'
#' @importFrom cli cli_alert_warning cli_ul
#'
#' @export
ua_check_idu <- function(ua, parca, verbose = FALSE) {
  # Get canonical idu field name
  idu <- seq_field("idu")$name

  # Extract idu vectors
  idu_pc_vals <- unique(parca[[idu]])
  idu_ua_vals <- unique(ua[[idu]])
  idu_ua_vals <- idu_ua_vals[!is.na(idu_ua_vals)]

  # Compute missing values
  missing_idu <- setdiff(idu_pc_vals, idu_ua_vals)

  # If mismatches found report + return FALSE
  if (length(missing_idu)) {
    cli::cli_warn(
      "Some cadastral IDUs from {.arg parca} are missing in {.arg ua}: {.val {sort(missing_idu)}}"
      )
    return(FALSE)
  }

  # All good return TRUE
  if (verbose) {
    cli::cli_alert_success(
      "All cadastral IDUs from {.arg parca} are present in {.arg ua}."
    )
  }

  return(TRUE)
}

#' Update cadastral area values in _UA_ using _PARCA_
#'
#' This function compares cadastral area values between _UA_ and _PARCA_,
#' and updates _UA_ wherever discrepancies are detected.
#'
#' @inheritParams ua_check_idu
#'
#' @return Updated `ua` object.
#'
#' @export
ua_check_area <- function(ua, parca, verbose = FALSE) {

  # Field names from YAML structure
  idu <- seq_field("idu")$name            # e.g. "IDU"
  surf_cad <- seq_field("surf_cad")$name  # e.g. "SURF_CA"

  # Drop geometry and keep only needed PARCA fields
  parca_tab <- parca |>
    sf::st_drop_geometry() |>
    subset(select = c(idu, surf_cad))

  # Match UA rows to PARCA rows
  m <- match(ua[[idu]], parca_tab[[idu]])

  # Extract PARCA area values aligned on UA order
  parca_vals <- parca_tab[[surf_cad]][m]

  # Detect differences
  diff_idx <- which(!is.na(parca_vals) & ua[[surf_cad]] != parca_vals)
  nb_diff <- length(diff_idx)

  # Apply corrections (vectorized)
  if (nb_diff > 0) {
    bad_idu <- ua[[idu]][diff_idx]
      cli::cli_warn(
        "{nb_diff} cadastral area value{?s} corrected in UA. Affected Idu{?s}: {.val {bad_idu}}"
      )

    ua[[surf_cad]][diff_idx] <- parca_vals[diff_idx]
  }

  if (verbose & nb_diff == 0) {
    cli_alert_success("No cadastral area discrepancies detected.")
  }

  return(invisible(ua))
}

#' Create management unit field (UG) in the _UA_ sf object
#'
#' Generates a standardized management unit identifier (UG) in the _UA_ object
#' based on configured parcel keys.
#'
#' @param ua `sf` object containing analysis units;
#' must contain fields used by `ug_keys`.
#' @param ug_keys `character` vector, default `c("parcelle", "sous_parcelle")`.
#' Keys used to build the UG identifier.
#' @param separator `character`, default `"."`. Separator between keys.
#' @param verbose `logical` If `TRUE`, display progress messages.
#'
#' @return `sf` object UA with UG field filled.
#'
#' @importFrom cli cli_alert_success
#'
#' @export
ua_generate_ug <- function(
    ua,
    ug_keys = c("parcelle", "sous_parcelle"),
    separator = ".",
    verbose = TRUE
    ){

  ug_ua <- seq_field("ug")$name
  fields <- vapply(ug_keys, function(k) seq_field(k)$name, character(1))

  # RMQ: Used to avoid bad filtering (ex : 1, 10, 2, 3 VS 01, 02, 03, 10)
  # Clean/pad each column
  cleaned <- lapply(fields, function(f) {
    x <- ua[[f]]
    # Replace NA with "00", pad numeric strings, keep letters as is
    ifelse(is.na(x), "00",
           ifelse(grepl("^[A-Za-z]+$", x), x, pad_left(x, 2))
    )
  })

  # Concatenate columns to create UG
  ua[[ug_ua]] <- do.call(paste, c(cleaned, sep = separator))

  if (verbose) cli::cli_alert_success(paste0("UG field '", ug_ua, "' created."))

  return(invisible(ua))
}

#' Calculate areas in the _UA_ sf object
#'
#' Computes the corrected cadastral areas for units of analysis according to
#' cartographic area.
#'
#' @param ua `sf` object containing analysis units.
#' @param verbose `logical` If `TRUE`, display progress messages.
#'
#' @return `sf` object with updated areas
#'
#' @export
ua_generate_area <- function(ua, verbose = TRUE) {
  # Field names from configuration
  idu <- seq_field("idu")$name
  surf_cad <- seq_field("surf_cad")$name
  surf_sig <- seq_field("surf_sig")$name
  surf_cor <- seq_field("surf_cor")$name

  # Calculate mapped surface in hectares
  ua[[surf_sig]] <- as.numeric(sf::st_area(ua)) / 10000

  # Compute correction factor per cadastral ID
  sum_sig <- stats::ave(ua[[surf_sig]], ua[[idu]], FUN = sum)
  # Avoid edge case when sum_sig == 0
  sum_sig <- replace(sum_sig, sum_sig == 0, 1)
  coeff <- ua[[surf_sig]] / sum_sig

  # Compute provisional corrected surfaces
  surf_temp <- round(ua[[surf_cad]] * coeff, 4)
  resid <- ua[[surf_cad]] - stats::ave(surf_temp, ua[[idu]], FUN = sum)

  # Identify pivot feature to absorb residual
  is_pivot <- surf_temp == stats::ave(surf_temp, ua[[idu]], FUN = max)

  # Assign corrected surface
  ua[[surf_cor]] <- ifelse(is_pivot, round(surf_temp + resid, 4), surf_temp)

  if (verbose) cli_alert_success("Corrected cadastral areas calculated.")

  return(invisible(ua))
}

#' Retrieve description field names
#'
#' Returns the list of _UA_ description fields as defined in the configuration.
#' This is an internal helper used to keep `ua_*()` functions consistent.
#'
#' @return A character vector of field names.
#' @keywords internal
#' @noRd
seq_desc_fields <- function() {
  keys <- c(
    "peuplement","richesse","stade","annee","structure",
    "ess1","ess1_pct","ess2","ess2_pct","ess3","ess3_pct",
    "taillis","regeneration","amenagement"
  )
  vapply(keys, function(k) seq_field(k)$name, character(1))
}

#' Check management unit (UG) consistency in the _UA_ sf object
#'
#' A management unit must (UG) can only have a single description. Therefore,
#' all units of analysis within the same UG must include the same descriptive
#' elements.
#' This function analyzes the consistency of the descriptive elements for each
#' management unit, and marks each row with a logical flag `ug_valid`
#' indicating whether it is consistent with the dominant description of the UG.
#'
#' The dominant description corresponds to the one with the largest surface
#' area share.
#'
#' @param ua `sf` object containing analysis units;
#' with at least the UG identifier field and relevant attribute fields.
#' @param verbose `logical` If `TRUE`, display progress messages.
#'
#' @return An `sf` object identical to `ua`, with an additional logical column
#' `ug_valid` indicating if each row is consistent with the dominant UG description.
#'
#' @export
ua_check_ug <- function(ua,
                        verbose = TRUE) {

  # Resolve field name
  # desc : descriptive fields
  ug <- seq_field("ug")$name
  desc <- intersect(seq_desc_fields(), names(ua))

  ua_no_geom <- sf::st_drop_geometry(ua)
  desc_id <- do.call(paste, c(ua_no_geom[desc], sep = "|"))

  # More than one desc_id in a UG -> inconsistency
  valid <- tapply(desc_id, ua[[ug]], \(x) length(unique(x)) == 1)
  invalid <- names(valid)[!valid]

  # CLI messages
  if (length(invalid)) {
    cli::cli_warn("{length(invalid)} inconsistent UG{?s}: {.val {invalid}}")
    return(invisible(FALSE))
  }

  if (verbose) cli::cli_alert_success("All UG are consistent.")

  return(invisible(TRUE))
}

#' Clean management units (UG) by correcting minor inconsistencies in the _UA_ sf object
#'
#' Detects and corrects minor inconsistencies within management units
#' (UG) in _UA_. Lines with small surfaces relative to their UG are updated
#' to match the dominant description.
#'
#' @param ua `sf` object containing analysis units;
#' with at least the UG identifier field and relevant attribute fields.
#' @param atol Absolute tolerance for surface correction (default 0.50 ha).
#' @param rtol Relative tolerance for surface correction within a UG (default 10%).
#'
#' @return An `sf` object identical to `ua`, with minor inconsistent lines
#' corrected and an additional logical column `ug_valid` indicating UG
#' consistency.
#'
#' @details
#' The function works by:
#' 1. Checking UG consistency with `ua_check_ug()`.
#' 2. Identifying the dominant row per UG (largest total surface).
#' 3. Correcting rows that differ from the dominant description and whose
#'    surface is smaller than `atol` and represents less than `rtol` of UG total.
#' 4. Returning the corrected UA and rechecking inconsistencies.
#'
#' @export
ua_clean_ug <- function(
    ua,
    atol = 0.50,
    rtol = 0.10){

  ug <- seq_field("ug")$name
  surf <- seq_field("surf_cor")$name
  desc <- intersect(seq_desc_fields(), names(ua))

  cleaned <- by(ua, ua[[ug]], function(df) {

    df_no_geom <- sf::st_drop_geometry(df)
    desc_id <- do.call(paste, c(df_no_geom[desc], sep = "|"))
    s <- df[[surf]]

    # Total surface per description
    s_by_desc <- tapply(s, desc_id, sum, na.rm = TRUE)
    main_id <- names(which.max(s_by_desc))
    main_row <- match(main_id, desc_id)

    # Rows that should be corrected
    to_fix <- (s <= atol) | (s <= rtol * max(s_by_desc))

    df[to_fix, desc] <- sf::st_drop_geometry(df[main_row, desc])

    return(df)
  })

  ua_cleaned <- sf::st_as_sf(do.call(rbind.data.frame, cleaned))

  return(ua_cleaned)
}

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
#'
#' @return An updated `sf` object identical to `ua`, but with:
#' - IDUs checked against PARCA,
#' - cadastral areas checked and corrected,
#' - management unit fields generated,
#' - corrected cadastral areas added,
#' - management unit consistency checked and corrected.
#'
#' @export
ua_to_ua <- function(ua, parca, verbose = TRUE){

  # Check ua
  # Why adding this warning ? it is already in ua_check_idu() right ?
  idu_valid <- ua_check_idu(ua, parca, verbose = verbose)
  if (!idu_valid){
    cli::cli_abort("Please correct IDU inconsistency before going further.")
  }

  # Compute ua
  ua <- ua_check_area(ua, parca, verbose = verbose) |>
    ua_generate_ug(verbose = verbose) |>
    ua_generate_area(verbose = verbose) |>
    ua_clean_ug()

  is_valid <- ua_check_ug(ua, verbose = verbose)
  if (!is_valid) {
    cli::cli_warn("You need to correct the inconsistent units in the UA layer.")
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
    overwrite = FALSE){

  # read
  parca <- seq_read("v.seq.parca.poly", dirname = dirname, verbose = verbose)
  ua <- seq_read("v.seq.ua.poly", dirname = dirname, verbose = verbose)

  # ua treatment
  seq_ua <- ua_to_ua(ua, parca, verbose = verbose)

  # write ua
  ua_path <- seq_write(
    seq_ua,
    "v.seq.ua.poly",
    dirname = dirname,
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

    sf::write_sf(seq_ua, ua_path, secure_ua_path)

    if (verbose) {
      cli::cli_alert_success(
        "UA also saved as {.file {basename(secure_ua_path)}} for safety."
      )
    }
  }

  return(invisible(ua_path))
}
