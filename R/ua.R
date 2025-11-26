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

#' Check cadastral IDU consistency between UA and PARCA sf objects
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

#' Update cadastral area values in UA using PARCA
#'
#' This function compares cadastral area values between UA and PARCA,
#' and updates UA wherever discrepancies are detected.
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

  return(ua)
}

#' Create management unit field (UG) in the UA sf object
#'
#' Generates a standardized management unit identifier (UG) in the UA object
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

#' Calculate areas in the UA sf object
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

#' Check management unit (UG) consistency in the UA sf object
#'
#' Check the internal consistency of management units (UG) in UA by
#' comparing the summed corrected surface (`surf_cor`) for each unique
#' combination of UG attributes against the total surface of the UG.
#' Marks each row with a logical flag `ug_valid` indicating whether it is
#' consistent with the majority description of the UG.
#'
#' @param ua `sf` object containing analysis units;
#' with at least the UG identifier field and relevant attribute fields.
#' @param ug_keys `character` vector of attribute keys used to define UG
#' descriptions.
#' @param verbose `logical` If `TRUE`, display progress messages.
#'
#' @return An `sf` object identical to `ua`, with an additional logical column
#' `ug_valid` indicating if each row is consistent with the dominant UG description.
#'
#' @export
ua_check_ug <- function(ua,
                        ug_keys = c("peuplement", "richesse", "stade", "annee", "structure",
                                    "ess1", "ess1_pct", "ess2", "ess2_pct", "ess3", "ess3_pct",
                                    "taillis", "regeneration", "amenagement"),
                        verbose = TRUE) {

  # Resolve field names
  ug_field   <- seq_field("ug")$name
  key_fields <- vapply(ug_keys, function(k) seq_field(k)$name, character(1))
  surf_cor   <- seq_field("surf_cor")$name

  # Keep only existing fields
  fields <- intersect(c(ug_field, key_fields), names(ua))

  # Diagnostic signature
  desc <- apply(as.data.frame(ua)[, fields, drop = FALSE], 1, function(r) paste0(na.omit(r), collapse = "|"))

  # Surface sums
  sum_by_desc <- stats::ave(ua[[surf_cor]], desc, FUN = function(x) sum(x, na.rm = TRUE))
  sum_by_ug   <- stats::ave(ua[[surf_cor]], ua[[ug_field]], FUN = function(x) sum(x, na.rm = TRUE))

  # Logical validity
  ua$ug_valid <- sum_by_desc == sum_by_ug

  # UG inconsistencies
  invalid_ugs <- unique(ua[[ug_field]][!ua$ug_valid])
  n_invalid <- length(invalid_ugs)

  # CLI messages
  if (verbose){
    if (n_invalid > 0) {
      cli::cli_warn(
        "{n_invalid} UG{?s} inconsistent. Affected UG{?s}: {.val {invalid_ugs}}"
      )
    } else {
      cli::cli_alert_success("All UG are consistent.")
    }
  }

  ua
}

#' Clean management units (UG) by correcting minor inconsistencies in the UA sf object
#'
#' Detects and optionally corrects minor inconsistencies within management units
#' (UG) in UA. Lines with small surfaces relative to their UG are updated
#' to match the dominant description.
#'
#' @param ua `sf` object containing analysis units;
#' with at least the UG identifier field and relevant attribute fields.
#' @param ug_keys `character` vector of attribute keys used to define UG
#' descriptions.
#' @param atol Absolute tolerance for surface correction (default 0.50 ha).
#' @param rtol Relative tolerance for surface correction within a UG (default 0.10).
#' @param verbose `logical` If `TRUE`, display progress messages.
#'
#' @return An `sf` object identical to `ua`, with minor inconsistent lines corrected
#' and an additional logical column `ug_valid` indicating UG consistency.
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
ua_clean_ug <- function(ua,
                        ug_keys = c("peuplement", "richesse", "stade", "annee", "structure",
                                    "ess1", "ess1_pct", "ess2", "ess2_pct", "ess3", "ess3_pct",
                                    "taillis", "regeneration", "amenagement"),
                        atol = 0.50,
                        rtol = 0.10,
                        verbose = TRUE) {

  # Resolve field names
  ug_field   <- seq_field("ug")$name
  key_fields <- vapply(ug_keys, function(k) seq_field(k)$name, character(1))
  surf_cor   <- seq_field("surf_cor")$name
  all_fields <- intersect(c(ug_field, key_fields), names(ua))

  # Initial inconsistency check
  ua_checked <- ua_check_ug(ua, ug_keys = ug_keys, verbose = FALSE)
  invalid_idx <- which(!ua_checked$ug_valid)

  if (length(invalid_idx) == 0) {
    if (verbose) cli::cli_alert_success("No inconsistencies detected.")
    return(ua_checked)
  }

  if (verbose) {
    bad_ugs <- unique(ua_checked[[ug_field]][invalid_idx])
    cli::cli_alert_warning("{length(bad_ugs)} UG{?s} inconsistent. Affected UG{?s}: {.val {bad_ugs}}")
    cli::cli_alert_warning("Attempt to fix")
  }

  # Split by UG
  ua_split <- split(ua_checked, ua_checked[[ug_field]])

  # Function to correct one UG
  correct_one_ug <- function(df, surf_cor, all_fields, atol, rtol) {
    # Build unique description per row
    df$desc <- apply(df[, all_fields, drop = FALSE], 1, function(r) paste0(na.omit(r), collapse = "|"))

    # Identify dominant row (largest total surface)
    sum_by_desc <- tapply(df[[surf_cor]], df$desc, sum, na.rm = TRUE)
    dominant_desc <- names(sum_by_desc)[which.max(sum_by_desc)]
    total_surface <- sum(df[[surf_cor]], na.rm = TRUE)
    dominant_row <- df[df$desc == dominant_desc, all_fields, drop = FALSE][1, ]

    # Lines that differ from dominant description
    diff_idx <- which(df$desc != dominant_desc)

    # Minor lines: small absolute and relative surface
    minor_idx <- diff_idx[df[[surf_cor]][diff_idx] < atol & df[[surf_cor]][diff_idx] / total_surface < rtol]

    # Correct minor lines by copying dominant row values
    if (length(minor_idx) > 0) {
      for (col_name in all_fields) {
        df[minor_idx, col_name] <- dominant_row[[col_name]]
      }
    }

    df[, setdiff(names(df), "desc")]
  }

  # Apply correction to each UG
  corrected_list <- lapply(
    ua_split,
    function(df) correct_one_ug(df, surf_cor = surf_cor, all_fields = all_fields, atol = atol, rtol = rtol)
  )
  ua_corrected <- do.call(rbind, corrected_list)

  # Recheck inconsistencies after correction
  ua_corrected <- ua_check_ug(ua_corrected, ug_keys = ug_keys, verbose = verbose)

  return(ua_corrected)
}
