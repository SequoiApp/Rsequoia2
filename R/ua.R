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
#' @param ua `sf` object containing analysis units; must contain the idu field.
#' @param parca `sf` Object containing cadastral parcels; must contain the idu field.
#' @param verbose `logical` If `TRUE`, display progress messages.
#'
#' @return `TRUE` if all `parca` idu values are found in `ua`;
#'         `FALSE` otherwise (with CLI messages if `verbose = TRUE`).
#'
#' @importFrom cli cli_alert_warning cli_ul
#'
#' @export
ua_check_idu <- function(ua, parca, verbose = TRUE) {
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
    if (verbose) {
      cli_alert_warning(
        "Some cadastral IDUs from {.field parca} are missing in {.field ua}."
      )
      cli_ul(sort(missing_idu))
    }
    return(FALSE)
  }

  # All good return TRUE
  if (verbose) {
    cli_alert_success(
      "All cadastral IDUs from {.field parca} are present in {.field ua}."
    )
  }

  TRUE
}

#' Update cadastral area values in UA using PARCA
#'
#' This function compares cadastral area values between UA and PARCA,
#' and updates UA wherever discrepancies are detected.
#'
#' @param ua `sf` object containing analysis units;
#' must contain the idu and surf_cad fields.
#' @param parca `sf` Object containing cadastral parcels;
#' must contain the idu and surf_cad fields.
#' @param verbose `logical` If `TRUE`, display progress messages.
#'
#' @return Updated `ua` object.
#'
#' @importFrom sf st_drop_geometry
#' @importFrom cli cli_alert_warning cli_li cli_alert_success
#'
#' @export
ua_update_surfcad <- function(ua, parca, verbose = TRUE) {

  # Field names from YAML structure
  idu <- seq_field("idu")$name            # e.g. "IDU"
  surf_cad <- seq_field("surf_cad")$name  # e.g. "SURF_CA"

  # Drop geometry and keep only needed PARCA fields
  parca_tab <- parca |>
    st_drop_geometry() |>
    subset(select = c(idu, surf_cad))

  # Match UA rows to PARCA rows
  m <- match(ua[[idu]], parca_tab[[idu]])

  # Extract PARCA area values aligned on UA order
  parca_vals <- parca_tab[[surf_cad]][m]

  # Detect differences
  diff_idx <- which(!is.na(parca_vals) & ua[[surf_cad]] != parca_vals)

  # Apply corrections (vectorized)
  if (length(diff_idx) > 0) {

    if (verbose) {
      cli_alert_warning(
        "{length(diff_idx)} cadastral area value{?s} corrected in UA."
      )
      cli_li("Affected IDU: {paste(ua[[idu]][diff_idx], collapse = ', ')}")
    }

    ua[[surf_cad]][diff_idx] <- parca_vals[diff_idx]

  } else if (verbose) {
    cli_alert_success("No cadastral area discrepancies detected.")
  }

  return(ua)
}

#' Create management unit field (UG) in the UA object
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
ua_create_ug <- function(ua,
                         ug_keys = c("parcelle", "sous_parcelle"),
                         separator = ".",
                         verbose = TRUE) {

  # Field names from YAML
  ug_ua <- seq_field("ug")$name
  fields <- vapply(ug_keys, function(k) seq_field(k)$name, character(1))

  # Clean/pad each column (vectorized)
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

  ua
}

#' Calculate areas in UA
#'
#' Computes the corrected cadastral areas for units of analysis according to
#' cartographic area.
#'
#' @param ua `sf` object containing analysis units.
#' @param verbose `logical` If `TRUE`, display progress messages.
#'
#' @return `sf` object with updated areas
#'
#' @importFrom sf st_area
#' @importFrom cli cli_alert_info cli_alert_success
#' @importFrom stats ave
#'
#' @export
ua_get_areas <- function(ua, verbose = TRUE) {
  # Field names from configuration
  idu      <- seq_field("idu")$name
  surf_cad  <- seq_field("surf_cad")$name
  surf_sig <- seq_field("surf_sig")$name
  surf_cor <- seq_field("surf_cor")$name

  # Calculate mapped surface in hectares
  ua[[surf_sig]] <- as.numeric(st_area(ua)) / 10000

  # Compute correction factor per cadastral ID
  sum_sig <- ave(ua[[surf_sig]], ua[[idu]], FUN = sum)
  coeff   <- ua[[surf_sig]] / sum_sig

  # Compute provisional corrected surfaces
  surf_temp <- round(ua[[surf_cad]] * coeff, 4)
  resid     <- ua[[surf_cad]] - ave(surf_temp, ua[[idu]], FUN = sum)

  # Identify pivot feature to absorb residual
  is_pivot <- surf_temp == ave(surf_temp, ua[[idu]], FUN = max)

  # Assign corrected surface
  ua[[surf_cor]] <- ifelse(is_pivot, round(surf_temp + resid, 4), surf_temp)

  if (verbose) cli_alert_success("Corrected cadastral areas calculated.")

  return(ua)
}
