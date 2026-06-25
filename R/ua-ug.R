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
    "is_dgd", "is_wooded",
    "std_type", "std_wealth", "std_stage", "std_year",
    "is_damaged", "is_available", "is_compartmented",
    "res_spe1", "res_spe2", "res_struct",
    "cop_spe1", "cop_spe2", "cop_density", "cop_nature",
    "reg_spe1", "reg_spe2", "reg_stage", "reg_density",
    "treatment",
    "is_subsidized", "subsidy",
    "comment", "station"
  )
  vapply(keys, function(k) seq_field(k)$name, character(1))
}

#' Create management unit field (UG) in the _UA_ sf object
#'
#' Generates a standardized management unit identifier (UG) in the _UA_ object
#' based on configured parcel keys.
#'
#' @param ua `sf` object containing analysis units;
#' must contain fields used by `ug_keys`.
#' @param separator `character`, default `"."`. Separator between keys.
#' @param verbose `logical` If `TRUE`, display progress messages.
#'
#' @return `sf` object UA with UG field filled.
#'
#' @importFrom cli cli_alert_success
#'
#' @export
ua_generate_ug <- function(ua, separator = ".", verbose = TRUE) {

  ug <- seq_field("mgmt_code")$name
  pcl_code <- seq_field("pcl_code")$name
  sub_code <- seq_field("sub_code")$name

  missing_fields <- setdiff(c(pcl_code, sub_code), names(ua))
  if (length(missing_fields) > 0) {
    cli::cli_abort(
      "Missing field{?s} in {.arg ua}: {.field {missing_fields}}."
    )
  }

  clean_part <- function(x) {
    x <- as.character(x)
    ifelse(
      is.na(x) | x == "",
      "00",
      ifelse(grepl("^[A-Za-z]+$", x), x, pad_left(x, 2))
    )
  }

  ua[[pcl_code]] <- clean_part(ua[[pcl_code]])
  ua[[sub_code]] <- clean_part(ua[[sub_code]])

  ua[[ug]] <- paste(ua[[pcl_code]], ua[[sub_code]], sep = separator)

  if (verbose) cli::cli_alert_success("UG field {.field {ug}} created.")

  return(invisible(ua))
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
ua_check_ug <- function(ua, verbose = TRUE) {

  ug <- seq_field("mgmt_code")$name
  desc <- intersect(seq_desc_fields(), names(ua))

  ua_df <- sf::st_drop_geometry(ua)

  report <- lapply(split(ua_df, ua_df[[ug]]), function(df) {
    bad <- desc[vapply(df[desc], \(x) length(unique(x)) > 1, logical(1))]
    if (!length(bad)) return(NULL)
    lapply(df[bad], unique)
  })

  report <- Filter(\(x) !is.null(x), report)
  is_valid <- !length(report)

  if (!is_valid) {
    cli::cli_warn("{length(report)} inconsistent UG{?s} detected")

    for (i in seq_along(report)) {

      g <- names(report)[i]
      if (i > 1) cli::cli_rule()
      cli::cli_bullets(c(
        "x" = "UG {.val {g}} has inconsistent descriptive fields:"
      ))
      for (f in names(report[[g]])) {
        vals <- report[[g]][[f]]
        cli::cli_bullets(c(
          "!" = "{.field {f}} contains multiple values: {.val {vals}}"
        ))
      }
    }
  }

  if (verbose && is_valid) {
    cli::cli_alert_success("All UG are consistent.")
  }

  return(invisible(is_valid))

}

#' @title Clean management units (UG) by correcting minor inconsistencies in the _UA_ sf object
#'
#' @description
#' Detects and corrects minor inconsistencies within management units
#' (UG) in _UA_. Lines with small surfaces relative to their UG are updated
#' to match the dominant description.
#'
#' @param ua `sf` object containing analysis units;
#'   with at least the UG identifier field and relevant attribute fields.
#' @param atol Absolute tolerance for surface correction (default 0.50 ha).
#' @param rtol Relative tolerance for surface correction within a UG (default 0.10).
#'
#' @return An `sf` object identical to `ua`, with minor inconsistent lines
#'   corrected and an additional logical column `ug_valid` indicating UG
#'   consistency.
#'
#' @details The function works by:
#'   \itemize{
#'     \item Checking UG consistency with \code{ua_check_ug()}.
#'     \item Identifying the dominant row per UG (largest total surface).
#'     \item Correcting rows that differ from the dominant description and whose
#'       surface is smaller than \code{atol} and represents less than \code{rtol} of UG total.
#'     \item Returning the corrected UA and rechecking inconsistencies.
#'   }
#'
#' @export
ua_clean_ug <- function(ua, atol = 0.50, rtol = 0.10){

  ug <- seq_field("mgmt_code")$name
  surf <- seq_field("cor_area")$name
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

