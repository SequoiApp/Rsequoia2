#' Check spatial coverage between _UA_ and _PARCA_
#'
#' Checks that every _UA_ feature intersects at least one _PARCA_ feature, and
#' that every _PARCA_ feature intersects at least one _UA_ feature.
#'
#' @param ua `sf` Object containing analysis units.
#' @param parca `sf` Object containing cadastral parcels.
#'
#' @return The input `ua` object, invisibly, if spatial coverage is valid.
#' Aborts otherwise.
#'
#' @export
ua_check_coverage <- function(ua, parca) {
  idu_field <- seq_field("idu")$name

  if (!inherits(ua, "sf") || !inherits(parca, "sf")) {
    cli::cli_abort("{.var ua} and {.var parca} must be {.cls sf} objects.")
  }

  if (!idu_field %in% names(ua) || !idu_field %in% names(parca)) {
    cli::cli_abort(
      "{.var ua} or {.var parca} is missing IDU field {.field {idu_field}}."
    )
  }

  if (sf::st_crs(ua) != sf::st_crs(parca)) {
    parca <- sf::st_transform(parca, sf::st_crs(ua))
  }

  ua_hits <- lengths(sf::st_intersects(ua, parca)) > 0
  if (any(!ua_hits)) {
    cli::cli_abort(c(
      "{sum(!ua_hits)} UA feature{?s} do not intersect PARCA.",
      "i" = "Affected UA IDU{?s}: {.val {unique(ua[[idu_field]][!ua_hits])}}."
    ))
  }

  parca_hits <- lengths(sf::st_intersects(parca, ua)) > 0
  if (any(!parca_hits)) {
    cli::cli_abort(c(
      "{sum(!parca_hits)} PARCA feature{?s} do not intersect UA.",
      "i" = "Affected PARCA IDU{?s}: {.val {unique(parca[[idu_field]][!parca_hits])}}."
    ))
  }

  return(invisible(ua))
}

#' Repair _UA_ cadastral IDU from _PARCA_
#'
#' Repairs _UA_ IDU values from the dominant intersecting _PARCA_ polygon.
#'
#' The spatial match is based on [sf::st_join()] with `largest = TRUE`, so the
#' _PARCA_ polygon with the largest overlap is used as reference.
#'
#' @param ua `sf` Object containing analysis units.
#' @param parca `sf` Object containing cadastral parcels.
#' @param verbose `logical` If `TRUE`, display messages.
#'
#' @return The input `ua` object with corrected IDU values.
#'
#' @export
ua_repair_idu <- function(ua, parca, verbose = TRUE) {
  idu_field <- seq_field("idu")$name
  mgmt_field <- seq_field("mgmt_code")$name

  if (!mgmt_field %in% names(ua)) {
    cli::cli_abort("{.var ua} is missing management field {.field {mgmt_field}}.")
  }

  if (sf::st_crs(ua) != sf::st_crs(parca)) {
    parca <- sf::st_transform(parca, sf::st_crs(ua))
  }

  suffix <- "_PARCA"

  joined <- suppressWarnings(
    sf::st_join(
      ua,
      parca[idu_field],
      join = sf::st_intersects,
      largest = TRUE,
      suffix = c("", suffix)
    )
  )

  ua_idu <- joined[[idu_field]]
  parca_idu <- joined[[paste0(idu_field, suffix)]]

  to_fix <- is.na(ua_idu) | ua_idu != parca_idu

  if (!any(to_fix)) {
    if (verbose) {
      cli::cli_alert_success("UA IDU values are correct.")
    }

    return(ua)
  }

  if (verbose) {
    cli::cli_alert_warning(
      "{sum(to_fix)} UA feature{?s} had an incorrect IDU and {?was/were} corrected:"
    )

    cli::cli_ul(unique(sprintf(
      "PARFOR %s: %s -> %s",
      ua[[mgmt_field]][to_fix],
      ua_idu[to_fix],
      parca_idu[to_fix]
    )))
  }

  ua[[idu_field]][to_fix] <- parca_idu[to_fix]

  return(invisible(ua))
}

#' Check cadastral IDU consistency between _UA_ and _PARCA_
#'
#' Checks that all _PARCA_ IDU values are present in _UA_, and that all _UA_
#' IDU values exist in _PARCA_.
#'
#' @param ua `sf` Object containing analysis units.
#' @param parca `sf` Object containing cadastral parcels.
#' @param verbose `logical` If `TRUE`, display messages.
#'
#' @return `TRUE` if IDU values are consistent; `FALSE` otherwise,
#' with CLI warnings.
#'
#' @export
ua_check_idu <- function(ua, parca, verbose = FALSE) {
  idu_field <- seq_field("idu")$name

  ua_idu <- unique(ua[[idu_field]])
  parca_idu <- unique(parca[[idu_field]])

  ua_idu <- ua_idu[!is.na(ua_idu)]
  parca_idu <- parca_idu[!is.na(parca_idu)]

  missing_in_ua <- setdiff(parca_idu, ua_idu)
  unknown_in_ua <- setdiff(ua_idu, parca_idu)

  if (length(missing_in_ua) > 0) {
    cli::cli_alert_warning(
      "PARCA IDU missing in UA: {.val {sort(missing_in_ua)}}."
    )
  }

  if (length(unknown_in_ua) > 0) {
    cli::cli_alert_warning(
      "UA IDU unknown in PARCA: {.val {sort(unknown_in_ua)}}."
    )
  }

  ok <- length(missing_in_ua) == 0 && length(unknown_in_ua) == 0

  if (ok && verbose) {
    cli::cli_alert_success("UA and PARCA IDU values are consistent.")
  }

  ok
}

#' Update _UA_ fields from _PARCA_
#'
#' Refreshes _UA_ cadastral fields from _PARCA_ using the IDU field as key.
#'
#' This function assumes that _UA_ IDU values are already correct.
#'
#' @param ua `sf` Object containing analysis units.
#' @param parca `sf` Object containing cadastral parcels.
#'
#' @return A `sf` _UA_ object with cadastral fields updated from _PARCA_.
#'
#' @export
ua_update_parca_fields <- function(ua, parca) {
  idu_field <- seq_field("idu")$name

  keys_to_update <- c(
    "reg_name", "reg_code", "dep_name", "dep_code",
    "com_name", "com_code", "insee", "prefix", "section",
    "number", "locality", "cad_area"
  )

  fields_to_update <- vapply(
    keys_to_update,
    \(x) seq_field(x)$name,
    character(1)
  )

  parca_tab <- parca |>
    sf::st_drop_geometry()

  parca_tab <- parca_tab[c(idu_field, fields_to_update)]

  ua_base <- ua[setdiff(names(ua), fields_to_update)]

  merge(
    ua_base,
    parca_tab,
    by = idu_field,
    all.x = TRUE,
    sort = FALSE
  ) |>
    seq_normalize("ua")
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
  cad_area <- seq_field("cad_area")$name
  gis_area <- seq_field("gis_area")$name
  cor_area <- seq_field("cor_area")$name

  # Calculate mapped surface in hectares
  ua[[gis_area]] <- as.numeric(sf::st_area(ua)) / 10000

  # Compute correction factor per cadastral ID
  sum_sig <- stats::ave(ua[[gis_area]], ua[[idu]], FUN = sum, na.rm = T)
  # Avoid edge case when sum_sig == 0
  sum_sig <- replace(sum_sig, sum_sig == 0, 1)
  coeff <- ua[[gis_area]] / sum_sig

  # Compute provisional corrected surfaces
  surf_temp <- round(ua[[cad_area]] * coeff, 4)
  resid <- ua[[cad_area]] - stats::ave(surf_temp, ua[[idu]], FUN = sum)

  # Identify pivot feature to absorb residual
  is_pivot <- surf_temp == stats::ave(surf_temp, ua[[idu]], FUN = max)

  # Assign corrected surface
  ua[[cor_area]] <- ifelse(is_pivot, round(surf_temp + resid, 4), surf_temp)

  if (verbose) cli_alert_success("Corrected cadastral areas calculated.")

  return(invisible(ua))
}
