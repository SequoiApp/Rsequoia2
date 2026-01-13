#' Aggregate _UA_ surfaces at the `parcelle` field level
#'
#' Aggregates corrected surface areas from a _UA_ layer by
#' grouping rows according to the parcel unit field, as defined by the
#' configuration returned by [seq_field()].
#'
#' This function is typically used internally by [seq_parcels()], but may also
#' be called directly when a PF-level summary of a UA dataset is required.
#'
#' @param ua `sf` object containing analysis units
#'
#' @return `sf` object where polygons are aggregated at the _PF_ level, with
#' corrected surfaces.
#'
#' @seealso [ua_to_sspf()], [seq_parcels()], [seq_field()], [seq_normalize()]
#'
#' @export
ua_to_pf <- function(ua){
  # Sanity check
  ua <- seq_normalize(ua, "ua")

  pcl_code <- seq_field("pcl_code")$name
  cor_area <- seq_field("cor_area")$name

  if (all(is.na(ua[[pcl_code]]))) {
    cli::cli_abort(c(
      "x" = "Failed to generate PF layers from UA.",
      "!" = "Field {.field {pcl_code}} in UA contains only missing values.",
      "i" = "Please populate this field before running this step."
    ))
  }

  # Generate pcl_code ----
  by_plot_unit <- list(ua[[pcl_code]]) |> setNames(pcl_code)
  plot_unit_poly <- aggregate(
    x = ua[, cor_area],
    by = by_plot_unit,
    FUN = sum,
    na.rm = TRUE
  )

  return(plot_unit_poly)
}

#' Aggregate _UA_ surfaces at the `sspf` field level
#'
#' Aggregates corrected surface areas from a _UA_ layer by
#' grouping rows according to the `sspf` field, as
#' defined by the configuration returned by [seq_field()].
#'
#' In addition to summing surfaces, descriptive fields are preserved.
#'
#' This function is used internally by [seq_parcels()], but may be called
#' directly when a SSPF-level summary of UA data is needed.
#'
#' @param ua `sf` object containing analysis units
#'
#' @return An `sf` object where polygons and descriptive fields are aggregated
#' at the _SSPF_ level.
#'
#' @seealso [ua_to_pf()], [seq_parcels()], [seq_field()], [seq_normalize()]
#'
#' @export
ua_to_sspf <- function(ua){
  # Sanity check
  ua <- seq_normalize(ua, "ua")

  mgmt_code <- seq_field("mgmt_code")$name
  s <- seq_field("cor_area")$name

  # Generate sspf ----
  by_mana_unit<- list(ua[[mgmt_code]]) |> setNames(mgmt_code)
  sspf_poly_raw <- aggregate(
    x = ua[, s],
    by = by_mana_unit,
    FUN = sum,
    na.rm = TRUE
  )

  # Adding description to sspf ----
  desc <- intersect(seq_desc_fields(), names(ua))
  desc_tbl <- unique(ua[, c(mgmt_code, desc)] |> sf::st_drop_geometry())
  sspf_poly <- merge(
    sspf_poly_raw,
    desc_tbl,
    by = mgmt_code,
    all.x = TRUE
  )

  return(sspf_poly)

}

#' Create _PF_ and _SSPF_ object from _UA_ for a Sequoia project
#'
#' This function reads the UA polygon layer (`v.seq.ua.poly`) of a Sequoia
#' project, aggregates surfaces at the `parcelle` and
#' `sous-parcelle` level and writes four derived layers:
#'
#' Created layers are automatically written to the project directory using
#' [seq_write()], respecting the `dirname`, `overwrite`, and `verbose`
#' parameters.
#'
#' @inheritParams seq_write
#'
#' @details
#' The function performs two levels of spatial aggregation ande create 4 layers:
#'
#' 1. **PF (Parcelle)**
#' Rows of the UA are grouped using the `parcelle` field, and corrected surface
#' areas are summed. A polygon and border lines layer are created.
#'
#' 2. **SSPF (Sous-parcelle)**
#' Rows of the UA are grouped using the `parcelle` and `sous-parcelle` field, and
#' corrected surface areas are summed. A polygon and border lines
#' layer are created.
#'
#' @return
#' A named list of four file paths, corresponding to the exported PF and SSPF
#' polygon and line layers.
#'
seq_parcels <- function(dirname = ".", verbose = FALSE, overwrite = FALSE){
  # tiny helper ----
  seq_write2 <- function(x, key) {
    seq_write(x, key, dirname = dirname, verbose = verbose, overwrite = overwrite)
  }

  # Resolve field and layer ----
  ua <- seq_read("v.seq.ua.poly", dirname = dirname)

  paths <- list()

  pf_poly <- ua_to_pf(ua)
  pf_line <- poly_to_line(pf_poly)
  paths$pf_poly <- seq_write2(pf_poly, "v.seq.pf.poly")
  paths$pf_line <- seq_write2(pf_line, "v.seq.pf.line")

  sspf_poly <- ua_to_sspf(ua)
  sspf_line <- poly_to_line(sspf_poly)
  paths$sspf_poly <- seq_write2(sspf_poly, "v.seq.sspf.poly")
  paths$sspf_line <- seq_write2(sspf_line, "v.seq.sspf.line")

  return(invisible(paths))

}
