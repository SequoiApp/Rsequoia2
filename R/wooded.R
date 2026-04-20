#' Aggregate _UA_ surfaces by wooded / non-wooded status
#'
#' Aggregates corrected surface areas from a _UA_ layer by
#' grouping rows according to the wooded status field, as defined by the
#' configuration returned by [seq_field()].
#'
#' This function is typically used internally by [seq_wooded()], but may also
#' be called directly when a summary of wooded versus non-wooded areas is
#' required.
#'
#' @param ua `sf` object containing analysis units.
#'
#' @return An `sf` object where polygons are aggregated by wooded status, with
#' corrected surfaces.
#'
#' @seealso [seq_wooded()], [seq_field()], [seq_normalize()]
#'
#' @export
ua_to_wooded <- function(ua){
  # Sanity check
  ua <- seq_normalize(ua, "ua")

  is_wooded <- seq_field("is_wooded")$name
  cor_area <- seq_field("cor_area")$name

  if (all(is.na(ua[[is_wooded]]))) {
    cli::cli_abort(c(
      "x" = "Failed to generate BOISE layers from UA.",
      "!" = "Field {.field {is_wooded}} in UA is missing or is empty."
    ))
  }

  by_is_wooded <- list(ua[[is_wooded]]) |> setNames(is_wooded)
  is_wooded_poly <- aggregate(
    x = ua[, cor_area],
    by = by_is_wooded,
    FUN = sum,
    na.rm = TRUE
  )

  return(is_wooded_poly)
}

#' Create wooded-area layer from _UA_ for a Sequoia project
#'
#' This function reads the UA polygon layer (`v.seq.ua.poly`) of a Sequoia
#' project, aggregates corrected surface areas by wooded / non-wooded status,
#' and writes the derived layer to the project directory.
#'
#' Created layers are automatically written using [seq_write()], respecting
#' the `dirname`, `overwrite`, and `verbose` parameters.
#'
#' @inheritParams seq_write
#'
#' @details
#' Rows of the UA layer are grouped according to the wooded status field
#' (`is_wooded`), and corrected surface areas are summed for each class.
#'
#' @return
#' An invisible named list containing the file path of the exported wooded
#' layer.
#'
#' @seealso [ua_to_wooded()], [seq_read()], [seq_write()], [seq_field()]
#'
#' @export
seq_wooded <- function(dirname = ".", verbose = FALSE, overwrite = FALSE){
  # tiny helper ----
  seq_write2 <- function(x, key, id) {
    seq_write(x, key, dirname = dirname, id = id, verbose = verbose, overwrite = overwrite)
  }

  # Resolve field and layer ----
  parca <- seq_read("v.seq.parca.poly", dirname = dirname)
  id_field <- seq_field("identifier")$name
  id <- unique(parca[[id_field]])

  ua <- seq_read("v.seq.ua.poly", dirname = dirname)

  wooded <- ua_to_wooded(ua)
  wooded <- seq_write2(wooded, "v.seq.wooded.point", id)

  return(invisible(wooded) |> as.list())
}
