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
#' areas (`surf_cor`) are summed. A polygon and border lines layer are created.
#'
#' 2. **SSPF (Sous-parcelle)**
#' Rows of the UA are grouped using the `parcelle` and `sous-parcelle` field, and
#' corrected surface areas (`surf_cor`) are summed. A polygon and border lines
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

  pf <- seq_field("parcelle")$name
  sspf <- seq_field("sous_parcelle")$name
  s <- seq_field("surf_cor")$name

  # Generate pf ----
  by_pf <- list(ua[[pf]]) |> setNames(pf)
  pf_poly <- aggregate(
    x = ua[, s],
    by = by_pf,
    FUN = sum,
    na.rm = TRUE
  )

  pf_line <- poly_to_line(pf_poly)

  path_pf_poly <- seq_write2(pf_poly, "v.seq.pf.poly")
  path_pf_line <- seq_write2(pf_line, "v.seq.pf.line")

  # Generate sspf ----
  by_sspf <- list(ua[[pf]], ua[[sspf]]) |> setNames(c(pf, sspf))
  sspf_poly <- aggregate(
    x = ua[, s],
    by = by_sspf,
    FUN = sum,
    na.rm = TRUE
  )

  sspf_line <- poly_to_line(sspf_poly)

  path_sspf_poly <- seq_write2(sspf_poly, "v.seq.sspf.poly")
  path_sspf_line <- seq_write2(sspf_line, "v.seq.sspf.line")

  return(
    invisible(
      c(path_pf_poly, path_pf_line, path_sspf_poly, path_sspf_line) |> as.list())
  )

}
