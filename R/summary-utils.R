#' Sum corrected area by grouping variables
#'
#' Internal helper that aggregates the corrected area (`cor_area`) of a Sequoia
#' layer by one or more grouping variables. Grouping variables can be provided
#' either as Sequoia field keys or as actual column names.
#'
#' Geometry is automatically dropped prior to aggregation.
#'
#' @param x `sf` or `data.frame`. Input layer.
#' @param ... `character`. One or more grouping variables (Sequoia keys or column names).
#'
#' @return A `data.frame` with aggregated corrected area.
#'
#' @keywords internal
#' @noRd
sum_surf_by <- function(x, ...){

  x <- sf::st_drop_geometry(x)
  cor_area <- seq_field("cor_area")$name

  by <- c(...)
  is_key <- by %in% names(seq_field())
  by_key <- vapply(by[is_key], \(x) seq_field(x)$name, character(1))
  by[is_key] <- by_key

  by_formula <- paste(by, collapse = " + ")

  aggregate(
    stats::as.formula(paste(cor_area, "~", by_formula)),
    data = x,
    FUN = sum,
    na.rm = TRUE,
    na.action = stats::na.pass
  )
}

#' Order a table by Sequoia field keys
#'
#' Internal helper that orders a table according to one or more Sequoia field keys.
#' Field keys are automatically translated to their corresponding column names.
#'
#' @param to_order `data.frame`. Table to sort.
#' @param ... `character`. One or more Sequoia field keys.
#' @param decreasing `logical`. Whether to sort in decreasing order.
#'
#' @return A reordered `data.frame`.
#'
#' @keywords internal
#' @noRd
order_by <- function(to_order, ..., decreasing = FALSE) {

  by <- vapply(
    c(...),
    function(x) seq_field(x)$name,
    character(1)
  )

  cols <- lapply(by, function(nm) to_order[[nm]])
  o <- do.call(order, c(cols, list(decreasing = decreasing)))
  to_order[o, , drop = FALSE]
}

#' Pivot a table to wide format using Sequoia field keys
#'
#' Internal helper that reshapes a table from long to wide format using
#' Sequoia field keys for row and column identifiers.
#'
#' @param to_pivot `data.frame`. Table to reshape.
#' @param row `character`. Sequoia field keys defining rows.
#' @param col `character`. Sequoia field key defining columns.
#' @param ... Unused.
#'
#' @return A reshaped `data.frame`.
#'
#' @keywords internal
#' @noRd
pivot <- function(to_pivot, row, col, ...){

  row_vars <- vapply(row, \(x) seq_field(x)$name, character(1))

  pivoted <- stats::reshape(to_pivot,
                            idvar = row_vars,
                            timevar = seq_field(col)$name,
                            sep = "___",
                            direction = "wide"
  )
  names(pivoted) <- sub("^.*___", "", names(pivoted))

  return(pivoted)
}

#' Add a total row to a summary table
#'
#' Internal helper that computes column totals for selected numeric fields
#' and appends a total row to the table.
#'
#' @param df `data.frame`. Input table.
#' @param ... `character`. Columns (Sequoia keys or names) to sum.
#' @param name `character`. Label for the total row.
#'
#' @return A `data.frame` with total row appended.
#'
#' @keywords internal
#' @noRd
add_total <- function(df, ..., name = "TOTAL"){
  cols <- c(...)
  is_key <- cols %in% names(seq_field())
  cols_key <- vapply(cols[is_key], \(x) seq_field(x)$name, character(1))
  cols[is_key] <- cols_key

  empty_row <- df[0, , drop = FALSE]
  empty_row[1, ] <- NA

  # total row
  total <- empty_row
  total[1, 1] <- name
  total[cols] <- colSums(df[cols], na.rm = TRUE)

  return(rbind(df, empty_row, total))
}

#' Add proportion column based on corrected area
#'
#' Internal helper that computes proportions of corrected area relative to
#' the total corrected area.
#'
#' @param df `data.frame`. Input table.
#' @param by `character`. Sequoia field key representing area column.
#' @param name `character`. Name of the proportion column.
#'
#' @return A `data.frame` with proportion column added.
#'
#' @keywords internal
#' @noRd
add_prop <- function(df, by, name = "PROPORTION"){
  by <- seq_field(by)$name
  df[[name]] <- if (nrow(df) > 0) df[[by]]/sum(df[[by]]) else numeric(0)
  return(df)
}

#' Safely read a Sequoia layer
#'
#' Internal helper that attempts to read a Sequoia layer and returns `NULL`
#' if the layer does not exist. Optionally prints a warning message.
#'
#' @param key `character`. Sequoia layer key.
#' @param dirname `character`. Sequoia directory.
#' @param verbose `logical`. Whether to print warning messages.
#'
#' @return An `sf`, `SpatRaster`, or `NULL`.
#'
#' @keywords internal
#' @noRd
safe_seq_read <- function(key, dirname, verbose = TRUE) {
  out <- tryCatch(
    seq_read(key, dirname = dirname),
    error = function(e) NULL
  )

  if (is.null(out) && verbose) {
    cli::cli_alert_warning("Layer {.val {key}} not found. Skipping related summary.")
  }

  return(out)
}
