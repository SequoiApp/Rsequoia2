#' Safely build and add a summary table to an Excel workbook
#'
#' Executes a summary table builder, validates its output, writes the table to
#' an `openxlsx2` workbook, and captures any error without stopping the full
#' summary workflow.
#'
#' This helper is used by [seq_summary()] to process tables one by one. If a
#' table fails during either the build step or the Excel writing step, the error
#' is reported with `cli` and the workflow continues with the next table.
#'
#' A builder function must return a list with at least:
#'
#' \describe{
#'   \item{`table`}{A `data.frame`-like object to write.}
#'   \item{`sheet_name`}{Character string. Name of the Excel worksheet.}
#'   \item{`total_row`}{Optional total row specification passed to
#'     [openxlsx2::wb_add_data_table()]. If missing, `FALSE` is used.}
#' }
#'
#' @param wb An `openxlsx2` workbook object.
#' @param sheet Character string. Internal table name used for console messages.
#' @param fun Function. A zero-argument builder function returning a table
#'   specification list.
#' @param verbose Logical. If `TRUE`, prints an `OK` or `BAD` message with
#'   `cli` for this table.
#'
#' @return A list with:
#' \describe{
#'   \item{`wb`}{The updated workbook if successful, otherwise the unchanged
#'     workbook.}
#'   \item{`ok`}{Logical. `TRUE` if the table was built and written
#'     successfully, `FALSE` otherwise.}
#'   \item{`table`}{The generated table on success, or `NULL` on failure.}
#' }
#'
#' @seealso [seq_summary()]
#'
#' @keywords internal
#'
safe_add_seq_table <- function(wb, sheet, fun, verbose = TRUE) {
  tryCatch(
    {
      out <- fun()

      if (!is.list(out)) {
        cli::cli_abort("Builder must return a list.")
      }

      table <- out$table
      total_row <- out$total_row

      if (is.null(total_row)) {
        total_row <- FALSE
      }

      if (!is.data.frame(table)) {
        cli::cli_abort("Builder must return a data frame in {.field table}.")
      }

      # use libelle for synthese
      fields <- seq_field()
      libelle_map <- setNames(
        vapply(fields, \(f) f$libelle, character(1)),
        vapply(fields, \(f) f$name, character(1))
      )

      hits <- match(names(table), names(libelle_map))
      names(table) <- replace(names(table), !is.na(hits), libelle_map[hits[!is.na(hits)]])

      wb <- wb |>
        openxlsx2::wb_add_worksheet(sheet = sheet) |>
        openxlsx2::wb_add_data_table(
          sheet = sheet,
          table_name = sheet,
          x = table,
          na = NULL,
          total_row = total_row
        ) |>
        openxlsx2::wb_add_cell_style(
          sheet = sheet,
          dims = wb_dims(x = table),
          horizontal = "center"
        ) |>
        openxlsx2::wb_set_col_widths(
          sheet = sheet,
          cols = seq_along(table),
          widths = "auto"
        ) |>
        openxlsx2::wb_add_numfmt(
          sheet = sheet,
          dims = wb_dims(x = table),
          numfmt = "0.0000"
        ) |>
        openxlsx2::wb_freeze_pane(
          sheet = sheet,
          first_row = TRUE
        )

      has_custom_formula <- "custom_formula" %in% names(out)
      if (has_custom_formula){
        wb <- wb |>
          openxlsx2::wb_add_formula(
            sheet = sheet,
            x = out$custom_formula$formula,
            dims = out$custom_formula$dims
          )
      }

      if (verbose) {
        cli::cli_alert_success("{.field {sheet}}")
      }

      list(wb = wb, ok = TRUE, sheet_name = sheet, table = table)
    },
    error = function(e) {
      if (verbose) {
        cli::cli_alert_danger("{.field {sheet}}: {conditionMessage(e)}")
      }

      list(wb = wb, ok = FALSE, sheet_name = sheet, table = NULL)
    }
  )
}

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
sum_surf_by <- function(x, ..., area_key = "cor_area") {
  if (!inherits(x, "data.frame")) {
    cli::cli_abort("{.arg x} must be a data frame or sf object.")
  }

  if (inherits(x, "sf")) {
    x <- sf::st_drop_geometry(x)
  }

  by <- c(...)

  if (length(by) == 0) {
    cli::cli_abort("No grouping field supplied.")
  }

  resolve_col <- function(key) {
    tryCatch(
      seq_field(key)$name,
      error = function(e) key
    )
  }

  area_col <- resolve_col(area_key)
  by_cols <- vapply(by, resolve_col, character(1))

  if (!area_col %in% names(x)) {
    cli::cli_abort("Missing area column {.field {area_col}}.")
  }

  missing <- by_cols[!by_cols %in% names(x)]
  if (length(missing) > 0) {
    cli::cli_abort(
      "Missing grouping column{?s}: {.field {unname(missing)}}."
    )
  }

  x[by_cols] <- lapply(x[by_cols], \(z) addNA(factor(z)))
  by_formula <- paste(by_cols, collapse = " + ")

  out <- aggregate(
    stats::as.formula(paste(area_col, "~", by_formula)),
    data = x,
    FUN = sum,
    na.rm = TRUE,
    na.action = stats::na.pass
  )

  # Convert factor as character to avoid weird format when using openxlsx2::wb_add_data_table
  out[by_cols] <- lapply(out[by_cols], as.character)
  rownames(out) <- NULL
  out
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
order_by <- function(x, ..., decreasing = FALSE, na.last = TRUE) {
  by_keys <- c(...)
  if (length(by_keys) == 0) {
    cli::cli_abort("No grouping field supplied.")
  }

  by <- vapply(
    by_keys,
    function(key) seq_field(key)$name,
    character(1)
  )

  existing <- by[by %in% names(x)]
  ignored <- by_keys[!by %in% names(x)]

  if (length(existing) == 0) {
    return(x)
  }

  cols <- x[existing]

  o <- do.call(
    order,
    c(cols, list(decreasing = decreasing, na.last = na.last))
  )

  x[o, , drop = FALSE]
}

#' Pivot a table to wide format using Sequoia field keys
#'
#' Internal helper that reshapes a table from long to wide format using
#' Sequoia field keys for row and column identifiers.
#'
#' @param to_pivot `data.frame`. Table to reshape.
#' @param row `character`. Sequoia field keys defining rows.
#' @param col `character`. Sequoia field key defining columns.
#'
#' @return A reshaped `data.frame`.
#'
#' @keywords internal
#' @noRd
pivot <- function(x, row, col) {
  row_keys <- row
  col_key <- col
  surf_key <- "cor_area"

  row_cols <- vapply(row_keys, function(key) seq_field(key)$name, character(1))
  col_col <- vapply(col_key, function(key) seq_field(key)$name, character(1))
  surf <- seq_field(surf_key)$name

  existing_row_cols <- row_cols[row_cols %in% names(x)]
  ignored_row_keys <- row_keys[!row_cols %in% names(x)]
  if (length(existing_row_cols) == 0) {
    cli::cli_abort(
      "Cannot pivot: row field{?s} {.field {row_cols}} is missing."
    )
  }

  if (!all(col_col %in% names(x))) {
    cli::cli_abort(
      "Cannot pivot: column field {.field {col_col}} is missing."
    )
  }

  x$key <- apply(x[col_col], 1, \(z) {
    z <- stats::na.omit(trimws(z))
    z <- z[z != ""]
    if (length(z)) paste(z, collapse = "__") else "(no data)"
  })

  pivoted <- stats::reshape(
    x[c(existing_row_cols, "key", surf)],
    idvar = existing_row_cols,
    timevar = "key",
    sep = "___",
    direction = "wide"
  )

  names(pivoted) <- sub("^.*___", "", names(pivoted))
  rownames(pivoted) <- NULL

  pivoted
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
