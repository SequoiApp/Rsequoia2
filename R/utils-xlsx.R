#' Style a Data Frame Table in an openxlsx2 Workbook
#'
#' Applies consistent formatting to a data frame that has been written into a worksheet.
#' This includes adding a bottom border and bold font to column names, centering all cells,
#' formatting numeric columns to two decimal places, and auto-adjusting column widths.
#'
#' @param wb A [openxlsx2::wb_workbook] object.
#' @param sheet Character or integer. The target worksheet name or index.
#' @param df A data frame that has been written to `sheet` and needs styling.
#' @param numfmt see [openxlsx2::wb_add_numfmt]
#'
#' @importFrom openxlsx2 wb_workbook wb_add_data wb_add_worksheet wb_save
#' wb_dims wb_set_col_widths wb_add_cell_style wb_add_numfmt wb_add_font
#' wb_add_border wb_color
#'
#' @return The modified `wb_workbook` object with styles applied.
#' @export
#'
#' @examples
#' \dontrun{
#' library(openxlsx2)
#' df <- data.frame(Name = c("A", "B"), Value = c(1.234, 2.345))
#' wb <- wb_workbook()
#' wb <- wb |>
#'   wb_add_worksheet("Data") |>
#'   wb_add_data("Data", x = df)
#' wb <- style_table(wb, "Data", df)
#' wb_save(wb, "styled.xlsx")
#' }
#'
style_table <- function(wb, sheet, df, numfmt = "0.00"){

  wb_dims_wrapper <- function(...) {
    wb_dims(x = df, ...)
  }

  # Manually create range because wb_dims is super slow
  nrow <- nrow(df) + 1
  num_cols <- which(vapply(df, is.numeric, logical(1)))
  col <- LETTERS[num_cols]
  fmt_dims <- paste(sprintf("%s2:%s%d", col, col, nrow), collapse = ",")

  col_dims <- wb_dims(x = df[1,], select = "col_names")
  all_dims <- paste(sprintf("A1:%s%d", LETTERS[ncol(df)], nrow), collapse = ",")

  wb <- wb |>
    wb_add_border(dims = col_dims, bottom_color = wb_color("black"),
                  left_border = NULL, right_border = NULL, top_border = NULL) |>
    wb_add_font(dims = col_dims, bold = TRUE) |>
    wb_add_cell_style(dims = all_dims, horizontal = "center") |>
    wb_add_numfmt(sheet = sheet, dims = fmt_dims, numfmt = numfmt) |>
    wb_set_col_widths(cols = seq_along(df), widths = "auto")

  return(wb)
}

#' Save Multiple Data Frames to an Excel Workbook with Styling
#'
#' Creates a new Excel workbook, adds each element of a named list of data frames
#' as a separate sheet, writes the data, and applies `style_table()` to each sheet.
#'
#' @param ... `data.frame` Each `data.frame` is wrote to a different sheet name.
#' If `...` contain named arg, name is used as sheet name else variable name is
#' used.
#' @param filename `character` File path where the workbook will be saved.
#' @param data_table `logical` If `TRUE`, data table is set up with total row.
#' Default to `FALSE`.
#' @param overwrite `logical` If `TRUE`, filename is overwritten.
#' @param verbose `logical` If `TRUE`, display messages.
#'
#' @return Invisibly returns the path `filename` after saving.
#' @export
#'
#' @examples
#' \dontrun{
#' library(openxlsx2)
#' df1 <- data.frame(A = 1:3, B = 4:6)
#' df2 <- data.frame(X = letters[1:3], Y = runif(3))
#'
#' Create xlsx with two sheet named THIS_DF1 and THIS_DF2
#' save_tables_to_xlsx(THIS_DF1 = df1, THIS_DF2 = df2, filename = "my_data.xlsx")
#'
#' Create xlsx with two sheet named df1 and df2
#' save_tables_to_xlsx(df1, df2, filename = "my_data.xlsx")
#'
#' Create xlsx with two sheet named df1 and THIS_DF2
#' save_tables_to_xlsx(df1, THIS_DF2 = df2, filename = "my_data.xlsx")
#'
#' }
seq_xlsx <- function(..., filename, data_table = FALSE, overwrite = FALSE, verbose = TRUE) {

  # convert to symbol then to list. First element is always the function name
  exprs <- as.list(substitute(list(...)))[-1]

  values <- list(...)
  if (!length(values)) {
    cli::cli_abort("No data provided.")
  }

  if (length(values) == 1 && is.list(values[[1]]) && !inherits(values[[1]], "data.frame")) {
    values <- values[[1]]
  }

  sheets <- list()
  for (i in seq_along(values)) {

    name <- names(values)[i]
    value <- values[[i]]

    is_df <- inherits(value, "data.frame")
    if (!is_df) {
      cli::cli_abort("All sheet inputs must be {.cls data.frame}.")
    }

    # If name is mising use variable name
    if (is.null(name) || name == "") {
      name <- deparse(exprs[[i]], nlines = 1)
    }

    sheets[[name]] <- value
  }

  too_long_name <- nchar(names(sheets)) > 31
  bad_character_in_name <- grepl("[:\\\\/\\?\\*\\[\\]]", names(sheets))
  invalid <- too_long_name | bad_character_in_name
  if (any(invalid)) {
    cli::cli_abort(c(
      "Invalid Excel sheet name(s): {paste(names(sheets)[invalid], collapse = ', ')}",
      "i" = "Names must be < 31 characters and not contain : \\ / ? * [ ]"
    ))
  }

  dup <- duplicated(names(sheets))
  if (any(dup)) {
    cli::cli_abort(c(
      "Duplicate sheet names detected.",
      "i" = "Duplicates: {paste(unique(names(sheets)[dup]), collapse = ', ')}"
    ))
  }

  # Force .xlsx extension
  filename <- normalizePath(filename, mustWork = FALSE)
  if (!grepl("\\.xlsx$", filename, ignore.case = TRUE)){
    filename <- paste0(filename, ".xlsx")
  }

  # Check parent directory exists
  dir <- dirname(filename)
  if (!dir.exists(dir)){
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    if (verbose){
      cli::cli_alert_info("Directory {.path {dir}} is created.")
    }
  }

  wb <- wb_workbook()

  # Prevent accidental overwrite
  if (file.exists(filename) && !overwrite) {
    cli::cli_abort(c(
      "x"= "File already exists at : {.path {normalizePath(filename)}}",
      "i" = "Use {.code overwrite = TRUE} to replace it.")
    )
  }

  for (sheet in names(sheets)) {
    df <- sheets[[sheet]]

    wb <- wb_add_worksheet(wb, sheet)
    if (data_table & nrow(df) > 0){
      wb <- openxlsx2::wb_add_data_table(wb, sheet, x = df, na.strings = "", total_row = TRUE)
    }else{
      wb <- wb_add_data(wb, sheet, x = df, na.strings = "")
    }
    wb <- style_table(wb, sheet, df)
  }

  wb_save(wb, filename, overwrite)

  if (verbose) {
    cli::cli_alert_success("Excel file created at: {.path {normalizePath(filename)}}")
  }

  invisible(filename)
}

