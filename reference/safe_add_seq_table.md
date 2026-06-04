# Safely build and add a summary table to an Excel workbook

Executes a summary table builder, validates its output, writes the table
to an \`openxlsx2\` workbook, and captures any error without stopping
the full summary workflow.

## Usage

``` r
safe_add_seq_table(wb, sheet, fun, verbose = TRUE)
```

## Arguments

- wb:

  An \`openxlsx2\` workbook object.

- sheet:

  Character string. Internal table name used for console messages.

- fun:

  Function. A zero-argument builder function returning a table
  specification list.

- verbose:

  Logical. If \`TRUE\`, prints an \`OK\` or \`BAD\` message with \`cli\`
  for this table.

## Value

A list with:

- \`wb\`:

  The updated workbook if successful, otherwise the unchanged workbook.

- \`ok\`:

  Logical. \`TRUE\` if the table was built and written successfully,
  \`FALSE\` otherwise.

- \`table\`:

  The generated table on success, or \`NULL\` on failure.

## Details

This helper is used by \[seq_summary()\] to process tables one by one.
If a table fails during either the build step or the Excel writing step,
the error is reported with \`cli\` and the workflow continues with the
next table.

A builder function must return a list with at least:

- \`table\`:

  A \`data.frame\`-like object to write.

- \`sheet_name\`:

  Character string. Name of the Excel worksheet.

- \`total_row\`:

  Optional total row specification passed to
  \[openxlsx2::wb_add_data_table()\]. If missing, \`FALSE\` is used.

## See also

\[seq_summary()\]
