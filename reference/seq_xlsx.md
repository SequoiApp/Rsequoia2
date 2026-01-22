# Save Multiple Data Frames to an Excel Workbook with Styling

Creates a new Excel workbook, adds each element of a named list of data
frames as a separate sheet, writes the data, and applies
[`style_table()`](https://mucau.github.io/Rsequoia2/reference/style_table.md)
to each sheet.

## Usage

``` r
seq_xlsx(..., filename, data_table = FALSE, overwrite = FALSE, verbose = TRUE)
```

## Arguments

- ...:

  `data.frame` Each `data.frame` is wrote to a different sheet name. If
  `...` contain named arg, name is used as sheet name else variable name
  is used.

- filename:

  `character` File path where the workbook will be saved.

- data_table:

  `logical` If `TRUE`, data table is set up with total row. Default to
  `FALSE`.

- overwrite:

  `logical` If `TRUE`, filename is overwritten.

- verbose:

  `logical` If `TRUE`, display messages.

## Value

Invisibly returns the path `filename` after saving.

## Examples
