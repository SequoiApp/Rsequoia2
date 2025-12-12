# Save Multiple Data Frames to an Excel Workbook with Styling

Creates a new Excel workbook, adds each element of a named list of data
frames as a separate sheet, writes the data, and applies
[`style_table()`](https://mucau.github.io/Rsequoia2/reference/style_table.md)
to each sheet.

## Usage

``` r
seq_xlsx(x, filename, overwrite = FALSE, verbose = TRUE)
```

## Arguments

- x:

  `list` A named list of data frames. List names are used as worksheet
  names.

- filename:

  `character` File path where the workbook will be saved.

- overwrite:

  `logical` If `TRUE`, filename is overwritten.

- verbose:

  `logical` If `TRUE`, display messages.

## Value

Invisibly returns the path `filename` after saving.

## Examples

``` r
if (FALSE) { # \dontrun{
library(openxlsx2)
df1 <- data.frame(A = 1:3, B = 4:6)
df2 <- data.frame(X = letters[1:3], Y = runif(3))
tables <- list(FirstSheet = df1, SecondSheet = df2)
save_tables_to_xlsx(tables, "my_data.xlsx")
} # }
```
