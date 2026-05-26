# Style a Data Frame Table in an openxlsx2 Workbook

Applies consistent formatting to a data frame that has been written into
a worksheet. This includes adding a bottom border and bold font to
column names, centering all cells, formatting numeric columns to two
decimal places, and auto-adjusting column widths.

## Usage

``` r
style_table(wb, sheet, df, numfmt = "0.00")
```

## Arguments

- wb:

  A
  [openxlsx2::wb_workbook](https://janmarvin.github.io/openxlsx2/reference/wb_workbook.html)
  object.

- sheet:

  Character or integer. The target worksheet name or index.

- df:

  A data frame that has been written to `sheet` and needs styling.

- numfmt:

  see
  [openxlsx2::wb_add_numfmt](https://janmarvin.github.io/openxlsx2/reference/wb_add_numfmt.html)

## Value

The modified `wb_workbook` object with styles applied.

## Examples

``` r
if (FALSE) { # \dontrun{
library(openxlsx2)
df <- data.frame(Name = c("A", "B"), Value = c(1.234, 2.345))
wb <- wb_workbook()
wb <- wb |>
  wb_add_worksheet("Data") |>
  wb_add_data("Data", x = df)
wb <- style_table(wb, "Data", df)
wb_save(wb, "styled.xlsx")
} # }
```
