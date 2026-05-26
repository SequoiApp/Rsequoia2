# Extract complete row from line data

Applies
[`extract_column()`](https://sequoiapp.github.io/Rsequoia2/reference/extract_column.md)
to all columns to build a complete data row.

## Usage

``` r
extract_row(line_data, col_positions)
```

## Arguments

- line_data:

  A `data.frame` for a single line

- col_positions:

  List from
  [`define_column_positions()`](https://sequoiapp.github.io/Rsequoia2/reference/define_column_positions.md)

## Value

A `data.frame` with one row containing all column values
