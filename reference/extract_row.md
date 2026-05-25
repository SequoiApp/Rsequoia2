# Extract complete row from line data

Applies \[extract_column()\] to all columns to build a complete data
row.

## Usage

``` r
extract_row(line_data, col_positions)
```

## Arguments

- line_data:

  A \`data.frame\` for a single line

- col_positions:

  List from \[define_column_positions()\]

## Value

A \`data.frame\` with one row containing all column values
