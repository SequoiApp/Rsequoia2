# Extract all data rows from table

Processes all lines in table data, extracting valid rows.

## Usage

``` r
extract_rows(table_data, col_positions)
```

## Arguments

- table_data:

  A \`data.frame\` from \[group_lines_by_y()\]

- col_positions:

  List from \[define_column_positions()\]

## Value

List of \`data.frame\`, one per extracted row
