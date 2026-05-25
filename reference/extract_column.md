# Extract single column from line data

Extracts text for a specific column based on x-position ranges.

## Usage

``` r
extract_column(line_data, col_name, col_positions)
```

## Arguments

- line_data:

  A \`data.frame\` for a single line

- col_name:

  \`character\`. Column name from \[define_column_positions()\]

- col_positions:

  List of column position definitions

## Value

Extracted text or NA if no elements found
