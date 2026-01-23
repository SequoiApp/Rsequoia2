# Group text elements into logical lines

Groups elements on the same visual line using vertical proximity.
Elements within tolerance pixels are considered part of the same line.

## Usage

``` r
group_lines_by_y(table_data, tolerance = 8)
```

## Arguments

- table_data:

  A `data.frame` from
  [`filter_column_headers()`](https://mucau.github.io/Rsequoia2/reference/filter_column_headers.md)

- tolerance:

  `Numeric`. Vertical tolerance in pixels (default 8)

## Value

A `data.frame` with added `y_group` column
