# Validate if line contains parcel data

Checks if a line contains valid parcel data based on text patterns.

## Usage

``` r
is_valid_row(line_data)
```

## Arguments

- line_data:

  A `data.frame` for a single line from
  [`group_lines_by_y()`](https://sequoiapp.github.io/Rsequoia2/reference/group_lines_by_y.md)

## Value

`logical`. `TRUE` if line contains valid data patterns
