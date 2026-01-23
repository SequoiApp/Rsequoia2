# Extract raw parcel data from PDF

Main function that orchestrates the extraction of unbuilt parcel data
from French cadastral PDFs. Calls all internal processing functions.

## Usage

``` r
get_raw_parcels(pdf)
```

## Arguments

- pdf:

  `character`. Path to PDF file

## Value

A `data.frame` with raw parcel data, column names from
[`define_column_positions()`](https://mucau.github.io/Rsequoia2/reference/define_column_positions.md)
