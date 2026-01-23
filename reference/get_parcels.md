# Extract and process complete parcel data from PDF

Orchestrates the full extraction pipeline: raw parcel data, reference
info, owner info, and combines everything into a final structured data
frame.

## Usage

``` r
get_parcels(pdf)
```

## Arguments

- pdf:

  `character`. Path to PDF file

## Value

A complete parcel `data.frame` with all processed fields and
commune/owner information
