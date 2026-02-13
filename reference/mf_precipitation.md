# Compute annual precipitation summaries

Computes annual precipitation averages from raw climatology data.

## Usage

``` r
mf_precipitation(clim)
```

## Arguments

- clim:

  `data.frame` Raw climatology data from
  [`mf_get_climatology()`](https://sequoiapp.github.io/Rsequoia2/reference/mf_get_climatology.md).

## Value

A `data.frame` containing annual precipitation averages.

## Details

Precipitation values are computed from monthly records and aggregated by
year.

## Examples

``` r
if (FALSE) { # \dontrun{
precip <- mf_precipitation(clim)
} # }
```
