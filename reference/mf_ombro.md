# Compute ombrothermic climatology summaries

Computes monthly ombrothermic summaries from raw climatology data over
specified rolling periods.

## Usage

``` r
mf_ombro(clim, periods = c(30, 5))
```

## Arguments

- clim:

  `data.frame` Raw climatology data from
  [`mf_get_climatology()`](https://sequoiapp.github.io/Rsequoia2/reference/mf_get_climatology.md)

- periods:

  `integer` Vector of periods (in years) used to compute averages.
  Default is `c(30, 5)`.

## Value

A `data.frame` containing monthly averages of temperature and
precipitation for each period.

## Examples

``` r
if (FALSE) { # \dontrun{
ombro <- mf_ombro(clim)
} # }
```
