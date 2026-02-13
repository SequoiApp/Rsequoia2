# Download monthly climatology from Météo-France

Downloads monthly climatological data for the *3 nearest meteorological
stations*.

## Usage

``` r
mf_get_climatology(x, cache = NULL, verbose = TRUE)
```

## Arguments

- x:

  `sf` or `sfc`.

- cache:

  `character`; Storage directory. Defaults to the user cache directory
  (see [`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html))

- verbose:

  `logical` If `TRUE`, display progress messages.

## Value

A `data.frame` containing monthly climatology records for the nearest
stations.

## Details

Cached files are reused to avoid redundant downloads.

## Examples

``` r
if (FALSE) { # \dontrun{
clim <- mf_get_climatology(parca)
} # }
```
