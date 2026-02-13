# Download climatological station fiche

Downloads the climatological fiche (PDF) of the nearest Météo-France
station to the provided geometry.

## Usage

``` r
mf_get_climate_fiche(x, dirname = NULL, verbose = TRUE)
```

## Arguments

- x:

  `sf` or `sfc`

- dirname:

  `character`; directory where the PDF will be saved. Defaults to
  [`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html)

- verbose:

  `logical` If `TRUE`, display progress messages.

## Value

Path to the downloaded PDF

## Details

The fiche contains station metadata and long-term climatological
summaries.

## Examples

``` r
if (FALSE) { # \dontrun{
mf_get_climate_fiche(parca, dirname = "METEO")
} # }
```
