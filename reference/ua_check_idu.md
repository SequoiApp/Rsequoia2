# Check cadastral IDU consistency between *UA* and *PARCA*

Checks that all *PARCA* IDU values are present in *UA*, and that all
*UA* IDU values exist in *PARCA*.

## Usage

``` r
ua_check_idu(ua, parca, verbose = FALSE)
```

## Arguments

- ua:

  `sf` Object containing analysis units.

- parca:

  `sf` Object containing cadastral parcels.

- verbose:

  `logical` If `TRUE`, display messages.

## Value

`TRUE` if IDU values are consistent; `FALSE` otherwise, with CLI
warnings.
