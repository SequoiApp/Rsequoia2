# Check cadastral IDU consistency between *UA* and *PARCA* sf objects

Check cadastral IDU consistency between *UA* and *PARCA* sf objects

## Usage

``` r
ua_check_idu(ua, parca, verbose = FALSE)
```

## Arguments

- ua:

  `sf` Object from
  [`parca_to_ua()`](https://sequoiapp.github.io/Rsequoia2/reference/parca_to_ua.md)
  containing analysis units

- parca:

  `sf` Object from
  [`seq_parca()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_parca.md)
  containing cadastral parcels.

- verbose:

  `logical` If `TRUE`, display progress messages.

## Value

`TRUE` if all `parca` idu values are found in `ua`; `FALSE` otherwise
(with CLI messages if `verbose = TRUE`).
