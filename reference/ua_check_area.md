# Update cadastral area values in *UA* using *PARCA*

This function compares cadastral area values between *UA* and *PARCA*,
and updates *UA* wherever discrepancies are detected.

## Usage

``` r
ua_check_area(ua, parca, verbose = FALSE)
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

Updated `ua` object.
