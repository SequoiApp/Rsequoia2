# Check and update *UA* consistency with cadastral *PARCA* data

This function verifies and updates the consistency of analysis units
(UA) using cadastral parcel data (PARCA). It checks matching IDUs,
validates areas, generates management units (UG), computes corrected
areas, and ensures the internal consistency of the resulting UA object.

## Usage

``` r
ua_to_ua(ua, parca, verbose = TRUE)
```

## Arguments

- ua:

  `sf` object containing analysis units.

- parca:

  `sf` object, typically produced by
  [`seq_parca()`](https://mucau.github.io/Rsequoia2/reference/seq_parca.md),
  containing cadastral parcels.

- verbose:

  `logical` If `TRUE`, display progress messages.

## Value

An updated `sf` object identical to `ua`, but with:

- IDUs checked against PARCA,

- cadastral areas checked and corrected,

- management unit fields generated,

- corrected cadastral areas added,

- management unit consistency checked and corrected.
