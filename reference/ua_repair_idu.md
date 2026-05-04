# Repair *UA* cadastral IDU from *PARCA*

Repairs *UA* IDU values from the dominant intersecting *PARCA* polygon.

## Usage

``` r
ua_repair_idu(ua, parca, verbose = TRUE)
```

## Arguments

- ua:

  `sf` Object containing analysis units.

- parca:

  `sf` Object containing cadastral parcels.

- verbose:

  `logical` If `TRUE`, display messages.

## Value

The input `ua` object with corrected IDU values.

## Details

The spatial match is based on
[`sf::st_join()`](https://r-spatial.github.io/sf/reference/st_join.html)
with `largest = TRUE`, so the *PARCA* polygon with the largest overlap
is used as reference.
