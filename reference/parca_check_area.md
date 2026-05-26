# Check inconsistencies between cadastral and cartographic areas

This function compares cadastral areas (in m2) with cartographic areas
computed from geometry
([`sf::st_area()`](https://r-spatial.github.io/sf/reference/geos_measures.html)).

## Usage

``` r
parca_check_area(parca, atol = 500, rtol = 0.05, verbose = TRUE)
```

## Arguments

- parca:

  `sf` Object from
  [`seq_parca()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_parca.md)
  representing cadastral parcels.

- atol:

  `numeric` Absolute difference tolerance in m2. Default to `500m`.

- rtol:

  `numeric` Relative difference. Default to `0.05`

- verbose:

  `logical` If `TRUE`, display messages.

## Value

The input `parca` with four additional fields: `AREA_SIG` (cartographic
area in ha), `ATOL_AREA` (absolute difference in m2), `RTOL_AREA`
(relative difference), `CHECK_AREA` (logical flag).
