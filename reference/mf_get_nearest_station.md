# Get nearest Meteo-France stations

Finds the nearest meteorological stations from Meteo-France to the
provided spatial geometry.

## Usage

``` r
mf_get_nearest_station(x, n = 1, verbose = TRUE)
```

## Arguments

- x:

  `sf` or `sfc`.

- n:

  `integer`; number of nearest stations to return. Default is 1.

- verbose:

  `logical` If `TRUE`, display progress messages.

## Value

An `sf` object containing the nearest meteorological stations.
