# Remove geometries below a minimum area threshold

Filters geometries whose area is smaller than a given threshold (in m²).
The input is projected to a metric CRS before area computation.

## Usage

``` r
remove_small_geometries(x, tol, crs = 2154)
```

## Arguments

- x:

  `sf` or `sfc`. Input geometries (POLYGON / MULTIPOLYGON).

- tol:

  `numeric`. Minimum area threshold in square meters.

- crs:

  `numeric`. EPSG code of a projected CRS (default: 2154).

## Value

An object of the same class as `x`, with geometries below the threshold
removed.
