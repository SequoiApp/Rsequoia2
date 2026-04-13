# Remove interior rings from polygons

Keeps only the exterior ring of each polygon, removing holes.

## Usage

``` r
remove_interior_ring(x)
```

## Arguments

- x:

  `sf` or `sfc`. Input geometries (POLYGON / MULTIPOLYGON).

## Value

An `sf` object with polygons stripped of interior rings.
