# Retrieve commune representative points around an area

Computes centroid points from commune boundary polygons, optionally
restricted to a graphical extent.

## Usage

``` r
get_com_point(x, graphic = FALSE)
```

## Arguments

- x:

  An `sf` object used as the input area.

- graphic:

  Logical. If `TRUE`, centroids are computed only on the intersection
  between commune polygons and a 500 m convex buffer around `x`, for
  cartographic display.

## Value

An `sf` object of type `POINT` representing commune centroids. Returns
`NULL` if no commune intersects the input area.

## Details

The function retrieves commune polygons using
[`get_com_poly()`](https://mucau.github.io/Rsequoia2/reference/get_com_poly.md),
then computes their centroids. When `graphic = TRUE`, centroids are
calculated from the clipped geometries to ensure points fall within the
display extent.

## See also

[`get_com_poly()`](https://mucau.github.io/Rsequoia2/reference/get_com_poly.md)
