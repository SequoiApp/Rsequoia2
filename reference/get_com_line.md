# Retrieve and assemble commune boundary lines around an area

Converts commune boundary polygons into line features, optionally
clipped for cartographic display.

## Usage

``` r
get_com_line(x, graphic = FALSE, verbose = TRUE)
```

## Arguments

- x:

  An `sf` object used as the input area.

- graphic:

  Logical. If `TRUE`, line geometries are clipped to a 500 m convex
  buffer around `x` for graphical purposes.

- verbose:

  `logical` If `TRUE`, display messages.

## Value

An `sf` object of type `LINESTRING` representing commune boundaries.
Returns `NULL` if no commune intersects the input area.

## Details

The function retrieves commune polygons using
[`get_com_poly()`](https://sequoiapp.github.io/Rsequoia2/reference/get_com_poly.md),
converts them to line geometries using
[`poly_to_line()`](https://sequoiapp.github.io/Rsequoia2/reference/poly_to_line.md),
and optionally intersects them with a reduced convex buffer to limit
graphical extent.

## See also

[`get_com_poly()`](https://sequoiapp.github.io/Rsequoia2/reference/get_com_poly.md)
