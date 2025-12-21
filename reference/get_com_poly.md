# Retrieve administrative boundary polygons around an area

Builds a convex buffer around the input geometry, retrieves commune
boundaries from BDTOPO, normalizes them, and returns a polygon layer.

## Usage

``` r
get_com_poly(x)
```

## Arguments

- x:

  An `sf` object used as the input area.

## Value

An `sf` object of type `POLYGON` containing commune boundaries, with
standardized fields as defined by `seq_normalize("com_poly")`. Returns
`NULL` if no commune intersects the search area.

## Details

The function builds a 2000 m convex buffer around `x`, retrieves the
BDTOPO commune layer (`BDTOPO_V3:commune`) intersecting this buffer, and
normalizes the resulting geometries and attributes.
