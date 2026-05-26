# Retrieve pedology polygon features around an area

Retrieves pedological polygon features from the INRA soil map
intersecting an area of interest and computes surface attributes.

## Usage

``` r
get_pedology(x)
```

## Arguments

- x:

  An `sf` object defining the input area of interest.

## Value

An `sf` object containing pedology polygon features intersecting the
input geometry, with additional surface fields.

## Details

The function retrieves pedology polygon features from the
`INRA.CARTE.SOLS:geoportail_vf` layer intersecting the input geometry
`x`. The resulting geometries are intersected with `x`, cast to
polygons, and surface attributes are computed using
[`ua_generate_area()`](https://sequoiapp.github.io/Rsequoia2/reference/ua_generate_area.md).

## See also

[`ua_generate_area()`](https://sequoiapp.github.io/Rsequoia2/reference/ua_generate_area.md)
