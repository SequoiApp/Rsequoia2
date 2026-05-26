# Get roadway polygons from lines or polygons

Generates normalized roadway polygons from either road line features or
completed cadastral voids polygons.

## Usage

``` r
get_roadway_poly(x)
```

## Arguments

- x:

  An `sf` object containing either:

  - LINE geometries representing road centerlines, or

  - POLYGON geometries representing completed cadastral gaps.

## Value

An `sf` object with POLYGON or MULTIPOLYGON geometries normalized as
roadway polygons.

## Details

The function dispatches internally depending on the geometry type:

- If `x` contains LINESTRING geometries, road polygons are created using
  hierarchical buffering and masking via
  [`road_to_roadway()`](https://sequoiapp.github.io/Rsequoia2/reference/road_to_roadway.md).

- If `x` contains POLYGON geometries, the polygons are cleaned and
  normalized via
  [`voids_to_roadway()`](https://sequoiapp.github.io/Rsequoia2/reference/voids_to_roadway.md).

The resulting polygons are standardized using
[`seq_normalize()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_normalize.md)
with the `"road_poly"` schema.
