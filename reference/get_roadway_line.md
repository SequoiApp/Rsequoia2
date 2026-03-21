# Derive roadway lines from roadway polygons

Generates roadway border line features from roadway polygons by
extracting internal edges using a buffer difference and convex hull
splitting.

## Usage

``` r
get_roadway_line(x)
```

## Arguments

- x:

  An `sf` polygon object produced by
  [`get_roadway_poly()`](https://sequoiapp.github.io/Rsequoia2/reference/get_roadway_poly.md).

## Value

An `sf` object with LINESTRING geometries normalized as roadway lines.

## Details

The function derives line features from roadway polygons through the
following steps:

1.  Polygon boundaries are cast to MULTILINESTRING.

2.  Internal edges are extracted using a negative buffer difference.

3.  The resulting lines are split using points derived from a simplified
    convex hull of the polygons.

4.  The result is normalized with
    [`seq_normalize()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_normalize.md)
    using the `"road_line"` schema.
