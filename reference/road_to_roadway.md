# Convert road lines to hierarchical polygons

Creates hierarchical road polygons from line features using buffers,
differences, and an external mask.

## Usage

``` r
road_to_roadway(x, dist = 3.5)
```

## Arguments

- x:

  An `sf` object with road line geometries.

- dist:

  Numeric. Base buffer distance (default 3.5 units).

## Value

An `sf` object with the final road polygons.
