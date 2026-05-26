# Create a road external mask

Generates a mask polygon around road lines, including buffered
intersection points.

## Usage

``` r
road_mask(x, dist)
```

## Arguments

- x:

  An `sf` object with road lines.

- dist:

  Numeric. Buffer distance in layer units.

## Value

An `sf` object containing a polygon mask of the road network.
