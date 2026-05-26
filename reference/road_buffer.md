# Create a road buffer polygon

Generates a polygon buffer for a given road type from a line layer.

## Usage

``` r
road_buffer(x, type, dist)
```

## Arguments

- x:

  An `sf` object with road line geometries.

- type:

  Character. Road type to process (e.g., "RN", "RD").

- dist:

  Numeric. Buffer distance in layer units.

## Value

An `sf` object containing a single (multi)polygon geometry for the road
type.
