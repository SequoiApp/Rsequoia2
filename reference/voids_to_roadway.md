# Get polygons from the completed cadastral gaps layer

Cleans polygon topology and normalizes the result as road polygons.

## Usage

``` r
voids_to_roadway(x)
```

## Arguments

- x:

  An \`sf\` polygon object from \[Rsequoia2::get_voids()\] completed by
  user

## Value

An \`sf\` object with POLYGON or MULTIPOLYGON geometries normalized as
road polygons.
