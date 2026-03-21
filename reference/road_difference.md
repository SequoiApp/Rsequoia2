# Compute hierarchical road differences

Performs sequential differences between buffered road polygons according
to a given hierarchy.

## Usage

``` r
road_difference(buffers, order)
```

## Arguments

- buffers:

  A named list of `sf` polygon buffers.

- order:

  Character vector. Defines the processing order of road types.

## Value

An `sf` object with combined polygons after hierarchical differences.
