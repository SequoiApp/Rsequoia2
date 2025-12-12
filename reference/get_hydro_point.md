# Retrieve and assemble hydrographic points around an area

Builds a convex buffer around the input geometry, retrieves hydrographic
point features from BDTOPO, normalizes them, and returns a combined `sf`
point layer.

## Usage

``` r
get_hydro_point(x)
```

## Arguments

- x:

  An `sf` object used as the input area.

## Value

An `sf` object containing hydrographic point features with four fields:

- `TYPE` — hydrographic class

  - `MAR` = Pond

- `NATURE` — Original BDTOPO nature field

- `NAME` — Official hydrographic name (when available)

- `ROTATION` — Rotation or orientation

## Details

The function retrieves BDTOPO hydrographic point details
(`detail_hydrographique`) within a 1000 m convex buffer around `x`,
assigns the hydrographic type, normalizes the geometries, and returns
them as a single combined layer.
