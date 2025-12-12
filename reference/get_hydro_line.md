# Retrieve and assemble hydrographic lines around an area

Builds a convex buffer around the input geometry, retrieves hydrographic
line features from BDTOPO, normalizes them, and returns a combined `sf`
linestring layer.

## Usage

``` r
get_hydro_line(x)
```

## Arguments

- x:

  An `sf` object used as the input area.

## Value

An `sf` object containing hydrographic line features with four fields:

- `TYPE` — hydrographic class

  - `RUI` = Permanent hydrographic line

  - `RIN` = Intermittent hydrographic line

- `NATURE` — Original BDTOPO nature field

- `NAME` — Official hydrographic name (when available)

- `OFFSET` — Offset information

## Details

The function retrieves BDTOPO hydrographic line segments
(`troncon_hydrographique`) within a 1000 m convex buffer around `x`,
assigns the hydrographic types, normalizes the geometries, and returns
them as a single combined layer.
