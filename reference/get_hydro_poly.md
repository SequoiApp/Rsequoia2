# Retrieve hydrographic polygons around an area

Retrieve hydrographic polygons around an area

## Usage

``` r
get_hydro_poly(x, buffer = 1000)
```

## Arguments

- x:

  An `sf` object used as the input area.

- buffer:

  `numeric`; Buffer around `x` (in **meters**) used to enlarge

## Value

An `sf` object containing hydrographic polygons with two fields:

- `TYPE` - hydrographic class

  - `RSO` = Reservoir or water tower

  - `SFP` = Permanent hydrographic surface

  - `SFI` = Intermittent hydrographic surface

- `NATURE` - Original BDTOPO nature field

- `NAME` - Official hydrographic name (when available)

## Details

The function retrieves BDTOPO layers within a convex buffer around `x`,
assigns the hydrographic types, and combines them into a single layer.
