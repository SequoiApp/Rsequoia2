# Retrieve OLD features around an area

Builds a convex buffer around the input geometry, retrieves OLD features
and returns an `sf` point layer.

## Usage

``` r
get_old(x, buffer = 1000, verbose = TRUE)
```

## Arguments

- x:

  An `sf` object defining the input area of interest.

- buffer:

  `numeric`; Buffer around `x` (in **meters**) used to enlarge

- verbose:

  `logical` If `TRUE`, display messages.

## Value

An `sf` object containing OLD features.

## Details

The function creates a convex buffer around the input geometry `x` and
retrieves OLD features before returns as a single `sf` point layer.
