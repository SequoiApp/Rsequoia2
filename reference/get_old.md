# Retrieve OLD features around an area

Builds a convex buffer around the input geometry, retrieves OLD features
and returns an `sf` point layer.

## Usage

``` r
get_old(x, verbose = TRUE)
```

## Arguments

- x:

  An `sf` object defining the input area of interest.

- verbose:

  `logical` If `TRUE`, display messages.

## Value

An `sf` object containing OLD features.

## Details

The function creates a 5000 m convex buffer around the input geometry
`x` and retrieves OLD features before returns as a single `sf` point
layer.
