# Retrieve PRSF point features around an area

Builds a convex buffer around the input geometry, retrieves PRSF point
features and returns an `sf` point layer.

## Usage

``` r
get_prsf(x)
```

## Arguments

- x:

  An `sf` object defining the input area of interest.

## Value

An `sf` object containing PRSF point features.

## Details

The function creates a 5000 m convex buffer around the input geometry
`x` and retrieves PRSF point features before returns as a single `sf`
point layer.
