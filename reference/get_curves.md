# Retrieve hypsometric curves around an area

Builds a convex buffer around the input geometry, retrieves hypsometric
curves and returns an `sf` line layer.

## Usage

``` r
get_curves(x)
```

## Arguments

- x:

  An `sf` object defining the input area of interest.

## Value

An `sf` object containing hypsometric curves.

## Details

The function creates a 1000 m convex buffer around the input geometry
`x` and retrieves hypsometric curves before returns as a single `sf`
point layer.
