# Retrieve and classify toponymic point features around an area

Builds a convex buffer around the input geometry, retrieves toponymic
point features from the BDTOPO dataset, classifies them by thematic type
(hydrographic, vegetation, or other), normalizes the result, and returns
a standardized `sf` point layer.

## Usage

``` r
get_toponyme(x)
```

## Arguments

- x:

  An `sf` object defining the input area of interest.

## Value

An `sf` object containing toponymic point features with standardized
attribute fields:

- `TYPE` — Toponym class

  - `THYDR` = Hydrographic toponym

  - `TVEGE` = Vegetation-related toponym

  - `TYPON` = Other toponyms

- `NATURE` — Original BDTOPO object nature

- `NAME` — Official toponym name (when available)

- `SOURCE` — Data source identifier (`IGNF_BDTOPO_V3`)

## Details

The function creates a 1000 m convex buffer around the input geometry
`x` and retrieves toponymic point features from the BDTOPO toponymy
layer. Retrieved features are classified into thematic types based on
their object class, attribute names are standardized, and geometries are
normalized before being returned as a single `sf` point layer.
