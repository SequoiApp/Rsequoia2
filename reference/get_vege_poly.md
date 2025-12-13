# Retrieve forest vegetation polygons around an area

Retrieve forest vegetation polygons around an area

## Usage

``` r
get_vege_poly(x)
```

## Arguments

- x:

  An `sf` object used as the input area.

## Value

An `sf` object containing forest vegetation polygons with standardized
fields, including:

- `TYPE` — Vegetation type

  - `FOR` = Forest

- `ORIGIN` — Source of the data (`IGNF_MASQUE-FORET`)

## Details

The function retrieves the IGN forest mask layer within a 1000 m convex
buffer around `x`. Retrieved polygons are then intersected with a 1500 m
convex buffer, cast to polygons, and normalized.

If no forest data are found, the function returns an empty `sf` object
with a standardized structure.
