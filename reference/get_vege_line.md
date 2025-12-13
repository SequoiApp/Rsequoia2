# Retrieve forest vegetation lines around an area

Retrieve forest vegetation lines around an area

## Usage

``` r
get_vege_line(x)
```

## Arguments

- x:

  An `sf` object used as the input area.

## Value

An `sf` object containing forest vegetation line features with
standardized fields, including:

- `TYPE` — Vegetation type

  - `FOR` = Forest

- `SOURCE` — Data source (`IGNF_MASQUE-FORET`)

## Details

The function derives forest vegetation linear features from vegetation
polygons obtained with
[`get_vege_poly()`](https://mucau.github.io/Rsequoia2/reference/get_vege_poly.md).

Polygons are dissolved with a tolerance of 5 meters, converted to linear
geometries, and intersected with a 1499 m convex buffer around `x`.
Resulting geometries are cast to `LINESTRING` and normalized.

If no vegetation polygons are available, the function returns an empty
standardized `sf` object.

## See also

get_vege_poly
