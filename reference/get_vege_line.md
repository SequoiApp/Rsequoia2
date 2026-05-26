# Retrieve forest vegetation lines around an area

Retrieve forest vegetation lines around an area

## Usage

``` r
get_vege_line(x)
```

## Arguments

- x:

  `sf` or `sfc`; Geometry located in France.

## Value

An `sf` object containing forest vegetation line features.

## Details

The function derives forest vegetation linear features from vegetation
polygons obtained with
[`get_vege_poly()`](https://sequoiapp.github.io/Rsequoia2/reference/get_vege_poly.md).

GIS workflow :

- Polygon are intersected to a 1500m buffer;

- Polygon are cast to `LINESTRING`;

- Line are intersected with a 1500 buffer to remove intersection line
  from first step

If no vegetation polygons are available, the function returns an empty
standardized `sf` object.

## See also

get_vege_poly
