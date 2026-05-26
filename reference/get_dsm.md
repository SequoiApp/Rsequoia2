# Download Digital Surface Model (DSM) raster from IGN RGEAlti

Downloads DSM from the IGN WMS service for the area covering `x`
expanded with a buffer. The result is returned as a masked `SpatRaster`,
clipped to the buffer geometry to keep file size minimal.

## Usage

``` r
get_dsm(x, buffer = 200, res = 1, crs = 2154, verbose = TRUE)
```

## Arguments

- x:

  `sf` or `sfc`; Geometry located in France.

- buffer:

  `numeric`; Buffer around `x` (in **meters**) used to enlarge the
  download area.

- res:

  `numeric`; resolution specified in the units of the coordinate system
  (see
  [`happign::get_wms_raster()`](https://paul-carteron.github.io/happign/reference/get_wms_raster.html))

- crs:

  `numeric` or `character`; CRS of the returned raster (see
  [`happign::get_wms_raster()`](https://paul-carteron.github.io/happign/reference/get_wms_raster.html))

- verbose:

  `logical`; If `TRUE`, display messages.

## Value

`SpatRaster` object from `terra` package

## See also

[`happign::get_wms_raster()`](https://paul-carteron.github.io/happign/reference/get_wms_raster.html)
