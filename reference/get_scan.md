# Download IGN scanned maps (SCAN25, SCAN100, OACI or IGN maps)

Downloads a scanned map layer from the IGN WMS service for the area
covering `x`, expanded with a buffer. The result is returned as a
`SpatRaster` with a fixed spatial resolution.

## Usage

``` r
get_scan(
  x,
  type = c("scan25", "scan100", "oaci", "carte_ign"),
  buffer = 1000,
  res = 0.8,
  crs = 2154,
  overwrite = FALSE,
  verbose = TRUE
)
```

## Arguments

- x:

  `sf` or `sfc`; Geometry located in France.

- type:

  `character`; Type of scanned map to download. Must be one of:

  - `"scan25"`: SCAN25 topographic map

  - `"scan100"`: SCAN100 topographic map

  - `"oaci"`: OACI aeronautical chart

  - `"carte_ign"`: standard IGN map background

- buffer:

  `numeric`; Buffer around `x` (in **meters**) used to enlarge the
  download area.

- res:

  `numeric`; Spatial resolution of the output raster (in map units,
  usually meters).

- crs:

  `numeric` or `character`; CRS of the returned raster.

- overwrite:

  `logical`; If `TRUE`, the cached file is overwritten.

- verbose:

  `logical`; If `TRUE`, display GDAL messages during download.

## Value

A `SpatRaster` object from the `terra` package.

## Details

Supported layers include SCAN25, SCAN100, OACI aeronautical charts and
standard IGN maps.

The scanned map is retrieved for the full bounding box (bbox) of `x`
expanded with the specified buffer.

To improve performance and avoid repeated downloads, the raster is
cached in a temporary directory. The cache key depends on the bounding
box, resolution and CRS. Set `overwrite = TRUE` to force a new download.

## See also

[`sf::gdal_utils()`](https://r-spatial.github.io/sf/reference/gdal_utils.html)

## Examples

``` r
if (FALSE) { # \dontrun{
p <- sf::st_sfc(
  sf::st_point(c(-4.3727, 47.7982)),
  crs = 4326
)

scan25 <- get_scan(p, type = "scan25", buffer = 500)
scan100 <- get_scan(p, type = "scan100", buffer = 500)

terra::plotRGB(scan25)
} # }
```
