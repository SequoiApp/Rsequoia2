# Compute a Aspect Raster from a DEM

Computes a aspect raster (in degrees) either by:

- downloading a DEM automatically using `x`, or

- using a DEM raster supplied manually through the `dem` argument.

## Usage

``` r
get_aspect(x = NULL, dem = NULL, agg = 5, verbose = TRUE, ...)
```

## Arguments

- x:

  `sf` or `sfc`; Geometry located in France.

- dem:

  `SpatRaster` representing ground elevation (DEM). Must be supplied
  only when `x` is `NULL`.

- agg:

  `numeric`; Target resolution (in meters) to which the DEM is
  aggregated if its native resolution is finer. Default: `5`.

- verbose:

  `logical`; If `TRUE`, display messages.

- ...:

  Additional parameters passed to
  [`get_dem()`](https://sequoiapp.github.io/Rsequoia2/reference/get_dem.md)
  when `x` is supplied.

## Value

A `SpatRaster` containing aspect values in degrees.

## Details

When `x` is provided, the DEM is obtained with
[`get_dem()`](https://sequoiapp.github.io/Rsequoia2/reference/get_dem.md),
using by default a resolution equal to `agg`. When `x` is not provided,
a DEM must be supplied via `dem`.

If the DEM resolution is finer than `agg`, the raster is aggregated to
avoid artefacts and reduce computation time.

Aspect is computed with
[`terra::terrain()`](https://rspatial.github.io/terra/reference/terrain.html)
using Horn's 8-neighbor algorithm, and returned in **degrees**.

Aggregation is performed with
[`terra::aggregate()`](https://rspatial.github.io/terra/reference/aggregate.html)
using a mean function. This is recommended when high-resolution DEMs
(\<5 m) would otherwise generate line artefacts.

## Examples

``` r
if (FALSE) { # \dontrun{
# Automatic download mode
s <- get_aspect(x = my_polygon, buffer = 200)

# Manual mode
dem <- get_dem(my_polygon)
s <- get_aspect(dem = dem)
} # }
```
