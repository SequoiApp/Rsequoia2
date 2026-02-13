# Compute Canopy Height Model (CHM)

Computes a **Canopy Height Model** (CHM = DSM - DEM) either by:

- downloading the necessary DEM and DSM from IGN WMS services using `x`,
  or

- using DEM and DSM rasters directly supplied by the user.

## Usage

``` r
get_chm(x = NULL, dem = NULL, dsm = NULL, minmax = c(0, 50), ...)
```

## Arguments

- x:

  `sf` or `sfc`; Geometry located in France.

- dem:

  `SpatRaster` representing ground elevation (DEM). Must be supplied
  only when `x` is `NULL`.

- dsm:

  A `SpatRaster` representing surface elevation (DSM). Must be supplied
  only when `x` is `NULL`.

- minmax:

  `numeric` length-2 vector giving the accepted CHM range as
  `c(min, max)`. Default: `c(0, 50)`.

- ...:

  Additional parameters passed to
  [`get_dem()`](https://mucau.github.io/Rsequoia2/reference/get_dem.md)
  and
  [`get_dem()`](https://mucau.github.io/Rsequoia2/reference/get_dem.md)
  when `x` is supplied.

## Value

A `SpatRaster` containing the CHM.

## Details

When `x` is provided, both DEM and DSM are automatically fetched via
[`get_dem()`](https://mucau.github.io/Rsequoia2/reference/get_dem.md)
and
[`get_dsm()`](https://mucau.github.io/Rsequoia2/reference/get_dsm.md).
When `x` is not provided, both `dem` and `dsm` must be manually
supplied.

Output values outside `minmax` are clamped: negative values are set to
`NA` and excessively high values are capped.

## Examples

``` r
if (FALSE) { # \dontrun{
# Automatic download mode
chm <- get_chm(x = my_polygon)

# Manual mode
dem <- get_dem(my_polygon)
dsm <- get_dsm(my_polygon)
chm <- get_chm(dem = dem, dsm = dsm)
} # }
```
