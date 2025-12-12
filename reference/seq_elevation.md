# Download and Compute Elevation Products for a Sequoia Project

Downloads elevation datasets (DEM, DSM) from the IGN WMS, computes
derived products (CHM, slope, aspect), and writes all resulting rasters
into the Sequoia project directory.

## Usage

``` r
seq_elevation(
  dirname = ".",
  buffer = 200,
  res = 1,
  crs = 2154,
  overwrite = FALSE,
  verbose = TRUE
)
```

## Arguments

- dirname:

  `character` Directory where the matrice file is located. Defaults to
  the current working directory.

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

- overwrite:

  `logical` If `TRUE`, file is overwritten.

- verbose:

  `logical`; If `TRUE`, display messages.

## Value

A named list of file paths written by
[`seq_write()`](https://mucau.github.io/Rsequoia2/reference/seq_write.md),
one per `type`.

## Details

This function is a high-level convenience wrapper around:

- [`get_dem()`](https://mucau.github.io/Rsequoia2/reference/get_dem.md)
  — Digital Elevation Model

- [`get_dsm()`](https://mucau.github.io/Rsequoia2/reference/get_dsm.md)
  — Digital Surface Model

- [`get_chm()`](https://mucau.github.io/Rsequoia2/reference/get_chm.md)
  — Canopy Height Model

- [`get_slope()`](https://mucau.github.io/Rsequoia2/reference/get_slope.md)
  — Slope raster

- [`get_aspect()`](https://mucau.github.io/Rsequoia2/reference/get_aspect.md)
  — Aspect raster

It automatically reads the project's `parca` layer, downloads the
necessary elevation data for its extent (with an optional buffer),
computes all derived products, and saves them using
[`seq_write()`](https://mucau.github.io/Rsequoia2/reference/seq_write.md).

## See also

[`get_dem()`](https://mucau.github.io/Rsequoia2/reference/get_dem.md),
[`get_dsm()`](https://mucau.github.io/Rsequoia2/reference/get_dsm.md),
[`get_chm()`](https://mucau.github.io/Rsequoia2/reference/get_chm.md),
[`get_slope()`](https://mucau.github.io/Rsequoia2/reference/get_slope.md),
[`get_aspect()`](https://mucau.github.io/Rsequoia2/reference/get_aspect.md)
