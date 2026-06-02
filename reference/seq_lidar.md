# Create LiDAR layers for a Sequoia project

Uses the project's *PARCA* layer to download, extract, and write LiDAR
HD altimetry rasters.

## Usage

``` r
seq_lidar(
  dirname = ".",
  key = c("mnt", "mnh"),
  buffer = 200,
  crs = 2154,
  cache = NULL,
  overwrite = FALSE,
  verbose = TRUE
)
```

## Arguments

- dirname:

  `character` Directory where the matrice file is located. Defaults to
  the current working directory.

- key:

  `character`; LiDAR product(s) to create. One or more of `"mnt"`,
  `"mns"` and `"mnh"`.

- buffer:

  `numeric`; Buffer distance, in meters, applied around `x` before
  masking.

- crs:

  `integer` or `character`; Target CRS of the returned raster. Defaults
  to EPSG:2154.

- cache:

  `character`; Cache directory. If `NULL`, the appropriate Rsequoia2
  LIDAR cache is used, see
  [`seq_cache()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_cache.md).

- overwrite:

  `logical`; If `TRUE`, re-download existing tiles.

- verbose:

  `logical`; If `TRUE`, display messages.

## Value

Invisibly returns a named `list` of created raster file paths.
