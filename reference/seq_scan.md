# Download scanned IGN maps for a Sequoia project

Downloads one or several scanned map products (SCAN25, SCAN100, OACI or
standard IGN maps) from the IGN WMS service for the `parca` layer of a
Sequoia project.

## Usage

``` r
seq_scan(
  dirname = ".",
  type = c("scan25", "scan100", "oaci", "carte_ign"),
  buffer = 1000,
  res = 0.8,
  crs = 2154,
  overwrite = FALSE,
  verbose = TRUE
)
```

## Arguments

- dirname:

  `character` Directory where the matrice file is located. Defaults to
  the current working directory.

- type:

  `character`; One or several scanned map types to download. Must be one
  or more of:

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

A named list of file paths written by
[`seq_write()`](https://mucau.github.io/Rsequoia2/reference/seq_write.md),
one per `type`.

## Details

This function is a convenience wrapper looping over
[`get_scan()`](https://mucau.github.io/Rsequoia2/reference/get_scan.md),
allowing the user to download several scanned map products in one call
and automatically write them to the project directory using
[`seq_write()`](https://mucau.github.io/Rsequoia2/reference/seq_write.md).

## See also

[`get_scan()`](https://mucau.github.io/Rsequoia2/reference/get_scan.md),
[`seq_write()`](https://mucau.github.io/Rsequoia2/reference/seq_write.md)
