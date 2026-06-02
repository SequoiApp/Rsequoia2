# Download LIDAR HD tiles

Downloads IGN LIDAR HD tiles intersecting an area of interest.

## Usage

``` r
download_lidar(
  x,
  key = c("mnt", "mnh"),
  cache = NULL,
  overwrite = FALSE,
  verbose = TRUE,
  max_tries = 3
)
```

## Arguments

- x:

  `sf` or `sfc`; Area used to select intersecting LIDAR tiles.

- key:

  `character`; LIDAR product to download. One of `"mnt"`, `"mns"` or
  `"mnh"`.

- cache:

  `character`; Cache directory. If `NULL`, the appropriate Rsequoia2
  LIDAR cache is used, see
  [`seq_cache()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_cache.md).

- overwrite:

  `logical`; If `TRUE`, re-download existing tiles.

- verbose:

  `logical`; If `TRUE`, display messages.

- max_tries:

  `integer`; Maximum number of download attempts.

## Value

Invisibly returns a `character` vector of local tile paths.
