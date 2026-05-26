# Read BRGM geology data for an area

Downloads and reads BRGM geology layers for the departments intersecting
`x`, then keeps only features intersecting a buffered envelope around
`x`.

## Usage

``` r
get_geol(
  x,
  key = c("carhab", "bdcharm50"),
  buffer = 100,
  cache = NULL,
  verbose = FALSE,
  overwrite = FALSE
)
```

## Arguments

- x:

  `sf` or `sfc`; Area used to determine departments and filter geology.

- key:

  `character`; Dataset to use. One of `"carhab"` or `"bdcharm50"`.

- buffer:

  `numeric`; Buffer distance, in meters, applied around `x` before
  spatial filtering. Default is `100`.

- cache:

  `character`; Optional cache directory. If `NULL`, the dataset-specific
  cache from
  [`seq_cache()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_cache.md)
  is used.

- verbose:

  `logical`; If `TRUE`, display progress messages.

- overwrite:

  `logical`; If `TRUE`, re-download archives even when they already
  exist in `cache`.

## Value

An `sf` object containing geology features intersecting the buffered
envelope of `x`, returned in EPSG:2154.

## Details

Supported datasets are `"carhab"` and `"bdcharm50"`.
