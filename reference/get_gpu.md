# Download GPU vector layer

Downloads a vector layer with `hhapign` for the area covering `x`
expanded with a buffer.

## Usage

``` r
get_gpu(x, key, verbose = TRUE)
```

## Arguments

- x:

  `sf` or `sfc`; Geometry located in France.

- key:

  `character`; Layer to download. Must be one of from
  `get_keys("gpu", reduce = FALSE)`

- verbose:

  `logical`; If `TRUE`, display messages.

## Value

`sf` object from `sf` package
