# Download patrimony vector layer

Downloads a vector layer with `frheritage` for the area covering `x`
expanded with a buffer.

## Usage

``` r
get_patrimony(x, key, buffer = 500, verbose = TRUE)
```

## Arguments

- x:

  `sf` or `sfc`; Geometry located in France.

- key:

  `character`; Layer to download. Must be one of from `get_keys("pat")`

- buffer:

  `numeric`; Buffer around `x` (in **meters**) used to enlarge the
  download area.

- verbose:

  `logical`; If `TRUE`, display messages.

## Value

`sf` object from `sf` package
