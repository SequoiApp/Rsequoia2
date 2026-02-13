# Download RGB and/or IRC orthophotos for a Sequoia project

Downloads one or several orthophotos (RGB and/or IRC) from the IGN WMTS
service for the `parca` layer of a Sequoia project.

## Usage

``` r
seq_ortho(
  dirname = ".",
  type = c("irc", "rgb"),
  buffer = 200,
  zoom = 12,
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

  `character` One or several orthophoto types to download. Must be one
  or both of:

  - `"rgb"` - true-color orthophoto

  - `"irc"` - near-infrared orthophoto

- buffer:

  `numeric`; Buffer around `x` (in **meters**) used to enlarge the
  download area.

- zoom:

  `integer` between 0 and 21. The smaller the zoom level, the less
  precise the resolution(see
  [`happign::get_wmts()`](https://paul-carteron.github.io/happign/reference/get_wmts.html))

- crs:

  `numeric` or `character`; CRS of the returned raster (see
  [`happign::get_wmts()`](https://paul-carteron.github.io/happign/reference/get_wmts.html))

- overwrite:

  `logical`; If `TRUE`, file is overwritten.

- verbose:

  `logical`; If `TRUE`, display messages.

## Value

A named list of file paths written by
[`seq_write()`](https://mucau.github.io/Rsequoia2/reference/seq_write.md),
one per `type`.

## Details

This function is a convenience wrapper looping over
[`get_ortho()`](https://mucau.github.io/Rsequoia2/reference/get_ortho.md),
allowing the user to download both products in one call and
automatically write them to the project directory using
[`seq_write()`](https://mucau.github.io/Rsequoia2/reference/seq_write.md).

## See also

[`get_ortho()`](https://mucau.github.io/Rsequoia2/reference/get_ortho.md),
[`seq_write()`](https://mucau.github.io/Rsequoia2/reference/seq_write.md)
