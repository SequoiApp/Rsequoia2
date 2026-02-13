# Generates infrastructure polygon, line and point layers for a Sequoia project.

This function is a convenience wrapper around
[`get_infra_poly()`](https://sequoiapp.github.io/Rsequoia2/reference/get_infra_poly.md),
[`get_infra_line()`](https://sequoiapp.github.io/Rsequoia2/reference/get_infra_line.md)
and
[`get_infra_point()`](https://sequoiapp.github.io/Rsequoia2/reference/get_infra_point.md),
allowing the user to download all products in one call and automatically
write them to the project directory using
[`seq_write()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_write.md).

## Usage

``` r
seq_infra(dirname = ".", verbose = TRUE, overwrite = FALSE)
```

## Arguments

- dirname:

  `character` Path to the directory. Defaults to the current working
  directory.

- verbose:

  `logical` If `TRUE`, display messages.

- overwrite:

  `logical` If `TRUE`, file is overwritten.

## Value

A named list of file paths written by
[`seq_write()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_write.md),
one per hydrographic layer.

## Details

Each infrastructure layer is always written to disk using
[`seq_write()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_write.md),
even when it contains no features (`nrow == 0`).

Informational messages are displayed to indicate whether a layer
contains features or is empty.

## See also

[`get_infra_poly()`](https://sequoiapp.github.io/Rsequoia2/reference/get_infra_poly.md),
[`get_infra_line()`](https://sequoiapp.github.io/Rsequoia2/reference/get_infra_line.md),
[`get_infra_point()`](https://sequoiapp.github.io/Rsequoia2/reference/get_infra_point.md),
[`seq_write()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_write.md)
