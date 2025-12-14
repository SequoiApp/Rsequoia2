# Generates hydrographic polygon, line and point layers for a Sequoia project.

This function is a convenience wrapper around
[`get_hydro_poly()`](https://mucau.github.io/Rsequoia2/reference/get_hydro_poly.md),
[`get_hydro_line()`](https://mucau.github.io/Rsequoia2/reference/get_hydro_line.md)
and
[`get_hydro_point()`](https://mucau.github.io/Rsequoia2/reference/get_hydro_point.md),
allowing the user to download all products in one call and automatically
write them to the project directory using
[`seq_write()`](https://mucau.github.io/Rsequoia2/reference/seq_write.md).

## Usage

``` r
seq_hydro(dirname = ".", verbose = TRUE, overwrite = FALSE)
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
[`seq_write()`](https://mucau.github.io/Rsequoia2/reference/seq_write.md),
one per hydrographic layer.

## Details

Each hydrographic layer is always written to disk using
[`seq_write()`](https://mucau.github.io/Rsequoia2/reference/seq_write.md),
even when it contains no features (`nrow == 0`).

Informational messages are displayed to indicate whether a layer
contains features or is empty.

## See also

[`get_hydro_poly()`](https://mucau.github.io/Rsequoia2/reference/get_hydro_poly.md),
[`get_hydro_line()`](https://mucau.github.io/Rsequoia2/reference/get_hydro_line.md),
[`get_hydro_point()`](https://mucau.github.io/Rsequoia2/reference/get_hydro_point.md),
[`seq_write()`](https://mucau.github.io/Rsequoia2/reference/seq_write.md)
