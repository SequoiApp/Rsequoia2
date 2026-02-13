# Generate road layers for a Sequoia project

Retrieves road features intersecting and surrounding the project area,
classifies them by thematic type, and writes the resulting layer to
disk.

## Usage

``` r
seq_road(dirname = ".", verbose = TRUE, overwrite = FALSE)
```

## Arguments

- dirname:

  `character` Path to the project directory. Defaults to the current
  working directory.

- verbose:

  `logical`; whether to display informational messages. Defaults to
  `TRUE`.

- overwrite:

  `logical`; whether to overwrite existing files. Defaults to `FALSE`.

## Value

Invisibly returns a named list of file paths written by
[`seq_write()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_write.md).
Returns `NULL` invisibly when no road features are found.

## Details

Road features are retrieved using
[`get_road()`](https://sequoiapp.github.io/Rsequoia2/reference/get_road.md).

If no road features are found, the function returns `NULL` invisibly and
no file is written.

When features are present, the layer is written to disk using
[`seq_write()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_write.md)
with the key `"v.road.topo.line"`.

## See also

[`get_road()`](https://sequoiapp.github.io/Rsequoia2/reference/get_road.md),
[`seq_write()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_write.md)
