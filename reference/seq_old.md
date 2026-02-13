# Generate OLD layer for a Sequoia project

Retrieves OLD features intersecting and surrounding the project area,
and writes the resulting layer to disk.

## Usage

``` r
seq_old(dirname = ".", verbose = TRUE, overwrite = FALSE)
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
Returns `NULL` invisibly when no OLD features are found.

## Details

OLD features are retrieved using
[`get_old()`](https://sequoiapp.github.io/Rsequoia2/reference/get_old.md).

If no OLD features are found, the function returns `NULL` invisibly and
no file is written.

When features are present, the layer is written to disk using
[`seq_write()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_write.md)
with the key `"v.old.poly"`.

## See also

[`get_prsf()`](https://sequoiapp.github.io/Rsequoia2/reference/get_prsf.md),
[`seq_write()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_write.md)
