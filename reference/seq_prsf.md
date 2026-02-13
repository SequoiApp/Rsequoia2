# Generate PRSF point layer for a Sequoia project

Retrieves PRSF point features intersecting and surrounding the project
area, and writes the resulting layer to disk.

## Usage

``` r
seq_prsf(dirname = ".", verbose = TRUE, overwrite = FALSE)
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
Returns `NULL` invisibly when no PRSF point features are found.

## Details

PRSF point features are retrieved using
[`get_prsf()`](https://sequoiapp.github.io/Rsequoia2/reference/get_prsf.md).

If no PRSF point features are found, the function returns `NULL`
invisibly and no file is written.

When features are present, the layer is written to disk using
[`seq_write()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_write.md)
with the key `"v.prsf.point"`.

## See also

[`get_prsf()`](https://sequoiapp.github.io/Rsequoia2/reference/get_prsf.md),
[`seq_write()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_write.md)
