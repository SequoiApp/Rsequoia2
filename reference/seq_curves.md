# Generate hypsometric curves line layer for a Sequoia project

Retrieves hypsometric curves line features intersecting and surrounding
the project area and writes the resulting layer to disk.

## Usage

``` r
seq_curves(dirname = ".", verbose = TRUE, overwrite = FALSE)
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
[`seq_write()`](https://mucau.github.io/Rsequoia2/reference/seq_write.md).
Returns `NULL` invisibly when no hypsometric curves features are found.

## Details

Hypsometric curves line features are retrieved using
[`get_curves()`](https://mucau.github.io/Rsequoia2/reference/get_curves.md).

If no hypsometric curves features are found, the function returns `NULL`
invisibly and no file is written.

When features are present, the layer is written to disk using
[`seq_write()`](https://mucau.github.io/Rsequoia2/reference/seq_write.md)
with the key `"v.curves.line"`.

## See also

[`get_curves()`](https://mucau.github.io/Rsequoia2/reference/get_curves.md),
[`seq_write()`](https://mucau.github.io/Rsequoia2/reference/seq_write.md)
