# Generate toponymic point layer for a Sequoia project

Retrieves toponymic point features intersecting and surrounding the
project area, classifies them by thematic type, and writes the resulting
layer to disk.

## Usage

``` r
seq_toponyme(dirname = ".", verbose = TRUE, overwrite = FALSE)
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
Returns `NULL` invisibly when no toponymic features are found.

## Details

Toponymic point features are retrieved using
[`get_toponyme()`](https://mucau.github.io/Rsequoia2/reference/get_toponyme.md).

If no toponymic features are found, the function returns `NULL`
invisibly and no file is written.

When features are present, the layer is written to disk using
[`seq_write()`](https://mucau.github.io/Rsequoia2/reference/seq_write.md)
with the key `"v.toponyme.point"`.

## See also

[`get_toponyme()`](https://mucau.github.io/Rsequoia2/reference/get_toponyme.md),
[`seq_write()`](https://mucau.github.io/Rsequoia2/reference/seq_write.md)
