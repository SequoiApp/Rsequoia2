# Generate toponymic point layer for a Sequoia project

Retrieves toponymic point features intersecting and surrounding the
project area, classifies them by thematic type, and writes the resulting
layer to disk.

## Usage

``` r
seq_toponyme(dirname = ".", buffer = 1000, verbose = TRUE, overwrite = FALSE)
```

## Arguments

- dirname:

  `character` Directory where the matrice file is located. Defaults to
  the current working directory.

- buffer:

  `numeric`; Buffer around `x` (in **meters**) used to enlarge

- verbose:

  `logical` If `TRUE`, display messages.

- overwrite:

  `logical` If `TRUE`, file is overwritten.

## Value

Invisibly returns a named list of file paths written by
[`seq_write()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_write.md).
Returns `NULL` invisibly when no toponymic features are found.

## Details

Toponymic point features are retrieved using
[`get_toponyme()`](https://sequoiapp.github.io/Rsequoia2/reference/get_toponyme.md).

If no toponymic features are found, the function returns `NULL`
invisibly and no file is written.

When features are present, the layer is written to disk using
[`seq_write()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_write.md)
with the key `"v.toponyme.point"`.

## See also

[`get_toponyme()`](https://sequoiapp.github.io/Rsequoia2/reference/get_toponyme.md),
[`seq_write()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_write.md)
